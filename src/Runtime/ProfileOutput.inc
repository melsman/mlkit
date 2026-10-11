/* Portable wire format: magic/version, then LE uint32 payload length, byte tag,
 * fixed-order LE uint64 counters, and LE uint32-length-prefixed byte strings.
 * Never serialize native structs. One buffered stdio write per record. */
static unsigned char *wire;
static size_t wire_capacity;
static void little_endian(unsigned char *p, uint64_t n, size_t bytes) {
  for (size_t i = 0; i < bytes; i++, n >>= 8) p[i] = (unsigned char)n;
}
static void emit_record(unsigned char tag, const uint64_t *values, size_t count,
                        const char *const *strings, size_t nstrings, const size_t *lengths) {
  size_t size = 1 + count*8;
  for (size_t i = 0; i < nstrings; i++) {
    size_t n = lengths ? lengths[i] : strlen(strings[i]);
    if (n > UINT32_MAX-4 || size > UINT32_MAX-4-n) fail("profile record too large");
    size += 4+n;
  }
  if (size > SIZE_MAX-4) fail("profile record too large");
  if (size+4 > wire_capacity) {
    unsigned char *p = realloc(wire,size+4);
    if (!p) fail("out of memory");
    wire = p; wire_capacity = size+4;
  }
  little_endian(wire,size,4); wire[4] = tag;
  size_t offset = 5;
  for (size_t i = 0; i < count; i++, offset += 8) little_endian(wire+offset,values[i],8);
  for (size_t i = 0; i < nstrings; i++) {
    size_t n = lengths ? lengths[i] : strlen(strings[i]);
    little_endian(wire+offset,n,4); offset += 4;
    memcpy(wire+offset,strings[i],n); offset += n;
  }
  if (fwrite(wire,1,size+4,output) != size+4) fail("cannot write profile record");
}
#define NUMS(...) (const uint64_t[]){__VA_ARGS__}, sizeof((const uint64_t[]){__VA_ARGS__})/sizeof(uint64_t)
#define STRS(...) (const char *const[]){__VA_ARGS__}, sizeof((const char *const[]){__VA_ARGS__})/sizeof(char *), NULL
#define NO_STRINGS NULL, 0, NULL

/* Shared encoder/metadata for region-profile and ordinary runtime variants. */
#include "TimeProfile.inc"

__attribute__((weak)) const volatile uintptr_t mlkit_rp_code_scope = 0;
#ifdef __APPLE__
__attribute__((weak)) const MlkitProfileCodeUnit * const volatile mlkit_rp_code_units[] = {NULL};
#else
extern const MlkitProfileCodeUnit * const volatile mlkit_rp_code_units[] __attribute__((weak));
#endif
#include "RuntimeCode.inc"
/* Before sampling is armed, catalog compiler ranges and named C ranges in
 * the static executable. UUID and load base identify it independently of ASLR. */
static void emit_code_metadata(void) {
  const char *scope = mlkit_rp_code_scope == 2 ? "unsupported-repl" : "unavailable";
#if defined(__APPLE__) && defined(__aarch64__)
  if (mlkit_rp_code_scope == 1) {
    const struct mach_header_64 *header = (const struct mach_header_64 *)_dyld_get_image_header(0);
    if (!header || header->magic != MH_MAGIC_64) fail("invalid executable image");
    const unsigned char *command = (const unsigned char *)(header+1);
    const unsigned char *limit = command + header->sizeofcmds;
    uintptr_t text_begin = 0, text_end = 0;
    char uuid[33] = {0};
    for (uint32_t i = 0; i < header->ncmds; i++) {
      if ((size_t)(limit-command) < sizeof(struct load_command)) fail("short image command");
      const struct load_command *lc = (const struct load_command *)command;
      if (lc->cmdsize < sizeof(*lc) || lc->cmdsize > (size_t)(limit-command))
        fail("invalid image command");
      if (lc->cmd == LC_UUID) {
        if (lc->cmdsize < sizeof(struct uuid_command)) fail("short image UUID");
        const struct uuid_command *id = (const struct uuid_command *)lc;
        for (size_t j = 0; j < 16; j++) sprintf(uuid+2*j,"%02x",id->uuid[j]);
      }
      if (lc->cmd == LC_SEGMENT_64) {
        if (lc->cmdsize < sizeof(struct segment_command_64)) fail("short image segment");
        const struct segment_command_64 *segment = (const struct segment_command_64 *)lc;
        if (segment->nsects > (lc->cmdsize-sizeof(*segment))/sizeof(struct section_64))
          fail("short image sections");
        const struct section_64 *sections = (const struct section_64 *)(segment+1);
        for (uint32_t j = 0; j < segment->nsects; j++)
          if (!strncmp(sections[j].segname,"__TEXT",16) && !strncmp(sections[j].sectname,"__text",16)) {
            text_begin = sections[j].addr + _dyld_get_image_vmaddr_slide(0);
            text_end = text_begin + sections[j].size;
          }
      }
      command += lc->cmdsize;
    }
    if (!uuid[0]) scope = "missing-image-id";
    else {
      uintptr_t base = (uintptr_t)header;
      uint64_t function_count = 0;
      for (size_t i = 0; mlkit_rp_code_units[i]; i++) {
        const MlkitProfileCodeUnit *unit = mlkit_rp_code_units[i];
        if (unit->version != 1 && unit->version != 2) fail("unsupported function metadata; rebuild ML libraries");
        if (unit->count > UINT64_MAX-function_count) fail("function metadata count overflow");
        function_count += unit->count;
      }
      collect_runtime_code(text_begin,text_end,uuid);
      function_count += runtime_code_count;
      emit_record(20,NUMS(1,function_count),STRS("static-executable"));
      emit_record(21,NUMS(1,base),STRS(uuid,_dyld_get_image_name(0)));
      for (size_t i = 0; mlkit_rp_code_units[i]; i++) {
        const MlkitProfileCodeUnit *unit = mlkit_rp_code_units[i];
        for (uintptr_t j = 0; j < unit->count; j++) {
          const MlkitProfileFunction *f = &unit->functions[j];
          if (f->begin < text_begin || f->end > text_end || f->begin >= f->end)
            fail("function range outside static executable text");
          emit_record(22,NUMS(1,f->begin-base,f->end-base),
                      STRS(f->unit->data,f->function->data,f->source->data,f->ir_identity->data));
        }
      }
      for (size_t i = 0; i < runtime_code_count; i++) {
        const RuntimeCode *f = &runtime_code[i];
        char unit[64];
        snprintf(unit,sizeof(unit),"<runtime/C>@%llx",(unsigned long long)(f->begin-base));
        emit_record(22,NUMS(1,f->begin-base,f->end-base),
                    STRS(unit,f->name,"<runtime>",""));
      }
      free(runtime_code); runtime_code = NULL; runtime_code_count = 0;
      if (runtime_image) munmap(runtime_image,runtime_image_size);
      runtime_image = NULL;
      return;
    }
  }
#else
  if (mlkit_rp_code_scope == 1) scope = "unsupported-platform";
#endif
  emit_record(20,NUMS(1,0),STRS(scope));
}
