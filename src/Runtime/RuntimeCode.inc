/* Named C functions in the main image. Mach-O function starts bound ranges;
 * symbols supply names only at exact starts. Never guess across unnamed code. */
#if defined(__APPLE__) && defined(__aarch64__)
#include <mach-o/nlist.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
/* Runtime-owned entry addresses are optional in standalone engine tests. */
__attribute__((weak)) const volatile uintptr_t mlkit_tp_gc_address = 0;
__attribute__((weak)) const volatile uintptr_t mlkit_tp_reset_address = 0;
typedef struct RuntimeCode { uintptr_t begin, end; const char *name; } RuntimeCode;
static RuntimeCode *runtime_code;
static size_t runtime_code_count;
static void *runtime_image;
static size_t runtime_image_size;
static int image_bytes(size_t offset, size_t size) {
  return offset <= runtime_image_size && size <= runtime_image_size-offset;
}
static int in_ml_code(uintptr_t start, uintptr_t end) {
  for (size_t i = 0; mlkit_rp_code_units[i]; i++) {
    const MlkitProfileCodeUnit *u = mlkit_rp_code_units[i];
    for (uintptr_t j = 0; j < u->count; j++)
      if (start < u->functions[j].end && end > u->functions[j].begin) return 1;
  }
  return 0;
}
static void collect_runtime_code(uintptr_t text_begin, uintptr_t text_end, const char *build_id) {
  int fd = open(_dyld_get_image_name(0),O_RDONLY);
  struct stat st;
  if (fd < 0) return;
  if (fstat(fd,&st) || st.st_size < (off_t)sizeof(struct mach_header_64)) { close(fd); return; }
  runtime_image_size = (size_t)st.st_size;
  runtime_image = mmap(NULL,runtime_image_size,PROT_READ,MAP_PRIVATE,fd,0);
  close(fd);
  if (runtime_image == MAP_FAILED) { runtime_image = NULL; return; }
  const struct mach_header_64 *h = runtime_image;
  /* Universal files require selecting a slice; leave them unresolved for now. */
  if (h->magic != MH_MAGIC_64 || !image_bytes(sizeof(*h),h->sizeofcmds)) return;
  const unsigned char *p = (const unsigned char *)(h+1), *limit = p+h->sizeofcmds;
  const struct symtab_command *symbols = NULL;
  const struct linkedit_data_command *starts = NULL;
  uintptr_t text_vm = 0;
  char file_id[33] = {0};
  for (uint32_t i = 0; i < h->ncmds; i++) {
    if ((size_t)(limit-p) < sizeof(struct load_command)) return;
    const struct load_command *c = (const struct load_command *)p;
    if (c->cmdsize < sizeof(*c) || c->cmdsize > (size_t)(limit-p)) return;
    if (c->cmd == LC_UUID && c->cmdsize >= sizeof(struct uuid_command)) {
      const struct uuid_command *id = (const void *)c;
      for (size_t j = 0; j < 16; j++) sprintf(file_id+2*j,"%02x",id->uuid[j]);
    }
    if (c->cmd == LC_SYMTAB && c->cmdsize >= sizeof(*symbols)) symbols = (const void *)c;
    if (c->cmd == LC_FUNCTION_STARTS && c->cmdsize >= sizeof(*starts)) starts = (const void *)c;
    if (c->cmd == LC_SEGMENT_64 && c->cmdsize >= sizeof(struct segment_command_64)) {
      const struct segment_command_64 *s = (const void *)c;
      if (!strncmp(s->segname,"__TEXT",16)) text_vm = s->vmaddr;
    }
    p += c->cmdsize;
  }
  if (strcmp(file_id,build_id) || !symbols || !starts || !image_bytes(starts->dataoff,starts->datasize) ||
      !image_bytes(symbols->symoff,(size_t)symbols->nsyms*sizeof(struct nlist_64)) ||
      !image_bytes(symbols->stroff,symbols->strsize)) return;
  uintptr_t *addresses = malloc((starts->datasize+1)*sizeof(uintptr_t));
  if (!addresses) fail("cannot allocate runtime function metadata");
  size_t count = 0;
  uintptr_t address = text_vm + _dyld_get_image_vmaddr_slide(0);
  p = (const unsigned char *)runtime_image+starts->dataoff;
  limit = p+starts->datasize;
  while (p < limit) {
    uint64_t delta = 0;
    unsigned shift = 0;
    unsigned char byte;
    do {
      if (p == limit || shift >= 64) { free(addresses); return; }
      byte = *p++;
      if (shift == 63 && (byte & 0x7e)) { free(addresses); return; }
      delta |= (uint64_t)(byte & 0x7f) << shift;
      shift += 7;
    } while (byte & 0x80);
    if (!delta) break;
    if (delta > UINTPTR_MAX-address) { free(addresses); return; }
    address += delta;
    if (address >= text_begin && address < text_end) addresses[count++] = address;
  }
  runtime_code = calloc(count,sizeof(*runtime_code));
  if (count && !runtime_code) fail("cannot allocate runtime function metadata");
  const struct nlist_64 *names = (const void *)((const unsigned char *)runtime_image+symbols->symoff);
  const char *strings = (const char *)runtime_image+symbols->stroff;
  for (size_t i = 0; i < count; i++) {
    uintptr_t end = i+1 < count ? addresses[i+1] : text_end;
    if (in_ml_code(addresses[i],end)) continue;
    const char *known = NULL;
    if (addresses[i] == mlkit_tp_gc_address) known = "gc";
    else if (addresses[i] == mlkit_tp_reset_address) known = "resetRegion";
    if (known) {
      runtime_code[runtime_code_count++] = (RuntimeCode){addresses[i],end,known};
      continue;
    }
    for (uint32_t j = 0; j < symbols->nsyms; j++) {
      const struct nlist_64 *n = &names[j];
      if ((n->n_type & N_STAB) || (n->n_type & N_TYPE) != N_SECT ||
          n->n_value+_dyld_get_image_vmaddr_slide(0) != addresses[i] ||
          n->n_un.n_strx >= symbols->strsize) continue;
      const char *name = strings+n->n_un.n_strx;
      if (!memchr(name,0,symbols->strsize-n->n_un.n_strx) || name[0] != '_' || !name[1]) continue;
      runtime_code[runtime_code_count++] = (RuntimeCode){addresses[i],end,name+1};
      break; /* Aliases share one row/range. */
    }
  }
  free(addresses);
}
#endif
