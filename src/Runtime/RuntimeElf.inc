/* ELF main-image identity and exact-sized native function symbols. Metadata is
 * collected before signals are armed. Shared-library PCs remain unresolved. */
#if defined(__linux__) && defined(__x86_64__)
#include <elf.h>
#include <link.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
__attribute__((weak)) const volatile uintptr_t mlkit_tp_gc_address = 0;
__attribute__((weak)) const volatile uintptr_t mlkit_tp_gc_end_address = 0;
__attribute__((weak)) const volatile uintptr_t mlkit_tp_reset_address = 0;
__attribute__((weak)) const volatile uintptr_t mlkit_tp_reset_end_address = 0;
typedef struct ElfProfileImage {
  uintptr_t bias, base;
  const ElfW(Phdr) *phdr;
  size_t count;
  char id[129];
} ElfProfileImage;
static int elf_build_id(const unsigned char *p, size_t size, char id[129]) {
  while (size >= sizeof(Elf64_Nhdr)) {
    Elf64_Nhdr note;
    memcpy(&note,p,sizeof(note));
    p += sizeof(note); size -= sizeof(note);
    size_t names = ((size_t)note.n_namesz+3)&~(size_t)3;
    size_t desc = ((size_t)note.n_descsz+3)&~(size_t)3;
    if (names > size || desc > size-names) return 0;
    if (note.n_type == NT_GNU_BUILD_ID && note.n_namesz == 4 &&
        !memcmp(p,"GNU",4) && note.n_descsz >= 16 && note.n_descsz <= 64) {
      for (uint32_t i = 0; i < note.n_descsz; i++) sprintf(id+2*i,"%02x",p[names+i]);
      return 1;
    }
    p += names+desc; size -= names+desc;
  }
  return 0;
}
static int elf_main_image(struct dl_phdr_info *info, size_t size, void *data) {
  (void)size;
  if (info->dlpi_name && *info->dlpi_name) return 0;
  ElfProfileImage *image = data;
  image->bias = info->dlpi_addr; image->base = UINTPTR_MAX;
  image->phdr = info->dlpi_phdr; image->count = info->dlpi_phnum;
  for (size_t i = 0; i < image->count; i++) {
    const ElfW(Phdr) *p = &image->phdr[i];
    if (p->p_type == PT_LOAD && image->bias+p->p_vaddr < image->base)
      image->base = image->bias+p->p_vaddr;
    if (p->p_type == PT_NOTE)
      elf_build_id((const unsigned char *)(image->bias+p->p_vaddr),(size_t)p->p_memsz,image->id);
  }
  return 1;
}
static int elf_in_text(const ElfProfileImage *image, uintptr_t begin, uintptr_t end) {
  if (begin < image->base || end <= begin) return 0;
  for (size_t i = 0; i < image->count; i++) {
    const ElfW(Phdr) *p = &image->phdr[i];
    if (p->p_type == PT_LOAD && (p->p_flags & PF_X) &&
        begin >= image->bias+p->p_vaddr && end <= image->bias+p->p_vaddr+p->p_memsz) return 1;
  }
  return 0;
}
static int elf_in_ml(uintptr_t begin, uintptr_t end) {
  for (size_t i = 0; mlkit_rp_code_units[i]; i++) {
    const MlkitProfileCodeUnit *u = mlkit_rp_code_units[i];
    for (uintptr_t j = 0; j < u->count; j++)
      if (begin < u->functions[j].end && end > u->functions[j].begin) return 1;
  }
  return 0;
}
typedef struct ElfProfileFunction { uintptr_t begin, end; const char *name; } ElfProfileFunction;
static int elf_function_order(const void *a, const void *b) {
  uintptr_t x = ((const ElfProfileFunction *)a)->begin, y = ((const ElfProfileFunction *)b)->begin;
  return x < y ? -1 : x > y;
}
static int elf_bytes(size_t size, uint64_t offset, uint64_t length) {
  return offset <= size && length <= size-offset;
}
static void emit_elf_code_metadata(void) {
  ElfProfileImage image = {0};
  dl_iterate_phdr(elf_main_image,&image);
  if (image.base == UINTPTR_MAX || !image.phdr) fail("cannot find ELF main executable");
  if (!image.id[0]) { emit_record(20,NUMS(1,0),STRS("missing-image-id")); return; }
  if (!mlkit_rp_code_units) fail("missing ELF function catalog; rebuild ML libraries");
  uint64_t count = 0;
  for (size_t i = 0; mlkit_rp_code_units[i]; i++) {
    const MlkitProfileCodeUnit *u = mlkit_rp_code_units[i];
    if (u->version != 1 && u->version != 2) fail("unsupported ELF function metadata; rebuild ML libraries");
    if (u->count > UINT64_MAX-count) fail("ELF function metadata count overflow");
    count += u->count;
  }
  char path[4096];
  ssize_t n = readlink("/proc/self/exe",path,sizeof(path)-1);
  if (n < 0 || (size_t)n >= sizeof(path)-1) fail("cannot identify ELF executable path");
  path[n] = 0;
  void *file = MAP_FAILED;
  size_t file_size = 0, functions = 0;
  ElfProfileFunction *native = NULL;
  int fd = open("/proc/self/exe",O_RDONLY);
  struct stat st;
  if (fd >= 0 && !fstat(fd,&st) && st.st_size >= (off_t)sizeof(Elf64_Ehdr)) {
    file_size = (size_t)st.st_size;
    file = mmap(NULL,file_size,PROT_READ,MAP_PRIVATE,fd,0);
  }
  if (fd >= 0) close(fd);
  if (file != MAP_FAILED) {
    const Elf64_Ehdr *h = file;
    if (!memcmp(h->e_ident,ELFMAG,SELFMAG) && h->e_ident[EI_CLASS] == ELFCLASS64 &&
        h->e_ident[EI_DATA] == ELFDATA2LSB && h->e_machine == EM_X86_64 &&
        h->e_phentsize == sizeof(Elf64_Phdr) && h->e_shentsize == sizeof(Elf64_Shdr) &&
        elf_bytes(file_size,h->e_phoff,(uint64_t)h->e_phnum*sizeof(Elf64_Phdr)) &&
        elf_bytes(file_size,h->e_shoff,(uint64_t)h->e_shnum*sizeof(Elf64_Shdr))) {
      const Elf64_Phdr *ph = (const void *)((const unsigned char *)file+h->e_phoff);
      char file_id[129] = {0};
      for (size_t i = 0; i < h->e_phnum; i++) if (ph[i].p_type == PT_NOTE &&
          elf_bytes(file_size,ph[i].p_offset,ph[i].p_filesz))
        elf_build_id((const unsigned char *)file+ph[i].p_offset,(size_t)ph[i].p_filesz,file_id);
      if (strcmp(file_id,image.id)) fail("ELF executable build identifier mismatch");
      const Elf64_Shdr *sections = (const void *)((const unsigned char *)file+h->e_shoff);
      for (size_t i = 0; i < h->e_shnum; i++) {
        const Elf64_Shdr *s = &sections[i];
        if (s->sh_type != SHT_SYMTAB || s->sh_entsize != sizeof(Elf64_Sym) ||
            s->sh_link >= h->e_shnum || !elf_bytes(file_size,s->sh_offset,s->sh_size)) continue;
        const Elf64_Shdr *str = &sections[s->sh_link];
        if (str->sh_type != SHT_STRTAB || !elf_bytes(file_size,str->sh_offset,str->sh_size)) continue;
        size_t limit = (size_t)(s->sh_size/sizeof(Elf64_Sym));
        if (limit > SIZE_MAX/sizeof(*native)-2) fail("ELF symbol count overflow");
        native = calloc(limit+2,sizeof(*native));
        if (!native) fail("cannot allocate ELF function catalog");
        const Elf64_Sym *symbols = (const void *)((const unsigned char *)file+s->sh_offset);
        const char *strings = (const char *)file+str->sh_offset;
        for (size_t j = 0; j < limit; j++) {
          const Elf64_Sym *f = &symbols[j];
          if (ELF64_ST_TYPE(f->st_info) != STT_FUNC || f->st_shndx == SHN_UNDEF ||
              !f->st_size || f->st_name >= str->sh_size || f->st_value > UINTPTR_MAX-image.bias) continue;
          uintptr_t begin = image.bias+f->st_value;
          if (f->st_size > UINTPTR_MAX-begin) continue;
          uintptr_t end = begin+f->st_size;
          const char *name = strings+f->st_name;
          if (!*name || !memchr(name,0,(size_t)str->sh_size-f->st_name) ||
              !elf_in_text(&image,begin,end) || elf_in_ml(begin,end)) continue;
          native[functions++] = (ElfProfileFunction){begin,end,name};
        }
        break;
      }
    }
  }
  if (!native) native = calloc(2,sizeof(*native));
  if (!native) fail("cannot allocate core runtime function catalog");
  if (elf_in_text(&image,mlkit_tp_gc_address,mlkit_tp_gc_end_address) &&
      !elf_in_ml(mlkit_tp_gc_address,mlkit_tp_gc_end_address))
    native[functions++] = (ElfProfileFunction){mlkit_tp_gc_address,mlkit_tp_gc_end_address,"gc"};
  if (elf_in_text(&image,mlkit_tp_reset_address,mlkit_tp_reset_end_address) &&
      !elf_in_ml(mlkit_tp_reset_address,mlkit_tp_reset_end_address))
    native[functions++] = (ElfProfileFunction){mlkit_tp_reset_address,mlkit_tp_reset_end_address,"resetRegion"};
  if (functions) qsort(native,functions,sizeof(*native),elf_function_order);
  size_t accepted = 0;
  for (size_t i = 0; i < functions; i++)
    if (!accepted || native[i].begin >= native[accepted-1].end) native[accepted++] = native[i];
  if (accepted > UINT64_MAX-count) fail("ELF function catalog count overflow");
  emit_record(20,NUMS(1,count+accepted),STRS("static-executable"));
  emit_record(21,NUMS(1,image.base),STRS(image.id,path));
  for (size_t i = 0; mlkit_rp_code_units[i]; i++) {
    const MlkitProfileCodeUnit *u = mlkit_rp_code_units[i];
    for (uintptr_t j = 0; j < u->count; j++) {
      const MlkitProfileFunction *f = &u->functions[j];
      if (!elf_in_text(&image,f->begin,f->end)) fail("function range outside ELF executable text");
      emit_record(22,NUMS(1,f->begin-image.base,f->end-image.base),
                  STRS(f->unit->data,f->function->data,f->source->data,f->ir_identity->data));
    }
  }
  for (size_t i = 0; i < accepted; i++) {
    char unit[64];
    snprintf(unit,sizeof(unit),"<runtime/C>@%llx",(unsigned long long)(native[i].begin-image.base));
    emit_record(22,NUMS(1,native[i].begin-image.base,native[i].end-image.base),STRS(unit,native[i].name,"<runtime>",""));
  }
  free(native);
  if (file != MAP_FAILED) munmap(file,file_size);
}
#endif
