/* Enlarge a private, thin Arm64 Mach-O seed's main-stack reservation.
 * Old release seeds reserve 128 MiB, which is too small for the current
 * backend functor. The caller must re-sign the modified executable. */
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <mach-o/loader.h>
#include <mach/machine.h>

int main(int argc, char **argv) {
  if (argc != 2) { fputs("usage: seed-stack PRIVATE-SEED\n",stderr); return 1; }
  FILE *f = fopen(argv[1],"r+b");
  struct mach_header_64 header;
  if (!f || fread(&header,sizeof header,1,f) != 1 ||
      header.magic != MH_MAGIC_64 || header.cputype != CPU_TYPE_ARM64) {
    fputs("Expected a thin Arm64 Mach-O seed\n",stderr);
    return 1;
  }
  uint64_t offset = sizeof header, end = offset+header.sizeofcmds;
  for (uint32_t i = 0; i < header.ncmds; ++i) {
    struct load_command command;
    if (offset+sizeof command > end || fseek(f,(long)offset,SEEK_SET) ||
        fread(&command,sizeof command,1,f) != 1 || command.cmdsize < sizeof command ||
        offset+command.cmdsize > end) break;
    if (command.cmd == LC_MAIN) {
      struct entry_point_command entry;
      if (command.cmdsize < sizeof entry || fseek(f,(long)offset,SEEK_SET) ||
          fread(&entry,sizeof entry,1,f) != 1) break;
      uint64_t stack = UINT64_C(512)*1024*1024;
      if (entry.stacksize < stack &&
          (fseek(f,(long)(offset+offsetof(struct entry_point_command,stacksize)),SEEK_SET) ||
           fwrite(&stack,sizeof stack,1,f) != 1)) break;
      return fclose(f) == 0 ? 0 : 1;
    }
    offset += command.cmdsize;
  }
  fputs("Missing or malformed LC_MAIN in seed\n",stderr);
  fclose(f);
  return 1;
}
