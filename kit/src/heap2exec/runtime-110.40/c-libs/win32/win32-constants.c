/* win32-constants.c
 *
 * COPYRIGHT (c) 1996 Bell Laboratories, Lucent Technologies
 *
 * interface to win32 constants
 */

#include <windows.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"

#include "name-val.h"

#define TAB_SZ(t)  ((sizeof t)/(sizeof(name_val_t)))

typedef struct {
  name_val_t *ptab;
  int sz;
} tab_desc_t;

/* general table */
PVT name_val_t general_tab[] = {
  {"INVALID_HANDLE_VALUE",  (Word_t)INVALID_HANDLE_VALUE}
};

/* FILE_ table */
PVT name_val_t file_tab[] = {
  {"BEGIN",  FILE_BEGIN},
  {"CURRENT",  FILE_CURRENT},
  {"END",  FILE_END},
};

/* FILE_ATTRIBUTE_ table */
PVT name_val_t file_attr_tab[] = {
  {"ARCHIVE",  FILE_ATTRIBUTE_ARCHIVE},
/** future win32 use
  {"ATOMIC_WRITE", FILE_ATTRIBUTE_ATOMIC_WRITE},
**/
  {"DIRECTORY", FILE_ATTRIBUTE_DIRECTORY},
  {"HIDDEN", FILE_ATTRIBUTE_HIDDEN},
  {"NORMAL", FILE_ATTRIBUTE_NORMAL},
  {"READONLY", FILE_ATTRIBUTE_READONLY},
  {"SYSTEM", FILE_ATTRIBUTE_SYSTEM},
  {"TEMPORARY", FILE_ATTRIBUTE_TEMPORARY},
/** future win32 use
  {"XACTION_WRITE", FILE_ATTRIBUTE_XACTION_WRITE},
**/
};

/* FILE_FLAG__ table */
PVT name_val_t file_flag_tab[] = {
  {"BACKUP_SEMANTICS", FILE_FLAG_BACKUP_SEMANTICS},
  {"DELETE_ON_CLOSE", FILE_FLAG_DELETE_ON_CLOSE},
  {"NO_BUFFERING", FILE_FLAG_NO_BUFFERING},
  {"OVERLAPPED", FILE_FLAG_OVERLAPPED},
  {"POSIX_SEMANTICS", FILE_FLAG_POSIX_SEMANTICS},
  {"RANDOM_ACCESS", FILE_FLAG_RANDOM_ACCESS},
  {"SEQUENTIAL_SCAN", FILE_FLAG_SEQUENTIAL_SCAN},
  {"WRITE_THROUGH", FILE_FLAG_WRITE_THROUGH},
};

/* FILE_MODE__ table */
PVT name_val_t file_mode_tab[] = {
  {"CREATE_ALWAYS", CREATE_ALWAYS},
  {"CREATE_NEW", CREATE_NEW},
  {"OPEN_ALWAYS", OPEN_ALWAYS},
  {"OPEN_EXISTING", OPEN_EXISTING},
  {"TRUNCATE_EXISTING", TRUNCATE_EXISTING},
};

/* FILE_SHARE_ table */
PVT name_val_t file_share_tab[] = {
  {"READ",  FILE_SHARE_READ},
  {"WRITE",  FILE_SHARE_WRITE},
};

/* GENERIC__ table */
PVT name_val_t generic_tab[] = {
  {"READ",  GENERIC_READ},
  {"WRITE",  GENERIC_WRITE},
};

/* STD_HANDLE table */
PVT name_val_t std_handle_tab[] = {
  {"ERROR",  STD_ERROR_HANDLE},
  {"INPUT",  STD_INPUT_HANDLE},
  {"OUTPUT",  STD_OUTPUT_HANDLE},
};

/* every constant table must have an entry in the descriptor table */
PVT tab_desc_t table[] = {
  {file_tab, TAB_SZ(file_tab)},
  {file_attr_tab, TAB_SZ(file_attr_tab)},
  {file_flag_tab, TAB_SZ(file_flag_tab)},
  {file_mode_tab, TAB_SZ(file_mode_tab)},
  {file_share_tab, TAB_SZ(file_share_tab)},
  {general_tab, TAB_SZ(general_tab)},
  {generic_tab, TAB_SZ(generic_tab)},
  {std_handle_tab, TAB_SZ(std_handle_tab)},
};

/* constant classes */
PVT name_val_t class[] = {
  {"FILE", 0},
  {"FILE_ATTRIBUTE", 1},
  {"FILE_FLAG", 2},
  {"FILE_MODE", 3},
  {"FILE_SHARE", 4},
  {"GENERAL", 5},
  {"GENERIC", 6},
  {"STD_HANDLE",7},
};
#define N_CLASSES TAB_SZ(class)


/* _ml_win32_get_const: (string * string) -> word
 * lookup (class,constant) pair
 */
ml_val_t _ml_win32_get_const(ml_state_t *msp, ml_val_t arg)
{
  char *s1 = (char *)REC_SEL(arg,0);
  char *s2 = (char *)REC_SEL(arg,1);
  name_val_t *ptab, *res;
  int index;
  ml_val_t v;

  printf("looking up: <%s,%s>: ", s1, s2);
  ptab = nv_lookup(s1, class, N_CLASSES);
  if (ptab) {
    index = ptab->data;
    ASSERT(index < TAB_SZ(table));
    if (res = nv_lookup(s2, table[index].ptab, table[index].sz)) {
      printf("%x\n", res->data);
      WORD_ALLOC(msp,v,res->data);
      return v;
    }
    return RAISE_ERROR(msp,"win32_cconst: unknown constant");
  }
  return RAISE_ERROR(msp,"win32_cconst: unknown constant class");
}

/* end of win32-constants.c */
