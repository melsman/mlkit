#ifndef DLSYM_H
#define DLSYM_H

#include "String.h"
#include "Region.h"
#include "Tagging.h"

uintptr_t REG_POLY_FUN_HDR(sml_dlopen,uintptr_t pair, Region s, String file1, size_t flags1);
String REG_POLY_FUN_HDR(resolveFun,Region sAddr, String our_name, String cname, void *libhandle);
void localResolveLibFnAuto(void **fp, char *fname);

#endif // DLSYM_H
