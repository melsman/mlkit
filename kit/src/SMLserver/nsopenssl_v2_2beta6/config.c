/*
 * The contents of this file are subject to the AOLserver Public License
 * Version 1.1 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://aolserver.lcs.mit.edu/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * The Original Code is AOLserver Code and related documentation
 * distributed by AOL.
 *
 * The Initial Developer of the Original Code is America Online,
 * Inc. Portions created by AOL are Copyright (C) 1999 America Online,
 * Inc. All Rights Reserved.
 *
 * Copyright (C) 2000-2002 Scott S. Goodwin
 * Copyright (C) 2000 Rob Mayoff
 * Copyright (C) 1999 Stefan Arentz.
 *
 * Alternatively, the contents of this file may be used under the terms
 * of the GNU General Public License (the "GPL"), in which case the
 * provisions of GPL are applicable instead of those above.  If you wish
 * to allow use of your version of this file only under the terms of the
 * GPL and not to allow others to use your version of this file under the
 * License, indicate your decision by deleting the provisions above and
 * replace them with the notice and other provisions required by the GPL.
 * If you do not delete the provisions above, a recipient may use your
 * version of this file under either the License or the GPL.
 */

static const char *RCSID =
    "@(#) $Header$, compiled: "
    __DATE__ " " __TIME__;

#include <ns.h>
#include "config.h"

/*
 *----------------------------------------------------------------------
 *
 * ConfigStringDefault --
 *
 *       Get the config value requested, or return the default
 *       specified.
 *
 * Results:
 *       Config value as a string.
 *
 * Side effects:
 *       None.
 *
 *----------------------------------------------------------------------
 */

extern char *
ConfigStringDefault (char *module, char *path, char *name, char *def)
{
    char *value = Ns_ConfigGetValue (path, name);
    if (value == NULL) {
	value = def;
    }
    Ns_Log (Notice, "%s: %s = %s", module, name, value ? value : "(null)");
    return value;
}

/*
 *----------------------------------------------------------------------
 *
 * ConfigBoolDefault --
 *
 *       Get the config value requested, or return the default
 *       specified.
 *
 * Results:
 *       Config value as a boolean.
 *
 * Side effects:
 *       None.
 *
 *----------------------------------------------------------------------
 */

extern int
ConfigBoolDefault (char *module, char *path, char *name, int def)
{
    int value;
    if (Ns_ConfigGetBool (path, name, &value) == NS_FALSE) {
	value = def;
    }
    Ns_Log (Notice, "%s: %s = %d", module, name, value);
    return value;
}

/*
 *----------------------------------------------------------------------
 *
 * ConfigIntDefault --
 *
 *       Get the config value requested, or return the default
 *       specified.
 *
  Results:
 *       Config value as an int.
 *
 * Side effects:
 *       None.
 *
 *----------------------------------------------------------------------
 */

extern int
ConfigIntDefault (char *module, char *path, char *name, int def)
{
    int value;
    if (Ns_ConfigGetInt (path, name, &value) == NS_FALSE) {
	value = def;
    }
    Ns_Log (Notice, "%s: %s = %d", module, name, value);
    return value;
}

/*
 *----------------------------------------------------------------------
 *
 * ConfigPathDefault --
 *
 *       Get the config value requested, or return the default
 *       specified.  If the value is not an absolute path, make
 *       it one (relative to the specified directory).
 *
 * Results:
 *       Config value as a string. The default can be NULL.
 *
 * Side effects:
 *       Caller is responsible for freeing the returned value (unlike
 *       ConfigStringDefault).
 *
 *----------------------------------------------------------------------
 */

extern char *
ConfigPathDefault (char *module, char *path, char *name, char *dir, char *def)
{
    char *value;
    Ns_DString ds;

    value = Ns_ConfigGetValue (path, name);
    if (value == NULL) {
	if (def == NULL) {
	    return value;
	}
	value = def;
    }

    if (Ns_PathIsAbsolute (value)) {
	value = ns_strdup (value);
    } else {
	Ns_DStringInit (&ds);
	Ns_MakePath (&ds, dir, value, NULL);
#if 0
	Ns_DStringVarAppend (&ds, dir, value, NULL);
#endif
	value = Ns_DStringExport (&ds);
	Ns_DStringFree (&ds);
    }

    Ns_Log (Notice, "%s: %s = %s", module, name, value);
    return value;
}
