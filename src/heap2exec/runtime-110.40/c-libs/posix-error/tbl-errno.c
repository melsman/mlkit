/* tbl-errno.c
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * The table of system constants representing the Posix error codes.
 */

#include "ml-unixdep.h"
#include "ml-base.h"
#include <errno.h>

PVT sys_const_t tbl[] = {
	{EACCES,	"acces"},
	{EAGAIN,	"again"},
#if (defined(EWOULDBLOCK) && (EWOULDBLOCK != EAGAIN))
	{EWOULDBLOCK,	"wouldblock"},
#endif
	{EBADF,		"badf"},
#ifdef EBADMSG
	{EBADMSG,	"badmsg"},
#else
	{0,		"badmsg"},
#endif
	{EBUSY,		"busy"},
#ifdef ECANCELED
	{ECANCELED,	"canceled"},
#else
	{0,		"canceled"},
#endif
	{ECHILD,	"child"},
	{EDEADLK,	"deadlk"},
	{EDOM,		"dom"},
	{EEXIST,	"exist"},
	{EFAULT,	"fault"},
	{EFBIG,		"fbig"},
	{EINPROGRESS,	"inprogress"},
	{EINTR,		"intr"},
	{EINVAL,	"inval"},
	{EIO,		"io"},
	{EISDIR,	"isdir"},
	{ELOOP,		"loop"},
	{EMFILE,	"mfile"},
	{EMLINK,	"mlink"},
	{EMSGSIZE,	"msgsize"},
	{ENAMETOOLONG,	"nametoolong"},
	{ENFILE,	"nfile"},
	{ENODEV,	"nodev"},
	{ENOENT,	"noent"},
	{ENOEXEC,	"noexec"},
	{ENOLCK,	"nolck"},
	{ENOMEM,	"nomem"},
	{ENOSPC,	"nospc"},
	{ENOSYS,	"nosys"},
	{ENOTDIR,	"notdir"},
	{ENOTEMPTY,	"notempty"},
#ifdef ENOTSUP
	{ENOTSUP,	"notsup"},
#else
	{0,		"notsup"},
#endif
	{ENOTTY,	"notty"},
	{ENXIO,		"nxio"},
	{EPERM,		"perm"},
	{EPIPE,		"pipe"},
	{ERANGE,	"range"},
	{EROFS,		"rofs"},
	{ESPIPE,	"spipe"},
	{ESRCH,		"srch"},
	{E2BIG,		"toobig"},
	{EXDEV,		"xdev"},
    };

sysconst_tbl_t	_ErrorNo = {
	sizeof(tbl) / sizeof(sys_const_t),
	tbl
    };
