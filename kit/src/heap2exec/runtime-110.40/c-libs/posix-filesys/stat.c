/* stat.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include <sys/types.h>
#include <sys/stat.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

#define MODE_BITS (S_IRWXU | S_IRWXG | S_IRWXO | S_ISUID | S_ISGID)

/* mkStatRep:
 *
 * This makes a representation of the struct stat to be returned
 * to the SML side. It is a tuple with the following fields:
 *
 *    file_type : int
 *    mode      : word
 *    ino       : word
 *    dev       : word
 *    nlink     : word
 *    uid       : word
 *    gid       : word
 *    size      : int
 *    atime     : Int32.int
 *    mtime     : Int32.int
 *    ctime     : Int32.int
 */
PVT ml_val_t mkStatRep (ml_state_t *msp, struct stat *buf)
{
    int		    ftype;
    ml_val_t        mode, ino, dev, uid, gid, nlink, sr, atime, mtime, ctime;

#if ((S_IFDIR != 0x4000) || (S_IFCHR != 0x2000) || (S_IFBLK != 0x6000) || (S_IFREG != 0x8000) || (S_IFIFO != 0x1000) || (S_IFLNK != 0xA000) || (S_IFSOCK != 0xC000))
    if (S_ISDIR(buf->st_mode)) ftype = 0x4000;
    else if (S_ISCHR(buf->st_mode)) ftype = 0x2000;
    else if (S_ISBLK(buf->st_mode)) ftype = 0x6000;
    else if (S_ISREG(buf->st_mode)) ftype = 0x8000;
    else if (S_ISFIFO(buf->st_mode)) ftype = 0x1000;
#ifdef S_ISLNK
    else if (S_ISLNK(buf->st_mode)) ftype = 0xA000;
#endif
#ifdef S_ISSOCK
    else if (S_ISSOCK(buf->st_mode)) ftype = 0xC000;
#endif
    else ftype = 0;
#else
    ftype = buf->st_mode & 0xF000;
#endif

    WORD_ALLOC (msp, mode, (Word_t)((buf->st_mode) & MODE_BITS));
    WORD_ALLOC (msp, ino, (Word_t)(buf->st_ino));
    WORD_ALLOC (msp, dev, (Word_t)(buf->st_dev));
    WORD_ALLOC (msp, nlink, (Word_t)(buf->st_nlink));
    WORD_ALLOC (msp, uid, (Word_t)(buf->st_uid));
    WORD_ALLOC (msp, gid, (Word_t)(buf->st_gid));
    INT32_ALLOC (msp, atime, buf->st_atime);
    INT32_ALLOC (msp, mtime, buf->st_mtime);
    INT32_ALLOC (msp, ctime, buf->st_ctime);

  /* allocate the stat record */
    ML_AllocWrite(msp,  0, MAKE_DESC(11, DTAG_record));
    ML_AllocWrite(msp,  1, INT_CtoML(ftype));
    ML_AllocWrite(msp,  2, mode);
    ML_AllocWrite(msp,  3, ino);
    ML_AllocWrite(msp,  4, dev);
    ML_AllocWrite(msp,  5, nlink);
    ML_AllocWrite(msp,  6, uid);
    ML_AllocWrite(msp,  7, gid);
    ML_AllocWrite(msp,  8, INT_CtoML(buf->st_size));
    ML_AllocWrite(msp,  9, atime);
    ML_AllocWrite(msp, 10, mtime);
    ML_AllocWrite(msp, 11, ctime);
    sr = ML_Alloc(msp, 11);

    return sr;

} /* end of mkStatRep */

/* _ml_P_FileSys_stat : string -> statrep
 *
 * Query file status given file name.
 */
ml_val_t _ml_P_FileSys_stat (ml_state_t *msp, ml_val_t arg)
{
    char            *path = STR_MLtoC(arg);
    int		    sts;
    struct stat     buf;

    sts = stat(path, &buf);

    if (sts < 0)
	return RAISE_SYSERR(msp, sts);

    return (mkStatRep(msp, &buf));

} /* end of _ml_P_FileSys_stat */

/* _ml_P_FileSys_fstat : word -> statrep
 *
 * Query file status given file descriptor.
 */
ml_val_t _ml_P_FileSys_fstat (ml_state_t *msp, ml_val_t arg)
{
    int             fd = INT_MLtoC(arg);
    int		    sts;
    struct stat     buf;

    sts = fstat(fd, &buf);

    if (sts < 0)
	return RAISE_SYSERR(msp, sts);

    return (mkStatRep(msp, &buf));

} /* end of _ml_P_FileSys_fstat */

/* _ml_P_FileSys_lstat : string -> statrep
 *
 * Query file status given file name, but do not follow
 * symbolic links.
 */
ml_val_t _ml_P_FileSys_lstat (ml_state_t *msp, ml_val_t arg)
{
    char            *path = STR_MLtoC(arg);
    int		    sts;
    struct stat     buf;

    sts = lstat(path, &buf);

    if (sts < 0)
	return RAISE_SYSERR(msp, sts);

    return (mkStatRep(msp, &buf));

} /* end of _ml_P_FileSys_lstat */

