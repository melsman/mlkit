/* boot.c
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * This is the bootstrap loader for booting from .bin files.
 */

#include "ml-osdep.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "ml-base.h"
#include "ml-limits.h"
#include "cache-flush.h"
#include "bin-file.h"
#include "ml-objects.h"
#include "gc.h"
#include "ml-globals.h"

#ifndef SEEK_SET
#  define SEEK_SET	0
#endif

/* The persistent ID list is stored in the PervStruct refcell.  It has
 * the following ML type:
 *
 *    datatype runDynEnv
 *      = NILrde
 *      | CONSrde of (Word8Vector.vector * Object.object * runDynEnv)
 */
#define PerIDList	(*PTR_MLtoC(ml_val_t, PervStruct))

PVT ml_val_t	BinFileList = LIST_nil;	/* A list of bin files to load */


/* local routines */
PVT ml_val_t BuildFileList (ml_state_t *msp, const char *bootlist,
			    int *max_boot_path_len_ptr);
PVT FILE *OpenBinFile (const char *fname, bool_t isBinary);
PVT void ReadBinFile (FILE *file, void *buf, int nbytes, const char *fname);
PVT void LoadBinFile (ml_state_t *msp, char *fname);
PVT void EnterPerID (ml_state_t *msp, pers_id_t *perID, ml_val_t obj);
PVT ml_val_t LookupPerID (pers_id_t *perID);
PVT void ShowPerID (char *buf, pers_id_t *perID);

# define HEX(c) (isdigit(c) ? (c) - '0' : (c) - 'a' + 10)

/* BootML:
 *
* Boot the system using the items read from the "bootlist" file.
*/
void BootML (const char *bootlist, heap_params_t *heapParams)
{
    ml_state_t	*msp;
    int         max_boot_path_len;
    char	*fname;
    int         rts_init = 0;

    msp = AllocMLState (TRUE, heapParams);

#ifdef HEAP_MONITOR
    if (HeapMon_Init(CmdLineArgs, msp->ml_heap) == FAILURE)
	Die("unable to start heap monitor");
#endif

    InitFaultHandlers ();
    AllocGlobals (msp);

  /* construct the list of files to be loaded */
    BinFileList = BuildFileList (msp, bootlist, &max_boot_path_len);

  /* this space is ultimately wasted */
    if ((fname = malloc (max_boot_path_len)) == NULL)
      Die ("unable to allocate space for boot file names");

  /* boot the system */
    while (BinFileList != LIST_nil) {
      /* need to make a copy of the path name because LoadBinFile is
       * going to scribble into it */
	strcpy(fname, STR_MLtoC(LIST_hd(BinFileList)));
	BinFileList = LIST_tl(BinFileList);
	if (fname[0] == '#')
	  if (rts_init)
	    Die ("runtime system registered more than once\n");
	  else {
	    /* register the runtime system under the given pers id */
	    pers_id_t pid;
	    int i, l = strlen (fname + 1);
	    for (i = 0; i < PERID_LEN; i++) {
	      int i2 = 2 * i;
	      if (i2 + 1 < l) {
		int c1 = fname[i2+1];
		int c2 = fname[i2+2];
		pid.bytes[i] = (HEX(c1) << 4) + HEX(c2);
	      }
	    }
	    if (!SilentLoad)
	      Say ("[Registering runtime system as %s]\n", fname+1);
	    EnterPerID (msp, &pid, RunTimeCompUnit);
	    rts_init = 1;	/* make sure we do this only once */
	  }
	else
	  LoadBinFile (msp, fname);
    }

} /* end of BootML */


/* BuildFileList:
 *
 * Given the directory path, build a list of the .bin files in the
 * heap.
 */
PVT ml_val_t BuildFileList (ml_state_t *msp, const char *bootlist, int *mbplp)
{
    FILE	*listF;
    ml_val_t	*fileNames = NULL;
    char	*nameBuf = NULL;
    int         max_num_boot_files = MAX_NUM_BOOT_FILES;
    int         max_boot_path_len = MAX_BOOT_PATH_LEN;
    int		i, j, numFiles;
    ml_val_t	fileList;
# define SIZE_BUF_LEN	128	/* this should be plenty for two numbers */
    char        sizeBuf[SIZE_BUF_LEN];
    char        c;

    numFiles = 0;

    listF = OpenBinFile (bootlist, FALSE);

    if (listF != NULL) {
      c = getc (listF);
      if (c == EOF)
	Die ("bootlist file \"%s\" is empty", bootlist);
      if (c == '%') {
	if (fgets (sizeBuf, SIZE_BUF_LEN, listF) != NIL(char *)) {
	  /* hardly any checking here... */
	  char *space = strchr (sizeBuf, ' ');
	  *space = '\0';
	  max_num_boot_files = strtoul (sizeBuf, NULL, 0);
	  max_boot_path_len = strtoul (space+1, NULL, 0) + 2;
	} else
	  Die ("unable to read first line in \"%s\" after %%", bootlist);
      } else {
	/* size spec is missing -- use defaults */
	ungetc (c, listF);
      }

      *mbplp = max_boot_path_len; /* tell the calling function... */

      if ((nameBuf = malloc (max_boot_path_len)) == NULL)
	Die ("unable to allocate space for boot file names");

      if ((fileNames = malloc (max_num_boot_files * sizeof (char *))) == NULL)
	Die ("unable to allocate space for boot file name table");

      /* read in the file names, converting them to ML strings. */
	while (fgets (nameBuf, max_boot_path_len, listF) != NIL(char *)) {
	    j = strlen(nameBuf)-1;
	    if (nameBuf[j] == '\n') nameBuf[j] = '\0';	/* remove "\n" */
	    if (numFiles < max_num_boot_files)
		fileNames[numFiles++] = ML_CString(msp, nameBuf);
	    else
		Die ("too many files\n");
	}
	fclose (listF);
    }

  /* create the in-heap list */
    for (fileList = LIST_nil, i = numFiles;  --i >= 0; ) {
	LIST_cons(msp, fileList, fileNames[i], fileList);
    }

    /* these guys are no longer needed from now on */
    if (fileNames)
      free (fileNames);
    if (nameBuf)
      free (nameBuf);

    return fileList;

} /* end of BuildFileList */


/* OpenBinFile:
 *
 * Open a file in the bin file directory.
 */
PVT FILE *OpenBinFile (const char *fname, bool_t isBinary)
{
    FILE	*file;

    if ((file = fopen (fname, isBinary ? "rb" : "r")) == NULL)
	Error ("unable to open \"%s\"\n", fname);

    return file;

} /* end of OpenBinFile */

/*
 * BINFILE FORMAT description:
 *
*************** The following really belongs in the header file ****************
 *  Every 4-byte integer field is stored in big-endian format.
 *
 *       Start Size Purpose
 * ----BEGIN OF HEADER----
 *            0 16  magic string
 *           16  4  number of import values (importCnt)
 *           20  4  number of exports (exportCnt = currently always 0 or 1)
 *           24  4  size of import tree area in bytes (importSzB)
 *           28  4  size of CM-specific info in bytes (cmInfoSzB)
 *           32  4  size of pickled lambda-expression in bytes (lambdaSzB)
 *           36  4  size of reserved area in bytes (reserved)
 *           40  4  size of padding area in bytes (pad)
 *           44  4  size of code area in bytes (codeSzB)
 *           48  4  size of pickled environment in bytes (envSzB)
 *           52  i  import trees [This area contains pickled import trees --
 *                    see below.  The total number of leaves in these trees is
 *                    importCnt.  The size impSzB of this area depends on the
 *                    shape of the trees.]
 *         i+52 ex  export pids [Each export pid occupies 16 bytes. Thus, the
 *                    size ex of this area is 16*exportCnt (0 or 16).]
 *      ex+i+52 cm  CM info [Currently a list of pid-pairs.] (cm = cmInfoSzB)
 * ----END OF HEADER----
 *            0  h  HEADER (h = 52+cm+ex+i)
 *            h  l  pickle of exported lambda-expr. (l = lambdaSzB)
 *          l+h  r  reserved area (r = reserved)
 *        r+l+h  p  padding (p = pad)
 *      p+r+l+h  c  code area (c = codeSzB) [Structured into several
 *                    segments -- see below.]
 *    c+p+r+l+h  e  pickle of static environment (e = envSzB)
 *  e+c+p+r+l+h  -  END OF BINFILE
 *
 * IMPORT TREE FORMAT description:
 *
 *  The import tree area contains a list of (pid * tree) pairs.
 *  The pids are stored directly as 16-byte strings.
 *  Trees are constructed according to the following ML-datatype:
 *    datatype tree = NODE of (int * tree) list
 *  Leaves in this tree have the form (NODE []).
 *  Trees are written recursively -- (NODE l) is represented by n (= the
 *  length of l) followed by n (int * node) subcomponents.  Each component
 *  consists of the integer selector followed by the corresponding tree.
 *
 *  The size of the import tree area is only given implicitly. When reading
 *  this area, the reader must count the number of leaves and compare it
 *  with importCnt.
 *
 *  Integer values in the import tree area (lengths and selectors) are
 *  written in "packed" integer format. In particular, this means that
 *  Values in the range 0..127 are represented by only 1 byte.
 *  Conceptually, the following pickling routine is used:
 *
 *    void recur_write_ul (unsigned long l, FILE *file)
 *    {
 *        if (l != 0) {
 *            recur_write_ul (l >> 7, file);
 *            putc ((l & 0x7f) | 0x80, file);
 *        }
 *    }
 *
 *    void write_ul (unsigned long l, FILE *file)
 *    {
 *        recur_write_ul (l >> 7, file);
 *        putc (l & 0x7f, file);
 *    }
 *
 * CODE AREA FORMAT description:
 *
 *  The code area contains multiple code segements.  There will be at least
 *  two.  The very first segment is the "data" segment -- responsible for
 *  creating literal constants on the heap.  The idea is that code in the
 *  data segment will be executed only once at link-time. Thus, it can
 *  then be garbage-collected immediatly. (In the future it is possible that
 *  the data segment will not contain executable code at all but some form
 *  of bytecode that is to be interpreted separately.)
 *
 *  In the binfile, each code segment is represented by its size s and its
 *  entry point offset (in bytes -- written as 4-byte big-endian integers)
 *  followed by s bytes of machine- (or byte-) code. The total length of all
 *  code segments (including the bytes spent on representing individual sizes
 *  and entry points) is codeSzB.  The entrypoint field for the data segment
 *  is currently ignored (and should be 0).
 *
 * LINKING CONVENTIONS:
 *
 *  Linking is achieved by executing all code segments in sequential order.
 *
 *  The first code segment (i.e., the "data" segment) receives unit as
 *  its single argument.
 *
 *  The second code segment receives a record as its single argument.
 *  This record has (importCnt+1) components.  The first importCnt
 *  components correspond to the leaves of the import trees.  The final
 *  component is the result from executing the data segment.
 *
 *  All other code segments receive a single argument which is the result
 *  of the preceding segment.
 *
 *  The result of the last segment represents the exports of the compilation
 *  unit.  It is to be paired up with the export pid and stored in the
 *  dynamic environment.  If there is no export pid, then the final result
 *  will be thrown away.
 *
 *  The import trees are used for constructing the argument record for the
 *  second code segment.  The pid at the root of each tree is the key for
 *  looking up a value in the existing dynamic environment.  In general,
 *  that value will be a record.  The selector fields of the import tree
 *  associated with the pid are used to recursively fetch components of that
 *  record.
 */

/* ReadBinFile:
 */
PVT void ReadBinFile (FILE *file, void *buf, int nbytes, const char *fname)
{
    if (fread(buf, nbytes, 1, file) == -1)
	Die ("cannot read file \"%s\"", fname);

} /* end of ReadBinFile */

/* ReadPackedInt32:
 *
 * Read an integer in "packed" format.  (Small numbers only require 1 byte.)
 */
PVT Int32_t ReadPackedInt32 (FILE *file, const char *fname)
{
    Unsigned32_t	n;
    Byte_t		c;

    n = 0;
    do {
	ReadBinFile (file, &c, sizeof(c), fname);
	n = (n << 7) | (c & 0x7f);
    } while ((c & 0x80) != 0);

    return ((Int32_t)n);

} /* end of ReadPackedInt32 */

/* ImportSelection:
 *
 * Select out the interesting bits from the imported object.
 */
PVT void ImportSelection (ml_state_t *msp, FILE *file, const char *fname,
			  int *importVecPos, ml_val_t tree)
{
    Int32_t cnt = ReadPackedInt32 (file, fname);
    if (cnt == 0) {
	ML_AllocWrite (msp, *importVecPos, tree);
	(*importVecPos)++;
    }
    else {
	while (cnt-- > 0) {
	    Int32_t selector = ReadPackedInt32 (file, fname);
	    ImportSelection (msp, file, fname, importVecPos,
			     REC_SEL(tree, selector));
	}
    }

} /* end of ImportSelection */

/* LoadBinFile:
 */
PVT void LoadBinFile (ml_state_t *msp, char *fname)
{
    FILE	    *file;
    int		    i, remainingCode, importRecLen;
    int             exportSzB = 0;
    ml_val_t	    codeObj, importRec, closure, val;
    binfile_hdr_t   hdr;
    pers_id_t	    exportPerID;
    Int32_t         thisSzB, thisEntryPoint;
    size_t          archiveOffset;
    char            *atptr, *colonptr;
    char            *objname = fname;
    

    if ((atptr = strchr (fname, '@')) == NULL)
	archiveOffset = 0;
    else {
	if ((colonptr = strchr (atptr + 1, ':')) != NULL) {
	    objname = colonptr + 1;
	    *colonptr = '\0';
	}
      /* not a lot of extensive checking here... */
	archiveOffset = strtoul (atptr + 1, NULL, 0);
	*atptr = '\0';
    }

    if (!SilentLoad)
      Say ("[Loading %s]\n", objname);

  /* open the file */
    file = OpenBinFile (fname, TRUE);
    if (file == NULL)
	Exit (1);

  /* if an offset is given (i.e., we are probably dealing with a stable
   * archive), then seek to the beginning of the section that contains
   * the binfile
   */
    if (archiveOffset != 0) {
	if (fseek (file, archiveOffset, SEEK_SET) == -1)
	    Die ("cannot seek on archive file \"%s@%ul\"",
		 fname, (unsigned long) archiveOffset);
    }

  /* get the header */
    ReadBinFile (file, &hdr, sizeof(binfile_hdr_t), fname);

  /* get header byte order right */
    hdr.importCnt	= BIGENDIAN_TO_HOST(hdr.importCnt);
    hdr.exportCnt	= BIGENDIAN_TO_HOST(hdr.exportCnt);
    hdr.importSzB	= BIGENDIAN_TO_HOST(hdr.importSzB);
    hdr.cmInfoSzB	= BIGENDIAN_TO_HOST(hdr.cmInfoSzB);
    hdr.lambdaSzB	= BIGENDIAN_TO_HOST(hdr.lambdaSzB);
    hdr.reserved	= BIGENDIAN_TO_HOST(hdr.reserved);
    hdr.pad             = BIGENDIAN_TO_HOST(hdr.pad);
    hdr.codeSzB		= BIGENDIAN_TO_HOST(hdr.codeSzB);
    hdr.envSzB		= BIGENDIAN_TO_HOST(hdr.envSzB);

  /* read the import PerIDs, and create the import vector */
    {
	int	importVecPos;

	importRecLen = hdr.importCnt + 1;

	if (NeedGC (msp, REC_SZB(importRecLen)))
	    InvokeGCWithRoots (msp, 0, &BinFileList, NIL(ml_val_t *));

	ML_AllocWrite (msp, 0, MAKE_DESC(importRecLen, DTAG_record));
	for (importVecPos = 1; importVecPos < importRecLen; ) {
	    pers_id_t	importPid;
	    ReadBinFile (file, &importPid, sizeof(pers_id_t), fname);
	    ImportSelection (msp, file, fname, &importVecPos,
			     LookupPerID(&importPid));
	}
	ML_AllocWrite(msp, importRecLen, ML_nil);
	importRec = ML_Alloc(msp, importRecLen);
    }

  /* read the export PerID */
    if (hdr.exportCnt == 1) {
	exportSzB = sizeof(pers_id_t);
	ReadBinFile (file, &exportPerID, exportSzB, fname);
    }
    else if (hdr.exportCnt != 0)
	Die ("# of export pids is %d (should be 0 or 1)", (int)hdr.exportCnt);

  /* seek to code section */
    {
	long	    off = archiveOffset
	                + sizeof(binfile_hdr_t)
			+ hdr.importSzB
	                + exportSzB
	                + hdr.cmInfoSzB
			+ hdr.lambdaSzB
			+ hdr.reserved
	                + hdr.pad;

	if (fseek(file, off, SEEK_SET) == -1)
	    Die ("cannot seek on bin file \"%s\"", fname);
    }

  /* Read code objects and run them.  The first code object will be the
   * data segment.  */

    remainingCode = hdr.codeSzB;

  /* read the size and the dummy entry point for the data object */
    ReadBinFile (file, &thisSzB, sizeof(Int32_t), fname);
    thisSzB = BIGENDIAN_TO_HOST(thisSzB);
    ReadBinFile (file, &thisEntryPoint, sizeof(Int32_t), fname);
    /* thisEntryPoint = BIGENDIAN_TO_HOST(thisEntryPoint); */

    remainingCode -= thisSzB + 2 * sizeof(Int32_t);
    if (remainingCode < 0)
	Die ("format error (data size mismatch) in bin file \"%s\"", fname);

    if (thisSzB > 0) {
	Byte_t		*dataObj = NEW_VEC(Byte_t, thisSzB);

	ReadBinFile (file, dataObj, thisSzB, fname);
	SaveCState (msp, &BinFileList, &importRec, NIL(ml_val_t *));
	val = BuildLiterals (msp, dataObj, thisSzB);
	FREE(dataObj);
	RestoreCState (msp, &BinFileList, &importRec, NIL(ml_val_t *));
    }
    else {
	val = ML_unit;
    }
  /* do a functional update of the last element of the importRec. */
    for (i = 0;  i < importRecLen;  i++)
	ML_AllocWrite(msp, i, PTR_MLtoC(ml_val_t, importRec)[i-1]);
    ML_AllocWrite(msp, importRecLen, val);
    val = ML_Alloc(msp, importRecLen);
  /* do a GC, if necessary */
    if (NeedGC (msp, PERID_LEN+REC_SZB(5)))
	InvokeGCWithRoots (msp, 0, &BinFileList, &val, NIL(ml_val_t *));

    while (remainingCode > 0) {

      /* read the size and entry point for this code object */
	ReadBinFile (file, &thisSzB, sizeof(Int32_t), fname);
	thisSzB = BIGENDIAN_TO_HOST(thisSzB);
	ReadBinFile (file, &thisEntryPoint, sizeof(Int32_t), fname);
	thisEntryPoint = BIGENDIAN_TO_HOST(thisEntryPoint);

      /* how much more? */
	remainingCode -= thisSzB + 2 * sizeof(Int32_t);
	if (remainingCode < 0)
	  Die ("format error (code size mismatch) in bin file \"%s\"", fname);

      /* allocate space and read code object */
	codeObj = ML_AllocCode (msp, thisSzB);
	ReadBinFile (file, PTR_MLtoC(char, codeObj), thisSzB, fname);

	FlushICache (PTR_MLtoC(char, codeObj), thisSzB);
      
      /* create closure (taking entry point into account) */
	REC_ALLOC1 (msp, closure,
		    PTR_CtoML (PTR_MLtoC (char, codeObj) + thisEntryPoint));

      /* apply the closure to the import PerID vector */
	SaveCState (msp, &BinFileList, NIL(ml_val_t *));
	val = ApplyMLFn (msp, closure, val, TRUE);
	RestoreCState (msp, &BinFileList, NIL(ml_val_t *));

      /* do a GC, if necessary */
	if (NeedGC (msp, PERID_LEN+REC_SZB(5)))
	    InvokeGCWithRoots (msp, 0, &BinFileList, &val, NIL(ml_val_t *));
    }

  /* record the resulting exported PerID */
    if (exportSzB != 0)
	EnterPerID (msp, &exportPerID, val);

    fclose (file);

} /* end of LoadBinFile */

/* EnterPerID:
 *
 * Enter a PerID/object binding in the heap allocated list of PerIDs.
 */
PVT void EnterPerID (ml_state_t *msp, pers_id_t *perID, ml_val_t obj)
{
    ml_val_t	    mlPerID;

  /* Allocate space for the PerID */
    mlPerID = ML_AllocString (msp, PERID_LEN);
    memcpy (STR_MLtoC(mlPerID), (char *)perID, PERID_LEN);

  /* Allocate the list element */
    REC_ALLOC3(msp, PerIDList, mlPerID, obj, PerIDList);

}

/* LookupPerID:
 */
PVT ml_val_t LookupPerID (pers_id_t *perID)
{
    ml_val_t        p, id;

    for (p = PerIDList;  p != ML_unit;  p = REC_SEL(p, 2)) {
	id = REC_SEL(p, 0);
	if (memcmp((char *)perID, STR_MLtoC(id), PERID_LEN) == 0)
	    return (REC_SEL(p, 1));
    }
    {
	char	buf[64];
	ShowPerID (buf, perID);
	Die ("unable to find PerID %s", buf);
    }
} /* end of LookupPerID */


/* ShowPerID:
 */
PVT void ShowPerID (char *buf, pers_id_t *perID)
{
    char	*cp = buf;
    int		i;

    *cp++ = '[';
    for (i = 0;  i < PERID_LEN;  i++) {
	sprintf (cp, "%02x", perID->bytes[i]);
	cp += 2;
    }
    *cp++ = ']';
    *cp++ = '\0';

} /* end of ShowPerID */
