/* new-boot.c
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * This is the new bootstrap loader for booting from .bin files (instead
 * of .mo files).
 */

#include "ml-osdep.h"
#include <stdio.h>
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

/** The names of the boot and binary file lists **/
PVT char	*FileLists[] = {
	"BOOTLIST", "PERVLIST", "BINLIST"
    };
#define NUM_FILE_LISTS	(sizeof(FileLists) / sizeof(char *))


pers_id_t	RunTimePerID = {RUNTIME_PERID};


/* The persistent ID list is stored in the PervStruct refcell.  It is a
 * list of (PerID, ML-object) pairs.
 */
#define PerIDList	(*PTR_MLtoC(ml_val_t, PervStruct))

PVT ml_val_t	BinFileList = LIST_nil;	/* A list of bin files to load */


/* local routines */
PVT ml_val_t BuildFileList (ml_state_t *msp, const char *binDir);
PVT FILE *OpenBinFile (const char *binDir, const char *fname, bool_t isBinary);
PVT void ReadBinFile (
    FILE *file, void *buf, int nbytes,
    const char *binDir, const char *fname
);
PVT void LoadBinFile (ml_state_t *msp, const char *binDir, const char *fname);
PVT void EnterPerID (ml_state_t *msp, pers_id_t *perID, ml_val_t obj);
PVT ml_val_t LookupPerID (pers_id_t *perID);
PVT void ShowPerID (char *buf, pers_id_t *perID);


/* BootML:
 *
 * Boot the system using the .bin files from binDir.
 */
void BootML (const char *binDir, heap_params_t *heapParams)
{
    ml_state_t	*msp;
    char	*fname;

    msp = AllocMLState (TRUE, heapParams);

#ifdef HEAP_MONITOR
    if (HeapMon_Init(CmdLineArgs, msp->ml_heap) == FAILURE)
	Die("unable to start heap monitor");
#endif

    InitFaultHandlers ();
    AllocGlobals (msp);

  /* Enter the runtime system binding */
    EnterPerID (msp, &RunTimePerID, RunTimeCompUnit);

  /* construct the list of files to be loaded */
    BinFileList = BuildFileList (msp, binDir);

  /* boot the system */
    while (BinFileList != LIST_nil) {
	fname = PTR_MLtoC(char, LIST_hd(BinFileList));
	Say ("[Loading %s]\n", fname);
	BinFileList = LIST_tl(BinFileList);
	LoadBinFile (msp, binDir, fname);
    }

} /* end of BootML */


/* BuildFileList:
 *
 * Given the directory path, build a list of the .bin files in the
 * heap.
 */
PVT ml_val_t BuildFileList (ml_state_t *msp, const char *binDir)
{
    FILE	*listF;
    ml_val_t	fileNames[MAX_NUM_BOOT_FILES];
    int		i, j, numFiles;
    char	nameBuf[MAX_BOOT_PATH_LEN];
    ml_val_t	fileList;

    for (numFiles = 0, i = 0;  i < NUM_FILE_LISTS;  i++) {
	listF = OpenBinFile (binDir, FileLists[i], FALSE);
	if (listF == NULL)
	    continue;
      /* read in the file names, converting them to ML strings. */
	while (fgets (nameBuf, MAX_BOOT_PATH_LEN, listF) != NIL(char *)) {
	    j = strlen(nameBuf)-1;
	    if (nameBuf[j] == '\n') nameBuf[j] = '\0';	/* remove "\n" */
	    if (numFiles < MAX_NUM_BOOT_FILES)
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

    return fileList;

} /* end of BuildFileList */


/* OpenBinFile:
 *
 * Open a file in the bin file directory.
 */
PVT FILE *OpenBinFile (const char *binDir, const char *fname, bool_t isBinary)
{
    char	path[MAX_BOOT_PATH_LEN];
    FILE	*file;

    sprintf(path, "%s%c%s", binDir, PATH_ARC_SEP, fname);

    if ((file = fopen(path, isBinary ? "rb" : "r")) == NULL)
	Error ("unable to open \"%s\"\n", path);

    return file;

} /* end of OpenBinFile */


/* ReadBinFile:
 */
PVT void ReadBinFile (
    FILE *file, void *buf, int nbytes, const char *binDir, const char *fname
)
{
    if (fread(buf, nbytes, 1, file) == -1)
	Die ("cannot read file \"%s%c%s\"", binDir, PATH_ARC_SEP, fname);

} /* end of ReadBinFile */


/* LoadBinFile:
 */
PVT void LoadBinFile (ml_state_t *msp, const char *binDir, const char *fname)
{
    FILE	    *file;
    int		    i, importSzB, exportSzB, remainingCode;
    ml_val_t	    codeObj, importVec, closure, exportVal, val;
    binfile_hdr_t   hdr;
    pers_id_t	    exportPerID;
    Int32_t         thisSzB;

  /* open the file */
    file = OpenBinFile (binDir, fname, TRUE);
    if (file == NULL)
	Exit (1);

  /* get the header */
    ReadBinFile (file, &hdr, sizeof(binfile_hdr_t), binDir, fname);

  /* get header byte order right */
    hdr.importCnt	= BIGENDIAN_TO_HOST(hdr.importCnt);
    hdr.exportCnt	= BIGENDIAN_TO_HOST(hdr.exportCnt);
    hdr.cmInfoSzB	= BIGENDIAN_TO_HOST(hdr.cmInfoSzB);
    hdr.lambdaSzB	= BIGENDIAN_TO_HOST(hdr.lambdaSzB);
    hdr.reserved1	= BIGENDIAN_TO_HOST(hdr.reserved1);
    hdr.reserved2	= BIGENDIAN_TO_HOST(hdr.reserved2);
    hdr.codeSzB		= BIGENDIAN_TO_HOST(hdr.codeSzB);
    hdr.envSzB		= BIGENDIAN_TO_HOST(hdr.envSzB);

  /* read the import PerIDs, and create the import vector */
    {
	pers_id_t	*imports;

	importSzB = hdr.importCnt*sizeof(pers_id_t);
	imports = (pers_id_t *) MALLOC (importSzB);
	ReadBinFile (file, imports, importSzB, binDir, fname);

	if (NeedGC (msp, REC_SZB(hdr.importCnt)))
	    InvokeGCWithRoots (msp, 0, &BinFileList, &exportVal, NIL(ml_val_t *));

      /* allocate the import PerID vector */
	ML_AllocWrite (msp, 0, MAKE_DESC(hdr.importCnt, DTAG_vector));
	for (i = 1;  i <= hdr.importCnt; i++)
	    ML_AllocWrite(msp, i, LookupPerID (&(imports[i-1])));
	importVec = ML_Alloc(msp, hdr.importCnt);

	FREE (imports);
    }

  /* read the export PerID */
    if (hdr.exportCnt == 1) {
	exportSzB = sizeof(pers_id_t);
	ReadBinFile (file, &exportPerID, exportSzB, binDir, fname);
    }
    else if (hdr.exportCnt != 0)
	Die ("# of export pids is %d (should be 0 or 1)", (int)hdr.exportCnt);
    else
	exportSzB = 0;

  /* seek to code section */
    {
	long	    off = sizeof(binfile_hdr_t)
			+ importSzB
	                + exportSzB
			+ hdr.cmInfoSzB
			+ hdr.lambdaSzB
			+ hdr.reserved1 + hdr.reserved2;

	if (fseek(file, off, SEEK_SET) == -1)
	    Die ("cannot seek on bin file \"%s%c%s\"", binDir, PATH_ARC_SEP, fname);
    }

  /* Read code objects and run them.  We add a comment string to each code
   * object to mark which bin file it came from.  This code should be the
   * same as that in ../c-libs/smlnj-runtime/mkcode.c.
   */
    val = importVec;
    remainingCode = hdr.codeSzB;
    while (remainingCode > 0) {
	int		strLen = strlen(fname);
	int		padLen, extraLen;

      /* read the size for this code object */
	ReadBinFile (file, &thisSzB, sizeof(Int32_t), binDir, fname);
	thisSzB = BIGENDIAN_TO_HOST(thisSzB);

      /* We use one byte for the length, so the longest string is 255
       * characters.  We need padding so that the code + string +
       * length byte is WORD_SZB bytes.  The padding is inserted between
       * the code and the string.
       */
	if (strLen > 255)
	    strLen = 255;
	extraLen = strLen+1;  /* include byte for length */
	padLen = ROUNDUP(thisSzB+extraLen, WORD_SZB) - (thisSzB+extraLen);
	extraLen += padLen;

      /* how much more? */
	remainingCode -= thisSzB + sizeof(Int32_t);
	if (remainingCode < 0)
	    Die ("format error (code size mismatch) in bin file \"%s%c%s\"",
		binDir, PATH_ARC_SEP, fname);

      /* allocate space and read code object */
	codeObj = ML_AllocCode (msp, thisSzB+extraLen);
	ReadBinFile (file, PTR_MLtoC(void, codeObj), thisSzB, binDir, fname);

      /* tack on the bin-file name as a comment string. */
	memcpy (PTR_MLtoC(char, codeObj)+thisSzB+padLen, fname, strLen);
	*(PTR_MLtoC(Byte_t, codeObj)+thisSzB+extraLen-1) = (Byte_t)strLen;
	
	FlushICache (PTR_MLtoC(void, codeObj), thisSzB);
      
      /* create closure */
	REC_ALLOC1 (msp, closure, PTR_CtoML(PTR_MLtoC(ml_val_t, codeObj) + 1));

      /* apply the closure to the import PerID vector */
	SaveCState (msp, &BinFileList, NIL(ml_val_t *));
	val = ApplyMLFn (msp, closure, val, TRUE);
	RestoreCState (msp, &BinFileList, NIL(ml_val_t *));

      /* do a GC, if necessary */
	if (NeedGC (msp, PERID_LEN+REC_SZB(5)))
	    InvokeGCWithRoots (msp, 0, &BinFileList, &val, NIL(ml_val_t *));
    }

  /* we are done: val -> exportVal */
    exportVal = val;

  /* record the resulting exported PerID */
    if (exportSzB != 0)
      EnterPerID (msp, &exportPerID, exportVal);

    fclose (file);

} /* end of LoadBinFile */

/* EnterPerID:
 *
 * Enter a PerID/object binding in the heap allocated list of PerIDs.
 */
PVT void EnterPerID (ml_state_t *msp, pers_id_t *perID, ml_val_t obj)
{
    ml_val_t	    mlPerID;

  /* Allocate a string for the PerID */
    mlPerID = ML_AllocString (msp, PERID_LEN);
    memcpy (PTR_MLtoC(char, mlPerID), (char *)perID, PERID_LEN);

  /* Allocate the list element */
    REC_ALLOC3(msp, PerIDList, mlPerID, obj, PerIDList);

}

/* LookupPerID:
 */
PVT ml_val_t LookupPerID (pers_id_t *perID)
{
    ml_val_t        p;

    for (p = PerIDList;  p != ML_unit;  p = REC_SEL(p, 2)) {
	if (memcmp((char *)perID, REC_SELPTR(char, p, 0), PERID_LEN) == 0)
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

