/* heap2exec.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 *
 * creates an executable from an SML/NJ runtime system and an SML/NJ heap
 * usage: heap2exec runtime heap executable
 *
 * requires minor runtime-system support
 *
 *
 * format of an executable: |-------------|
 *                          |             |
 *                          | runtime     |
 *                          | of size R   |
 *                          |             |
 *                          |-------------|
 *                          |             |
 *                          | heap        |
 *                          | of size H   |
 *                          |             |
 *                          |-------------|
 *                          | R (4 bytes) |
 *                          |-------------|
 *
 * on invocation, the executable looks like an executable runtime except
 * that a heap and an integer are piggy-backed onto it.  The runtime finds
 * its size by reading the last 4 bytes of its image.  It then loads the
 * heap from that offset (R).
 */

#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#define TRUE 1
#define FALSE 0
#define MIN(a,b) ((a) < (b) ? (a) : (b))

static 
void quit(void)
{
    exit(1);
}

#define ONE_M (1024*1024)
#define BUF_SZB (10*1024)
#define SAY_DOT (ONE_M)   /* when to print a '.' */

static
int cp(int from_fd,int to_fd,FILE *dot_stream)
{
    char buf[BUF_SZB];
    int bytes;
    int i = 0;

    while ((bytes = read(from_fd,buf,BUF_SZB)) > 0) {
	i++;
	if (dot_stream && ((i % (SAY_DOT/BUF_SZB)) == 0)) {
	    fprintf(dot_stream,".");
	    fflush(dot_stream);
	}
	if (write(to_fd,buf,bytes) != bytes)
	    return FALSE;
    }
    return !bytes;
}

static struct stat stat_buf;

static
void chk_stat(char *fname,struct stat *stat_buf,int flag)
{
    if (stat(fname,stat_buf) == -1) {
	fprintf(stderr,"%ccouldn't stat \"%s\"\n",(flag ? '\n' : '\0'), fname);
	quit();
    }
}

/* argv positions */
#define RUNTIME_POS 1
#define HEAP_POS    2
#define EXEC_POS    3

main(int argc, char *argv[])
{
    int i;
    int runtime_szb, npad;
    int fd, out_fd;

    if (argc != 4) {
	fprintf(stderr,"usage: %s runtime heap executable\n", argv[0]);
	quit();
    }

    /* open output file for executable */
    if ((out_fd = open(argv[EXEC_POS],O_WRONLY|O_CREAT|O_TRUNC,0777)) == -1) {
	fprintf(stderr,"couldn't create executable \"%s\"\n",argv[EXEC_POS]);
	quit();
    }
    /* set execute,read,write permission on executable */
    chk_stat(argv[EXEC_POS],&stat_buf,FALSE);
    if (fchmod(out_fd,stat_buf.st_mode | S_IRWXU) == -1) {
	fprintf(stderr,	"couldn't set exec permission on \"%s\"\n",
		argv[EXEC_POS]);
	quit();
    }
    /* dump runtime, and heap to stdout */
    for (i = RUNTIME_POS; i <= HEAP_POS; i++) {
	fprintf(stderr,"bundling %s ",argv[i]);
	if ((fd = open(argv[i],O_RDONLY)) == -1) {
	    fprintf(stderr,"\ncouldn't open \"%s\"\n",argv[i]);
	    quit();
	}
	if (i == RUNTIME_POS) {
	    chk_stat(argv[i],&stat_buf,TRUE);
	    runtime_szb = stat_buf.st_size;
	}
	if (!cp(fd,out_fd,stdout)) {
	    fprintf(stderr,"\ncouldn't copy \"%s\"\n",argv[i]);
	    quit();
	}
	close(fd);
	fprintf(stderr,"\n");
    }
    fprintf(stderr,"setting heap offset to %ld.",(long)runtime_szb);
    write(out_fd,&runtime_szb,sizeof(runtime_szb));
    /* write(out_fd,&runtime_szb,sizeof(off_t)); */
    close(out_fd);
    fprintf(stderr,"\n");
    fprintf(stderr,"done.\n");
    return 0;
}

/* end of heap2exec.c */
