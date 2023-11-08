// _GNU_SOURCE is necessary on Linux for dlfcn.h to define
// RTLD_DEFAULT - and it must be defined before other
// #includes...

#define _GNU_SOURCE

#include <stdio.h>
#include <signal.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <dlfcn.h>

#include "Runtime.h"
#include "Flags.h"
#include "Tagging.h"
#include "String.h"
#include "Math.h"
#include "Exception.h"
#include "Region.h"
#include "Table.h"
#include "String.h"

size_t
max(size_t a, size_t b) {
  return (a > b) ? a : b;
}

FILE* repllog = NULL;

size_t pretty_string_size = 100;
size_t pretty_depth = 5;

char *
read_str(int fd, char** pbuf, size_t* buf_sz) {
  int ret;
  size_t n = 0;
  char *buf = *pbuf;
  while ( 1 == (ret = read(fd,buf,1)) && *buf != ' ' && *buf != ';' ) {
    buf++; n++;
    if ( n >= *buf_sz ) {  // incremental resize
      size_t new_sz = 2 * (*buf_sz);
      char* new = (char*) malloc(new_sz);
      if ( !new ) {
	die ("read_str: failed to increase size of buffer");
      }
      *buf_sz = new_sz;
      buf = stpncpy(new,*pbuf,n);
      free(*pbuf);
      *pbuf = new;
    }
  }
  if ( ret == 0 ) {
    die ("Repl.read_str: end-of-file");
  }
  if ( ret == -1 ) {
    die ("Repl.read_str: cannot read file");
  }
  *buf = '\0';
  char *start = *pbuf;
  fprintf(repllog, "{read string '%s'}\n", start);
  fflush(repllog);
  return start;
}

char *
read_qstr(int fd, char** pbuf, size_t* buf_sz) {
  char sp;
  int ret;
  size_t n = 0;
  char* buf = *pbuf;
  if ( 1 == (ret = read(fd,buf,1)) && *buf != '"' ) {
    die ("read_qstr: expecting double quote");
  }
  while ( 1 == (ret = read(fd,buf,1)) && *buf != '"' ) {
    buf++; n++;
    if ( n >= *buf_sz ) {  // incremental resize
      size_t new_sz = 2 * (*buf_sz);
      char* new = (char*) malloc(new_sz);
      if ( !new ) {
	die ("read_qstr: failed to increase buffer size");
      }
      *buf_sz = new_sz;
      buf = stpncpy(new,*pbuf,n);
      free(*pbuf);
      *pbuf = new;
    }
  }
  if ( ret == 0 ) {
    die ("Repl.read_qstr: end-of-file");
  }
  if ( ret == -1 ) {
    die ("Repl.read_qstr: cannot read file");
  }
  if ( read(fd,&sp,1) != 1 || sp != ' ' ) {
    die ("read_qstr: failed to read final space");
  }
  *buf = '\0';         // overwrite double-quote
  char *start = *pbuf;
  fprintf(repllog, "{read quoted string '%s'}\n", start);
  fflush(repllog);
  return start;
}

#define BUF_LEN 100

// ------------------
// printing support
// ------------------

char* pretty_topbuf;
size_t pretty_topbuf_sz;

char* pretty_hidden_ty = "int";   // type
void* pretty_hidden_v = NULL;     // value

String
REG_POLY_FUN_HDR(pretty_ML_GetTy, Region rAddr) {
  return REG_POLY_CALL(convertStringToML, rAddr, pretty_hidden_ty);
}

void*
pretty_ML_GetVal (void) {
  return *(void**)pretty_hidden_v;
}


// maybe_resize_buf(name, buf, buf_sz, sz, n) may resize buf to be at
// least of size sz.
void
maybe_resize_buf(char* name, char** buf, size_t* buf_sz, size_t sz) {
  if ( sz > *buf_sz ) {    // resize buffer
    size_t new_sz = max(sz, *buf_sz * 2);
    fprintf(repllog, "{resizing buffer %s: %ld -> %ld}\n", name, *buf_sz, new_sz);
    fflush(repllog);
    *buf_sz = new_sz;
    free(*buf);
    *buf = (char*)malloc(new_sz);
    if ( ! (*buf) ) {
      die ("maybe_resize_buf: failed to increase size of buffer for printing");
    }
  }
  return;
}


void
REG_POLY_FUN_HDR(pretty_ML_Print, Context ctx, String s, uintptr_t exn) {
  // side-effecting toplevel buffer pretty_topbuf
  size_t sz = get_string_size(s->size);
  maybe_resize_buf("topbuf", &pretty_topbuf, &pretty_topbuf_sz, sz+2);
  convertStringToC(ctx, s, pretty_topbuf, pretty_topbuf_sz, exn);
}

// ------------------
// Command handling
// ------------------

void*
load(char* sopath) {
  fprintf(repllog, "{loading '%s'}\n", sopath);
  fflush(repllog);
  void* h_lib = dlopen(sopath, RTLD_NOW|RTLD_GLOBAL);
  if ( !h_lib ) {
    fprintf(stderr, "Repl.load.dlopen: Error openening %s: %s", sopath, dlerror());
    exit(EXIT_FAILURE);
  }
  return h_lib;
}

void
load_run(char* sopath, char* lab) {
  fprintf(repllog, "{loading '%s' and running '%s'}\n", sopath, lab);
  fflush(repllog);
  void* h_lib = load(sopath);
  void (*f)();
  f = (void(*)())dlsym(h_lib,lab);
  if ( !f ) {
    fprintf(stderr, "Repl.load_run.dlsym: Error resolving %s: %s", lab, dlerror());
    exit(EXIT_FAILURE);
  }
  fprintf(repllog, "{found symbol '%s'}\n", lab);
  fflush(repllog);
  (*f)();  // execute function
  fprintf(repllog, "{execution done}\n");
  fflush(repllog);
  return;
}

// prints into pretty_topbuf
void
print_value(char* tau, char* lab) {
  fprintf(repllog, "{printing content of %s : %s}\n", lab, tau);
  fflush(repllog);
  void* symb = (void*)dlsym(RTLD_DEFAULT,lab);
  if ( !symb ) {
    fprintf(stderr, "Repl.print_value: Error resolving %s: %s\n", lab, dlerror());
    exit(EXIT_FAILURE);
  }
  int ret = 0;
  if ( strcmp(tau, "int") == 0 || strcmp(tau, "int64") == 0 ) {
    ret = snprintf(pretty_topbuf, pretty_topbuf_sz, "%lld", convertIntToC(*(long long*)symb));
  } else if ( strcmp(tau, "bool") == 0 ) {
    if ( *(long long*)symb == mlTRUE ) {
      ret = snprintf(pretty_topbuf, pretty_topbuf_sz, "true");
    } else {
      ret = snprintf(pretty_topbuf, pretty_topbuf_sz, "false");
    }
  } else if ( strcmp(tau, "real") == 0 ) {
    ret = snprintf(pretty_topbuf, pretty_topbuf_sz, "%f", convertRealToC(*(size_t**)symb));
  } else {
    fprintf(stderr, "WARNING: Repl.print_value cannot print values of type %s\n", tau);
    snprintf(pretty_topbuf, pretty_topbuf_sz, "unknown");
  }
  if ( ret < 0 ) {
    fprintf(stderr, "Repl.print_value: Error printing into buffer: %s\n", dlerror());
    exit(EXIT_FAILURE);
  }
  return;
}

void
print_value2(char* tau, char* lab) {
  fprintf(repllog, "{attempt printing with pretty_exported function %s : %s}\n", lab, tau);
  fflush(repllog);
  size_t(*pretty_exported)() = (size_t(*)())dlsym(RTLD_DEFAULT,"pretty_exported");
  if ( !pretty_exported ) {
    print_value(tau,lab);
    return;
  }
  fprintf(repllog, "{found pretty_exported function}\n");
  fflush(repllog);
  void* symb = (void*)dlsym(RTLD_DEFAULT,lab);
  if ( !symb ) {
    fprintf(stderr, "Repl.print_value2: Error resolving %s: %s\n", lab, dlerror());
    exit(EXIT_FAILURE);
  }
  pretty_hidden_ty = tau;
  pretty_hidden_v = symb;
  int sz = pretty_exported(0);
  if ( sz != strlen(pretty_topbuf) ) {
    fprintf(stderr, "Repl.print_value2: sz error: %d - %s: %s\n", sz, pretty_topbuf, dlerror());
    exit(EXIT_FAILURE);
  }
  return;
}

void
write_str(int fd, char* s) {
  fprintf(repllog, "{writing reply %s}\n", s);
  fflush(repllog);
  int len = strlen(s);
  int ret = write(fd,s,len);
  if ( ret != len ) {
    fprintf(stderr, "Repl.write_str: Failed to write string '%s'\n", s);
    fflush(stderr);
    exit(EXIT_FAILURE);
  }
}

extern const char* command_pipe;
extern const char* reply_pipe;
extern const char* repl_logfile;

void
repl_interp(Context ctx) {

  // Called by function 'code', which is generated by
  // CodeGenX86.generate_repl_init_code

  // At this point, global regions and global exception constructors
  // have been declared.

  // The parent of the process running the interpreter sends commands
  // to this process for interpretation. The parent communicates with
  // its child (this process) using two named pipes, which have been
  // created by the parent with mkfifo(...):
  //
  //   - parent sends commands on the named pipe 'command_pipe'
  //
  //   - child sends replies on the named pipe 'reply_pipe'
  //
  // The variables command_pipe and reply_pipe are passed to the child
  // process using command line options.

  if ( !repl_logfile ) {
    die ("Use '-repl_logfile file' to set REPL logfile name!");
  }
  repllog = fopen(repl_logfile, "w");
  if ( !repllog ) {
    perror("Failed to open REPL logfile for writing");
    exit(EXIT_FAILURE);
  }

  if ( !command_pipe ) {
    die ("Use '-command_pipe file' to set command pipe file name!");
  }
  int command_fd = open(command_pipe, O_RDONLY);
  if ( command_fd == -1 ) {
    perror("Failed to open command_pipe file for reading");
    exit(EXIT_FAILURE);
  }

  if ( !reply_pipe ) {
    die ("Use '-reply_pipe f' to set reply pipe file name!");
  }
  int reply_fd = open(reply_pipe, O_WRONLY);
  if ( reply_fd == -1 ) {
    perror("Failed to open reply_pipe file for writing");
    exit(EXIT_FAILURE);
  }

  // Initialize string buffers and their sizes
  size_t buf1_sz = BUF_LEN;
  size_t buf2_sz = BUF_LEN;
  size_t buf3_sz = BUF_LEN;
  size_t buf4_sz = BUF_LEN;
  char* buf1 = (char *) malloc(buf1_sz);
  char* buf2 = (char *) malloc(buf2_sz);
  char* buf3 = (char *) malloc(buf3_sz);
  char* buf4 = (char *) malloc(buf4_sz);
  pretty_topbuf_sz = BUF_LEN;
  pretty_topbuf = (char *) malloc(pretty_topbuf_sz);

  if ( !buf1 || !buf2 || !buf3 || !buf4 || !pretty_topbuf ) {
    die ("repl_interp: failed to allocate buffers");
  }

  while (1)  {
    fprintf(repllog, "{reading command}\n");
    fflush(repllog);
    char* cmd = read_str(command_fd, &buf1, &buf1_sz);

    if ( strcmp(cmd, "PRINT") == 0 ) {
      char* tau = read_qstr(command_fd, &buf2, &buf2_sz);
      char* lab = read_str(command_fd, &buf3, &buf3_sz);
      print_value2(tau,lab);      // prints into pretty_topbuf;
      size_t sz = strlen(pretty_topbuf);
      maybe_resize_buf("buf4", &buf4, &buf4_sz, sz+50);
      snprintf(buf4,buf4_sz,"STR %ld %s;",sz,pretty_topbuf);
      write_str(reply_fd, buf4);
    } else if ( strcmp(cmd, "SET") == 0 ) {
      char *key = read_str(command_fd, &buf2, &buf2_sz);
      char *value = read_str(command_fd, &buf3, &buf3_sz);
      if ( strcmp(key, "pretty_depth") == 0 || strcmp(key, "pretty_string_size") == 0 ) {
	size_t val = strtol(value, NULL, 10);
	if ( val <= 0 ) {
	  fprintf(repllog, "{failed to set %s - expecting integer larger than 0}\n", key);
	  fflush(repllog);
	} else {
	  fprintf(repllog, "{setting %s to %ld}\n", key, val);
	  fflush(repllog);
	  if ( strcmp(key, "pretty_depth") == 0 ) {
	    pretty_depth = val;
	  } else if ( strcmp(key, "pretty_string_size") == 0 ) {
	    pretty_string_size = val;
	  }
	}
      } else {
	fprintf(repllog, "{unknown variable %s}\n", key);
	fflush(repllog);
      }
      write_str(reply_fd, "DONE;");
    } else if ( strcmp(cmd, "LOADRUN") == 0 ) {
      char* sofile = read_str(command_fd, &buf2, &buf2_sz);
      load_run(sofile, "main");
      if ( uncaught_exn_raised ) {
	uncaught_exn_raised = 0;
	write_str(reply_fd, "EXN;");
      } else {
	write_str(reply_fd, "DONE;");
      }
    } else if ( strcmp(cmd, "TERMINATE") == 0 ) {
      close(command_fd);
      close(reply_fd);
      write_str(reply_fd, "DONE;");
      fclose(repllog);
      exit(0);
    } else {
      fprintf(stderr, "REPL Error: unrecognised command '%s'\n", cmd);
      fflush(stderr);
      exit(EXIT_FAILURE);
    }
  }
}

// ------------------------
// Tuple selector function
// (for REPL pretty printing)
// ------------------------
void*
selectTuple(void* x, size_t i) {
  i = convertIntToC(i);
  return (void*)elemRecordML(x,i);
}

// -------------------------------------------
// Deconstructors for datatype constructors
// -------------------------------------------
unsigned long
val_con_tag(void* x) {
  x = *(void **)x;
  return convertIntToML(((unsigned long)x) >> 6);
}

unsigned long
val_is_con1(void* x) {
  x = *(void **)x;
  if ( (((unsigned long)x) & 0x3) == 3 ) {
    return mlTRUE;
  } else {
    return mlFALSE;
  }
}

unsigned long
val_ubcon_tag(void* x) {
  // return the three least significant bits
  unsigned long y = ((unsigned long)x) & 0x3;
  if ( y == 0x3 ) {  // con0
    return convertIntToML((unsigned long)x >> 2);
  } else { // con1
    return convertIntToML(y);
  }
}

unsigned long
val_is_ubcon1(void* x) {
  if ( (((unsigned long)x) & 0x3) == 0x3 ) {
    return mlFALSE;
  } else {
    return mlTRUE;
  }
}

void *
val_con1_prj(void* x) {
  return *(((void **)x)+1);
}

void *
val_ubcon1_prj(void* x) {
  // clear the three least significant bits
  return (void *)(((unsigned long)x) & (UINTPTR_MAX ^ 0x7));
}

// -------------------------------------------
// Pretty-printing control
// -------------------------------------------

size_t
get_pretty_depth() {
  return convertIntToML(pretty_depth);
}

size_t
get_pretty_string_size() {
  return convertIntToML(pretty_string_size);
}
