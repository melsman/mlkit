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

FILE* repllog = NULL;

char *
read_str(int fd, char* buf, int len) {
  char *start = buf;
  int ret;
  while ( 1 == (ret = read(fd,buf,1)) && *buf != ' ' && *buf != ';' && len > 1 ) {
    buf++; len--;
  }
  if (ret == 0) {
    die ("Repl.read_str: end-of-file");
  }
  if (ret == -1) {
    die ("Repl.read_str: cannot read file");
  }
  *buf = '\0';
  fprintf(repllog, "{read string '%s'}\n", start);
  fflush(repllog);
  return start;
}

char *
read_qstr(int fd, char* buf, int len) {
  char *start = buf;
  char sp;
  int ret;
  if ( 1 == (ret = read(fd,buf,1)) && *buf != '"' ) {
    die ("read_qstr: expecting double quote");
  }
  while ( 1 == (ret = read(fd,buf,1)) && *buf != '"' && len > 1 ) {
    buf++; len--;
  }
  if (ret == 0) {
    die ("Repl.read_qstr: end-of-file");
  }
  if (ret == -1) {
    die ("Repl.read_qstr: cannot read file");
  }
  if ( read(fd,&sp,1) != 1 || sp != ' ' ) {
    die ("read_qstr: failed to read final space");
  }
  *buf = '\0';
  fprintf(repllog, "{read quoted string '%s'}\n", start);
  fflush(repllog);
  return start;
}

#define BUF_LEN  1000
#define BUF_LEN2 2000

// ------------------
// printing support
// ------------------

char* pretty_topbuf200;
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

void
REG_POLY_FUN_HDR(pretty_ML_Print, Context ctx, String s, uintptr_t exn) {
  convertStringToC(ctx, s, pretty_topbuf200, 200, exn);
}  // side-effecting toplevel buffer topbuf200...

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

void
print_value(char* tau, char* lab, size_t len, char* buf) {
  fprintf(repllog, "{printing content of %s : %s}\n", lab, tau);
  fflush(repllog);
  void* symb = (void*)dlsym(RTLD_DEFAULT,lab);
  if ( !symb ) {
    fprintf(stderr, "Repl.print_value: Error resolving %s: %s\n", lab, dlerror());
    exit(EXIT_FAILURE);
  }
  int ret = 0;
  if ( strcmp(tau, "int") == 0 || strcmp(tau, "int64") == 0 ) {
    ret = snprintf(buf, len, "%lld", convertIntToC(*(long long*)symb));
  } else if ( strcmp(tau, "bool") == 0 ) {
    if ( *(long long*)symb == mlTRUE ) {
      ret = snprintf(buf, len, "true");
    } else {
      ret = snprintf(buf, len, "false");
    }
  } else if ( strcmp(tau, "real") == 0 ) {
    ret = snprintf(buf, len, "%f", convertRealToC(*(size_t**)symb));
  } else {
    fprintf(stderr, "WARNING: Repl.print_value cannot print values of type %s\n", tau);
    snprintf(buf, len, "unknown");
  }
  if ( ret < 0 ) {
    fprintf(stderr, "Repl.print_value: Error printing into buffer: %s\n", dlerror());
    exit(EXIT_FAILURE);
  }
  return;
}

void
print_value2(char* tau, char* lab, size_t len, char* buf) {
  fprintf(repllog, "{attempt printing with pretty_exported function %s : %s}\n", lab, tau);
  fflush(repllog);
  size_t(*pretty_exported)() = (size_t(*)())dlsym(RTLD_DEFAULT,"pretty_exported");
  if ( !pretty_exported ) {
    print_value(tau,lab,len,buf);
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
  if ( sz != strlen(pretty_topbuf200) ) {
    fprintf(stderr, "Repl.print_value2: sz error: %d - %s: %s\n", sz, pretty_topbuf200, dlerror());
    exit(EXIT_FAILURE);
  }
  int ret = snprintf(buf, len, "%s", pretty_topbuf200);
  if ( ret < 0 ) {
    fprintf(stderr, "Repl.print_value2: Error printing into buffer: %s\n", dlerror());
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

  char* buf = (char *) malloc(BUF_LEN);
  char* buf2 = (char *) malloc(BUF_LEN);
  char* buf3 = (char *) malloc(BUF_LEN);
  char* buf4 = (char *) malloc(BUF_LEN);
  char* buf5 = (char *) malloc(BUF_LEN2);
  pretty_topbuf200 = (char *) malloc(BUF_LEN2);

  while (1)  {
    fprintf(repllog, "{reading command}\n");
    fflush(repllog);
    char* cmd = read_str(command_fd, buf, BUF_LEN);

    if ( strcmp(cmd, "PRINT") == 0 ) {
      char* tau = read_qstr(command_fd, buf2, BUF_LEN);
      char* lab = read_str(command_fd, buf3, BUF_LEN);
      print_value2(tau,lab,BUF_LEN,buf4);
      int sz = strlen(buf4);
      snprintf(buf5,BUF_LEN2,"STR %d %s;",sz,buf4);
      write_str(reply_fd, buf5);
    } else if ( strcmp(cmd, "LOADRUN") == 0 ) {
      char* sofile = read_str(command_fd, buf2, BUF_LEN);
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
      free(buf); free(buf2); free(buf3);
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
