/* unix-sigtbl.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 *
 * NOTE: this file is generated --- do not edit!!!
 */


PVT sys_const_t SigInfo[NUM_SIGS] = {
    { SIGHUP, "HUP" },
    { SIGINT, "INT" },
    { SIGQUIT, "QUIT" },
    { SIGPIPE, "PIPE" },
    { SIGALRM, "ALRM" },
    { SIGTERM, "TERM" },
    { SIGUSR1, "USR1" },
    { SIGUSR2, "USR2" },
    { SIGCHLD, "CHLD" },
    { SIGWINCH, "WINCH" },
    { SIGURG, "URG" },
    { SIGIO, "IO" },
    { SIGTSTP, "TSTP" },
    { SIGCONT, "CONT" },
    { SIGTTIN, "TTIN" },
    { SIGTTOU, "TTOU" },
    { SIGVTALRM, "VTALRM" },
  /* Run-time signals */
    { RUNSIG_GC, "GC" },
};
PVT sysconst_tbl_t SigTbl = {
    /* numConsts */ NUM_SYSTEM_SIGS,
    /* consts */    SigInfo
};
