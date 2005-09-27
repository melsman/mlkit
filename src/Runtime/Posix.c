#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>
#include "Tagging.h"

int
sml_WIFEXITED(int status)
{
  if (WIFEXITED(convertIntToC(status)))
    return mlTRUE;
  else return mlFALSE;
}

int
sml_WIFSIGNALED(int status)
{
  if (WIFSIGNALED(convertIntToC(status)))
    return mlTRUE;
  else return mlFALSE;
}

int
sml_WIFSTOPPED(int status)
{
  if (WIFSTOPPED(convertIntToC(status)))
    return mlTRUE;
  else return mlFALSE;
}

int
sml_WEXITSTATUS(int status)
{
  return convertIntToML(WEXITSTATUS(convertIntToC(status)));
}

int
sml_WTERMSIG(int status)
{
  return convertIntToML(WTERMSIG(convertIntToC(status)));
}

int
sml_WSTOPSIG(int status)
{
  return convertIntToML(WSTOPSIG(convertIntToC(status)));
}

int 
sml_waitpid(int pair, int waitpid_arg, int flags) 
{
  int status;
  int pid = waitpid(convertIntToC(waitpid_arg),
		    &status, 
		    convertIntToC(flags));
  mkTagPairML(pair);
  first(pair) = convertIntToML(pid);
  second(pair) = convertIntToML(status);
  return pair;
}

