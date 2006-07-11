
#ifndef LOGLEVEL
#define LOGLEVEL

#ifdef APACHE
enum reportLevel
{
  DIE,
  NOTICE,
  INFO,
  DEBUG
};

#else

enum reportLevel
{
  DIE,
  CONTINUE
};

#endif

#endif // LOGLEVEL
