struct ulList
{
  struct ulList *next;
  char *ul;
  char *loc;
};

struct uoList
{
  struct uoList *next;
  char *uo;
};

struct smlList
{
  struct smlList *next;
  char *sml;
  char *as;
};

struct parsetree
{
  struct ulList *ul;
  struct uoList *uo;
  struct smlList *sml;
};


