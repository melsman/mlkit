/* c-calls.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 *
 * C-side support for calling user C functions from SML/NJ.
 *
 */

#include <string.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#if defined(INDIRECT_CFUNC) 
#  include "c-library.h"
#endif
#include "ml-c.h"
#include "c-calls.h"


/* assumptions:
 *
 *     Word_t fits in a machine word
 *
 *     restrictions:
 *	   C function args must fit in Word_t
 *         C's double is the largest return value from a function 
 */

ml_val_t	dummyRoot = ML_unit;  /* empty root for GC */

/* visible_msp used to expose msp to C code */
ml_state_t	*visible_msp = NULL;

#define CONS_SZB (3*WORD_SZB)     /* desc + car + cdr */
#define CADDR_SZB (2*WORD_SZB)    /* string desc + ptr */

#define MK_SOME(msp,v) recAlloc1(msp,v)

#define NULLARY_DATACON  INT_CtoML(0)

/* this map must correspond to the layout of the type cdata and ctype
   datatypes defined in ML_FILES
*/

#define MLADDR_CODE          '@'
#define MLARRAY_CODE         'A'
#define MLCHAR_CODE          'C'
#define MLDOUBLE_CODE        'D'
#define MLFLOAT_CODE         'R'
#define MLFUNCTION_CODE      'F'
#define MLINT_CODE           'I'
#define MLLONG_CODE          'L'
#define MLPTR_CODE           'P'
#define MLSHORT_CODE         'i'
#define MLSTRING_CODE        'S'
#define MLOPENSTRUCT_CODE    '('
#define MLCLOSESTRUCT_CODE   ')'
#define MLOPENUNION_CODE     '<'
#define MLCLOSEUNION_CODE    '>'
#define MLVECTOR_CODE        'B'
#define MLVOID_CODE          'V'
#define MLPAD_CODE           '#'

#define MLSTRUCT_CODE MLOPENSTRUCT_CODE
#define MLUNION_CODE  MLOPENUNION_CODE


/* this enumeration must match the sml/nj tags on the cdata datatype */
/* see c-calls.sml */

#define MLADDR_TAG      0
#define MLARRAY_TAG     1
#define MLCHAR_TAG      2
#define MLDOUBLE_TAG    3
#define MLFLOAT_TAG     4
#define MLFUNCTION_TAG  5
#define MLINT_TAG       6
#define MLLONG_TAG      7
#define MLPTR_TAG       8
#define MLSHORT_TAG     9
#define MLSTRING_TAG    10
#define MLSTRUCT_TAG    11
#define MLUNION_TAG     12
#define MLVECTOR_TAG     13
/* #define MLVOID_TAG   not used  */

/* map from datatype tags to single char descriptor (aka code) */
char typeMap[] = {MLADDR_CODE,
		  MLARRAY_CODE,
		  MLCHAR_CODE,
		  MLDOUBLE_CODE,
		  MLFLOAT_CODE,
		  MLFUNCTION_CODE,
		  MLINT_CODE,
		  MLLONG_CODE,
		  MLPTR_CODE,
		  MLSHORT_CODE,
		  MLSTRING_CODE,
		  MLSTRUCT_CODE,
		  MLUNION_CODE,
		  MLVECTOR_CODE,
		  MLVOID_CODE};

/* utility functions */

#define CHAR_RANGE 255   /* must agree with CharRange in c-calls.sml */

PVT int extractUnsigned(unsigned char **s,int bytes)
{
    int r = 0;
    
    while (bytes--)
	r = r * CHAR_RANGE + (int) *((*s)++) - 1;
    return r;
}



/* could (should) use stdlib's strdup instead of this */
PVT char *mk_strcpy(char *s)
{
    char *p;

    if ((p = (char *) MALLOC(strlen(s)+1)) == NULL)
	Die("couldn't make string copy during C call\n");
    return strcpy(p,s);
}

Word_t *checked_memalign(int n,int align)
{
    Word_t *p;

    if (align < sizeof(Word_t))
	align = sizeof(Word_t);
    if ((p = (Word_t *)MALLOC(n)) == NULL)
	Die("couldn't alloc memory for C call\n");

    ASSERT(((Word_t)p & (Word_t)(align-1)) != 0);

    return p;
}

PVT ml_val_t recAlloc1(ml_state_t *msp,ml_val_t v)
{
    ml_val_t ret;

    REC_ALLOC1(msp,ret,v);
    return ret;
}

PVT ml_val_t mkWord32(ml_state_t *msp, Word_t p)
{
    ML_AllocWrite(msp, 0, MAKE_DESC(sizeof(Word_t), DTAG_string));
    ML_AllocWrite(msp, 1, (ml_val_t)p);
    return ML_Alloc(msp, sizeof(Word_t));
}

PVT Word_t getWord32(ml_val_t v)
{
    return (Word_t) REC_SEL(v,0);
}

#define MK_CADDR(msp,p) mkWord32(msp,(Word_t) (p))
#define GET_CADDR(v)    (Word_t *)getWord32(v)

PVT ml_val_t double_CtoML(ml_state_t *msp,double g)
{
    ml_val_t res;

#ifdef DEBUG_C_CALLS
SayDebug("double_CtoML: building an ML double %l.15f\n", g);
#endif
    /* Force REALD_SZB alignment */
    msp->ml_allocPtr = (ml_val_t *)((Addr_t)(msp->ml_allocPtr) | WORD_SZB);
    ML_AllocWrite(msp,0,DESC_reald);
    res = ML_Alloc(msp,(sizeof(double)>>2));
    memcpy (res, &g, sizeof(double));
    return res;
}

/* ptrs to storage alloc'd by the interface. */
typedef struct ptr_desc {
    Word_t *ptr;
    struct ptr_desc *next;
} ptrlist_t;

PVT ptrlist_t *ptrlist = NULL;

#ifdef DEBUG_C_CALLS
PVT int ptrlist_len()
{
    int i = 0;
    ptrlist_t *p = ptrlist;

    while (p != NULL) {
	i++;
	p = p->next;
    }
    return i;
}
#endif
    
    
PVT void keep_ptr(Word_t *p)
{
    ptrlist_t *q = (ptrlist_t *) checked_alloc(sizeof(ptrlist_t));

#ifdef DEBUG_C_CALLS
    SayDebug("keeping ptr %x, |ptrlist|=%d\n", p, ptrlist_len());
#endif

    q->ptr = p;
    q->next = ptrlist;
    ptrlist = q;
}

PVT void free_ptrlist()
{
    ptrlist_t *p;

#ifdef DEBUG_C_CALLS
    SayDebug("freeing ptr list, |ptrlist|=%d\n",ptrlist_len());
#endif
    p = ptrlist;
    while (p != NULL) {
	ptrlist = ptrlist->next;
	FREE(p->ptr);               /* the block */
	FREE(p);                    /* the block's descriptor */
	p = ptrlist;
    }
}

PVT ml_val_t ptrlist_to_MLlist(ml_state_t *msp)
{
    ml_val_t lp = LIST_nil;
    ml_val_t v;
    ptrlist_t *p;

#ifdef DEBUG_C_CALLS
    int i = 0;
    SayDebug("converting ptrlist (|ptrlist|=%d) to ML list ",ptrlist_len());
#endif
    p = ptrlist;
    while (p != NULL) {
#ifdef DEBUG_C_CALLS
	i++;
#endif
	ptrlist = p->next;
	v = MK_CADDR(msp,p->ptr);
	LIST_cons(msp, lp, v, lp);
	FREE(p);
	p = ptrlist;
    }
#ifdef DEBUG_C_CALLS
    SayDebug("of length %d\n", i);
#endif
    return lp;
}

/* return the number of bytes the ptrlist will occupy in the ML heap */
PVT int ptrlist_space()
{
    int n = 0;
    ptrlist_t *p;

    p = ptrlist;
    while (p != NULL) {
	p = p->next;
	n += CONS_SZB + CADDR_SZB;
    }
#ifdef DEBUG_C_CALLS
    SayDebug("space for ptrlist is %d, |ptrlist|=%d\n",n,ptrlist_len());
#endif
    return n;
}

PVT void save_ptrlist(ptrlist_t **save)
{
#ifdef DEBUG_C_CALLS
    SayDebug("saving ptrlist, |ptrlist|=%d\n", ptrlist_len());
#endif
    *save = ptrlist;
    ptrlist = NULL;
}

PVT void restore_ptrlist(ptrlist_t *save)
{
    ptrlist = save;
#ifdef DEBUG_C_CALLS
    SayDebug("restoring ptrlist, |ptrlist|=%d\n", ptrlist_len());
#endif
}

ml_val_t revMLList(ml_val_t l,ml_val_t r)
{
    if (l == LIST_nil)
	return r;
    else {
	ml_val_t tmp = LIST_tl(l);

	LIST_tl(l) = r;
	return revMLList(tmp,l);
    }
}

	
#define SMALL_SPACE 0    /* size to 'NeedGC' for a small obj, say <10 words */

PVT void spaceCheck(ml_state_t *msp, int bytes, ml_val_t *one_root)
{
    /* assume the ONE_K buffer will absorb descriptors, '\0' terminators */
    if (NeedGC(msp,bytes + ONE_K)) {
#ifdef DEBUG_C_CALLS
SayDebug("spaceCheck: invoking GC\n");
#endif
	InvokeGCWithRoots(msp,0,one_root,NIL(ml_val_t *));
	if (NeedGC(msp,bytes + ONE_K))
	    Error("spaceCheck: cannot alloc ML space for ML-C conversion\n");
    }
}


/* interface functions */

PVT char *too_many_args = "c-calls with more than 15 args not supported\n";

/* call_word_fn
 * used when the return type fits into a machine word (Word_t)
 */
PVT Word_t call_word_fn(Word_t (*f)(),int n,Word_t *args)
{
    Word_t ret = 0;

    switch(n) {
      case 0: 
	ret = (*f)();
	break;
      case 1:
	ret = (*f)(args[0]);
	break;
      case 2:
	ret = (*f)(args[0],args[1]);
	break;
      case 3:
	ret = (*f)(args[0],args[1],args[2]);
	break;
      case 4:
	ret = (*f)(args[0],args[1],args[2],args[3]);
	break;
      case 5:
	ret = (*f)(args[0],args[1],args[2],args[3],args[4]);
	break;
      case 6:
	ret = (*f)(args[0],args[1],args[2],args[3],args[4],
		   args[5]);
	break;
      case 7:
	ret = (*f)(args[0],args[1],args[2],args[3],args[4],
		   args[5],args[6]);
	break;
      case 8:
	ret = (*f)(args[0],args[1],args[2],args[3],args[4],
		   args[5],args[6],args[7]);
	break;
      case 9:
	ret = (*f)(args[0],args[1],args[2],args[3],args[4],
		   args[5],args[6],args[7],args[8]);
	break;
      case 10:
	ret = (*f)(args[0],args[1],args[2],args[3],args[4],
		   args[5],args[6],args[7],args[8],args[9]);
	break;
      case 11:
	ret = (*f)(args[0],args[1],args[2],args[3],args[4],
		   args[5],args[6],args[7],args[8],args[9],
		   args[10]);
	break;
      case 12:
	ret = (*f)(args[0],args[1],args[2],args[3],args[4],
		   args[5],args[6],args[7],args[8],args[9],
		   args[10],args[11]);
	break;
      case 13:
	ret = (*f)(args[0],args[1],args[2],args[3],args[4],
		   args[5],args[6],args[7],args[8],args[9],
		   args[10],args[11],args[12]);
	break;
      case 14:
	ret = (*f)(args[0],args[1],args[2],args[3],args[4],
		   args[5],args[6],args[7],args[8],args[9],
		   args[10],args[11],args[12],args[13]);
	break;
      case 15:
	ret = (*f)(args[0],args[1],args[2],args[3],args[4],
		   args[5],args[6],args[7],args[8],args[9],
		   args[10],args[11],args[12],args[13],args[14]);
	break;
      default:
	/* shouldn't happen; ML side assures this */
	Error(too_many_args);
    }

#ifdef DEBUG_C_CALLS
    SayDebug("call_word_fn: return=0x%x\n",ret);
#endif
    return ret;
}    

/* call_double_fn
 */
PVT double call_double_fn(double (*f)(),int n,Word_t *args)
{
    double ret;

    switch(n) {
      case 0: 
	ret = (*f)();
	break;
      case 1:
	ret = (*f)(args[0]);
	break;
      case 2:
	ret = (*f)(args[0],args[1]);
	break;
      case 3:
	ret = (*f)(args[0],args[1],args[2]);
	break;
      case 4:
	ret = (*f)(args[0],args[1],args[2],args[3]);
	break;
      case 5:
	ret = (*f)(args[0],args[1],args[2],args[3],args[4]);
	break;
      case 6:
	ret = (*f)(args[0],args[1],args[2],args[3],args[4],
		   args[5]);
	break;
      case 7:
	ret = (*f)(args[0],args[1],args[2],args[3],args[4],
		   args[5],args[6]);
	break;
      case 8:
	ret = (*f)(args[0],args[1],args[2],args[3],args[4],
		   args[5],args[6],args[7]);
	break;
      case 9:
	ret = (*f)(args[0],args[1],args[2],args[3],args[4],
		   args[5],args[6],args[7],args[8]);
	break;
      case 10:
	ret = (*f)(args[0],args[1],args[2],args[3],args[4],
		   args[5],args[6],args[7],args[8],args[9]);
	break;
      default:
	/* shouldn't happen; ML side assures this */
	Error(too_many_args);
    }
    return ret;
}    

/* call_float_fn
 */
PVT float call_float_fn(float (*f)(),int n,Word_t *args)
{
    float ret;

    switch(n) {
      case 0: 
	ret = (*f)();
	break;
      case 1:
	ret = (*f)(args[0]);
	break;
      case 2:
	ret = (*f)(args[0],args[1]);
	break;
      case 3:
	ret = (*f)(args[0],args[1],args[2]);
	break;
      case 4:
	ret = (*f)(args[0],args[1],args[2],args[3]);
	break;
      case 5:
	ret = (*f)(args[0],args[1],args[2],args[3],args[4]);
	break;
      case 6:
	ret = (*f)(args[0],args[1],args[2],args[3],args[4],
		   args[5]);
	break;
      case 7:
	ret = (*f)(args[0],args[1],args[2],args[3],args[4],
		   args[5],args[6]);
	break;
      case 8:
	ret = (*f)(args[0],args[1],args[2],args[3],args[4],
		   args[5],args[6],args[7]);
	break;
      case 9:
	ret = (*f)(args[0],args[1],args[2],args[3],args[4],
		   args[5],args[6],args[7],args[8]);
	break;
      case 10:
	ret = (*f)(args[0],args[1],args[2],args[3],args[4],
		   args[5],args[6],args[7],args[8],args[9]);
	break;
      default:
	/* shouldn't happen; ML side assures this */
	Error(too_many_args);
    }
    return ret;
}    

/* error handling */

#define NO_ERR                   0
#define ERR_TYPEMISMATCH         1
#define ERR_EMPTY_AGGREGATE      2
#define ERR_SZ_MISMATCH    3
#define ERR_WRONG_ARG_COUNT      4
#define ERR_TOO_MANY_ARGS        5

PVT char *errtbl[] = {
    "no error",
    "type mismatch",
    "empty aggregate",
    "array/vector size does not match registered size",
    "wrong number of args in C call",
    "current max of 10 args to C fn",
    };

PVT char errbuf[100];

PVT ml_val_t RaiseError(ml_state_t *msp,int err)
{
    sprintf(errbuf,"SML/NJ-C-Interface: %s",errtbl[err]);
    return RAISE_ERROR(msp, errbuf);
}


/* char *nextdatum(char *t)
 *
 * must match typeToCtl in c-calls.sml
 */
PVT char *nextdatum(char *t)
{
    int level = 0;

    do {
	switch(*t) {
	  case MLFUNCTION_CODE: {
	      int nargs, i;

	      t++;   /* skip code */
	      nargs = extractUnsigned((unsigned char **)&t,1);
	      /* skip arg types AND return type */
	      for (i = 0; i < nargs+1; i++) {
		  t = nextdatum(t);
	      }
	    }
	    break;
	  case MLPTR_CODE:
	    /* can fall through as long as Cptr has 4 bytes of sz info */
	  case MLARRAY_CODE:
	  case MLVECTOR_CODE:
	    t = nextdatum(t+5);    /* skip 4 bytes of sz info & code */
	    break;
	  case MLOPENUNION_CODE:
	    t++;                   /* skip 1 byte of sz info ; fall through */
	  case MLOPENSTRUCT_CODE:
	    t++;                   /* skip code */
	    level++;
	    break;
	  case MLCLOSEUNION_CODE:
	  case MLCLOSESTRUCT_CODE:
	    t++;                   /* skip code */
	    level--;
	    break;
	  case MLINT_CODE:
	  case MLSHORT_CODE:
	  case MLLONG_CODE:
	    /* skip 1 byte of size; fall through */
	    t++;
	  default:
	    t++;                   /* skip simple type */
	    break;
	}
    } while (level);
    return t;
}

PVT void mkCint(Word_t src,Word_t **dst,int bytes)
{
#ifdef DEBUG_C_CALLS
    SayDebug("mkCint: placing integer into %d bytes at %x\n", bytes, *dst);
#endif

#ifdef BYTE_ORDER_BIG
    src <<= (sizeof(Word_t) - bytes)*8;
#endif
    memcpy (*dst, &src, bytes);
    (*(Byte_t **)dst) += bytes;
}

PVT void mkMLint(Word_t **src,Word_t *dst,int bytes)
{
#ifdef DEBUG_C_CALLS
    SayDebug("mkMLint: reading integer from %x into %d bytes\n", *src, bytes);
#endif
    
    memcpy (dst, *src, bytes);
#ifdef BYTE_ORDER_BIG
    *dst >>= (sizeof(Word_t) - bytes)*8;
#endif
    *(Byte_t **)src += bytes;
}


#define DO_PAD(p,t)         (*(Byte_t **)(p) += extractUnsigned((unsigned char **)(t),1))
#define IF_PAD_DO_PAD(p,t)  {if (**t == MLPAD_CODE) {++(*t); DO_PAD(p,t);}}

int datumMLtoC(ml_state_t *msp,char **t,Word_t **p,ml_val_t datum)
{
    int tag = REC_SELINT(datum,0);
    ml_val_t val = REC_SEL(datum,1);
    int err = NO_ERR;
    int sz = 0;

    while (**t == MLPAD_CODE) {
	++(*t);  /* advance past code */
#ifdef DEBUG_C_CALLS
	SayDebug("datumMLtoC: adding pad from %x ", *p);
#endif
	DO_PAD(p,t);
#ifdef DEBUG_C_CALLS
	SayDebug(" to %x\n", *p);
#endif
    }
    if (typeMap[tag] != **t) {
#ifdef DEBUG_C_CALLS
       SayDebug("datumMLtoC: type mismatch %c != %d\n",**t,tag);
#endif
	return ERR_TYPEMISMATCH;
    }
    switch(*(*t)++) {
      case MLFUNCTION_CODE: {
	  char *argtypes[N_ARGS], *rettype;
	  char *this_arg, *next_arg;
	  int nargs, len, i;

	  nargs = extractUnsigned((unsigned char **)t,1);
#ifdef DEBUG_C_CALLS
	  SayDebug("datumMLtoC: function with %d args\n", nargs);
#endif
	  this_arg = *t;
	  for (i = 0; i < nargs; i++) {
	      next_arg = nextdatum(this_arg);
	      len = next_arg - this_arg;
	      argtypes[i] = (char *)checked_alloc(len+1);  /* len plus null */
	      strncpy(argtypes[i],this_arg,len);
	      argtypes[i][len] = '\0';
	      this_arg = next_arg;
#ifdef DEBUG_C_CALLS
	      SayDebug("datumMLtoC: function arg[%d] is \"%s\"\n", 
		     i,argtypes[i]);
#endif
	  }
	  /* get the return type */
	  next_arg = nextdatum(this_arg);
	  len = next_arg - this_arg;
	  rettype = (char *)checked_alloc(len+1);  /* len plus null */
	  strncpy(rettype,this_arg,len);
	  rettype[len] = '\0';
#ifdef DEBUG_C_CALLS
	  SayDebug("datumMLtoC: function returns \"%s\"\n", 
		 rettype);
#endif
	  *t = next_arg;
	  *(*p)++ = mk_C_function(msp,val,nargs,argtypes,rettype);
#ifdef DEBUG_C_CALLS
	  SayDebug("datumMLtoC: made C function\n");
#endif
        }
        break;
      case MLPTR_CODE: {
	  int szb, align;
	  Word_t *q;

	  szb = extractUnsigned((unsigned char **)t,4);
	  align = extractUnsigned((unsigned char **)t,1);
#ifdef DEBUG_C_CALLS
	  SayDebug("datumMLtoC: ptr szb=%d, align=%d\n", szb, align);
#endif
	  q = checked_memalign(szb,align);
	  keep_ptr(q);
	  *(*p)++ = (Word_t) q;
#ifdef DEBUG_C_CALLS
	  SayDebug("datumMLtoC: ptr substructure at %x\n", q);
#endif
	  if (err = datumMLtoC(msp,t,&q,val))
	      return err;
        }
	break;
      case MLCHAR_CODE:
	*(*(Byte_t **)p)++ = (Byte_t) INT_MLtoC(val);
	break;
      case MLFLOAT_CODE: 
	sz = sizeof(float);
	/* fall through */
      case MLDOUBLE_CODE: {
	  double g;

	  if (!sz) {
	      /* came in through MLDOUBLE_CODE */
	      sz = sizeof(double);
	  }
	  memcpy (&g, (Word_t *)val, sizeof(double));
#ifdef DEBUG_C_CALLS
SayDebug("datumMLtoC: ML real %l.15f:%l.15f %.15f\n", *(double *)val, g, (float) g);
#endif
	  if (sz == sizeof(float))
	      *(*(float **)p)++ = (float) g;
	  else
	      *(*(double **)p)++ = g;
        }
	break;
      case MLINT_CODE:
      case MLSHORT_CODE:
      case MLLONG_CODE:
#ifdef DEBUG_C_CALLS
	SayDebug("datumMLtoC: integer %d\n", getWord32(val));
#endif
	mkCint(getWord32(val),p,extractUnsigned((unsigned char **)t,1));
	break;
      case MLADDR_CODE:
#ifdef DEBUG_C_CALLS
       SayDebug("datumMLtoC: addr %x\n", GET_CADDR(val));
#endif
	*(*p)++ = (Word_t) GET_CADDR(val);
	break;
      case MLSTRING_CODE: {
	  char *r, *s;

	  s = PTR_MLtoC(char,val);
#ifdef DEBUG_C_CALLS
	  SayDebug("datumMLtoC: string \"%s\"\n",s);
#endif
	  r = (char *) checked_alloc(strlen(s)+1);
	  strcpy(r,s);
	  keep_ptr((Word_t *) r);
	  *(*p)++ =  (Word_t) r;
#ifdef DEBUG_C_CALLS
	  SayDebug("datumMLtoC: copied string \"%s\"=%x\n",r,r);
#endif
        }
	break;
      case MLOPENSTRUCT_CODE: {
	  ml_val_t lp = val;
	  ml_val_t hd;

#ifdef DEBUG_C_CALLS
	  SayDebug("datumMLtoC: struct\n");
#endif
	  while (**t != MLCLOSESTRUCT_CODE) {
	      hd = LIST_hd(lp);
	      if (err = datumMLtoC(msp,t,p,hd))
		  return err;
	      lp = LIST_tl(lp);
	      IF_PAD_DO_PAD(p,t);
	  }
	  (*t)++;  /* advance past MLCLOSESTRUCT_CODE */
        }
	break;
      case MLOPENUNION_CODE: {
	  Byte_t *init_p = (Byte_t *) *p;
	  char *next_try;

	  sz = extractUnsigned((unsigned char **)t,1);
#ifdef DEBUG_C_CALLS
	  SayDebug("datumMLtoC: union of size %d\n", sz);
#endif
	  if ((**t) == MLCLOSEUNION_CODE)
	      return ERR_EMPTY_AGGREGATE;
	  next_try = nextdatum(*t);
	  /* try union types until one matches or all fail */
	  while ((err = datumMLtoC(msp,t,p,val)) == ERR_TYPEMISMATCH) {
	      *t = next_try;
	      if ((**t) == MLCLOSEUNION_CODE) {
		  err = ERR_TYPEMISMATCH;
		  break;
	      }
	      next_try = nextdatum(*t);
	      *p = (Word_t *) init_p;
	  }
	  if (err)
	      return err;
	  while (**t != MLCLOSEUNION_CODE)
	      *t = nextdatum(*t);
	  (*t)++; /* advance past MLCLOSEUNION_CODE */
	  *p = (Word_t *) (init_p + sz);
        }
	break;
      case MLARRAY_CODE: 
      case MLVECTOR_CODE: {
	  int nelems,elem_sz, i;
	  char *saved_t;

	  nelems = extractUnsigned((unsigned char **)t,2);
	  elem_sz = extractUnsigned((unsigned char **)t,2);
#ifdef DEBUG_C_CALLS
	 SayDebug("datumMLtoC: array/vector of %d elems of size %d\n", 
		nelems, elem_sz);
#endif
	  i = sz = OBJ_LEN(val);
#ifdef DEBUG_C_CALLS
	  SayDebug("datumMLtoC: array/vector size is %d\n", sz);
#endif

	  if (sz != nelems)
	      return ERR_SZ_MISMATCH;
	  saved_t = *t;
	  while (!err && i--) {
	      *t = saved_t;
	      err = datumMLtoC(msp,t,p,*(ml_val_t *)val++);
	  }
	  if (err)
	      return err;
        }
	break;
      case MLCLOSESTRUCT_CODE:
      case MLCLOSEUNION_CODE:
	return ERR_EMPTY_AGGREGATE;
	break;
      default:
	Die("datumMLtoC: cannot yet handle type\n");
    }
    return err;
}

/* ML entry point for 'datumMLtoC' */
ml_val_t ml_datumMLtoC(ml_state_t *msp, ml_val_t arg)
{
    /* no GCs can occur since no allocation on ML heap */
    /* guaranteed that datum is a pointer (Cptr or Cstring) */
    char *type = REC_SELPTR(char,arg,0);
    ml_val_t datum = REC_SEL(arg,1);
    int err = 0;
    Word_t p, *q = &p;
    ml_val_t lp, ret;
    ptrlist_t *saved_pl;

    save_ptrlist(&saved_pl);
    err = datumMLtoC(msp,&type,&q,datum);
    if (err) {
      free_ptrlist();
      restore_ptrlist(saved_pl);
      return RaiseError(msp,err);
    }
    /* return (result,list of pointers to alloc'd C objects) */
    spaceCheck(msp,ptrlist_space(),&dummyRoot);
    lp = ptrlist_to_MLlist(msp);   /* this frees the ptr descriptors */
    restore_ptrlist(saved_pl);
    ret = MK_CADDR(msp,(Word_t *)p);
    REC_ALLOC2(msp, ret, ret, lp);
    return ret;
}

PVT ml_val_t word_CtoML(ml_state_t *msp,char **t,Word_t **p, ml_val_t *root)
{
    ml_val_t ret = ML_unit;
    ml_val_t mlval = ML_unit;
    int tag;
    char code;

    switch(code = *(*t)++) {
      case MLPAD_CODE:
#ifdef DEBUG_C_CALLS
	SayDebug("word_CtoML: skipping pad %x ", *p);
#endif
	DO_PAD(p,t);
#ifdef DEBUG_C_CALLS
	SayDebug(" to %x\n", *p);
#endif
	return word_CtoML(msp,t,p,root);
      case MLVOID_CODE:
	return NULLARY_DATACON;
      case MLCHAR_CODE:
	tag = MLCHAR_TAG;
	mlval = INT_CtoML(**(Byte_t **)p);
	(*(Byte_t **)p)++;
	break;
      case MLPTR_CODE: {
	  Word_t q;
#ifdef DEBUG_C_CALLS
	  SayDebug("word_CtoML: ptr %x\n", **(Word_t ****)p);
#endif
	  tag = MLPTR_TAG;
#ifdef DEBUG_C_CALLS
	  SayDebug("word_CtoML: size is %d\n", 
		   extractUnsigned((unsigned char **)t,4));
	  SayDebug("word_CtoML: align is %d\n", 
		   extractUnsigned((unsigned char **)t,1));
#else
	  *t += 5;  /* 5 bytes of size */
#endif
	  q = **p;
	  mlval = word_CtoML(msp,t,(Word_t **) &q,root);
	  (*p)++;
        }
	break;
      case MLINT_CODE: 
	tag = MLINT_TAG;
	goto handle_int;
      case MLSHORT_CODE:
	tag = MLSHORT_TAG;
	goto handle_int;
      case MLLONG_CODE:
	tag = MLLONG_TAG;
handle_int:
	{
	    Word_t w;

	    mkMLint(p,&w,extractUnsigned((unsigned char **)t,1));
	    mlval = mkWord32(msp,w);
	}
	break;
      case MLADDR_CODE: {
	  Word_t *cp = ** (Word_t ***) p;

#ifdef DEBUG_C_CALLS
	  SayDebug("word_CtoML:  C addr %x\n", cp);
#endif
	  tag = MLADDR_TAG;
	  mlval = MK_CADDR(msp,cp);
	  (*p)++;
        }
	break;
      case MLFLOAT_CODE: {
	  /* C floats become ML reals, which are doubles... */
	  tag = MLFLOAT_TAG;
	  mlval = double_CtoML(msp,(double) *(*(float **)p)++);
#ifdef DEBUG_C_CALLS
	  SayDebug("word_CtoML: made float %l.15f\n", *(double*)mlval);
#endif
        }
	break;
      case MLDOUBLE_CODE: {
	  tag = MLDOUBLE_TAG;
	  mlval = double_CtoML(msp,*(*(double **)p)++);
#ifdef DEBUG_C_CALLS
	  SayDebug("word_CtoML: made double %l.15f\n", *(double*)mlval);
#endif
        }
	break;
      case MLSTRING_CODE:
#ifdef DEBUG_C_CALLS
	SayDebug("word_CtoML:  string \"%s\"\n", (char *)**p);
#endif
	tag = MLSTRING_TAG;
	spaceCheck(msp,strlen((char *)**p),root);
	mlval = ML_CString(msp,(char *) **p);
	(*p)++;
	break;
      case MLOPENSTRUCT_CODE: {
	  ml_val_t local_root;

	  tag = MLSTRUCT_TAG;
	  mlval = LIST_nil;

#ifdef DEBUG_C_CALLS
	  SayDebug("word_CtoML: open struct\n");
#endif
	  while (**t != MLCLOSESTRUCT_CODE) {
	      LIST_cons(msp,local_root,mlval,*root);
	      ret = word_CtoML(msp,t,p,&local_root);
	      mlval = LIST_hd(local_root);
	      *root = LIST_tl(local_root);
	      LIST_cons(msp,mlval,ret,mlval);
	      IF_PAD_DO_PAD(p,t);
	  }
	  (*t)++;  /* advance past MLCLOSESTRUCT_CODE */
	  mlval = revMLList(mlval,LIST_nil);
        }
	break;
      case MLCLOSESTRUCT_CODE:
	Die("word_CtoML: found lone MLCLOSESTRUCT_CODE");
      case MLARRAY_CODE: 
      case MLVECTOR_CODE: {
	  int szb;
	  char *saved_t;
	  ml_val_t res,local_root;
	  int n,i;
	  Word_t dtag;

	  tag = (code == MLARRAY_CODE) ? MLARRAY_TAG : MLVECTOR_TAG;
	  dtag = (code == MLARRAY_CODE) ? DTAG_array : DTAG_vector;
	  n = extractUnsigned((unsigned char **)t,2);  /* number of elements */
	  szb = extractUnsigned((unsigned char **)t,2);/* element sz (bytes)*/
#ifdef DEBUG_C_CALLS
	  SayDebug("word_CtoML: array/vector with %d elems of size %d\n", n, szb);
#endif
	  saved_t = *t;
	  spaceCheck(msp,szb*n,root);
	  /* ML_AllocArray isn't used here since it might call GC */
	  ML_AllocWrite (msp, 0, MAKE_DESC(n,dtag));
	  mlval = ML_Alloc (msp, n);
	  /* clear the array/vector so that it can be GC'd if necessary */
	  for (i = 0; i < n; i++) {
	      PTR_MLtoC(ml_val_t,mlval)[i] = ML_unit;
	  }
	  for (i = 0; i < n; i++) {
	      *t = saved_t;
	      LIST_cons(msp,local_root,mlval,*root);
	      res = word_CtoML(msp,t,p,&local_root);
	      mlval = LIST_hd(local_root);
	      *root = LIST_tl(local_root);
	      PTR_MLtoC(ml_val_t,mlval)[i] = res;
	  }
        }
	break;
      default:
#ifdef DEBUG_C_CALLS
	SayDebug("word_CtoML: bad type is '%c'\n", *(*t-1));
#endif
	Die("word_CtoML: cannot yet handle type\n");
    }
    REC_ALLOC2(msp,ret,INT_CtoML(tag),mlval);
    return ret;
}

/* static  c-calls-fns.c needs to see this */
ml_val_t datumCtoML(ml_state_t *msp, char *type, Word_t p, ml_val_t *root)
{
    ml_val_t ret;

#ifdef DEBUG_C_CALLS
   SayDebug("datumCtoML: C address is %x\n", p);
#endif

#ifdef DEBUG_C_CALLS
   SayDebug("datumCtoML: type is %s\n", type);
#endif

    switch (*type) {
      case MLDOUBLE_CODE:
	ret = double_CtoML(msp, *(double *)p);
	REC_ALLOC2(msp,ret,INT_CtoML(MLDOUBLE_TAG),ret);
	break;
      case MLFLOAT_CODE:
	ret = double_CtoML(msp, (double) (*(float *)p));
	REC_ALLOC2(msp,ret,INT_CtoML(MLFLOAT_TAG),ret);
	break;
      default: {
	  Word_t *q = &p;
	  ret = word_CtoML(msp,&type,&q,root);
      }
      break;
    }
#ifdef DEBUG_C_CALLS
    SayDebug("datumCtoML: returning\n");
#endif
    return ret;
}


/* ML entry point for 'datumCtoML' */
ml_val_t ml_datumCtoML(ml_state_t *msp, ml_val_t arg)
{
    /* make copies of things that GC may move */
    char *type = mk_strcpy(REC_SELPTR(char,arg,0));
    Word_t *caddr = GET_CADDR(REC_SEL(arg,1));
    ml_val_t ret;

    ret = datumCtoML(msp,type,(Word_t) caddr,&arg);
    FREE(type);
    return ret;
}


/* ML entry point for 'c_call' */
ml_val_t ml_c_call(ml_state_t *msp, ml_val_t arg)
{
#if !defined(INDIRECT_CFUNC)
    Word_t (*f)() = (Word_t (*)())
                      REC_SELPTR(Word_t,arg,0);
#else
    Word_t (*f)() = (Word_t (*)())
                      ((cfunc_binding_t *)REC_SELPTR(Word_t,arg,0))->cfunc;
#endif
    int n_cargs = REC_SELINT(arg,1);
    ml_val_t carg_types = REC_SEL(arg,2);                   /* string list */
    char *cret_type = REC_SELPTR(char,arg,3);
    ml_val_t cargs = REC_SEL(arg,4);                        /* cdata list */
    bool_t auto_free = REC_SELINT(arg,5);
    ptrlist_t *saved_pl;

    ml_val_t p,q;
    ml_val_t ret;
    int i;
    Word_t vals[N_ARGS];
    Word_t w;
    int err = NO_ERR;

    if (n_cargs > N_ARGS)  /* shouldn't see this; ML side insures this */
	return RaiseError(msp,ERR_TOO_MANY_ARGS);
	
    /* save the ptrlist since C can call ML can call C ... */
    save_ptrlist(&saved_pl);

    p = carg_types;
    q = cargs;
    i = 0;
    while (p != LIST_nil && q != LIST_nil) {
	char *carg_type = PTR_MLtoC(char,LIST_hd(p));
	Word_t *vp;

#ifdef DEBUG_C_CALLS
	SayDebug("ml_c_call: arg %d:\"%s\"\n",i,carg_type);
#endif

	vp = &vals[i];
	if (err = datumMLtoC(msp,&carg_type,&vp,LIST_hd(q)))
	    break;
	i++;
	p = LIST_tl(p);
	q = LIST_tl(q);
    }
#ifdef DEBUG_C_CALLS
    SayDebug("ml_c_call: rettype is \"%s\"\n", cret_type);
#endif

    /* within ml_c_call, no ML allocation occurs above this point */

    if (!err && (i != n_cargs))
	err = ERR_WRONG_ARG_COUNT;
    if (err) {
	free_ptrlist();
	restore_ptrlist(saved_pl);
	return RaiseError(msp,err);
    }
#ifdef DEBUG_C_CALLS
    SayDebug("ml_c_call: calling C function at %x\n", f);
#endif

    /* expose msp so C has access to it */
    visible_msp = msp;
    switch (*cret_type) {
      case MLDOUBLE_CODE:
	ret = double_CtoML(msp,call_double_fn((double (*)())f,n_cargs,vals));
	REC_ALLOC2(msp,ret,INT_CtoML(MLDOUBLE_TAG),ret);
	break;
      case MLFLOAT_CODE:
	ret = double_CtoML(msp,
			   (double) call_float_fn((float(*)())f,n_cargs,vals));
	REC_ALLOC2(msp,ret,INT_CtoML(MLFLOAT_TAG),ret);
	break;
      case MLCHAR_CODE: {
	  Byte_t b = (Byte_t) call_word_fn(f,n_cargs,vals);
	  Byte_t *bp = &b;
	
	  ret = word_CtoML(msp,&cret_type,(Word_t **)&bp,&dummyRoot);
        }
	break;
      default: {
	Word_t w = call_word_fn(f,n_cargs,vals);
	Word_t *wp = &w;
	
	ret = word_CtoML(msp,&cret_type,&wp,&dummyRoot);
      }
    }
#ifdef DEBUG_C_CALLS
    SayDebug("ml_c_call: returned from C function\n");
#endif

#ifdef DEBUG_C_CALLS
    SayDebug("ml_c_call: auto_free is %d\n",auto_free);
#endif

    /* setup the return value, always a pair */
    {
	ml_val_t lp = LIST_nil;

	if (auto_free) {
#ifdef DEBUG_C_CALLS
	    SayDebug("ml_c_call: performing auto-free\n");
#endif

	    free_ptrlist();
	} else {
	    /* return (result,list of pointers to alloc'd C objects) */
#ifdef DEBUG_C_CALLS
	    SayDebug("ml_c_call: returning list of caddrs\n");
#endif
	    spaceCheck(msp,ptrlist_space(),&ret);
	    lp = ptrlist_to_MLlist(msp); /* this frees the ptr descriptors */
	}
	REC_ALLOC2(msp, ret, ret, lp);
    }
    restore_ptrlist(saved_pl);     /* restore the previous ptrlist */
    return ret;
}


/* end of c-calls.c */
