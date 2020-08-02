/* Runtime library */

#define _GNU_SOURCE 1

# include <stdio.h>
# include <stdio.h>
# include <string.h>
# include <stdarg.h>
# include <stdlib.h>
# include <sys/mman.h>
# include <assert.h>
# include <errno.h>
# include <regex.h>
# include <limits.h>

# define __ENABLE_GC__
# ifndef __ENABLE_GC__
# define alloc malloc
# endif

# define WORD_SIZE (CHAR_BIT * sizeof(int))

// # define DEBUG_PRINT 1

#ifdef DEBUG_PRINT
int indent = 0;
void print_indent (void) {
  for (int i = 0; i < indent; i++) printf (" ");
  printf("| ");
}
#endif

extern size_t __gc_stack_top, __gc_stack_bottom;

/* GC pool structure and data; declared here in order to allow debug print */
typedef struct {
  size_t * begin;
  size_t * end;
  size_t * current;
  size_t   size;
} pool;

static pool space;
// size_t      *current;
/* end */

# ifdef __ENABLE_GC__

/* GC extern invariant for built-in functions */
extern void __pre_gc  ();
extern void __post_gc ();

# else

# define __pre_gc __pre_gc_subst
# define __post_gc __post_gc_subst

void __pre_gc_subst () {}
void __post_gc_subst () {}

# endif
/* end */

# define TAG_BITS    3 // NEW: was 2

# define TAG_MASK    0x3
// We mark the word rigth before the data; in Sexp tag it is the second word of the block
# define SEXP_TAG    0x0
# define STRING_TAG  0x00000001
# define ARRAY_TAG   0x00000002
# define CLOSURE_TAG 0x00000003
# define LEN(x) ((x & ((0xFFFFFFFF >> TAG_BITS) << TAG_BITS)) >> TAG_BITS)
# define TAG(x)  (x & TAG_MASK)
# define IS_SEXP(x) ((((int) (x)) & TAG_MASK) == 0)
# define TO_DATA(x) ((data*)((char*)(x) - sizeof(int)))
# define TO_SEXP(x) ((sexp*)((char*)(x) - 2 * sizeof(int)))
// # define GET_SEXP_TAG(x) ((int)(((size_t)x) >> TAG_BITS))
# define GET_SEXP_TAG(x) ((int)(((size_t)x) >> 2))
# define UNBOXED(x)  (((int) (x)) &  0x0001)
# define UNBOX(x)    (((int) (x)) >> 1)
# define BOX(x)      ((((int) (x)) << 1) | 0x0001)

typedef struct {
  int tag; 
  char contents[0];
} data; 

typedef struct {
  int tag; 
  data contents; 
} sexp;

// # define MARK_BIT_SHIFT 2
size_t MARK_BIT = 1; // 0 or 1; meaning of being marked
size_t MARK_TAG = 0; // 4 or 0; for allocation only
// size_t BLOCK_MARKED_MASK = 4; // 0 or 0xFFFFFFFB; -||-
// x is a pointer to the DATA!!
void MARK_BLOCK (size_t* x) {
  data* t = TO_DATA(x);
  if (MARK_BIT == 1) {
    t->tag = t->tag | 0x4;
  } else {
    t->tag = t->tag & (~4);
  }
}
// x is a pointer to the begin of the BLOCK! Not data!
int BLOCK_IS_MARKED (size_t* x) {
  size_t * t = x;
  if (IS_SEXP(*t)) { t += 1; }
  if (MARK_BIT == 1) {
    return (((int)*t & 0x4) != 0);
  } else {
    return (((int)*t & 0x4) == 0);
  }
}
// x is a pointer to the begin of the DATA!
int BLOCK_IS_MARKED_DATA (size_t* x) {
  data* t = TO_DATA(x);
  if (MARK_BIT == 1) {
    return (((int)t->tag & 0x4) != 0);
  } else {
    return (((int)t->tag & 0x4) == 0);
  }
}
static inline void reverse_mark_bit ( void ) {
  if (MARK_BIT == 1) {
    MARK_BIT = 0;
    MARK_TAG = 4;
  } else {
    MARK_BIT = 1;
    MARK_TAG = 0;
  }
}

/* GC extra roots */
#define MAX_EXTRA_ROOTS_NUMBER 16
typedef struct {
  int current_free;
  void ** roots[MAX_EXTRA_ROOTS_NUMBER];
} extra_roots_pool;

static extra_roots_pool extra_roots;

void clear_extra_roots (void) {
  extra_roots.current_free = 0;
}

void push_extra_root (void ** p) {
#ifdef DEBUG_PRINT
  indent++; print_indent ();
  printf ("push_extra_root %p %p\n", p, &p); fflush (stdout);
#endif
  if (extra_roots.current_free >= MAX_EXTRA_ROOTS_NUMBER) {
    perror ("ERROR: push_extra_roots: extra_roots_pool overflow");
    exit   (1);
  }
  extra_roots.roots[extra_roots.current_free] = p;
  extra_roots.current_free++;
#ifdef DEBUG_PRINT
  indent--;
#endif
}

void pop_extra_root (void ** p) {
#ifdef DEBUG_PRINT
  indent++; print_indent ();
  printf ("pop_extra_root %p %p\n", p, &p); fflush (stdout);
#endif
  if (extra_roots.current_free == 0) {
    perror ("ERROR: pop_extra_root: extra_roots are empty");
    exit   (1);
  }
  extra_roots.current_free--;
  if (extra_roots.roots[extra_roots.current_free] != p) {
#ifdef DEBUG_PRINT
    print_indent ();
    printf ("%i %p %p", extra_roots.current_free,
	    extra_roots.roots[extra_roots.current_free], p);
    fflush (stdout);
#endif
    perror ("ERROR: pop_extra_root: stack invariant violation");
    exit   (1);
  }
#ifdef DEBUG_PRINT
  indent--;
#endif
}

/* end */

static void vfailure (char *s, va_list args) {
  fprintf  (stderr, "*** FAILURE: ");
  vfprintf (stderr, s, args); /* vprintf (char *, va_list) <-> printf (char *, ...) */
  exit     (255);
}

static void failure (char *s, ...) {
  va_list args;

  va_start (args, s);
  vfailure (s, args);
}

# define ASSERT_BOXED(memo, x)               \
  do if (UNBOXED(x)) failure ("boxed value expected in %s\n", memo); while (0)
# define ASSERT_UNBOXED(memo, x)             \
  do if (!UNBOXED(x)) failure ("unboxed value expected in %s\n", memo); while (0)
# define ASSERT_STRING(memo, x)              \
  do if (!UNBOXED(x) && TAG(TO_DATA(x)->tag) \
	 != STRING_TAG) failure ("sting value expected in %s\n", memo); while (0)

extern void* alloc (size_t);
extern void* Bsexp (int n, ...);

void *global_sysargs;

/* Functional synonym for built-in operator ":"; */
void* Ls__Infix_58 (void *p, void *q) {
  void *res;
  
  __pre_gc ();

  push_extra_root(&p);
  push_extra_root(&q);
  res = Bsexp (BOX(3), p, q, 848787);
  pop_extra_root(&q);
  pop_extra_root(&p);

  __post_gc ();

  return res;
}

/* Functional synonym for built-in operator "!!"; */
int Ls__Infix_3333 (void *p, void *q) {
  ASSERT_UNBOXED("captured !!:1", p);
  ASSERT_UNBOXED("captured !!:2", q);

  return BOX(UNBOX(p) || UNBOX(q));
}

/* Functional synonym for built-in operator "&&"; */
int Ls__Infix_3838 (void *p, void *q) {
  ASSERT_UNBOXED("captured &&:1", p);
  ASSERT_UNBOXED("captured &&:2", q);

  return BOX(UNBOX(p) && UNBOX(q));
}

/* Functional synonym for built-in operator "=="; */
int Ls__Infix_6161 (void *p, void *q) {
  ASSERT_UNBOXED("captured ==:1", p);
  ASSERT_UNBOXED("captured ==:2", q);

  return BOX(UNBOX(p) == UNBOX(q));
}

/* Functional synonym for built-in operator "!="; */
int Ls__Infix_3361 (void *p, void *q) {
  ASSERT_UNBOXED("captured !=:1", p);
  ASSERT_UNBOXED("captured !=:2", q);

  return BOX(UNBOX(p) != UNBOX(q));
}

/* Functional synonym for built-in operator "<="; */
int Ls__Infix_6061 (void *p, void *q) {
  ASSERT_UNBOXED("captured <=:1", p);
  ASSERT_UNBOXED("captured <=:2", q);

  return BOX(UNBOX(p) <= UNBOX(q));
}

/* Functional synonym for built-in operator "<"; */
int Ls__Infix_60 (void *p, void *q) {
  ASSERT_UNBOXED("captured <:1", p);
  ASSERT_UNBOXED("captured <:2", q);

  return BOX(UNBOX(p) < UNBOX(q));
}

/* Functional synonym for built-in operator ">="; */
int Ls__Infix_6261 (void *p, void *q) {
  ASSERT_UNBOXED("captured >=:1", p);
  ASSERT_UNBOXED("captured >=:2", q);

  return BOX(UNBOX(p) >= UNBOX(q));
}

/* Functional synonym for built-in operator ">"; */
int Ls__Infix_62 (void *p, void *q) {
  ASSERT_UNBOXED("captured >:1", p);
  ASSERT_UNBOXED("captured >:2", q);

  return BOX(UNBOX(p) > UNBOX(q));
}

/* Functional synonym for built-in operator "+"; */
int Ls__Infix_43 (void *p, void *q) {
  ASSERT_UNBOXED("captured +:1", p);
  ASSERT_UNBOXED("captured +:2", q);

  return BOX(UNBOX(p) + UNBOX(q));
}

/* Functional synonym for built-in operator "-"; */
int Ls__Infix_45 (void *p, void *q) {
  ASSERT_UNBOXED("captured -:1", p);
  ASSERT_UNBOXED("captured -:2", q);

  return BOX(UNBOX(p) - UNBOX(q));
}

/* Functional synonym for built-in operator "*"; */
int Ls__Infix_42 (void *p, void *q) {
  ASSERT_UNBOXED("captured *:1", p);
  ASSERT_UNBOXED("captured *:2", q);

  return BOX(UNBOX(p) * UNBOX(q));
}

/* Functional synonym for built-in operator "/"; */
int Ls__Infix_47 (void *p, void *q) {
  ASSERT_UNBOXED("captured /:1", p);
  ASSERT_UNBOXED("captured /:2", q);

  return BOX(UNBOX(p) / UNBOX(q));
}

/* Functional synonym for built-in operator "%"; */
int Ls__Infix_37 (void *p, void *q) {
  ASSERT_UNBOXED("captured %:1", p);
  ASSERT_UNBOXED("captured %:2", q);

  return BOX(UNBOX(p) % UNBOX(q));
}

extern int Blength (void *p) {
  data *a = (data*) BOX (NULL);
  
  ASSERT_BOXED(".length", p);
  
  a = TO_DATA(p);
  return BOX(LEN(a->tag));
}

char* de_hash (int n1) {
  static char *chars = (char*) BOX (NULL);
  static char buf[6] = {0,0,0,0,0,0};
  char *p = (char *) BOX (NULL);
  chars =  "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
  p = &buf[5];
  size_t n = n1;

#ifdef DEBUG_PRINT
  indent++; print_indent ();
  printf ("de_hash: tag: %d\n", n); fflush (stdout);
#endif
  
  *p-- = 0;

  while (n != 0) {
#ifdef DEBUG_PRINT
    print_indent ();
    printf ("char: %c %i %i\n", chars [n & 0x003F], n, n & 0x003F); fflush (stdout);
#endif
    *p-- = chars [n & 0x003F];
    n = n >> 6;
  }

#ifdef DEBUG_PRINT
  indent--;
#endif
  
  return ++p;
}

typedef struct {
  char *contents;
  int ptr;
  int len;
} StringBuf;

static StringBuf stringBuf;

# define STRINGBUF_INIT 128

static void createStringBuf () {
  stringBuf.contents = (char*) malloc (STRINGBUF_INIT);
  stringBuf.ptr      = 0;
  stringBuf.len      = STRINGBUF_INIT;
}

static void deleteStringBuf () {
  free (stringBuf.contents);
}

static void extendStringBuf () {
  int len = stringBuf.len << 1;

  stringBuf.contents = (char*) realloc (stringBuf.contents, len);
  stringBuf.len      = len;
}

static void vprintStringBuf (char *fmt, va_list args) {
  int     written = 0,
          rest    = 0;
  char   *buf     = (char*) BOX(NULL);

 again:
  buf     = &stringBuf.contents[stringBuf.ptr];
  rest    = stringBuf.len - stringBuf.ptr;
  written = vsnprintf (buf, rest, fmt, args);
  
  if (written >= rest) {
    extendStringBuf ();
    goto again;
  }

  stringBuf.ptr += written;
}

static void printStringBuf (char *fmt, ...) {
  va_list args;

  va_start (args, fmt);
  vprintStringBuf (fmt, args);
}

int is_valid_heap_pointer (void *p);

static void printValue (void *p) {
  data *a = (data*) BOX(NULL);
  int i   = BOX(0);
  if (UNBOXED(p)) printStringBuf ("%d", UNBOX(p));
  else {
    if (! is_valid_heap_pointer(p)) {
      printStringBuf ("0x%x", p);
      return;
    }
    
    a = TO_DATA(p);

    switch (TAG(a->tag)) {      
    case STRING_TAG:
      printStringBuf ("\"%s\"", a->contents);
      break;

    case CLOSURE_TAG:
      printStringBuf ("<closure ");
      for (i = 0; i < LEN(a->tag); i++) {
	if (i) printValue ((void*)((int*) a->contents)[i]);
	else printStringBuf ("0x%x", (void*)((int*) a->contents)[i]);
	
	if (i != LEN(a->tag) - 1) printStringBuf (", ");
      }
      printStringBuf (">");
      break;
      
    case ARRAY_TAG:
      printStringBuf ("[");
      for (i = 0; i < LEN(a->tag); i++) {
        printValue ((void*)((int*) a->contents)[i]);
	if (i != LEN(a->tag) - 1) printStringBuf (", ");
      }
      printStringBuf ("]");
      break;
      
    case SEXP_TAG: {
      char * tag = de_hash (GET_SEXP_TAG(TO_SEXP(p)->tag));
      
      if (strcmp (tag, "cons") == 0) {
	data *b = a;
	
	printStringBuf ("{");

	while (LEN(a->tag)) {
	  printValue ((void*)((int*) b->contents)[0]);
	  b = (data*)((int*) b->contents)[1];
	  if (! UNBOXED(b)) {
	    printStringBuf (", ");
	    b = TO_DATA(b);
	  }
	  else break;
	}
	
	printStringBuf ("}");
      }
      else {
	printStringBuf ("%s", tag);
	if (LEN(a->tag)) {
	  printStringBuf (" (");
	  for (i = 0; i < LEN(a->tag); i++) {
	    printValue ((void*)((int*) a->contents)[i]);
	    if (i != LEN(a->tag) - 1) printStringBuf (", ");
	  }
	  printStringBuf (")");
	}
      }
    }
    break;

    default:
      printStringBuf ("*** invalid tag: 0x%x ***", TAG(a->tag));
    }
  }
}

static void stringcat (void *p) {
  data *a;
  int i;
  
  if (UNBOXED(p)) ;
  else {
    a = TO_DATA(p);

    switch (TAG(a->tag)) {      
    case STRING_TAG:
      printStringBuf ("%s", a->contents);
      break;
      
    case SEXP_TAG: {
      char * tag = de_hash (GET_SEXP_TAG(TO_SEXP(p)->tag));
      if (strcmp (tag, "cons") == 0) {
	data *b = a;
	
	while (LEN(a->tag)) {
	  stringcat ((void*)((int*) b->contents)[0]);
	  b = (data*)((int*) b->contents)[1];
	  if (! UNBOXED(b)) {
	    b = TO_DATA(b);
	  }
	  else break;
	}
      }
      else printStringBuf ("*** non-list tag: %s ***", tag);
    }
    break;

    default:
      printStringBuf ("*** invalid tag: 0x%x ***", TAG(a->tag));
    }
  }
}

extern int LmatchSubString (char *subj, char *patt, int pos) {
  data *p = TO_DATA(patt), *s = TO_DATA(subj);
  int   n;

  ASSERT_STRING("matchSubString:1", subj);
  ASSERT_STRING("matchSubString:2", patt);
  ASSERT_UNBOXED("matchSubString:3", pos);
  
  n = LEN (p->tag);

  if (n + UNBOX(pos) > LEN(s->tag))
    return BOX(0);
  
  return BOX(strncmp (subj + UNBOX(pos), patt, n) == 0);
}

extern void* Lsubstring (void *subj, int p, int l) {
  data *d = TO_DATA(subj);
  int pp = UNBOX (p), ll = UNBOX (l);

  ASSERT_STRING("substring:1", subj);
  ASSERT_UNBOXED("substring:2", p);
  ASSERT_UNBOXED("substring:3", l);
      
  if (pp + ll <= LEN(d->tag)) {
    data *r;
    
    __pre_gc ();

    push_extra_root (&subj);
    r = (data*) alloc (ll + 1 + sizeof (int));
    pop_extra_root (&subj);

    r->tag = STRING_TAG | (ll << TAG_BITS) | MARK_TAG;

    strncpy (r->contents, (char*) subj + pp, ll);
    ((char*)(r->contents))[ll] = '\0';
    
    __post_gc ();

    return r->contents;    
  }
  
  failure ("substring: index out of bounds (position=%d, length=%d, \
            subject length=%d)", pp, ll, LEN(d->tag));
}

extern struct re_pattern_buffer *Lregexp (char *regexp) {
  regex_t *b = (regex_t*) malloc (sizeof (regex_t));

  memset (b, 0, sizeof (regex_t));
  
  int n = (int) re_compile_pattern (regexp, strlen (regexp), b);
  
  if (n != 0) {
    failure ("%", strerror (n));
  };

  return b;
}

extern int LregexpMatch (struct re_pattern_buffer *b, char *s, int pos) {
  int res;
  
  ASSERT_BOXED("regexpMatch:1", b);
  ASSERT_STRING("regexpMatch:2", s);
  ASSERT_UNBOXED("regexpMatch:3", pos);

  res = re_match (b, s, LEN(TO_DATA(s)->tag), UNBOX(pos), 0);

  if (res) {
    return BOX (res);
  }

  return BOX (res);
}

extern void* Bstring (void*);

void *Lclone (void *p) {
  data *obj;
  sexp *sobj;
  void* res;
  int n;
#ifdef DEBUG_PRINT
  register int * ebp asm ("ebp");
  indent++; print_indent ();
  printf ("Lclone arg: %p %p\n", &p, p); fflush (stdout);
#endif
  __pre_gc ();
  
  if (UNBOXED(p)) return p;
  else {
    data *a = TO_DATA(p);
    int t   = TAG(a->tag), l = LEN(a->tag);

    push_extra_root (&p);
    switch (t) {
    case STRING_TAG:
#ifdef DEBUG_PRINT
      print_indent ();
      printf ("Lclone: string1 &p=%p p=%p\n", &p, p); fflush (stdout);
#endif
      res = Bstring (TO_DATA(p)->contents);
#ifdef DEBUG_PRINT
      print_indent ();
      printf ("Lclone: string2 %p %p\n", &p, p); fflush (stdout);
#endif
      break;

    case ARRAY_TAG:      
    case CLOSURE_TAG:
#ifdef DEBUG_PRINT
      print_indent ();
      printf ("Lclone: closure or array &p=%p p=%p ebp=%p\n", &p, p, ebp); fflush (stdout);
#endif
      obj = (data*) alloc (sizeof(int) * (l+1));
      memcpy (obj, TO_DATA(p), sizeof(int) * (l+1));
      /* obj->tag = obj->tag | MARK_TAG; */
      res = (void*) (obj->contents);
      break;
      
    case SEXP_TAG:
#ifdef DEBUG_PRINT
      print_indent (); printf ("Lclone: sexp\n"); fflush (stdout);
#endif
      sobj = (sexp*) alloc (sizeof(int) * (l+2));
      memcpy (sobj, TO_SEXP(p), sizeof(int) * (l+2));
      /* obj->tag = obj->tag | MARK_TAG; */
      res = (void*) sobj->contents.contents;
      break;
       
    default:
      failure ("invalid tag %d in clone *****\n", t);
    }
    pop_extra_root (&p);
  }
#ifdef DEBUG_PRINT
  print_indent (); printf ("Lclone ends1\n"); fflush (stdout);
#endif

  __post_gc ();
#ifdef DEBUG_PRINT
  print_indent ();
  printf ("Lclone ends2\n"); fflush (stdout);
  indent--;
#endif
  return res;
}

// # define HASH_DEPTH 3
# define HASH_DEPTH 1
# define HASH_APPEND(acc, x) (((acc + (unsigned) x) << (WORD_SIZE / 2)) | ((acc + (unsigned) x) >> (WORD_SIZE / 2)))

int inner_hash (int depth, unsigned acc, void *p) {
  if (depth > HASH_DEPTH) return acc;

  if (UNBOXED(p)) return HASH_APPEND(acc, UNBOX(p));
  else if (is_valid_heap_pointer (p)) {
    data *a = TO_DATA(p);
    int t = TAG(a->tag), l = LEN(a->tag), i;

    acc = HASH_APPEND(acc, t);
    acc = HASH_APPEND(acc, l);    

    switch (t) {
    case STRING_TAG: {
      char *p = a->contents;

      while (*p) {
        int n = (int) *p++;
	acc = HASH_APPEND(acc, n);
      }

      return acc;
    }
      
    case CLOSURE_TAG:
      acc = HASH_APPEND(acc, ((void**) a->contents)[0]);
      i = 1;
      break;
      
    case ARRAY_TAG:
      i = 0;
      break;

    case SEXP_TAG: {
      int ta = GET_SEXP_TAG(TO_SEXP(p)->tag);
      acc = HASH_APPEND(acc, ta);
      i = 0;
      break;
    }

    default:
      failure ("invalid tag %d in hash *****\n", t);
    }

    for (; i<l; i++) 
      acc = inner_hash (depth+1, acc, ((void**) a->contents)[i]);

    return acc;
  }
  else return HASH_APPEND(acc, p);
}

extern void* LstringInt (char *b) {
  int n;
  sscanf (b, "%d", &n);
  return (void*) BOX(n);
}

extern int Lhash (void *p) {
  return BOX(inner_hash (0, 0, p));
}

extern int Lcompare (void *p, void *q) {
# define COMPARE_AND_RETURN(x,y) do if (x != y) return BOX(x - y); while (0)
  
  if (p == q) return BOX(0);
 
  if (UNBOXED(p)) {
    if (UNBOXED(q)) return BOX(UNBOX(p) - UNBOX(q));    
    else return BOX(-1);
  }
  else if (UNBOXED(q)) return BOX(1);
  else {
    if (is_valid_heap_pointer (p)) {
      if (is_valid_heap_pointer (q)) {
        data *a = TO_DATA(p), *b = TO_DATA(q);
        int ta = TAG(a->tag), tb = TAG(b->tag);
        int la = LEN(a->tag), lb = LEN(b->tag);
        int i;
    
        COMPARE_AND_RETURN (ta, tb);
      
        switch (ta) {
        case STRING_TAG:
          return BOX(strcmp (a->contents, b->contents));
      
        case CLOSURE_TAG:
          COMPARE_AND_RETURN (((void**) a->contents)[0], ((void**) b->contents)[0]);
          COMPARE_AND_RETURN (la, lb);
          i = 1;
          break;
      
        case ARRAY_TAG:
          COMPARE_AND_RETURN (la, lb);
          i = 0;
          break;

        case SEXP_TAG: {
/* #ifndef DEBUG_PRINT */
/*           int ta = TO_SEXP(p)->tag, tb = TO_SEXP(q)->tag;       */
/* #else */
          int ta = GET_SEXP_TAG(TO_SEXP(p)->tag), tb = GET_SEXP_TAG(TO_SEXP(q)->tag);
/* #endif       */
          COMPARE_AND_RETURN (ta, tb);
          COMPARE_AND_RETURN (la, lb);
          i = 0;
          break;
        }

        default:
          failure ("invalid tag %d in compare *****\n", ta);
        }

        for (; i<la; i++) {
          int c = Lcompare (((void**) a->contents)[i], ((void**) b->contents)[i]);
          if (c != BOX(0)) return BOX(c);
        }
    
        return BOX(0);
      }
      else return BOX(-1);
    }
    else if (is_valid_heap_pointer (q)) return BOX(1);
    else return BOX (p - q);
  }
}

extern void* Belem (void *p, int i) {
  data *a = (data *)BOX(NULL);

  ASSERT_BOXED(".elem:1", p);
  ASSERT_UNBOXED(".elem:2", i);
  
  a = TO_DATA(p);
  i = UNBOX(i);
  
  if (TAG(a->tag) == STRING_TAG) {
    return (void*) BOX(a->contents[i]);
  }
  
  return (void*) ((int*) a->contents)[i];
}

extern void* LmakeArray (int length) {
  data *r;
  int n;

  ASSERT_UNBOXED("makeArray:1", length);
  
  __pre_gc ();

  n = UNBOX(length);
  r = (data*) alloc (sizeof(int) * (n+1));

  r->tag = ARRAY_TAG | (n << TAG_BITS) | MARK_TAG;
  
  memset (r->contents, 0, n * sizeof(int));
  
  __post_gc ();

  return r->contents;
}

extern void* LmakeString (int length) {
  int   n = UNBOX(length);
  data *r;

  ASSERT_UNBOXED("makeString", length);
  
  __pre_gc () ;
  
  r = (data*) alloc (n + 1 + sizeof (int));
  r->tag = STRING_TAG | (n << TAG_BITS) | MARK_TAG;

  __post_gc();
  
  return r->contents;
}

extern void* Bstring (void *p) {
  int   n = strlen (p);
  data *s = NULL;
  
  __pre_gc ();
#ifdef DEBUG_PRINT
  indent++; print_indent ();
  printf ("Bstring: call LmakeString %s %p %p %p %i\n", p, &p, p, s, n);
  fflush(stdout);
#endif
  push_extra_root (&p);
  s = LmakeString (BOX(n));  
  pop_extra_root(&p);
#ifdef DEBUG_PRINT
  print_indent ();
  printf ("\tBstring: call strncpy: %p %p %p %i\n", &p, p, s, n); fflush(stdout);
#endif
  strncpy ((char*)s, p, n + 1);
#ifdef DEBUG_PRINT
  print_indent ();
  printf ("\tBstring: ends\n"); fflush(stdout);
  indent--;
#endif
  __post_gc ();
  
  return s;
}

extern void* Lstringcat (void *p) {
  void *s;

  ASSERT_BOXED("stringcat", p);
  
  __pre_gc ();
  
  createStringBuf ();
  stringcat (p);

  push_extra_root(&p);
  s = Bstring (stringBuf.contents);
  pop_extra_root(&p);
  
  deleteStringBuf ();

  __post_gc ();

  return s;  
}

extern void* Bstringval (void *p) {
  void *s = (void *) BOX (NULL);

  __pre_gc () ;
  
  createStringBuf ();
  printValue (p);

  push_extra_root(&p);
  s = Bstring (stringBuf.contents);
  pop_extra_root(&p);
  
  deleteStringBuf ();

  __post_gc ();

  return s;
}

extern void* Bclosure (int bn, void *entry, ...) {
  va_list args; 
  int     i, ai;
  register int * ebp asm ("ebp");
  size_t  *argss;
  data    *r; 
  int     n = UNBOX(bn);
  
  __pre_gc ();
#ifdef DEBUG_PRINT
  indent++; print_indent ();
  printf ("Bclosure: create n = %d\n", n); fflush(stdout);
#endif
  argss = (ebp + 12);
  /* for (i = 0; i<n; i++, argss++) { */
  /*   push_extra_root ((void**)argss); */
  /* } */

  r = (data*) alloc (sizeof(int) * (n+2));
  
  r->tag = CLOSURE_TAG | ((n + 1) << TAG_BITS) | MARK_TAG;
  ((void**) r->contents)[0] = entry;
  
  va_start(args, entry);
  
  for (i = 0; i<n; i++) {
    ai = va_arg(args, int);
    ((int*)r->contents)[i+1] = ai;
  }
  
  va_end(args);

  __post_gc();

  argss--;
  /* for (i = 0; i<n; i++, argss--) { */
  /*   pop_extra_root ((void**)argss); */
  /* } */

#ifdef DEBUG_PRINT
  print_indent ();
  printf ("Bclosure: ends\n", n); fflush(stdout);
  indent--;
#endif

  return r->contents;
}

extern void* Barray (int bn, ...) {
  va_list args; 
  int     i, ai; 
  data    *r; 
  int     n = UNBOX(bn);
    
  __pre_gc ();
  
#ifdef DEBUG_PRINT
  indent++; print_indent ();
  printf ("Barray: create n = %d\n", n); fflush(stdout);
#endif
  r = (data*) alloc (sizeof(int) * (n+1));

  r->tag = ARRAY_TAG | (n << TAG_BITS) | MARK_TAG;
  
  va_start(args, n);
  
  for (i = 0; i<n; i++) {
    ai = va_arg(args, int);
    ((int*)r->contents)[i] = ai;
  }
  
  va_end(args);

  __post_gc();
#ifdef DEBUG_PRINT
  indent--;
#endif
  return r->contents;
}

extern void* Bsexp (int bn, ...) {
  va_list args;
  int     i;
  int     ai;
  size_t *p;
  sexp   *r;
  data   *d;
  int n = UNBOX(bn);

  __pre_gc () ;
  
#ifdef DEBUG_PRINT
  indent++; print_indent ();
  printf("Bsexp: allocate %zu!\n",sizeof(int) * (n+1)); fflush (stdout);
#endif
  r = (sexp*) alloc (sizeof(int) * (n+1));
  d = &(r->contents);
  r->tag = 0;
    
  d->tag = SEXP_TAG | ((n-1) << TAG_BITS) | MARK_TAG;
  
  va_start(args, n);
  
  for (i=0; i<n-1; i++) {
    ai = va_arg(args, int);
    
    p = (size_t*) ai;
    ((int*)d->contents)[i] = ai;
  }

  // r->tag = ((size_t)(va_arg(args, int))) << TAG_BITS;
  r->tag = (((size_t)(va_arg(args, int))) << 2) | SEXP_TAG;

#ifdef DEBUG_PRINT
  print_indent ();
  printf("Bsexp: ends\n"); fflush (stdout);
  indent--;
#endif

  va_end(args);

  __post_gc();

  return d->contents;
}

extern int Btag (void *d, int t, int n) {
  data *r; 
  
  if (UNBOXED(d)) return BOX(0);
  else {
    r = TO_DATA(d);
    return BOX(TAG(r->tag) == SEXP_TAG &&
               GET_SEXP_TAG(TO_SEXP(d)->tag) == UNBOX(t) && LEN(r->tag) == UNBOX(n));
  }
}

extern int Barray_patt (void *d, int n) {
  data *r; 
  
  if (UNBOXED(d)) return BOX(0);
  else {
    r = TO_DATA(d);
    return BOX(TAG(r->tag) == ARRAY_TAG && LEN(r->tag) == UNBOX(n));
  }
}

extern int Bstring_patt (void *x, void *y) {
  data *rx = (data *) BOX (NULL),
       *ry = (data *) BOX (NULL);
  
  ASSERT_STRING(".string_patt:2", y);
      
  if (UNBOXED(x)) return BOX(0);
  else {
    rx = TO_DATA(x); ry = TO_DATA(y);

    if (TAG(rx->tag) != STRING_TAG) return BOX(0);
    
    return BOX(strcmp (rx->contents, ry->contents) == 0 ? 1 : 0);
  }
}

extern int Bclosure_tag_patt (void *x) {
  if (UNBOXED(x)) return BOX(0);
  
  return BOX(TAG(TO_DATA(x)->tag) == CLOSURE_TAG);
}

extern int Bboxed_patt (void *x) {
  return BOX(UNBOXED(x) ? 0 : 1);
}

extern int Bunboxed_patt (void *x) {
  return BOX(UNBOXED(x) ? 1 : 0);
}

extern int Barray_tag_patt (void *x) {
  if (UNBOXED(x)) return BOX(0);
  
  return BOX(TAG(TO_DATA(x)->tag) == ARRAY_TAG);
}

extern int Bstring_tag_patt (void *x) {
  if (UNBOXED(x)) return BOX(0);
  
  return BOX(TAG(TO_DATA(x)->tag) == STRING_TAG);
}

extern int Bsexp_tag_patt (void *x) {
  if (UNBOXED(x)) return BOX(0);
  
  return BOX(TAG(TO_DATA(x)->tag) == SEXP_TAG);
}

extern void* Bsta (void *v, int i, void *x) {
  ASSERT_BOXED(".sta:3", x);
  ASSERT_UNBOXED(".sta:2", i);
  
  if (TAG(TO_DATA(x)->tag) == STRING_TAG)((char*) x)[UNBOX(i)] = (char) UNBOX(v);
  else ((int*) x)[UNBOX(i)] = (int) v;

  return v;
}

static void fix_unboxed (char *s, va_list va) {
  size_t *p = (size_t*)va;
  int i = 0;
  
  while (*s) {
    if (*s == '%') {
      size_t n = p [i];
      if (UNBOXED (n)) {
	p[i] = UNBOX(n);
      }
      i++;
    }
    s++;
  }
}

extern void Lfailure (char *s, ...) {
  va_list args;
  
  va_start    (args, s);
  fix_unboxed (s, args);
  vfailure    (s, args);
}

extern void Bmatch_failure (void *v, char *fname, int line, int col) {
  createStringBuf ();
  printValue (v);
  failure ("match failure at %s:%d:%d, value '%s'\n",
	   fname, UNBOX(line), UNBOX(col), stringBuf.contents);
}

extern void* /*Lstrcat*/ Li__Infix_4343 (void *a, void *b) {
  data *da = (data*) BOX (NULL);
  data *db = (data*) BOX (NULL);
  data *d  = (data*) BOX (NULL);

  ASSERT_STRING("++:1", a);
  ASSERT_STRING("++:2", b);
  
  da = TO_DATA(a);
  db = TO_DATA(b);

  __pre_gc () ;

  push_extra_root (&a);
  push_extra_root (&b);
  d  = (data *) alloc (sizeof(int) + LEN(da->tag) + LEN(db->tag) + 1);
  pop_extra_root (&b);
  pop_extra_root (&a);

  da = TO_DATA(a);
  db = TO_DATA(b);
  
  d->tag = STRING_TAG | ((LEN(da->tag) + LEN(db->tag)) << TAG_BITS);
 
  strncpy (d->contents               , da->contents, LEN(da->tag));
  strncpy (d->contents + LEN(da->tag), db->contents, LEN(db->tag));
  d->contents[LEN(da->tag) + LEN(db->tag)] = '\0';
  
  d->contents[LEN(da->tag) + LEN(db->tag)] = 0;

  __post_gc();
  
  return d->contents;
}

extern void* Lsprintf (char * fmt, ...) {
  va_list args;
  void *s;

  ASSERT_STRING("sprintf:1", fmt);
  
  va_start (args, fmt);
  fix_unboxed (fmt, args);
  
  createStringBuf ();

  vprintStringBuf (fmt, args);

  __pre_gc ();

  push_extra_root ((void**)&fmt);
  s = Bstring (stringBuf.contents);
  pop_extra_root ((void**)&fmt);

  __post_gc ();
  
  deleteStringBuf ();

  return s;
}

extern void* LgetEnv (char *var) {
  char *e = getenv (var);
  void *s;
  
  if (e == NULL)
    return BOX(0);

  __pre_gc ();

  s = Bstring (e);

  __post_gc ();

  return s;
}

extern int Lsystem (char *cmd) {
  return BOX (system (cmd));
}

extern void Lfprintf (FILE *f, char *s, ...) {
  va_list args = (va_list) BOX (NULL);

  ASSERT_BOXED("fprintf:1", f);
  ASSERT_STRING("fprintf:2", s);  
  
  va_start    (args, s);
  fix_unboxed (s, args);
  
  if (vfprintf (f, s, args) < 0) {
    failure ("fprintf (...): %s\n", strerror (errno));
  }
}

extern void Lprintf (char *s, ...) {
  va_list args = (va_list) BOX (NULL);

  ASSERT_STRING("printf:1", s);

  va_start    (args, s);
  fix_unboxed (s, args);
  
  if (vprintf (s, args) < 0) {
    failure ("fprintf (...): %s\n", strerror (errno));
  }

  fflush (stdout);
}

extern FILE* Lfopen (char *f, char *m) {
  FILE* h;

  ASSERT_STRING("fopen:1", f);
  ASSERT_STRING("fopen:2", m);

  h = fopen (f, m);
  
  if (h)
    return h;

  failure ("fopen (\"%s\", \"%s\"): %s, %s, %s\n", f, m, strerror (errno));
}

extern void Lfclose (FILE *f) {
  ASSERT_BOXED("fclose", f);

  fclose (f);
}

extern void* LreadLine () {
  char *buf;

  if (scanf ("%m[^\n]", &buf) == 1) {
    void * s = Bstring (buf);

    getchar ();
    
    free (buf);
    return s;
  }
  
  if (errno != 0)
    failure ("readLine (): %s\n", strerror (errno));

  return (void*) BOX (0);
}

extern void* Lfread (char *fname) {
  FILE *f;

  ASSERT_STRING("fread", fname);

  f = fopen (fname, "r");
  
  if (f) {
    if (fseek (f, 0l, SEEK_END) >= 0) {
      long size = ftell (f);
      void *s   = LmakeString (BOX(size));
      
      rewind (f);

      if (fread (s, 1, size, f) == size) {
	fclose (f);
	return s;
      }
    }
  }

  failure ("fread (\"%s\"): %s\n", fname, strerror (errno));
}

extern void Lfwrite (char *fname, char *contents) {
  FILE *f;

  ASSERT_STRING("fwrite:1", fname);
  ASSERT_STRING("fwrite:2", contents);
  
  f = fopen (fname, "w");

  if (f) {
    if (fprintf (f, "%s", contents) < 0);
    else {
      fclose (f);
      return;
    }
  }

  failure ("fwrite (\"%s\"): %s\n", fname, strerror (errno));
}

extern void* Lfst (void *v) {
  return Belem (v, BOX(0));  
}

extern void* Lsnd (void *v) {
  return Belem (v, BOX(1));  
}

extern void* Lhd (void *v) {
  return Belem (v, BOX(0));  
}

extern void* Ltl (void *v) {
  return Belem (v, BOX(1));  
}

/* Lread is an implementation of the "read" construct */
extern int Lread () {
  int result = BOX(0);

  printf ("> "); 
  fflush (stdout);
  scanf  ("%d", &result);

  return BOX(result);
}

/* Lwrite is an implementation of the "write" construct */
extern int Lwrite (int n) {
  printf ("%d\n", UNBOX(n));
  fflush (stdout);

  return 0;
}

extern void set_args (int argc, char *argv[]) {
  data *a;
  int n = argc, *p = NULL;
  int i;
  
  __pre_gc ();

#ifdef DEBUG_PRINT
  indent++; print_indent ();
  printf ("set_args: call: n=%i &p=%p p=%p: ", n, &p, p); fflush(stdout);
  for (i = 0; i < n; i++)
    printf("%s ", argv[i]);
  printf("EE\n");
#endif

  p = LmakeArray (BOX(n));
  push_extra_root ((void**)&p);
  
  for (i=0; i<n; i++) {
#ifdef DEBUG_PRINT
    print_indent ();
    printf ("set_args: iteration %i %p %p ->\n", i, &p, p); fflush(stdout);
#endif
    ((int*)p) [i] = (int) Bstring (argv[i]);
#ifdef DEBUG_PRINT
    print_indent ();
    printf ("set_args: iteration %i <- %p %p\n", i, &p, p); fflush(stdout);
#endif
  }

  pop_extra_root ((void**)&p);
  __post_gc ();

  global_sysargs = p;
  push_extra_root ((void**)&global_sysargs);
#ifdef DEBUG_PRINT
  print_indent ();
  printf ("set_args: end\n", n, &p, p); fflush(stdout);
  indent--;
#endif
}

/* GC starts here */

extern const size_t __start_custom_data, __stop_custom_data;

# ifdef __ENABLE_GC__

extern void L__gc_init ();

# else

# define L__gc_init __gc_init_subst
void __gc_init_subst () {}

# endif

extern void __gc_root_scan_stack ();

/* ======================================== */
/*           Mark-and-copy                  */
/* ======================================== */

static size_t SPACE_SIZE = 16;
/* static size_t SPACE_SIZE = 1024 * 1024; */
/* static size_t SPACE_SIZE = 128 * 1024; */
/* static size_t SPACE_SIZE = 128; */
/* static size_t SPACE_SIZE = 1024; */
static size_t MAX_SPACE_SIZE = 1024 * 1024 * 1024;

static int free_pool (pool * p) {
  size_t *a = p->begin, b = p->size;
  p->begin   = NULL;
  p->size    = 0;
  p->end     = NULL;
  p->current = NULL;
  return munmap((void *)a, b);
}

# define IS_VALID_HEAP_POINTER(p)\
  (!UNBOXED(p) &&		 \
   (size_t)space.begin   <= (size_t)p &&	\
   (size_t)space.current >=  (size_t)p)

int is_valid_heap_pointer (void *p)  {
  return IS_VALID_HEAP_POINTER(p);
}

static int extend_space (void) {
  size_t new_space_size = (SPACE_SIZE << 1) * sizeof(size_t);
  assert (MAX_SPACE_SIZE >= new_space_size);
  if (mprotect (space.begin, new_space_size,
  		PROT_READ | PROT_WRITE) == -1) {
    perror ("EROOR: extend_space: mprotect failed\n");
    exit   (1);
  }
  space.end    += SPACE_SIZE;
  SPACE_SIZE   =  SPACE_SIZE << 1;
  space.size   =  SPACE_SIZE;
  return 0;
}

/* extern size_t * gc_copy_element (size_t *obj) { */
/*   data   *d    = TO_DATA(obj); */
/*   sexp   *s    = NULL; */
/*   size_t *copy = NULL; */
/*   int i = 0, j=0; */
/*   copy = current; */

/*   if (IS_FORWARD_PTR(d->tag)) { */
/*     return (size_t *) d->tag; */
/*   } */
  
/*   switch (TAG(d->tag)) { */
/*   case STRING_TAG: */
/* #ifdef DEBUG_PRINT */
/*     indent++; */
/*     print_indent (); */
/*     printf ("gc_copy:string_tag; len = %d\n", LEN(d->tag) + 1); fflush (stdout); */
/* #endif */
/*     j = LEN(d->tag) + 1; */
/*     current += (LEN(d->tag) + sizeof(int)) / sizeof(size_t) + 1; */
/*     *copy = d->tag; */
/*     copy++; */
/*     d->tag = (int) copy; */
/*     strcpy ((char*)&copy[0], (char*) obj); */
/*     break; */
/*   case CLOSURE_TAG: */
/*   case ARRAY_TAG: */
/* #ifdef DEBUG_PRINT */
/*       print_indent (); */
/*       printf ("gc_copy:closure or array; len =  %zu\n", LEN(d->tag)); fflush (stdout); */
/* #endif */
/*       i = LEN(d->tag); */
/*       current += i+1; */
/*       *copy = d->tag; */
/*       copy++; */
/*       d->tag = (int) copy; */
/*       memcpy (copy, obj,  sizeof(int) * i); */
/*       break; */
/*   case SEXP_TAG: */
/*     s = TO_SEXP(obj); */
/*     i = LEN(s->contents.tag); */
/*     current += i + 2; */
/*     *copy = s->tag; */
/*     copy++; */
/*     *copy = d->tag; */
/*     copy++; */
/*     d->tag = (int) copy; */
/*     memcpy (copy, obj,  sizeof(int) * i); */
/* #ifdef DEBUG_PRINT */
/*     indent++; */
/*     print_indent (); */
/*     printf ("gc_copy:sexp_tag; len = %d\n", i); fflush (stdout); */
/* #endif */
/*     break; */
/*   default: */
/* #ifdef DEBUG_PRINT */
/*     print_indent (); */
/*     printf ("ERROR: gc_copy: weird tag: %p", TAG(d->tag)); fflush (stdout); */
/*     indent--; */
/* #endif */
/*     perror ("ERROR: gc_copy: weird tag"); */
/*     exit (1); */
/*   } */
/* #ifdef DEBUG_PRINT */
/*   print_indent (); */
/*   printf ("gc_copy: %p -> %p; new-current = %p\n", */
/* 	  obj, copy, current); */
/*   fflush (stdout); */
/*   indent--; */
/* #endif */
/*   to_space.current = current; */
/*   return copy; */
/* } */

typedef struct {
  size_t size;
  size_t top;
  size_t stack[0];
} ptr_stack;
# define MARK_STACK_SIZE 1024
ptr_stack * mark_stack;
static void init_mark_stack ( void ) {
  mark_stack = mmap (NULL, MARK_STACK_SIZE * sizeof(size_t), PROT_READ | PROT_WRITE,
		     MAP_PRIVATE | MAP_ANONYMOUS | MAP_32BIT, -1, 0);
  if (mark_stack == MAP_FAILED) {
    perror ("EROOR: init_mark_stack: mmap failed\n");
    exit   (1);
  }
  mark_stack->size = MARK_STACK_SIZE;
  mark_stack->top  = 0;
}
static void mark_stack_overflow_recovery ( void ) {
  perror ("EROOR: mark stack overflow \n");
  exit   (1);
}
static void mark_stack_push (size_t* p) {
  if (mark_stack->top == mark_stack->size) {
    mark_stack_overflow_recovery ();
  }
  mark_stack->stack[mark_stack->top++] = (size_t)p;
}
static size_t* mark_stack_pop ( void ) {
  if (mark_stack->top == 0) {
    return NULL;
  }
  return (size_t*)mark_stack->stack[--mark_stack->top];
}

static inline void mark_block_and_refs (size_t* p) {
  data   *d  = NULL;
  size_t len = 0;
mark_block_and_refs_begin:
  MARK_BLOCK(p);
  d = TO_DATA(p);  
  switch (TAG(d->tag)) {
  case SEXP_TAG:
  case CLOSURE_TAG:
  case ARRAY_TAG:
    len = LEN(d->tag);
    break;
  case STRING_TAG:
    len = 0;
    break;
  default:
    perror ("mark_block_and_refs: WEIRG TAG: non-rec copying");
    exit   (1);
  }
  for (int i = 0; i < len; i++, p++) {
    if (!UNBOXED(*p) && *p >= space.begin && *p < space.current) {
      if (!BLOCK_IS_MARKED_DATA(*p))
	mark_stack_push (*p);
    }
  }
  if (p = mark_stack_pop()) goto mark_block_and_refs_begin;
}

static int MARK_PHASE = 0;
static size_t* update_pointer (size_t * p);
extern void gc_mark_root (size_t ** root) {
#ifdef DEBUG_PRINT
    indent++;
#endif
  if (IS_VALID_HEAP_POINTER(*root)) {
#ifdef DEBUG_PRINT
    print_indent ();
    printf ("gc_mark_root: root %p top=%p bot=%p  *root %p \n",
	    root, __gc_stack_top, __gc_stack_bottom, *root);
    fflush (stdout);
#endif
    if (MARK_PHASE) {
      mark_block_and_refs (*root);
    } else {
      *root = update_pointer (*root);
    }
  }
#ifdef DEBUG_PRINT
  else {
    print_indent ();
    printf ("gc_mark_root: INVALID HEAP POINTER root %p  *root %p\n", root, *root);
    fflush (stdout);
  }
  indent--;
#endif
}
extern inline void gc_test_and_copy_root (size_t ** root) {
  gc_mark_root (root);
}

extern void gc_root_scan_data (void) {
  size_t * p = (size_t*)&__start_custom_data;
  while (p < (size_t*)&__stop_custom_data) {
    gc_test_and_copy_root ((size_t**)p);
    p++;
  }
}

static inline void init_extra_roots (void) {
  extra_roots.current_free = 0;
}

extern void init_pool (void) {
  size_t space_size = SPACE_SIZE * sizeof(size_t);
  assert (MAX_SPACE_SIZE > SPACE_SIZE * 2);
  space.begin = mmap (NULL, MAX_SPACE_SIZE, PROT_NONE,
		      MAP_PRIVATE | MAP_ANONYMOUS | MAP_32BIT, -1, 0);
  if (space.begin == MAP_FAILED) {
    perror ("EROOR: init_pool: mmap failed\n");
    exit   (1);
  }
  if (mprotect (space.begin, space_size, PROT_READ | PROT_WRITE) == -1) {
    perror ("EROOR: init_pool: mprotect failed\n");
    exit   (1);
  }

  space.current = space.begin;
  space.end     = space.begin + SPACE_SIZE;
  space.size    = SPACE_SIZE;
  init_extra_roots ();

  init_mark_stack ();
}

/* #ifdef DEBUG_PRINT */
static void printFromSpace (void) {
  size_t * cur = space.begin, *tmp = NULL;
  data   * d   = NULL;
  sexp   * s   = NULL;
  size_t   len = 0;
  size_t   elem_number = 0;
  
  printf ("\nHEAP SNAPSHOT\n===================\n");
  printf ("f_begin = %p, f_cur = %p, f_end = %p, cur = %p \n",
	  space.begin, space.current, space.end, cur);
  while (cur < space.current) { //  && cur < current) {
    printf ("data at %p ", cur);
    d  = (data *) cur;

    switch (TAG(d->tag)) {
    case SEXP_TAG:
      s = (sexp *) d;
      d = (data *) &(s->contents);
      printf (", mark bit %i", d->tag & 0x4);
      char * tag = de_hash (GET_SEXP_TAG(s->tag));
      printf ("(=>%p): SEXP\n\ttag(%s) ", s->contents.contents, tag);
      len = LEN(d->tag);
      tmp = (s->contents.contents);
      for (int i = 0; i < len; i++) {
	int elem = ((int*)tmp)[i];
	if (UNBOXED(elem)) printf ("%d ", UNBOX(elem));
	else printf ("%p ", elem);
      }
      len += 2;
      printf ("\n");
      fflush (stdout);
      break;
	
    case STRING_TAG:
      printf (", mark bit %i", d->tag & 0x4);
      printf ("(=>%p): STRING\n\t%s; len = %i %zu\n",
	      d->contents, d->contents,
	      LEN(d->tag), LEN(d->tag) + 1 + sizeof(int));
      fflush (stdout);
      len = (LEN(d->tag) + sizeof(int)) / sizeof(size_t) + 1;
      break;

    case CLOSURE_TAG:
      printf (", mark bit %i", d->tag & 0x4);
      printf ("(=>%p): CLOSURE\n\t", d->contents);
      len = LEN(d->tag);
      for (int i = 0; i < len; i++) {
	int elem = ((int*)d->contents)[i];
	if (UNBOXED(elem)) printf ("%d ", elem);
	else printf ("%p ", elem);
      }
      len += 1;
      printf ("\n");
      fflush (stdout);
      break;

    case ARRAY_TAG:
      printf (", mark bit %i", d->tag & 0x4);
      printf ("(=>%p): ARRAY\n\t", d->contents);
      len = LEN(d->tag);
      for (int i = 0; i < len; i++) {
	int elem = ((int*)d->contents)[i];
	if (UNBOXED(elem)) printf ("%d ", elem);
	else printf ("%p ", elem);
      }
      len += 1;
      printf ("\n");
      fflush (stdout);
      break;

    default:
      printf ("\nprintFromSpace: ERROR: bad tag %d", TAG(d->tag));
      perror ("\nprintFromSpace: ERROR: bad tag");
      fflush (stdout);
      exit   (1);
    }
    cur += len;
    printf ("len = %zu, new cur = %p\n", len, cur);
    elem_number++;
  }
  printf ("\nprintFromSpace: end: the whole space is printed:\
            %zu elements\n===================\n\n", elem_number);
  fflush (stdout);
}
/* #endif */

static void trace_roots_and_mark_live_blocks ( void ) {
#ifdef DEBUG_PRINT
  printf ("trace data\n"); fflush(stdout);
#endif
  gc_root_scan_data ();
#ifdef DEBUG_PRINT
  printf ("trace stack\n"); fflush(stdout);
#endif
  __gc_root_scan_stack ();
#ifdef DEBUG_PRINT
  printf ("trace extra roots\n"); fflush(stdout);
#endif
  for (int i = 0; i < extra_roots.current_free; i++) {
    gc_test_and_copy_root ((size_t**)extra_roots.roots[i]);
  }
}

/* typedef struct { */
/*   size_t * next; */
/*   size_t   size[0]; */
/* } intervals_list; */
static inline size_t* skip_block (size_t * p) {
  data * d   = NULL;
  if (IS_SEXP(*p)) {
    p += LEN(((data*)(p+1))->tag) + 2;
  } else {
    d = (data*)p;
    switch (TAG(d->tag)) {
    case STRING_TAG:
      p += (LEN(d->tag) + sizeof(int)) / sizeof(size_t) + 1;
      break;
    case ARRAY_TAG:
    case CLOSURE_TAG:
      p += LEN(d->tag) + 1;
      break;
    case SEXP_TAG:
      perror ("skip_block: weird tag: sexp");
      exit   (1);
      break;
    default:
      perror ("skip_block: weird tag");
      exit   (1);
      break;
    }
  }
  return p;
}
static size_t* next_dead (size_t * p) {
  for (; p < space.current; p = skip_block (p)) {
    if (!BLOCK_IS_MARKED(p)) return p;
  }
  return space.current;
}
static size_t* next_live (size_t * p) {
  for (; p < space.current; p = skip_block (p)) {
    if (BLOCK_IS_MARKED(p)) return p;
  }
  return space.current;
}

static inline void set_interval (size_t * dead, const size_t * live, const size_t * next_dead) {
  if ((unsigned)live - (unsigned)dead == 1) {
    dead[0] = (unsigned)next_dead | 1;
  } else {
    dead[0] = (size_t)next_dead;
    dead[1] = (size_t)live;
  }
}
static inline void get_interval (const size_t * dead, size_t** live, size_t** next_dead){
  *next_dead = dead[0];
  if ((size_t)(*next_dead) & 1) {
    *next_dead = (size_t)(*next_dead) & ~1;
    *live = dead + 1;
  } else {
    *live = (size_t *)dead[1];
  }
}

size_t * first_dead = NULL;
static void create_dead_intervals_list ( void ) {
  size_t * dead = next_dead (space.begin), * live = NULL, * next_d = NULL;
  first_dead = dead;
  for (; dead < space.current;) {
    live = next_live (dead);
    next_d = next_dead (live);
    set_interval (dead, live, next_d);
    dead = next_d;
  }
}

static inline void compact ( void ) {
  size_t * dead = first_dead, * live = NULL, * next_dead = NULL, * current = dead,
    dead_interval_size = 0;
  if (first_dead == space.current) return;
  do {
    get_interval (dead, &live, &next_dead);
    // dead_interval_size = live - dead;
    // memcpy (current, live, dead_interval_size * sizeof (size_t));
    // current += dead_interval_size;
    dead_interval_size = next_dead - live;
    memcpy (current, live, dead_interval_size * sizeof (size_t));
    current += dead_interval_size;
    dead = next_dead;
  } while (dead < space.current);
  space.current = current;
}

static size_t* update_pointer (size_t * p) {
  size_t * live = NULL, * dead_it = NULL, * next_dead = NULL;
  size_t shift = 0;
  for (dead_it = first_dead; dead_it < p; dead_it = next_dead) {
    assert (dead_it < space.current);
    get_interval (dead_it, &live, &next_dead);
    shift += live - dead_it;
  }
#ifdef DEBUG_PRINT
  printf ("update_pointer: %p -> %p\n", p , p - shift);
  fflush (stdout);
#endif
  return p - shift;
}

// returns a pointer to the next object header
static size_t * update_object_pointers (size_t * p) {
  data * d = NULL;
  size_t len = 0;
  if (IS_SEXP(*p)) {
    len  = LEN(((sexp*)p)->contents.tag);
    p += 2;
  } else {
    d = (data*) p;
    switch (TAG(d->tag)) {
    case STRING_TAG:
      return p + (LEN(d->tag) + sizeof(int)) / sizeof(size_t) + 1;;
    case ARRAY_TAG:
    case CLOSURE_TAG:
      len = LEN(d->tag);
      p++;
      break;
    deafult:
      perror ("update_object_pointers: weird tag\n");
      exit (1);
      break;
    }
  }
  for (size_t i = 0; i < len; i++, p++) {
    if (!UNBOXED(*p) && *p >= space.begin && *p < space.current) {
      *p = update_pointer (*p);
    }
  }
  return p;
}

static void update_heap_pointers ( void ) {
  size_t * it = NULL;
  //  size_t * dead = first_dead, * live = next_live (space.begin), * next_dead = NULL;
  size_t * dead = first_dead, * live = NULL, * next_dead = NULL;

  if (first_dead == space.begin) {
    get_interval (dead, &live, &next_dead);
  } else {
    live = space.begin;
  }
  
update_pointers_loop:
  for (; live < dead;) {
    live = update_object_pointers (live);
  }
  if (dead < space.current) {
    get_interval (dead, &live, &next_dead);
    dead = next_dead;
    goto update_pointers_loop;
  }
}

static inline void update_stack_pointers ( void ) {
  trace_roots_and_mark_live_blocks ();
}

size_t * old_current = NULL;
static void* gc (size_t size) {
  size_t * result = NULL;

  old_current = space.current;
  
#ifdef DEBUG_PRINT
  printf ("gc===\n");
  printFromSpace ();
#endif
  // 1. trace roots and mark blocks
  MARK_PHASE = 1;
#ifdef DEBUG_PRINT
  printf ("mark\n"); fflush(stdout);
#endif
  trace_roots_and_mark_live_blocks ();
#ifdef DEBUG_PRINT
  printFromSpace ();
#endif
  MARK_PHASE = 0;
  /* printFromSpace (); */
  // 2. create intervals list
#ifdef DEBUG_PRINT
  printf ("intervals\n"); fflush(stdout);
#endif
  create_dead_intervals_list ();
  // TODO: 3. fix pointers _AND_ FIX ROOTS!
#ifdef DEBUG_PRINT
  printf ("update\n"); fflush(stdout);
#endif
  update_heap_pointers ();
  update_stack_pointers ();
  //    It will be an extra pass through the heap since we have no free words in object headers
  // 4. compact
#ifdef DEBUG_PRINT
  printf ("compact: first_dead = %p\n", first_dead);
  fflush(stdout);
#endif
  compact ();
  // 5. reverce mark bit meaning
  reverse_mark_bit ();
#ifdef DEBUG_PRINT
  printFromSpace ();
#endif
  // 6. Check that it is enough space for new object; else try to extand space
  while (space.end <= space.current + size) {
    extend_space ();
  }
  /* printFromSpace (); */

  for (size_t* i = space.current; i < old_current; i++) {
    *i = NULL;
  }

  result = space.current;
  space.current += size;

  return result;
}

#ifdef __ENABLE_GC__
/* alloc: allocates `size` bytes in heap */
extern void * alloc (size_t size) {
  void * p = (void*)BOX(NULL);
  size = (size - 1) / sizeof(size_t) + 1; /* convert bytes to words */
#ifdef DEBUG_PRINT
  indent++; print_indent ();
  printf ("alloc: current: %p %zu words!", space.current, size);
  fflush (stdout);
#endif
  if (space.current + size < space.end) {
    p = (void*) space.current;
    space.current += size;
#ifdef DEBUG_PRINT
    print_indent ();
    printf (";new current: %p \n", space.current); fflush (stdout);
    indent--;
    /* Nullify new memory */
    size_t *p1 = p;
    for (int i = 0; i < size; p1++, i++) { *p1 = 0; }
#endif    
    return p;
  }
  
#ifdef DEBUG_PRINT
/*   print_indent (); */
/*   printf ("alloc: call gc: %zu\n", size); fflush (stdout); */
/*   /\* current = space.current; printFromSpace(); fflush (stdout); *\/ */
/*   p = gc (size); */
/*   print_indent (); */
/*   printf("alloc: gc END %p %p %p %p\n\n", space.begin, */
/* 	 space.end, space.current, p); fflush (stdout); */
/*   printFromSpace(); fflush (stdout); */
  indent--;
/*   return p; */
/* #else */
#endif
  return gc (size);
}
# endif
