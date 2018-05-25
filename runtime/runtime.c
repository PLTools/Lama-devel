/* Runtime library */

# include <stdio.h>
# include <stdio.h>
# include <malloc.h>
# include <string.h>
# include <stdarg.h>
# include <alloca.h>

# define STRING_TAG 0x00000000
# define ARRAY_TAG  0x01000000
# define SEXP_TAG   0x02000000

# define LEN(x) (x & 0x00FFFFFF)
# define TAG(x) (x & 0xFF000000)

# define TO_DATA(x) ((data*)((char*)(x)-sizeof(int)))
# define TO_SEXP(x) ((sexp*)((char*)(x)-2*sizeof(int)))

/* A structure to represent a tagged array/string */
typedef struct {
  int tag;          
  char contents[0];
} data; 

/* A structure to represent S-expression */
typedef struct {
  int tag; 
  data contents; 
} sexp; 

/* Length builtin */
extern int Blength (void *p) {
  data *a = TO_DATA(p);
  return LEN(a->tag);
}

/* Element extraction builtin */
extern void* Belem (void *p, int i) {
  data *a = TO_DATA(p);

  if (TAG(a->tag) == STRING_TAG) return (void*)(int)(a->contents[i]);
  
  return (void*) ((int*) a->contents)[i];
}

/* String constructor */
extern void* Bstring (void *p) {
  int n = strlen (p);
  data *r = (data*) malloc (n + 1 + sizeof (int));

  r->tag = n;
  strncpy (r->contents, p, n + 1);
  
  return r->contents;
}

/* Array constructor; takes the number of elements and the elements themselves */
extern void* Barray (int n, ...) {
  va_list args;
  int i;
  data *r = (data*) malloc (sizeof(int) * (n+1));

  r->tag = ARRAY_TAG | n;
  
  va_start(args, n);
  
  for (i=0; i<n; i++) {
    int ai = va_arg(args, int);
    ((int*)r->contents)[i] = ai; 
  }
  
  va_end(args);

  return r->contents;
}

/* S-expression constructor; takes the number of parameters; the parameters are subexpressions and the tag */
extern void* Bsexp (int n, ...) {
  va_list args;
  int i;
  sexp *r = (sexp*) malloc (sizeof(int) * (n+2));
  data *d = &(r->contents);

  d->tag = SEXP_TAG | (n-1);
  
  va_start(args, n);
  
  for (i=0; i<n-1; i++) {
    int ai = va_arg(args, int);
    ((int*)d->contents)[i] = ai; 
  }

  r->tag = va_arg(args, int);
  va_end(args);
  
  return d->contents;
}

/* Tag checking */
extern int Btag (void *d, int t) {
  data *r = TO_DATA(d);
  return TAG(r->tag) == SEXP_TAG && TO_SEXP(d)->tag == t;
}

/* Array store builtin; takes the number of indices, the value to store, the array, and the indices themselves */
extern void Bsta (int n, int v, void *s, ...) {
  va_list args;
  int i, k;
  data *a;
  
  va_start(args, s);

  for (i=0; i<n-1; i++) {
    k = va_arg(args, int);
    s = ((int**) s) [k];
  }

  k = va_arg(args, int);
  a = TO_DATA(s);
  
  if (TAG(a->tag) == STRING_TAG)((char*) s)[k] = (char) v;
  else ((int*) s)[k] = v;
}
   
/* Read builtin */
extern int Lread () {
  int result;

  printf ("> "); 
  fflush (stdout);
  scanf  ("%d", &result);

  return result;
}

/* Write builtin */
extern int Lwrite (int n) {
  printf ("%d\n", n);
  fflush (stdout);

  return 0;
}
