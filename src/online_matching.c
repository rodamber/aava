#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int max(int const a, int const b) {
  return a > b ? a : b;
}

void print_array(int *arr, int n) {
  int i;
  printf("<");
  for (i = 0; i < n - 1; i++) {
    printf("%d ", arr[i]);
  }
  printf("%d>\n", arr[n - 1]);
}

/* ************************************************************************** */

#define DEFINE_VECTOR(TYPE)                     \
  typedef struct {                              \
    int volume; /* used + free */               \
    int size; /* Used slots */                  \
    TYPE *array;                                \
  } vector_##TYPE;                              \

DEFINE_VECTOR(char)
DEFINE_VECTOR(int)

typedef vector_char string;

#define DEFINE_NEW_VECTOR(TYPE)                               \
  vector_##TYPE *new_vector_##TYPE() {                        \
    int const volume = 1; /* 128 */                           \
    vector_##TYPE* const da = malloc(sizeof(vector_##TYPE));  \
                                                              \
    da->array  = malloc(sizeof(TYPE) * volume);               \
    da->size   = 0;                                           \
    da->volume = volume;                                      \
                                                              \
    return da;                                                \
  }                                                           \

DEFINE_NEW_VECTOR(char)
DEFINE_NEW_VECTOR(int)

#define DEFINE_FREE_VECTOR(TYPE)                      \
  void free_vector_##TYPE(vector_##TYPE * const da) { \
    free(da->array);                                  \
    free(da);                                         \
  }                                                   \

DEFINE_FREE_VECTOR(char)
DEFINE_FREE_VECTOR(int)

#define DEFINE_UPVOLUME(TYPE)                                             \
  void upvolume_##TYPE(vector_##TYPE * const da) {                        \
    if (da->size >= da->volume) { /* We don't have enough free space. */  \
      da->volume = 2 * da->volume;                                        \
      da->array  = realloc((TYPE*) da->array, da->volume * sizeof(TYPE)); \
    }                                                                     \
  }                                                                       \

DEFINE_UPVOLUME(char)
DEFINE_UPVOLUME(int)

#define DEFINE_DOWNVOLUME(TYPE)                                           \
  void downvolume_##TYPE(vector_##TYPE * const da) {                      \
    if (/* da->size > 127 && */ da->volume >= 4 * da->size) {             \
      da->volume = da->volume / 2;                                        \
      da->array  = realloc((TYPE*) da->array, da->volume * sizeof(TYPE)); \
    }                                                                     \
  }                                                                       \

DEFINE_DOWNVOLUME(char)
DEFINE_DOWNVOLUME(int)

#define DEFINE_INSERT(TYPE)                                       \
  void insert_##TYPE(vector_##TYPE * const da, const TYPE elem) { \
    upvolume_##TYPE(da);                                          \
    da->array[da->size++] = elem;                                 \
  }                                                               \

DEFINE_INSERT(char)
DEFINE_INSERT(int)

#define DEFINE_DELETE(TYPE)                       \
  void delete_##TYPE(vector_##TYPE * const da) {  \
    downvolume_##TYPE(da);                        \
    da->size--;                                   \
  }                                               \

DEFINE_DELETE(char)
DEFINE_DELETE(int)

#define DEFINE_WRITE(TYPE)                                                     \
  void write_##TYPE(vector_##TYPE * const da, const int at, const TYPE elem) { \
    if (at == da->size) {                                                      \
      insert_##TYPE(da, elem);                                                 \
    } else {                                                                   \
      da->array[at] = elem;                                                    \
    }                                                                          \
  }                                                                            \

DEFINE_WRITE(char)
DEFINE_WRITE(int)

#ifdef DEBUG

#define DEFINE_AT(TYPE)                                               \
  TYPE *at_##TYPE(vector_##TYPE const * const v, const int i) {       \
    if (i >= v->size)                                                 \
      printf("Out of bounds access: size = %d, i = %d", v->size, i);  \
    return &(v->array[i]);                                            \
  }                                                                   \

#else

#define DEFINE_AT(TYPE)                                         \
  TYPE *at_##TYPE(vector_##TYPE const * const v, const int i) { \
    return &(v->array[i]);                                      \
  }                                                             \

#endif

DEFINE_AT(char)
DEFINE_AT(int)

#define DEFINE_AT_BACK(TYPE)                                         \
  TYPE *at_back_##TYPE(vector_##TYPE const * const v, const int i) { \
    return &(v->array[v->size - i - 1]);                             \
  }                                                                  \

DEFINE_AT_BACK(char)
DEFINE_AT_BACK(int)

#define DEFINE_CONST_ITERATOR(TYPE)                                   \
  TYPE const *const_iterator_##TYPE(vector_##TYPE const * const da) { \
    return da->array;                                                 \
  }                                                                   \

DEFINE_CONST_ITERATOR(char)
DEFINE_CONST_ITERATOR(int)

#define DEFINE_FROM_ARRAY(TYPE)                                           \
  vector_##TYPE *from_array_##TYPE(TYPE const * const arr, const int n) { \
    vector_##TYPE *vec = new_vector_##TYPE();                             \
                                                                          \
    int i = 0;                                                            \
    for (; i < n; i++) {                                                  \
      insert_##TYPE(vec, arr[i]);                                         \
    }                                                                     \
    return vec;                                                           \
  }                                                                       \

DEFINE_FROM_ARRAY(char)
DEFINE_FROM_ARRAY(int)

#define DEFINE_PRINT_VECTOR(TYPE, X)                          \
  void print_vector_##TYPE(vector_##TYPE const * const da) {  \
    int i;                                                    \
    for (i = 0; i < da->size; i++) {                          \
      printf(X, da->array[i]);                                \
    }                                                         \
    printf("\n");                                             \
  }                                                           \

DEFINE_PRINT_VECTOR(char, "%c")
DEFINE_PRINT_VECTOR(int, "%d ")

string *new_string(char const *const s) {
  string *const str = new_vector_char();

  int i = 0;
  while (s[i]) {
    insert_char(str, s[i]);
    i++;
  }

  return str;
}

/* ************************************************************************** */

typedef struct {
  vector_int *positions;
  int comparisons;
} result;

result *new_result() {
  result *res      = malloc(sizeof(result));
  res->positions   = new_vector_int();
  res->comparisons = 0;
  return res;
}

void free_result(result *result) {
  free_vector_int(result->positions);
  free(result);
}

void add(result const * const result, const int new_pos) {
  insert_int(result->positions, new_pos);
}

void inc(result * const result) {
  result->comparisons++;
}

int const left_right = 1;
int const right_left = -1;

/* Returns index of mismatch, or -1 if it matches and direction is right to
   left, or n if it matches and direction is left to right */
int ncmp(char const * const s1, char const * const s2, int const n,
         int const direction, result * const res) {
  int i = 0;
  for (; i < n; i++) {
    inc(res);
    if (s1[direction * i] != s2[direction * i]) {
      return direction == left_right ? i : n - i - 1;
    }
  }
  return direction == left_right ? n : -1;
}

/* ************************************************************************** */

void read(string * const vec) {
  char c;
  int i = 0;

  while ((c = getchar()) != '\n') {
    write_char(vec, i++, c);
  }
  vec->size = i;
  downvolume_char(vec);
}

/* ************************************************************************** */
/* Naive                                                                      */
/* ************************************************************************** */

result *N(string const * const T, string const * const P) {
  result *const res = new_result();
  int const n = P->size, m = T->size;

  /* Don't forget the arrays are not null terminated. */
  int t;
  for (t = 0; t <= m - n; t++) {
    /* FIXME: We're counting the comparisons here */
    if (ncmp(at_char(T, t), at_char(P, 0), n, left_right, res) == n) {
      add(res, t);
    }
  }
  return res;
}

/* ************************************************************************** */
/* Knuth-Morris-Pratt                                                         */
/* ************************************************************************** */

result *K(string const * const T, string const * const P) {
  (void) T; (void) P;
  result *result = new_result();
  return result;
}

/* ************************************************************************** */
/* Z algorithm                                                                */
/* ************************************************************************** */

int match_count(char const *const s1, char const *const s2, int const n) {
  int i = 0;
  while (i > -n && s1[i] == s2[i]) {
    i--;
  }
  return -i;
}

/* gdb debug
printf "z = <%d %d %d %d %d %d %d %d %d %d %d>\n", z[0],z[1],z[2],z[3],z[4],z[5],z[6],z[7],z[8],z[9],z[10]
printf "l = %d, r = %d\n", l, r
printf "k = %d, z[k] = %d\n", k, z[k]
*/

/*
  Reverse Z algorithm.
 */
int *Z(string const * const str) {
  int *const z = calloc(str->size, sizeof(int));
  int l, r, k;

  for (l = r = str->size - 1, k = l - 1; k >= 0; k--) {
    if (k < l) { /* case 1 */ 
      z[k] = match_count(at_back_char(str, 0), at_char(str, k), k + 1);

      if (z[k] > 0) {
        l = k - z[k] + 1;
        r = k;
      }
    } else { /* case 2 */
      int const k_prime   = k + (str->size - 1) - r;
      int const beta_size = k - l + 1;

      if (z[k_prime] < beta_size) { /* case 2a */
        z[k] = z[k_prime];
      } else { /* case 2b */
        int const count = match_count(at_back_char(str, beta_size - 1),
                                      at_char(str, l - 1), l);
        z[k] = beta_size + count;
        l -= count;
        r = k;
      }

    }
  }
  return z;
}

/* ************************************************************************** */
/* Boyer-Moore                                                                */
/* ************************************************************************** */

int *R_at(int const *const R, char const x) {
  enum {a, c, t, g};
  switch (x) {
  case 'a': return (int*) &R[a];
  case 'c': return (int*) &R[c];
  case 't': return (int*) &R[t];
  case 'g': return (int*) &R[g];
  default:
    fprintf(stderr, "R_at: bad character '%c'", x);
    return NULL;
  }
}

/* Don't forget to free the array R */
int *bad_char_preprocessing(string const *const S) {
  int *const R = calloc(4, sizeof(int));

  int i;
  for (i = 0; i < S->size; i++) {
    *R_at(R, *at_char(S, i)) = i;
  }
  return R;
}

int bad_char_shift(int const *const R, int const i, char const c) {
  return max(1, i - *R_at(R, c));
}


int **strong_good_suffix_preprocessing(string const *const str) {
  int *const L_prime = calloc(str->size, sizeof(int));
  int *const l_prime = calloc(str->size, sizeof(int));
  int *const z = Z(str);

  /* printf("z': "); print_array(z, str->size); */

  int j;
  for (j = 0; j < str->size - 1; j++) {
    int const i = str->size - z[j];
    L_prime[i] = j;

    if (z[j] == j + 1) {
      l_prime[i] = j + 1;
    }
  }
  free(z);

  int **pair = malloc(2 * sizeof(int*));
  pair[0] = L_prime;
  pair[1] = l_prime;

  return pair;
}

int strong_good_suffix_shift(int **const ls, int const i, int const n) {
  int const *const L_prime = ls[0]; 
  int const *const l_prime = ls[1];

  if (i == n - 1) {
    return 1;
  } else if (i == -1) {
    return (n - 1) - l_prime[1] ;
  } else if (L_prime[i + 1] > 0) {
    return (n - 1) - L_prime[i + 1]; 
  } else {
    return (n - 1) - l_prime[i + 1];
  }
}

result *B(string const * const T, string const * const P) {
  result *const res = new_result();
  int const n = P->size, m = T->size;

  /* Right-most character occurrence array */
  int *const R = bad_char_preprocessing(P);
  int **const ls = strong_good_suffix_preprocessing(P);

  /* printf("L': "); print_array(ls[0], P->size); */
  /* printf("l': "); print_array(ls[1], P->size); */

  int t     = n - 1; /* T index */
  int shift = 0;     /* amount to shift P */
  for (; t < m; t += shift) {
    int const i = ncmp(at_char(T, t), at_char(P, n - 1), n, right_left, res);
    if (i == -1) { /* match */
      add(res, t - n + 1);
    }
    shift = max(bad_char_shift(R, i, *at_char(T, t)),
                strong_good_suffix_shift(ls, i, n));
  }

  free(R);
  free(ls[0]);
  free(ls[1]);
  free(ls);
  return res;
}

/* ************************************************************************** */

int main() {
  puts("");

  char *txts[] = {"tcgcagggcg", "aaaaaaaaaa", "gcccaaagac"};
  char *pats[] = {"tc", "aaa", "ca"};
  char *poss[] = {"0", "0 1 2 3 4 5 6 7", "3"};
  char *cmps[] = {"7", "24", "9"};

  /* char *txts[] = {"cagtagtagctgacagtagtagtacggcagtagtag"}; */
  /* char *pats[] = {"cagtagtag"}; */
  /* char *poss[] = {"0 13 27"}; */
  /* char *cmps[] = {"..."}; */

  size_t i;
  for (i = 0; i < sizeof(txts) / sizeof(char*); i++) {
    string *txt = new_string(txts[i]);
    string *pat = new_string(pats[i]);

    result *res = B(txt, pat);
    print_vector_int(res->positions);
    printf("(should be %s)\n", poss[i]);
    printf("%d (should be %s)\n", res->comparisons, cmps[i]);
    
    free(txt);
    free(pat);
    free(res);
  }

  return 0;
}

/* /\* don't forget to reread the spec and check if we're printing the right amount */
/*    of spaces and newlines *\/ */
/* int main() { */
/*   char command; */
/*   vector_char *T = new_vector_char(); */
/*   vector_char *P = new_vector_char(); */
/*   result *result; */

/*   while ((command = getchar()) != 'X') { */
/*     getchar(); /\* space character *\/ */

/*     switch (command) { */
/*     case 'T': */
/*       read(T); */
/*       /\* printf("T: T = "); print_vector_char(T); *\/ */
/*       break; */
/*     case 'N': */
/*       read(P); */
/*       result = N(T, P); */

/*       print_vector_int(result->positions); */
/*       printf("%d \n", result->comparisons); */

/*       free_result(result); */
/*       break; */
/*     case 'K': */
/*       read(P); */
/*       result = K(T, P); */

/*       print_vector_int(result->positions); */
/*       printf("%d \n", result->comparisons); */

/*       free_result(result); */
/*       break; */
/*     case 'B': */
/*       read(P); */
/*       result = B(T, P); */

/*       print_vector_int(result->positions); */
/*       printf("%d \n", result->comparisons); */

/*       free_result(result); */
/*       break; */
/*     default: */
/*       printf("Ignoring command: '%c'.\n", command); */
/*     } */
/*   } */

/*   free_vector_char(T); */
/*   free_vector_char(P); */
/*   return 0; */
/* } */
