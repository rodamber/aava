#include <limits.h>
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

#define DEFINE_GET_ARRAY(TYPE)                             \
  TYPE *get_array_##TYPE(vector_##TYPE const *const vec) { \
    return vec->array;                                     \
  }                                                        \

DEFINE_GET_ARRAY(char)
DEFINE_GET_ARRAY(int)

#define DEFINE_GET_SIZE(TYPE)                           \
  int get_size_##TYPE(vector_##TYPE const *const vec) { \
    return vec->size;                                   \
  }                                                     \

DEFINE_GET_SIZE(char)
DEFINE_GET_SIZE(int)

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
    if (at == da->size + 1) {                                                  \
      insert_##TYPE(da, elem);                                                 \
    } else {                                                                   \
      da->array[at - 1] = elem;                                                \
    }                                                                          \
  }                                                                            \

DEFINE_WRITE(char)
DEFINE_WRITE(int)

#ifdef DEBUG

#define DEFINE_AT(TYPE)                                                \
  TYPE *at_##TYPE(vector_##TYPE const * const v, const int i) {        \
    if (i > v->size)                                                   \
      printf("Out of bounds access: size = %d, i = %d\n", v->size, i); \
    return &(v->array[i - 1]);                                         \
  }                                                                    \

#else

#define DEFINE_AT(TYPE)                                         \
  TYPE *at_##TYPE(vector_##TYPE const * const v, const int i) { \
    return &(v->array[i - 1]);                                  \
  }                                                             \

#endif

DEFINE_AT(char)
DEFINE_AT(int)

#define DEFINE_AT_BACK(TYPE)                                         \
  TYPE *at_back_##TYPE(vector_##TYPE const * const v, const int i) { \
    return &(v->array[v->size - i]);                                 \
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

string *new_string(char const *const s) {
  vector_char *const vec = new_vector_char();

  int i = 0;
  while (s[i]) {
    insert_char(vec, s[i]);
    i++;
  }

  return vec;
}

#define DEFINE_NEW_VECTOR_INIT(TYPE)                                       \
  vector_##TYPE *new_vector_init_##TYPE(TYPE const val, int const nmemb) { \
    vector_##TYPE *const vec = new_vector_##TYPE();                        \
                                                                           \
    int n = nmemb;                                                         \
    while (n--) insert_##TYPE(vec, val);                                   \
                                                                           \
    return vec;                                                            \
  }                                                                        \

DEFINE_NEW_VECTOR_INIT(int)
DEFINE_NEW_VECTOR_INIT(char)

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

#define DEFINE_REVERSE(TYPE)                                      \
  vector_##TYPE *reverse_##TYPE(vector_##TYPE const *const vec) { \
    vector_##TYPE *const rev = new_vector_##TYPE();               \
                                                                  \
    int i;                                                        \
    for (i = 1; i <= vec->size; i++) {                            \
      insert_##TYPE(rev, *at_back_##TYPE(vec, i));                \
    }                                                             \
                                                                  \
    return rev;                                                   \
  }                                                               \

DEFINE_REVERSE(char)
DEFINE_REVERSE(int)

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

void print_result(result const *const result) {
  printf("positions: "); print_vector_int(result->positions);
  printf("comparison: %d\n", result->comparisons);
}

int get_positions_size(result const *const res) {
  return res->positions->size;
}

int *get_positions(result const *const res) {
  const int n = sizeof(int) * res->positions->size;
  int *pos = malloc(n);
  memcpy(pos, res->positions->array, n);
  return pos;
}

int get_comparisons(result const *const res) {
  return res->comparisons;
}

void add(result const * const result, const int new_pos) {
  insert_int(result->positions, new_pos - 1);
}

void inc(result * const result) {
  result->comparisons++;
}

/* ************************************************************************** */

int const left_right = 1;
int const right_left = -1;

/* Returns index of mismatch, or -n-1 if it matches and direction is right to
   left, or n+1 if it matches and direction is left to right */
int ncmp(char const *const s1, char const *const s2, int const n,
         int const direction, result *const res) {
  int i;
  for (i = 0; i < n; i++) {
    if (res) inc(res);
    if (s1[direction * i] != s2[direction * i]) {
      break;
    }
  }
  return direction * (i + 1);
}

// Note: Investigate this Stack/GHC bug, where this function is run for no reason.
/* void read() { static int i = 1; printf("Call %d\n", i++); } */

void read_(string * const vec) {
  char c;
  int i = 1;

  while ((c = getchar()) != '\n') {
    write_char(vec, i++, c);
  }
  vec->size = i - 1;
  downvolume_char(vec);
}

/* ************************************************************************** */
/* Naive                                                                      */
/* ************************************************************************** */

result *naive (string const * const T, string const * const P) {
  result *const res = new_result();
  int const n = P->size, m = T->size;

  if (n == 0 || m == 0) return res;

  int t;
  for (t = 1; t <= m - n + 1; t++) {
    if (ncmp(at_char(T, t), at_char(P, 1), n, left_right, res) == n + 1) {
      add(res, t);
    }
  }
  return res;
}

/* ************************************************************************** */
/* Z algorithm                                                                */
/* ************************************************************************** */

int match_count(char const *const s1, char const *const s2, int const n) {
  int i = 0;
  while (i < n && s1[i] == s2[i]) {
    i++;
  }
  return i;
}

vector_int *z_algorithm(string const *const str) {
#define Z(K) *at_int(z, (K))
#define S(K) at_char(str, K) /* mind the '*' */

  int const n         = str->size;
  vector_int *const z = new_vector_int();
  insert_int(z, 0);

  int l = 0, r = 0, k = 2;
  for (; k <= n; k++) {
    if (k > r) {
      insert_int(z, match_count(S(k), S(1), n - k + 1));

      if (Z(k) > 0) { /* case 1 */
        r = k + Z(k) - 1;
        l = k;
      }

    } else {
      int const k_prime   = k - l + 1;
      int const beta_size = r - k + 1;

      if (Z(k_prime) < beta_size) { /* case 2a */
        insert_int(z, Z(k_prime));

      } else { /* case 2b */
        int const count =
          (r == n ? 0 : match_count(S(r + 1), S(beta_size + 1), n - r));
        insert_int(z, beta_size + count);
        r += count;
        l = k;
      }
    }
  }

  return z;

#undef S
#undef Z
}

vector_int *reverse_z_algorithm(string const *const str) {
  string *const rev_str = reverse_char(str);

  vector_int *const z = z_algorithm(rev_str);
  free_vector_char(rev_str);

  vector_int *const rev_z = reverse_int(z);
  free_vector_int(z);

  return rev_z;
}

/* ************************************************************************** */
/* Knuth-Morris-Pratt                                                         */
/* ************************************************************************** */

#define P(i) *at_char(pat, (i))
#define PI(i) *at_int(pi, (i))

vector_int *compute_prefix_function(string const *const pat) {

  int const m = pat->size;
  vector_int *const pi = new_vector_init_int(0, m);

  int k = 0;
  int q = 2;
  for (; q <= m; q++) {
    while (k > 0 && P(k + 1) != P(q)) {
      k = PI(k);
    }

    if (P(k + 1) == P(q)) {
      k++;
    }

    PI(q) = k;
  }

  return pi;
}

#undef P
#undef PI

#define P(i) *at_char(pat, (i))
#define T(i) *at_char(txt, (i))
#define PI(i) *at_int(pi, (i))

result *knuth_morris_pratt(string const *const txt, string const *const pat) {
  result *const res = new_result();

  int const n = txt->size;
  int const m = pat->size;

  vector_int *const pi = compute_prefix_function(pat);

  int q = 0;
  int i = 1;
  for (; i <= n; i++) {
    while (q > 0 && P(q + 1) != T(i)) {
      inc(res);
      q = PI(q);
    }

    if (inc(res), P(q + 1) == T(i)) {
      q++;
    }

    if (q == m) {
      if (i != n) {
        inc(res);
      }
      add(res, i - m + 1);
      q = PI(q);
    }
  }
  free_vector_int(pi);

  return res;
}

#undef P
#undef T
#undef PI

/* ************************************************************************** */
/* Boyer-Moore                                                                */
/* ************************************************************************** */

int *R_at(int const *const R, char const x) {
  enum {A, C, T, G};
  switch (x) {
  case 'A': return (int*) &R[A];
  case 'C': return (int*) &R[C];
  case 'T': return (int*) &R[T];
  case 'G': return (int*) &R[G];
  default:
    fprintf(stderr, "R_at: bad character '%c'", x);
    return NULL;
  }
}

/* Don't forget to free the array R */
int *bad_char_preprocessing(string const *const pat) {
  int *const R = calloc(4, sizeof(int));

  int i;
  for (i = 1; i <= pat->size; i++) {
    *R_at(R, *at_char(pat, i)) = i;
  }
  return R;
}

int bad_char_shift(int const *const R, int const i, char const c) {
  return max(1, i - *R_at(R, c));
}

vector_int *build_big_l_prime(string const *const str, vector_int const *const N) {
  // FIXME: WE DON'T THE STR...
  int const n = str->size;
  vector_int *const big_l_prime = new_vector_init_int(0, n);

  int j;
  for (j = 1; j <= n - 1; j++) {
    int const i = n - *at_int(N, j) + 1;

    if (*at_int(N, j) > 0) {
      *at_int(big_l_prime, i) = j;
    }
  }

  return big_l_prime;
}

vector_int *build_small_l_prime(string const *const str, vector_int const *const N) {
  int const n = str->size;
  vector_int *const l_prime = new_vector_init_int(0, n);

  int i;
  for (i = 1; i <= n; i++) {
    int j;
    for (j = 1; j <= n - 1; j++) {
      if (j <= n - i + 1 && *at_int(N, j) == j) {
        *at_int(l_prime, i) = j;
      }
    }
  }

  return l_prime;
}

int strong_good_suffix_shift(vector_int const *const big_l_prime,
                             vector_int const *const small_l_prime, int const ix) {
  int const n = big_l_prime->size;

  if (ix == -1) { /* Failed on first character */
    return 1;
  } else if (ix == -n - 1) { /* Matched pattern */
    return n - *at_int(small_l_prime, 2);
  }

  int i = n + ix + 1;
  if (*at_int(big_l_prime, i) > 0) { /* Mismatch occurs at i */
    return n - *at_int(big_l_prime, i);
  } else {
    return n - *at_int(small_l_prime, i);
  }
}

result *boyer_moore(string const * const txt, string const * const pat) {
  result *const res = new_result();
  int const n = pat->size, m = txt->size;

  /* Preprocessing */
  int *const R = bad_char_preprocessing(pat);
  vector_int *const N = reverse_z_algorithm(pat);
  vector_int *const big_l_prime = build_big_l_prime(pat, N);
  vector_int *const small_l_prime = build_small_l_prime(pat, N);

  /* Search */
  int t; /* txt index */
  int shift = 0;
  for (t = n; t <= m; t += shift) {
    int const ix = ncmp(at_char(txt, t), at_back_char(pat, 1), n, right_left, res);
    if (ix == -n - 1) { /* match */
      add(res, t - n + 1);
    }

    int const bc = bad_char_shift(R, n + ix + 1, *at_char(txt, t));
    int const gs = strong_good_suffix_shift(big_l_prime, small_l_prime, ix);

    shift = max(bc, gs);
  }

  free(R);
  free(N);
  free_vector_int(big_l_prime);
  free_vector_int(small_l_prime);

  return res;
}

/* ************************************************************************** */

int main_() {
  char command;
  vector_char *T = new_vector_char();
  vector_char *P = new_vector_char();
  result *result;

  while ((command = getchar()) != 'X') {
    getchar(); /* space character */

    switch (command) {
    case 'T':
      read_(T);
      break;
    case 'N':
      read_(P);
      result = naive(T, P);

      print_vector_int(result->positions);

      free_result(result);
      break;
    case 'K':
      read_(P);
      result = knuth_morris_pratt(T, P);

      print_vector_int(result->positions);
      printf("%d \n", result->comparisons);

      free_result(result);
      break;
    case 'B':
      read_(P);
      result = boyer_moore(T, P);

      print_vector_int(result->positions);
      printf("%d \n", result->comparisons);

      free_result(result);
      break;
    default:
      printf("Ignoring command: '%c'.\n", command);
    }
  }

  free_vector_char(T);
  free_vector_char(P);
  return 0;
}

/* int main() { return main_(); } */
