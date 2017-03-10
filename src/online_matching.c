#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ************************************************************************** */

#define DEFINE_VECTOR(TYPE)                     \
  typedef struct {                              \
    int volume; /* used + free */               \
    int size; /* Used slots */                  \
    TYPE *array;                                \
  } vector_##TYPE;                              \

DEFINE_VECTOR(char)
DEFINE_VECTOR(int)

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

/* ************************************************************************** */

void read(vector_char * const vec) {
  char c;
  int i = 0;

  while ((c = getchar()) != '\n') {
    write_char(vec, i++, c);
  }
  vec->size = i;
  downvolume_char(vec);
}

result *N(vector_char const * const T, vector_char const * const P) {
  result *result = new_result();

  /* Don't forget the arrays are not null terminated. */
  int t;
  for (t = 0; t <= T->size - P->size; t++) {
    if (strncmp(at_char(T, t), at_char(P, 0), P->size) == 0) {
      add(result, t);
    }
  }

  return result;
}

result *K(vector_char const * const T, vector_char const * const P) {
  (void) T; (void) P;
  result *result = new_result();
  return result;
}

result *B(vector_char const * const T, vector_char const * const P) {
  (void) T; (void) P;
  result *result = new_result();
  return result;
}

/* ************************************************************************** */

/* don't forget to reread the spec and check if we're printing the right amount
   of spaces and newlines */
int main() {
  char command;
  vector_char * T = new_vector_char();
  vector_char * P = new_vector_char();
  result *result;

  while ((command = getchar()) != 'X') {
    getchar(); /* space character */

    switch (command) {
    case 'T':
      read(T);
      /* printf("T: T = "); print_vector_char(T); */
      break;
    case 'N':
      read(P);
      result = N(T, P);

      print_vector_int(result->positions);

      free_result(result);
      break;
    case 'K':
      read(P);
      result = K(T, P);

      print_vector_int(result->positions);
      printf("%d \n", result->comparisons);

      free_result(result);
      break;
    case 'B':
      read(P);
      result = B(T, P);

      print_vector_int(result->positions);
      printf("%d \n", result->comparisons);

      free_result(result);
      break;
    default:
      printf("Ignoring command: '%c'.\n", command);
    }
  }

 /* finally: */
  free_vector_char(T);
  free_vector_char(P);
  return 0;
}
