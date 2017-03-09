#include <stdio.h>
#include <stdlib.h>

/* ************************************************************************** */

/* MinUnit */
int tests_run = 0;

#ifdef DEBUG

#define mu_assert(message, test) do { if (!(test)) return message; } while (0)
#define mu_run_test(test) do { char *message = test(); tests_run++; \
    if (message) return message; } while (0)

#else

#define mu_assert(message,test)
#define mu_run_test

#endif

/* ************************************************************************** */

#define DEFINE_VECTOR(TYPE)      \
typedef struct {                   \
  size_t volume; /* used + free */ \
  size_t size; /* Used slots */    \
  TYPE *array;                     \
} vector_##TYPE;                 \

DEFINE_VECTOR(char)
DEFINE_VECTOR(size_t)

#define DEFINE_NEW_VECTOR(TYPE)                              \
vector_##TYPE *new_vector_##TYPE() {                       \
  size_t const volume = 1; /* 0 or 128 */                      \
  vector_##TYPE* const da = malloc(sizeof(vector_##TYPE)); \
                                                               \
  da->array  = malloc(sizeof(TYPE) * volume);                  \
  da->size   = 0;                                              \
  da->volume = volume;                                         \
                                                               \
  return da;                                                   \
}                                                              \

DEFINE_NEW_VECTOR(char)
DEFINE_NEW_VECTOR(size_t)

#define DEFINE_UPVOLUME(TYPE)                                          \
void upvolume_##TYPE(vector_##TYPE * const da) {                     \
  if (da->size >= da->volume) { /* We don't have enough free space. */ \
    da->volume = 2 * da->volume;                                       \
    da->array  = realloc((char*) da->array, da->volume);               \
  }                                                                    \
}                                                                      \

DEFINE_UPVOLUME(char)
DEFINE_UPVOLUME(size_t)

#define DEFINE_DOWNVOLUME(TYPE)                             \
void downvolume_##TYPE(vector_##TYPE * const da) {        \
  if (/* da->size > 127 && */ da->volume >= 4 * da->size) { \
    da->volume = da->volume / 2;                            \
    da->array  = realloc((char*) da->array, da->volume);    \
  }                                                         \
}                                                           \

DEFINE_DOWNVOLUME(char)
DEFINE_DOWNVOLUME(size_t)

#define DEFINE_INSERT(TYPE)                                       \
void insert_##TYPE(vector_##TYPE * const da, const char elem) { \
  upvolume_##TYPE(da);                                            \
  da->array[da->size++] = elem;                                   \
}                                                                 \

DEFINE_INSERT(char)
DEFINE_INSERT(size_t)

#define DEFINE_DELETE(TYPE)                      \
void delete_##TYPE(vector_##TYPE * const da) { \
  downvolume_##TYPE(da);                         \
  da->size--;                                    \
}                                                \

DEFINE_DELETE(char)
DEFINE_DELETE(size_t)

#define DEFINE_WRITE(TYPE)                                                        \
void write_##TYPE(vector_##TYPE * const da, const size_t at, const char elem) { \
  if (at == da->size) {                                                           \
    insert_##TYPE(da, elem);                                                      \
  } else {                                                                        \
    da->array[at] = elem;                                                         \
  }                                                                               \
}                                                                                 \

DEFINE_WRITE(char)
DEFINE_WRITE(size_t)

#define DEFINE_FREE_VECTOR(TYPE)                      \
void free_vector_##TYPE(vector_##TYPE * const da) { \
  free(da->array);                                      \
  free(da);                                             \
}                                                       \

DEFINE_FREE_VECTOR(char)
DEFINE_FREE_VECTOR(size_t)

#define DEFINE_PRINT_ARRAY(TYPE, X)                         \
void print_array_##TYPE(vector_##TYPE const * const da) { \
  size_t i;                                                 \
  for (i = 0; i < da->size; i++) {                          \
    printf(X, da->array[i]);                                \
  }                                                         \
  puts("");                                                 \
}                                                           \

DEFINE_PRINT_ARRAY(char, "%c")
DEFINE_PRINT_ARRAY(size_t, "%zu")

/* ************************************************************************** */

typedef struct {
  vector_size_t *positions;
  size_t comparisons;
} result;

/* ************************************************************************** */

void read(vector_char * const da) {
  char c;
  int i = 0;

  while ((c = getchar()) != '\n') {
    write_char(da, i++, c);
  }
}

void N(vector_char const * const T, vector_char const * const P) {
  (void) T; printf("N: P = "); print_array_char(P);
}

void K(vector_char const * const T, vector_char const * const P) {
  (void) T; printf("K: P = "); print_array_char(P);
}

void B(vector_char const * const T, vector_char const * const P) {
  (void) T; printf("B: P = "); print_array_char(P);
}

/* ************************************************************************** */

/* don't forget to reread the spec and check if we're printing the right amount
   of spaces and newlines */
int main() {
  char command;
  vector_char * T = new_vector_char();
  vector_char * P = new_vector_char();

  while ((command = getchar()) != 'X') {
    getchar(); /* space character */

    switch (command) {
    case 'T':
      read(T);
      printf("T: T = "); print_array_char(T);
      break;
    case 'N':
      read(P);
      N(T, P);
      break;
    case 'K':
      read(P);
      K(T, P);
      break;
    case 'B':
      read(P);
      B(T, P);
      break;
    default:
      printf("Invalid command: '%c'. Abort!\n", command);
      return -1;
    }
  }

  free_vector_char(T);
  free_vector_char(P);
  return 0;
}

