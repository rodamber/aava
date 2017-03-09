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

typedef struct {
  size_t volume; /* used + free */
  size_t size; /* Used slots; equal to the index of the last element plus one */
  char *array;
} dynarray;

/* ************************************************************************** */

dynarray *new_dynarray() {
  size_t const volume = 1; /* 0 or 128 */
  dynarray* const da = malloc(sizeof(dynarray));

  da->array  = malloc(sizeof(char) * volume);
  da->size   = 0;
  da->volume = volume;

  return da;
}

void upvolume(dynarray * const da) {
  if (da->size >= da->volume) { /* We don't have enough free space. */
    da->volume = 2 * da->volume;
    da->array  = realloc((char*) da->array, da->volume);
  }
}

void downvolume(dynarray * const da) {
  if (/* da->size > 127 && */ da->volume >= 4 * da->size) {
    da->volume = da->volume / 2;
    da->array  = realloc((char*) da->array, da->volume);
  }
}

void insert(dynarray * const da, const char elem) {
  upvolume(da);
  da->array[da->size++] = elem;
}

void delete(dynarray * const da) {
  downvolume(da);
  da->size--;
}

void write(dynarray * const da, const size_t at, const char elem) {
  if (at == da->size) {
    insert(da, elem);
  } else {
    da->array[at] = elem;
  }
}

void free_dynarray(dynarray * const da) {
  free(da->array);
  free(da);
}

void print_array(dynarray * const da) {
  size_t i;
  for (i = 0; i < da->size; i++) {
    printf("%c", da->array[i]);
  }
  puts("");
}

/* ************************************************************************** */

typedef struct {
  size_t *positions; /* this could be a dynamic array if it was char* =/ */
  size_t comparisons;
} result;

/* ************************************************************************** */

void read(dynarray * const da) {
  char c;
  int i = 0;

  while ((c = getchar()) != '\n') {
    write(da, i++, c);
  }
}

void N(dynarray const * const T, dynarray const * const P) {
  (void) T; (void) P;
  printf("N command.\n");
}

void K(dynarray const * const T, dynarray const * const P) {
  (void) T; (void) P;
  printf("K command.\n");
}

void B(dynarray const * const T, dynarray const * const P) {
  (void) T; (void) P;
  printf("B command.\n");
}

/* ************************************************************************** */

/* don't forget to reread the spec and check if we're printing the right amount
   of spaces and newlines */
int main() {
  char command;
  dynarray * T = new_dynarray();
  dynarray * P = new_dynarray();

  while ((command = getchar()) != 'X') {
    getchar(); /* space character */

    switch (command) {
    case 'T':
      read(T);
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
    }
  }

  free_dynarray(T);
  free_dynarray(P);
  return 0;
}

