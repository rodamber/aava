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

int write(dynarray * const da, const size_t at, const char elem) {
  if (at == da->size) {
    insert(da, elem);
    return 0;
  } else if (at < da->size) {
    da->array[at] = elem;
    return 0;
  } else {
    return -1;
  }
}

void free_dynarray(dynarray * const  da) {
  free(da->array);
  free(da);
}

void print_array(dynarray * const  da) {
  size_t i;
  for (i = 0; i < da->size; i++) {
    printf("%c", da->array[i]);
  }
  puts("");
}

/* ************************************************************************** */

/* later check if this is too slow, altough I doubt that */
void T(dynarray * da) {
  char c;
  while ((c = getchar()) != '\n') {
    write(da, da->size, c);
  }
}

/* ************************************************************************** */

void T_impl_1(char **s, size_t *sz, size_t s_count) {
  size_t i = 0, j = 0;

  for (; j < s_count; j++) {
    dynarray * da = new_dynarray();

    for (; i < sz[j]; i++) {
      insert(da, s[j][i]);
    }
    free_dynarray(da);
  }
}

void T_impl_2(char **s, size_t *sz, size_t s_count) {
  dynarray * da = new_dynarray();
  size_t i = 0, j = 0;

  for (; j < s_count; j++) {
    for (; i < sz[j]; i++) {
      write(da, i, s[j][i]);
    }
  }

  free_dynarray(da);
}

int main() {
  char s128[128];
  char s256[256];
  char s512[512];
  char s1024[1024];
  char s2048[2048];
  char s4096[4096];

  s128[127]   = '\0';
  s256[255]   = '\0';
  s512[511]   = '\0';
  s1024[1023] = '\0';
  s2048[2047] = '\0';
  s4096[4095] = '\0';

  size_t s_count = 6;

  /* char *s__[] = { s128, s256, s512, s1024, s2048, s4096 }; */
  /* size_t sz_[] = { 128, 256, 512, 1024, 2048, 4096}; */

  /* char *s__[]  = { s4096, s2048, s1024, s512, s256, s128 }; */
  /* size_t sz_[] = { 4096, 2048, 1024, 512, 256, 128 }; */

  /* char *s__[] = { s128, s512, s256, s2048, s1024, s4096 }; */
  /* size_t sz_[] = { 128, 512, 256, 2048, 1024, 4096 }; */

  char *s__[] = { s256, s128, s1024, s512, s4096, s2048 };
  size_t sz_[] = { 256, 128, 1024, 512, 4096, 2048 };

  T_impl_2(s__, sz_, s_count);

  return 0;
}

