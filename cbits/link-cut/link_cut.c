#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>

/*----------------------------------------------------------------------------*/
/* Utilities                                                                  */
/*----------------------------------------------------------------------------*/

void *undefined(const char *s, ...) {
  fprintf(stderr, "*** undefined: %s\n", s);
  exit(-1);
}

void fail(const char *s) {
  fprintf(stderr, "*** %s\n", s);
  raise(SIGSEGV);
}

/*----------------------------------------------------------------------------*/
/* Link/Cut tree internal representation                                       */
/*----------------------------------------------------------------------------*/

typedef struct node {
  struct node *left;
  struct node *right;
  struct node *hook;
  bool flipped;
} node;

node *forest;

node *new_forest(int n) {
  assert(n > 0);

  node *f = malloc(n * sizeof(node));

  if (f != NULL) {
    int i;
    for (i = 0; i < n; i++) {
      node x = { .left = NULL, .right = NULL, .hook = NULL, .flipped = false };
      f[i] = x;
    }
  }

  return f;
}

/*----------------------------------------------------------------------------*/

void rotr(const node *x, const node *y) {
  assert(x != NULL);
  assert(y != NULL);
}

void rotl(const node *x, const node *y) {
  assert(x != NULL);
  assert(y != NULL);
}

void unflip(const node *x) {
  assert(x != NULL);
}

void zig(const node *x, const node *y) {
  assert(x != NULL);
  assert(y != NULL);
}

void zigzig(const node *x, const node *y, const node *z) {
  assert(x != NULL);
  assert(y != NULL);
  assert(z != NULL);
}

void zigzag(const node *x, const node *y, const node *z) {
  assert(x != NULL);
  assert(y != NULL);
  assert(z != NULL);
}

void splay_step(const node *x, const node *y) {
  assert(x != NULL);
  assert(y != NULL);
}

void splay(const node *x) {
  assert(x != NULL);
}

/*----------------------------------------------------------------------------*/

/* void access(const node *x) { */
/*   assert(x != NULL); */
/* } */
#define access(X) splay(X)

void reroot(const node *x) {
  assert(x != NULL);
}

void link(const node *x, const node *y) {
  assert(x != NULL);
  assert(y != NULL);
}

void cut(const node *x, const node *y) {
  assert(x != NULL);
  assert(y != NULL);
}

bool connected(const node *x, const node *y) {
  assert(x != NULL);
  assert(y != NULL);
  return false;
}

/*----------------------------------------------------------------------------*/

/* For QuickCheck */
void init(int n) {
  forest = new_forest(n);
}

void finally() {
  free(forest);
};

int main_link_cut() {
  int forest_size;
  if (scanf(" %d", &forest_size) != 1)
    exit(-1);
  init(forest_size);

  int u, v;
  while (true) {
    if        (scanf(" L %d %d", &u, &v) == 2) {
      link(&forest[u-1], &forest[v-1]);
    } else if (scanf(" C %d %d", &u, &v) == 2) {
      cut(&forest[u-1], &forest[v-1]);
    } else if (scanf(" Q %d %d", &u, &v) == 2) {
      bool c = connected(&forest[u-1], &forest[v-1]);
      printf("%c\n", c ? 'T' : 'F');
    } else {
      break;
    }
  }

  finally();
  return 0;
}

int main() {
  return main_link_cut();
}
