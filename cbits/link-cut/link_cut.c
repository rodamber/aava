#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>

/*----------------------------------------------------------------------------*/
/* Utilities                                                                  */
/*----------------------------------------------------------------------------*/

void *undefined(char *s, ...) {
  fprintf(stderr, "*** undefined: %s\n", s);
  exit(-1);
}

void fail(char *s) {
  fprintf(stderr, "*** %s\n", s);
  raise(SIGSEGV);
}

#define not(X) (!(X))

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

  if (f) {
    int i;
    for (i = 0; i < n; i++) {
      node x = { .left = NULL, .right = NULL, .hook = NULL, .flipped = false };
      f[i] = x;
    }
  }

  return f;
}

/*----------------------------------------------------------------------------*/

/* FIXME: REVIEW */
void rotr(node *x, node *y) {
  assert(x != NULL);
  assert(y != NULL);

  node *z = y->hook;
  node *B = x->right;

  y->left = B;

  if (B) {
    B->hook = y;
  }

  x->right = y;
  y->hook = x;

  if (z) {
    if (y == z->left) {
      z->left = x;
    } else {
      assert(y == z->right);
      z->right = x;
    }
  }

  x->hook = z;
}

/* FIXME: REVIEW */
void rotl(node *x, node *y) {
  assert(x != NULL);
  assert(y != NULL);

  node *z = y->hook;
  node *B = x->left;

  y->right = B;

  if (B) {
    B->hook = y;
  }

  x->left = y;
  y->hook = x;

  if (z) {
    if (y == z->right) {
      z->right = x;
    } else {
      assert(y == z->left);
      z->left = x;
    }
  }

  x->hook = z;
}

/* FIXME: REVIEW */
void unflip(node *x) {
  assert(x != NULL);

  if (not(x->flipped)) {
    return;
  }

  assert(x->flipped);

  node *y = x->left;
  x->left = x->right;
  x->right = y;

  x->flipped = false;
  x->left->flipped = not(x->left->flipped);
  x->right->flipped = not(x->right->flipped);

  assert(not(x->flipped));
}

void zig(node *x, node *y) {
  assert(x != NULL);
  assert(y != NULL);
}

void zigzig(node *x, node *y, node *z) {
  assert(x != NULL);
  assert(y != NULL);
  assert(z != NULL);
}

void zigzag(node *x, node *y, node *z) {
  assert(x != NULL);
  assert(y != NULL);
  assert(z != NULL);
}

void splay_step(node *x, node *y) {
  assert(x != NULL);
  assert(y != NULL);
}

void splay(node *x) {
  assert(x != NULL);
}

/*----------------------------------------------------------------------------*/

/* void access(node *x) { */
/*   assert(x != NULL); */
/* } */
#define access(X) splay(X)

void reroot(node *x) {
  assert(x != NULL);
}

void link(node *x, node *y) {
  assert(x != NULL);
  assert(y != NULL);
}

void cut(node *x, node *y) {
  assert(x != NULL);
  assert(y != NULL);
}

bool connected(node *x, node *y) {
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
