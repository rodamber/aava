#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>

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
int forest_size;

node *new_forest(int forest_size) {
  assert(forest_size > 0);

  node *f = calloc(forest_size, sizeof(node));

  if (f) {
    int i;
    for (i = 0; i < forest_size; i++) {
      node x = { .left = NULL, .right = NULL, .hook = NULL, .flipped = false };
      f[i] = x;
    }
  }

  return f;
}

/*----------------------------------------------------------------------------*/

void pnode(node *x) {
  if (!x) {
    printf("pnode: nil");
    return;
  }

  printf("    %ld: {left: %ld, right: %ld, hook: %ld, flipd: %d}\n",
         x - forest + 1,
         x->left  ? x->left  - forest + 1: 0,
         x->right ? x->right - forest + 1: 0,
         x->hook  ? x->hook  - forest + 1: 0,
         x->flipped);
}

void pstate() {
  puts("");
  int i;
  for (i = 0; i < forest_size; i++)
    pnode(&(forest[i]));
}

/*----------------------------------------------------------------------------*/

void rotr(node *x, node *y) {
  assert(x != NULL);
  assert(y != NULL);
  assert(x->hook == y);

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
    } else if (y == z->right){
      /* assert(y == z->right); */
      z->right = x;
    }
  }

  x->hook = z;

  assert(y->hook == x);
}

void rotl(node *x, node *y) {
  assert(x != NULL);
  assert(y != NULL);
  assert(x->hook == y);

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
    } else if (y == z->left){
      /* assert(y == z->left); */
      z->left = x;
    }
  }

  x->hook = z;

  assert(y->hook == x);
}

void flip(node *x) {
  assert(x != NULL);
  x->flipped = true;
}

void unflip(node *x) {
  assert(x != NULL);

  if (x->flipped == false) {
    return;
  }

  node *y = x->left;
  x->left = x->right;
  x->right = y;

  x->flipped = false;

  if (x->left) {
    x->left->flipped = !(x->left->flipped);
  }
  if (x->right) {
    x->right->flipped = !(x->right->flipped);
  }

  assert(!(x->flipped));
}

node *solid_parent(node *x) {
  if (x->hook == NULL) {
    return NULL;
  } else if (x != x->hook->right && x != x->hook->left) {
    return NULL;
  } else {
    return x->hook;
  }
}

void splay_step(node *x, node *y) {
  assert(x != NULL);
  assert(y != NULL);

  node *z = solid_parent(y);

  /* are the unflips in the correct place? it shouldn't matter */
  if (!z) { /* zig */
    unflip(y);
    unflip(x);

    if (x == y->left) {
      rotr(x,y);
    } else {
      assert(x == y->right);

      rotl(x,y);
    }
  } else {
    unflip(z);
    unflip(y);
    unflip(x);

    if (x == y->left && y == z->left) { /* zig-zig */
      rotr(y,z);
      rotr(x,y);
    } else if (x == y->right && y == z->right) { /* zig-zig */
      rotl(y,z);
      rotl(x,y);
    } else if (x == y->left) { /* zig-zag */
      assert(y == z->right);

      rotr(x,y);
      rotl(x,z);
    } else {/* zig-zag */
      assert(x == y->right);
      assert(y == z->left);

      rotl(x,y);
      rotr(x,z);
    }
  }

  assert(x->flipped == false);
  assert(y->flipped == false);
}

void splay(node *u) {
  assert(u != NULL);

  node *x, *y;

  /* First pass */
  for (x = u; x; x = x->hook) {
    for (y = solid_parent(x); y; y = solid_parent(x)) {
      splay_step(x,y);
    }
    unflip(x);
  }

  /* Second pass */
  for (x = u; x; x = x->hook) {
    if (x->hook) {
      x->hook->left = x;
    }
  }

  /* Third pass */
  for (x = u, y = x->hook; y; y = x->hook) {
    splay_step(x, y);
  }

  assert(u->hook == NULL);
  assert(u->flipped == false);
}

/*----------------------------------------------------------------------------*/

void access(node *x) {
  assert(x != NULL);

  splay(x);
  x->left = NULL;
}

void reroot(node *x) {
  assert(x != NULL);

  access(x);
  flip(x);
}

/* Follows left pointers of x. Returns if y is found. */
bool search_left(node *x, node *y) {
  for (x = x->left; x; x = x->left) {
    if (x == y) {
      return true;
    }
  }
  return false;
}

/* Follows right pointers of x. Returns if y is found. */
bool search_right(node *x, node *y) {
  for (x = x->right; x; x = x->right) {
    if (x == y) {
      return true;
    }
  }
  return false;
}

bool connected(node *x, node *y) {
  assert(x != NULL);
  assert(y != NULL);

  reroot(x);
  access(y);

  return search_left(y,x) || search_right(y,x);
}

void link(node *x, node *y) {
  assert(x != NULL);
  assert(y != NULL);

  if (connected(x,y)) {
    return;
  }

  x->hook = y;
}

void cut(node *x, node *y) {
  assert(x != NULL);
  assert(y != NULL);

  if (!connected(x,y)) {
    return;
  }

  if (x == y->left) {
    y->left = NULL;
    x->hook = NULL;
  } else if (x == y->right){
    y->right = NULL;
    x->hook = NULL;
  }
}

/*----------------------------------------------------------------------------*/

/* This is here just for QuickCheck */
node *alloc(int forest_size) {
  forest = new_forest(forest_size);
  return forest;
}

void link_(int x, int y) {
  link(&forest[x-1], &forest[y-1]);
}

void cut_(int x, int y) {
  cut(&forest[x-1], &forest[y-1]);
}

void connected_(int x, int y) {
  connected(&forest[x-1], &forest[y-1]);
}

int main_link_cut() {
  if (scanf(" %d", &forest_size) != 1)
    exit(-1);
  alloc(forest_size);

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

  free(forest);
  return 0;
}

/* int main() {return main_link_cut();} */
