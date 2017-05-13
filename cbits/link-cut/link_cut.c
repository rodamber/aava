#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

/*----------------------------------------------------------------------------*/

typedef struct node {
  struct node *left;
  struct node *right;
  struct node *hook;
  bool flipped;
} node;

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

void rotr(node *x, node *y) {
  assert(x);
  assert(y);
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
      z->right = x;
    }
  }

  x->hook = z;

  assert(y->hook == x);
}

void rotl(node *x, node *y) {
  assert(x);
  assert(y);
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
      z->left = x;
    }
  }

  x->hook = z;

  assert(y->hook == x);
}

void flip(node *x) {
  assert(x);
  x->flipped = true;
}

void unflip(node *x) {
  assert(x);

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
  assert(x);
  assert(y);

  node *z = solid_parent(y);

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
  assert(u);

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
  assert(x);

  splay(x);
  x->left = NULL;
}

void reroot(node *x) {
  assert(x);

  access(x);
  flip(x);
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

bool connected_(node *x, node *y) {
  assert(x);
  assert(y);

  reroot(x);
  access(y);

  return search_right(y,x);
}

void link_(node *x, node *y) {
  assert(x);
  assert(y);

  if (connected_(x,y)) {
    return;
  }

  x->hook = y;
}

void cut_(node *x, node *y) {
  assert(x);
  assert(y);

  reroot(x);
  access(y);

  if (y->right == x && y->right->left == NULL) {
    y->right = NULL;
    x->hook = NULL;
  }
}

/*----------------------------------------------------------------------------*/

void link(node *forest, int x, int y) {
  link_(&forest[x-1], &forest[y-1]);
}

void cut(node *forest, int x, int y) {
  cut_(&forest[x-1], &forest[y-1]);
}

bool connected(node *forest, int x, int y) {
  return connected_(&forest[x-1], &forest[y-1]);
}

/*----------------------------------------------------------------------------*/

int main() {
  int forest_size;

  if (scanf(" %d", &forest_size) != 1)
    exit(-1);
  node *forest = new_forest(forest_size);

  int u, v;
  while (true) {
    if        (scanf(" L %d %d", &u, &v) == 2) {
      link(forest, u,v);
    } else if (scanf(" C %d %d", &u, &v) == 2) {
      cut(forest, u,v);
    } else if (scanf(" Q %d %d", &u, &v) == 2) {
      bool c = connected(forest, u,v);
      printf("%c\n", c ? 'T' : 'F');
    } else {
      break;
    }
  }

  free(forest);
  return 0;
}
