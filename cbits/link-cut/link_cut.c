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

struct node {
  struct node *left;
  struct node *right;

  /* FIXME: REVIEW: Why double pointer? */
  struct node *hook;

  /* FIXME: REVIEW */
  bool reversed;
};

typedef struct node node;

node *new_node() {
  node *x = malloc(sizeof(node));

  x->left = NULL;
  x->right = NULL;
  x->hook = NULL;
  x->reversed = false;

  return x;
}

node **new_forest(int n) {
  node **f = malloc(n * sizeof(node*));
  int i = 0;
  for (; i < n; i++) f[i] = new_node();
  return f;
}

void free_forest(node **f, int n) {
  int i = 0;
  for (; i < n; i++)
    free(f[i]);
}

bool reversal_state(node *x);

node **left__(node *x) {
#ifdef DEBUG
  if (!x) fail("left: null argument");
#endif
  return reversal_state(x) ? &(x->right) : &(x->left);
}

node **right__(node *x) {
#ifdef DEBUG
  if (!x) fail("right: null argument");
#endif
  return reversal_state(x) ? &(x->left) : &(x->right);
}

node *left(node *x) {return *left__(x);}
void set_left(node *x, node *y) {*left__(x) = y;}
node *right(node *x) {return *right__(x);}
void set_right(node *x, node *y) {*right__(x) = y;}

/* FIXME: REVIEW */
/* Parent in the solid subtree of x. */
node *solid_parent(node *x) {
#ifdef DEBUG
  if (!x) fail("sparent: null argument");
#endif
  if (x->hook == NULL)
    return NULL;
  if (x != right(x->hook) && x != left(x->hook))
    return NULL;
  return x->hook;
}

/* FIXME: REVIEW */
void set_hook(node *x, node* y) {
#ifdef DEBUG
  if (!x) fail("set_hook: null 1st argument");
#endif
  x->hook = y;
}

node *solid_root(node *x) {
#ifdef DEBUG
  if (!x) fail("solid_root: null argument");
#endif
  node *y = solid_parent(x);
  while (y) {x = y; y = solid_parent(y);}
  return x;
}

/* FIXME: REVIEW */
/* Path-parent. */
node *path_parent(node *x) {
#ifdef DEBUG
  if (!x) fail("pparent: null argument");
#endif
  return solid_root(x)->hook;
}

void reverse(node *x) {
#ifdef DEBUG
  if (!x) fail("reverse: null argument");
#endif
  x->reversed = !(x->reversed);
}

/* FIXME: REVIEW */
bool reversal_state(node *x) {
#ifdef DEBUG
  if (!x) fail("reversal_state: null argument");
#endif
  bool b;
  for (b = x->reversed, x = solid_parent(x); x; x = solid_parent(x))
    b = (b != x->reversed); /* Exclusive OR */
  return b;
}

node *leftmost(node *x) {
#ifdef DEBUG
  if (!x) fail("leftmost: null argument");
#endif
  node *y = left(x);
  while (y) {x = y; y = left(y);}
  return x;
}

node *rightmost(node *x) {
#ifdef DEBUG
  if (!x) fail("rightmost: null argument");
#endif
  node *y = right(x);
  while (y) {x = y; y = right(y);}
  return x;
}

/* FIXME: REVIEW */
void rotr(node *x) {
#ifdef DEBUG
  if (!x) fail("rotr: null argument");
#endif
  node *y = x->hook; /* solid_parent(x); */
  node *z = right(x);

  set_left(y, z);
  if (z) set_hook(z, y);

  set_right(x, y);
  set_hook(x, solid_parent(y));

  set_hook(y, x);
}

/* FIXME: REVIEW */
void rotl(node *x) {
#ifdef DEBUG
  if (!x) fail("rotl: null argument");
#endif
  node *y = x->hook; /* solid_parent(x); */
  node *z = left(x);

  set_right(y, z);
  if (z) set_hook(z, y);

  set_left(x, y);
  set_hook(x, solid_parent(y));

  set_hook(y, x);
}

/* FIXME: REVIEW */
void splay_step(node *x, node *y) {
#ifdef DEBUG
  if (!x) fail("splay_step: null x");
  if (!y) fail("splay_step: null y");
#endif
  node *z = solid_parent(y);

  if (!z) { /* zig */
    (x == left(y)) ? rotr(x) : rotl(x);
  } else {
    if (x == left(y) && y == left(z)) { /* zig-zig */
      rotr(y);
      rotr(x);
    } else if (x == right(y) && y == right(z)) { /* zig-zig */
      rotl(y);
      rotl(x);
    } else if (x == left(y)) { /* zig-zag */
      rotr(x);
      rotl(x);
    } else {/* zig-zag */
      rotl(x);
      rotr(x);
    }
  }
}

/* FIXME: REVIEW */
void splay(node *u) {
#ifdef DEBUG
  if (!u) fail("splay: null argument");
#endif
  node *x, *y;

  /* First pass */
  for (x = u; x; x = x->hook)
    for (y = solid_parent(x); y; y = solid_parent(x))
      splay_step(x, y);

  /* Second pass */
  for (x = u; x; x = x->hook)
    if (x->hook)
      set_left(x->hook, x);

  /* Third pass */
  for (x = u, y = x->hook; y; y = x->hook)
    splay_step(x, y);
}

/* Return the root of the tree containing node x. */
node *root(node *x) {
#ifdef DEBUG
  if (!x) fail("root: null argument");
#endif
  splay(x);

  node *y = rightmost(x);
  splay(y);

  return y;
}

/* Add an edge from x to y, thereby making x a child of y in the forest. This
   operation assumes that x is the root of one tree and y is in another tree. */
/* FIXME: REVIEW */
void link(node *x, node *y) {
#ifdef DEBUG
  if (!x) fail("link: null x");
  if (!y) fail("link: null y");
#endif
  splay(x);
  splay(y);
  set_hook(x, y);
}

/* Delete the edge from x to its parent, thereby dividing the tree containing v
   into two trees. This operation assumes that x is not a tree root. */
/* FIXME: REVIEW */
void cut(node *x) {
#ifdef DEBUG
  if (!x) fail("cut: null argument");
#endif
  splay(x);

  if (right(x)) {
    set_hook(right(x), NULL);
    set_right(x, NULL);
  }
}

/* FIXME: REVIEW */
void expose(node *x) { /* access */
#ifdef DEBUG
  if (!x) fail("expose: null argument");
#endif
  for (x = solid_root(x); x->hook; x = solid_root(x))
    if (x->hook)
      set_left(x->hook, x);
}

/* evert(vertex v): Modify the tree containing vertex v by making v the root.
                    (This operation can be regarded as reversing the direction
                    of everty edge on the path from v to the original root.) */
void reroot(node *x) { /* evert */
#ifdef DEBUG
  if (!x) fail("evert: null argument");
#endif
  expose(x);
  reverse(x);
  x->hook = NULL;
  splay(x);
}

/*----------------------------------------------------------------------------*/
/* Project operations                                                         */
/*----------------------------------------------------------------------------*/

node **nodes;

void print_state(int n) {
  int i = 0;
  for (; i < n; i++) {
    printf("(%d,%p): {left: %p, right: %p, hook: %p, reversed: %d}\n",
           i+1, nodes[i], nodes[i]->left, nodes[i]->right, nodes[i]->hook,
           nodes[i]->reversed);
  }
}

/* Adds an edge linking the node u to the node v. If such an edge already exists
   or this insertion would create a cycle then the operation has no effect. */
/* FIXME: REVIEW */
void Link(int u, int v) {
  node *x = nodes[u-1], *y = nodes[v-1];

  if (x->hook == y || x == y->hook) /* edge already exists */
    return;
  if (root(x) == root(y)) /* cycle */
    return;

  reroot(x);
  link(x,y);
#ifdef DEBUG
  printf("Finished: Link(%d,%d)\n", u, v);
#endif
}

/* Removes the edge linking the node u to the node v, if such an edge exists. If
the edge does not exist this operation has no effect. */
/* FIXME: REVIEW */
void Cut(int u, int v) {
  node *x = nodes[u-1], *y = nodes[v-1];

  if (x->hook != y && x != y->hook) /* edge does not exist */
    return;

  if (x->hook == y) {
    cut(x);
    splay(y);
  } else {
    cut(y);
    splay(x);
  }
#ifdef DEBUG
  printf("Finished: Cut(%d,%d)\n", u, v);
#endif
}

/* Returns true if there is a connection from u to v. If such a connection does
   not exist it returns false. A connection may consist of a single edge, or a
   sequence of edges, provided that it links u to v. */
/* FIXME: REVIEW */
void ConnectedQ(int u, int v) {
  node *x = nodes[u-1], *y = nodes[v-1];
  printf("%c\n", root(x) == root(y) ? 'T' : 'F');
#ifdef DEBUG
  printf("Finished: ConnectedQ(%d,%d)\n", u, v);
#endif
}

/*----------------------------------------------------------------------------*/
/* Main                                                                       */
/*----------------------------------------------------------------------------*/

int main_link_cut() {
  int n;
  scanf(" %d", &n);
  nodes = new_forest(n);

  int u, v;
  while (true) {
#ifdef DEBUG
    print_state(n);
#endif
    if        (scanf(" L %d %d", &u, &v) == 2) {
      Link(u,v);
    } else if (scanf(" C %d %d", &u, &v) == 2) {
      Cut(u,v);
    } else if (scanf(" Q %d %d", &u, &v) == 2) {
      ConnectedQ(u,v);
    } else {
      break;
    }
  }

  free_forest(nodes, n);
  return 0;
}

int main() { return main_link_cut(); }
