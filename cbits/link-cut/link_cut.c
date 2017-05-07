#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

/*----------------------------------------------------------------------------*/
/* Utilities                                                                  */
/*----------------------------------------------------------------------------*/

void *undefined(const char *s, ...) {
  fprintf(stderr, "*** undefined: %s\n", s);
  exit(-1);
}

void *fail(const char *s) {
  fprintf(stderr, "*** %s\n", s);
  exit(-1);
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
  return calloc(1, sizeof(node));
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

node **left(node *x) {
#ifdef DEBUG
  if (!x) fail("left: null argument");
#endif
  if (x->reversed) return x->right ? &(x->right) : NULL;
  else             return x->left  ? &(x->left)  : NULL;
}

node **right(node *x) {
#ifdef DEBUG
  if (!x) fail("right: null argument");
#endif
  if (x->reversed) return x->left  ? &(x->left)  : NULL;
  else             return x->right ? &(x->right) : NULL;
}

/* Parent in the auxiliary tree. */
node **sparent(node *x) {
#ifdef DEBUG
  if (!x) fail("sparent: null argument");
#endif
  if (x->hook == NULL)
    return NULL;
  if (x != *right(x->hook) && x != *left(x->hook))
    return NULL;
  return &(x->hook);
}

/* Path-parent. */
node **pparent(node *x) {
#ifdef DEBUG
  if (!x) fail("pparent: null argument");
#endif
  if (x->hook == NULL)
    return NULL;
  if (x == *right(x->hook) || x == *left(x->hook))
    return NULL;
  return &(x->hook);
}

node *solid_root(node *x) {
#ifdef DEBUG
  if (!x) fail("solid_root: null argument");
#endif
  node **sp = sparent(x);
  node *y = sp ? *sp : NULL;

  while (y) {x = y; y = *sparent(y);}
  return x;
}

node *leftmost(node *x) {
#ifdef DEBUG
  if (!x) fail("leftmost: null argument");
#endif
  node **l = left(x);
  node *y = l ? *l : NULL;

  while (y) {x = y; y = *left(y);}
  return x;
}

node *rightmost(node *x) {
#ifdef DEBUG
  if (!x) fail("rightmost: null argument");
#endif
  node **r = right(x);
  node *y = r ? *r : NULL;

  while (y) {x = y; y = *right(y);}
  return x;
}

/* FIXME: REVIEW */
void reverse(node *x) {
#ifdef DEBUG
    if (!x) fail("reverse: null argument");
#endif
  x->reversed = !x->reversed;
}

/* FIXME: REVIEW */
void rotr(node *x) {
#ifdef DEBUG
  if (!x) fail("rotr: null argument");
#endif
  node *y = *sparent(x);
  node *z = *right(x);

  *left(y) = z;
  *sparent(z) = y;

  *right(x) = y;
  *sparent(x) = *sparent(y);

  *sparent(y) = x;
}

/* FIXME: REVIEW */
void rotl(node *x) {
#ifdef DEBUG
  if (!x) fail("rotl: null argument");
#endif
  node *y = *sparent(x);
  node *z = *left(x);

  *right(y) = z;
  *sparent(z) = y;

  *left(x) = y;
  *sparent(x) = *sparent(y);

  *sparent(y) = x;
}

/* FIXME: REVIEW */
void splay_step(node *x, node *y) {
#ifdef DEBUG
  if (!x) fail("splay_step: null x");
  if (!y) fail("splay_step: null y");
#endif
  node *z = *sparent(y);

  if (!z) { /* zig */
    (x == *left(y)) ? rotr(x) : rotl(x);
  } else {
    if (x == *left(y) && y == *left(z)) { /* zig-zig */
      rotr(y);
      rotr(x);
    } else if (x == *right(y) && y == *right(z)) { /* zig-zig */
      rotl(y);
      rotl(x);
    } else if (x == *left(y)) { /* zig-zag */
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
  for (x = u, y = *sparent(x); y; x = y, y = *sparent(y)) {
    if (x != *left(y) && x != *right(y)) {
      /* If x is a middle child, we continue splaying from the its parent. */
      continue;
    }
    splay_step(x, y);
  }

  /* Second pass */
  for (x = u; x; x = *sparent(x)) {
    *left(*sparent(x)) = x;
  }

  /* Third pass */
  for (x = u, y = *sparent(x); y; x = y, y = *sparent(y)) {
    splay_step(x, y);
  }
}

/* FIXME: REVIEW */
void splice(node *x) {
#ifdef DEBUG
  if (!x) fail("splice: null argument");
#endif
  undefined("splice", x);
}

/* FIXME: We probably should splay here. */
/* FIXME: REVIEW */
void expose(node *x) {
#ifdef DEBUG
  if (!x) fail("expose: null argument");
#endif
  undefined("expose",x);
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
  *sparent(x) = y; /* FIXME: we're not changing the parent correctly */
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
    *sparent(*right(x)) = NULL; /* FIXME: we're not changing the parent correctly */
    *right(x) = NULL;
  }
}

/* evert(vertex v): Modify the tree containing vertex v by making v the root.
                    (This operation can be regarded as reversing the direction
                    of everty edge on the path from v to the original root.) */
/* FIXME: REVIEW */
void evert(node *x) {
#ifdef DEBUG
  if (!x) fail("evert: null argument");
#endif
  undefined("evert", x);
  /* expose(x); */
  /* reverse(path_of(x)); */
  /* *pparent(x) = NULL; */
}

/*----------------------------------------------------------------------------*/
/* Project operations                                                         */
/*----------------------------------------------------------------------------*/

node **nodes;

/* Adds an edge linking the node u to the node v. If such an edge already exists
   or this insertion would create a cycle then the operation has no effect. */
/* FIXME: REVIEW */
void Link(int u, int v) {printf("Link(%d,%d)\n", u, v);}

/* Removes the edge linking the node u to the node v, if such an edge exists. If
the edge does not exist this operation has no effect. */
/* FIXME: REVIEW */
void Cut(int u, int v) {printf("Cut(%d,%d)\n",u,v);}

/* Returns true if there is a connection from u to v. If such a connection does
   not exist it returns false. A connection may consist of a single edge, or a
   sequence of edges, provided that it links u to v. */
/* FIXME: REVIEW */
void ConnectedQ(int u, int v) {printf("ConnectedQ(%d,%d)\n",u,v);}

/*----------------------------------------------------------------------------*/
/* Main                                                                       */
/*----------------------------------------------------------------------------*/

int main_link_cut() {
  int n;
  scanf(" %d", &n);
  nodes = new_forest(n);

  int u, v;
  while (true) {
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
