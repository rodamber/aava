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

/*----------------------------------------------------------------------------*/
/* Link/Cut tree internal representation                                       */
/*----------------------------------------------------------------------------*/

struct node {
  struct node *left;
  struct node *right;
  struct node *sparent;
  struct node *dparent;
  bool reversed;
};

typedef struct node node;
typedef node path;

node *new_node() {return calloc(1, sizeof(node));}
node **new_forest(int n) {
  node **f = malloc(n * sizeof(node*));
  int i = 0;
  for (; i < n; i++) f[i] = new_node();
  return f;
}
void free_forest(node **f, int n) {int i = 0; for (; i < n; i++) free(f[i]);}

node **left(node *x) {
  if (x->reversed) return x->right ? &(x->right) : NULL;
  else             return x->left  ? &(x->left)  : NULL;
}

node **right(node *x) {
  if (x->reversed) return x->left  ? &(x->left)  : NULL;
  else             return x->right ? &(x->right) : NULL;
}

/* FIXME: Check if we're dereferencing possible null pointers */
node **sparent(node *x) {return x->sparent ? &(x->sparent) : NULL;}
node **dparent(node *x) {return x->dparent ? &(x->dparent) : NULL;}
/* dparent(v): ("dashed parent") the parent of v (via an outgoing dashed edge)
   IF IT IS THE TAIL OF THE PATH. */

node *solid_root(node *x) {
  node *y;
  for (y = *sparent(x);
       y && x != *left(y) && x != *right(y);
       x = y, y = *sparent(y));
  return x;
}

node *leftmost(node *x) {
  node **l = left(x);
  node *y = *l ? *l : NULL;
  while (y) {x = y; y = *left(y);}
  return x;
}

node *rightmost(node *x) {
  node **r = right(x);
  node *y = *r ? *r : NULL;
  while (y) {x = y; y = *right(y);}
  return x;
}

void rotr(node *x) {
  node *y = *sparent(x);
  node *z = *right(x);

  *left(y) = z;
  *sparent(z) = y;

  *right(x) = y;
  *sparent(x) = *sparent(y);

  *sparent(y) = x;
}

void rotl(node *x) {
  node *y = *sparent(x);
  node *z = *left(x);

  *right(y) = z;
  *sparent(z) = y;

  *left(x) = y;
  *sparent(x) = *sparent(y);

  *sparent(y) = x;
}

void splay_step(node *x, node *y) {
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

/* FIXME: Check this. */
void splay(node *u) {
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

/*----------------------------------------------------------------------------*/
/* Primitive operations                                                       */
/*----------------------------------------------------------------------------*/

/* path(vertex v): return the path containing v */
#define path_of(X) (X)

/* head(path p): return the bottommost vertex of the path */
node *head(path *p) {return leftmost(solid_root(p));}

/* tail(path p): return the topmost vertex of the path */
node *tail(path *p) {return rightmost(solid_root(p));}

/* before(vertex v): return the vertex before v on path(v), or null if v is
                     the head of the path */
node *before(node *x) {return solid_root(x) ? rightmost(*left(x)) : *sparent(x);}

/* after(vertex v): return vertex after v on path(v), or null if v is the tail
                    of the path */
node *after(node *x) {return solid_root(x) ? leftmost(*right(x)) : *sparent(x);}

/* reverse(path p): reverse the direction of p, making the head the tail and
                    vice versa */
void reverse(path *p) {p->reversed = !p->reversed;}

/* concatenate(path p, path q): combine p and q by adding the edge (tail(p),
                                head(q)) and return the combined path */
void concatenate(path *p, path *q) {*sparent(tail(p)) = head(q);}

/* split(vertex v): divide path(v) into up to 3 parts: the vertices from
                    head(path(v)) to before(v) (p), v, and those from after(v) to
                    tail(path(v)) (q). (v = head(path(v)) => p = null &&
                    v = tail(path(v)) => q = null); */
void split(path **p, node *x, path **q) {
  node *b = before(x), *a = after(x);
  *sparent(b) = *sparent(x) = NULL;
  *p = b; *q = a;
}


/* FIXME: "To make this program robust, an error should be added to ensure that
   on entry dparent(tail(p)) /= null." */
void splice(path *p) {
  path *q, *r;
  node *x = *dparent(tail(p));

  split(&q, x, &r);
  if (q) *dparent(tail(q)) = x;
  concatenate(p, path_of(x));
}

/* FIXME: We probably should splay here. */
void expose(node *x) {
  if (!sparent(x)) return;

  path *q, *r;
  split(&q, x, &r);

  if (q) *dparent(tail(q)) = x;
  if (r) concatenate(path_of(x), r);
  for (; dparent(tail(path_of(x))); splice(x));
}

/*----------------------------------------------------------------------------*/
/* Operations that do not change the forest                                   */
/*----------------------------------------------------------------------------*/

/* parent(vertex v): return the parent of v. if v has no parent (it is a tree
   root), return null */
node *parent(node *x) {
  if (x == tail(path_of(x)))
    return *dparent(x);
  return after(x);
}

/* root(vertex v): return the root of the tree containing v */
node *root(node *x) {
  expose(x);
  return tail(path_of(x));
}

/*----------------------------------------------------------------------------*/
/* Operations that change the forest                                          */
/*----------------------------------------------------------------------------*/

/* link(vertex v, w): combine the trees containing v and w by adding the edge
                      (v,w) of cost x, making w the parent of v. THIS OPERATION
                      ASSUMES THAT V IS NOT A TREE ROOT. */
void link(node *x, node *y) {
  expose(y);
  concatenate(path_of(x), path_of(y));
}

/* cut(vertex v): divide the tree containing vertex v into two trees by deleting
                  the edge(v,parent(v)); THIS OPERATION ASSUMES THAT V IS NOT A
                  TREE ROOT. */
void cut(node *x) {
  path *p, *q;
  expose(x);
  split(&p, x, &q);
  *dparent(x) = NULL;
}

/* evert(vertex v): Modify the tree containing vertex v by making v the root.
                    (This operation can be regarded as reversing the direction
                    of everty edge on the path from v to the original root.) */
void evert(node *x) {
  expose(x);
  reverse(path_of(x));
  *dparent(x) = NULL;
}


/*----------------------------------------------------------------------------*/
/* Project operations                                                         */
/*----------------------------------------------------------------------------*/

node **nodes;

/* Adds an edge linking the node u to the node v. If such an edge already exists
   or this insertion would create a cycle then the operation has no effect. */
void Link(int u, int v) {printf("Link(%d,%d)\n", u, v);}

/* Removes the edge linking the node u to the node v, if such an edge exists. If
the edge does not exist this operation has no effect. */
void Cut(int u, int v) {printf("Cut(%d,%d)\n",u,v);}

/* Returns true if there is a connection from u to v. If such a connection does
   not exist it returns false. A connection may consist of a single edge, or a
   sequence of edges, provided that it links u to v. */
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
