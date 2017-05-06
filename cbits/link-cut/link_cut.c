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

bool not(bool b) { return !b; }

/*----------------------------------------------------------------------------*/
/* Link/Cut tree internal representation                                       */
/*----------------------------------------------------------------------------*/

struct node {
  struct node *left;
  struct node *right;
  struct node *hook;
  bool reversed;
};

typedef struct node node;
typedef node path;

node *left(node *x) { return undefined("left", x); }
node *right(node *x) { return undefined("right", x); }

/* dparent(v): ("dashed parent") the parent of v (via an outgoing dashed edge)
               IF IT IS THE TAIL OF THE PATH. */
node **dparent(node *x) { return undefined("dparent", x); }


/*----------------------------------------------------------------------------*/
/* Primitive operations                                                       */
/*----------------------------------------------------------------------------*/

/* path(vertex v): return the path containing v */
path *path_of(node *x) { return undefined("path_of", x); }

/* head(path p): return the bottommost vertex of the path */
node *head(path *p) { return undefined("head", p); }

/* tail(path p): return the topmost vertex of the path */
node *tail(path *p) { return undefined("tail", p); }

/* before(vertex v): return the vertex before v on path(v), or null if v is
                     the head of the path */
node *before(node *x) { return undefined("before", x); }

/* after(vertex v): return vertex after v on path(v), or null if v is the tail
                    of the path */
node *after(node *x) { return undefined("after", x); }

/* reverse(path p): reverse the direction of p, making the head the tail and
                    vice versa */
void reverse(path *p) { undefined("reverse", p); }

/* concatenate(path p, path q): combine p and q by adding the edge (tail(p),
                                head(q)) and return the combined path */
void concatenate(path *p, path *q) { undefined("concatenate", p, q); }

/* split(vertex v): divide path(v) into up to 3 parts: the vertices from
                    head(path(v)) to before(v) (p), v, and those from after(v) to
                    tail(path(v)) (q). (v = head(path(v)) => p = null &&
                    v = tail(path(v)) => q = null); */
void split(path **p, node *x, path **q) { undefined("split", p, x, q); }


/* FIXME: "To make this program robust, an error should be added to ensure that
   on entry dparent(tail(p)) /= null." */
void splice(path *p) {
  path *q, *r;
  node *x = *dparent(tail(p));

  split(&q, x, &r);
  if (q) *dparent(tail(q)) = x;
  concatenate(p, path_of(x));
}

void expose(node *x) {
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

void Link(node u, node v) { undefined("Link",u,v); }
void Cut(node u, node v) { undefined("Cut",u,v); }
void ConnectedQ(node u, node v) { undefined("ConnectedQ",u,v); }

/*----------------------------------------------------------------------------*/
/* Main                                                                       */
/*----------------------------------------------------------------------------*/

int main_link_cut() {
  return 0;
}

int main() { undefined("main"); return main_link_cut(); }
