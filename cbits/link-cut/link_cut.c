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

/*
  Operations that do not change the forest

  - parent(vertex v): return the parent of v. if v has no parent (it is a tree root), return null
  - root(vertex v): return the root of the tree containing v

  Operations that change the forest

  - link(vetex v, w): combine the trees containing v and w by adding the edge
    (v,w) of cost x, making w the parent of v. THIS OPERATION ASSUMES THAT V IS
    NOT A TREE ROOT.
  - cut(vertex v): divide the tree containing vertex v into two trees by
    deleting the edge(v,parent(v)); THIS OPERATION ASSUMES THAT V IS NOT A TREE
    ROOT.
  - evert(vertex v): Modify the tree containing vertex v by making v the root.
    (This operation can be regarded as reversing the direction of everty edge on
    the path from v to the original root.)

  Nodes storage

  - dparent(v): ("dashed parent") the parent of v (via an outgoing dashed edge)
    IF IT IS THE TAIL OF THE PATH.

*/

/*----------------------------------------------------------------------------*/
/* Link/Cut tree internal representation                                       */
/*----------------------------------------------------------------------------*/

/*
   Primitive operations

   - path(vertex v): return the path containing v
   - head(path p): return the bottommost vertex of the path
   - tail(path p): return the topmost vertex of the path
   - before(vertex v): return the vertex before v on path(v), or null if v is
                       the head of the path
   - after(vertex v): return vertex after v on path(v), or null if v is the tail
                      of the path
   - reverse(path p): reverse the direction of p, making the head the tail and
                      vice versa
   - concatenate(path p, path q): combine p and q by adding the edge (tail(p),
                                  head(q)) and return the combined path
   - split(vertex v): divide path(v) into up to 3 parts: the vertices from
                      head(path(v)) to before(v) (p), v, and those from after(v) to
                      tail(path(v)) (q). (v = head(path(v)) => p = null &&
                                          v = tail(path(v)) => q = null);

   Composite operations

   - splice(path p): Extend the solid path p by converting the dashed edge
                     leaving tail(p) to solid and converting the original solid
                     edge entering parent(tail(p)) (if any) to dashed. Return
                     the extended path. THIS OPERATION ASSUMES THAT tail(p) IS
                     NOT A TREE ROOT (THAT IS, THERE IS A DASHED EDGE LEAVING
                     tail(p));
   - expose(vertex v): Create a single solid path with head v and tail root(v)
                     by converting dashed edges to solid along the tree path
                     from v to root(v) and converting solid edges incident to
                     this path to dashed. Return the resulting solid path.


 */

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
