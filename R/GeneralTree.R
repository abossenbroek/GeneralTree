#'
#' @export GeneralTree
#' @importFrom R6 R6Class
GeneralTree <- R6Class('GeneralTree',
  lock_objects = FALSE,
  private = list(
    data = NULL,
    left_child = NULL,
    siblings = NULL,
    root = NULL,
    id = NULL,
    tree_depth = 0
  ),
  public = list(
   initialize = function(id, data) {
     private$id = id
     private$data = data

     invisible(self)
   },
   add_node = function(parent_id, id, data) {
     # Three cases,
     # i.   tree is completely empty,
     # ii.  tree contains parent, but:
     #        ii.a.    parent has no child
     #        ii.b.    parent has child
     # iii. tree does not contain parent

     if (is.null(private$root) && is.null(private$siblings) &&
         is.null(private$left_child)) {
       if (parent_id != private$id) {
         stop("parent_id could not be found in the tree")
       } else {
         # Add the child and set up all the references in the child correctly.
         private$left_child = GeneralTree$new(id, data)
         private$left_child$set_root(self)
         added_node = private$left_child
       }
     }
     invisible(added_node)
   },
   set_root = function(node) {
     private$root = node
   },
   get_root = function(node) {
     invisible(private$root)
   }
  ),
  active = list(
    depth = function() {
    }
  )
)

