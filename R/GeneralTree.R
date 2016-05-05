#'
#' @export GeneralTree
#' @importFrom R6 R6Class
GeneralTree <- R6Class('GeneralTree',
  lock_objects = FALSE,
  private = list(
    .data = NULL,
    .left_child = NULL,
    .siblings = NULL,
    .root = NULL,
    .id = NULL,
    .tree_depth = 0
  ),
  public = list(
   initialize = function(id, data) {
     private$.id = id
     private$.data = data

     invisible(self)
   },
   add_node = function(parent_id, id, data) {
     # i.   tree contains only root node
     if (is.null(private$.root) && is.null(private$.siblings) &&
         is.null(private$.left_child)) {
       if (parent_id != private$.id) {
         stop("parent_id could not be found in the tree")
       } else {
         # Add the child and set up all the references in the child correctly.
         private$.left_child = GeneralTree$new(id, data)
         private$.left_child$set_root(self)
         added_node = private$.left_child
       }
     } else {
       # Find the parent node.
       parent_node = self$search_id(parent_id)

       #TODO: implement
     }
     invisible(added_node)
   },
   search_id = function(id) {
     # Determine whether search was called at the root node.
     if (is.null(private$.root))
       result = self$search_id_starting_at_node(id)
     else
       result = private$.root$search_id_starting_at_node(id)

     invisible(result)
   },
   search_id_starting_at_node = function(id) {
     # Verify whether the current node matches the id.
     if (identical(id, self$id)) {
       result = self
     } else if (!is.null(private$.siblings)) {
       # Find whether any id in the siblings matches the one we are looking
       # for.
       find_sibling <- sapply(private$.siblings, identical, id)

       if (any(find_sibling)) {
         result = private$.siblings[[find_sibling]]
       } else {
         result = NULL
       }
     } else {
       # Search the left child if it is present.
       if (!is.null(private$.left_child)) {
         result = private$.left_child$search_id_starting_at_node(id)
       } else {
         result = NULL
       }
     }

     invisible(result)
   },
   set_root = function(node) {
     private$.root = node
   }
  ),
  active = list(
    depth = function() {
    },
    root = function() {
      invisible(private$.root)
    },
    left_child = function() {
      invisible(private$.left_child)
    },
    siblings= function() {
      invisible(private$.siblings)
    },
    id = function() {
      invisible(private$.id)
    }
  )
)

