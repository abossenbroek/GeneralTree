#' GeneralTree is a R6 implementation of a tree that can have multiple child
#' per parent.
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
     new_node = NULL

     # Find the parent node.
     parent_node = self$search_id(parent_id)

     if (is.null(parent_node)) stop("Could not find the parent node with id ", id)

     if (is.null(private$.root) && is.null(private$.siblings) &&
         is.null(private$.left_child)) {
       if (parent_id != private$.id) {
         stop("parent_id could not be found in the tree")
       } else {
         # Add the child and set up all the references in the child correctly.
         private$.left_child = GeneralTree$new(id, data)
         private$.left_child$set_root(parent_node)
         new_node = private$.left_child
       }
     } else {
       if (!is.null(parent_node)) {
         new_node = GeneralTree$new(id, data)

         # Verify whether any childs are already present.
         if (parent_node$have_child) {
           parent_node$left_child$add_sibling(new_node)
         } else {
           parent_node$set_left_child(new_node)
           new_node$set_root(parent_node$root)
         }
       } else {
         stop("Could not find matching parent node with parent id ", parent_id)
       }
     }
     #browser()
     invisible(new_node)
   },
   search_id = function(id) {
     # Determine whether search was called at the root node.
     if (is.null(private$.root))
       result = self$search_id_starting_at_node(id)
     else
       result = private$.root$search_id_starting_at_node(id)

     invisible(result)
   },
   add_sibling = function(node) {
     private$.siblings = c(private$.siblings, list(node))
     node$set_root(self$root)

     invisible(node)
   },
   search_id_starting_at_node = function(id) {
     result = NULL
     # Verify whether the current node matches the id.
     if (identical(id, private$.id)) {
       result = self
     }

     if (!is.null(private$.siblings) && is.null(result)) {
       for (s in private$.siblings) {
         result = s$search_id_starting_at_node(id)
         if (!is.null(result)) break
       }
     }

     if (is.null(result)) {
       # Search the left child if it is present.
       if (!is.null(private$.left_child)) {
         result = private$.left_child$search_id_starting_at_node(id)
       } else {
         result = NULL
       }
     }

     invisible(result)
   },
   get_sibling = function(id) {
     result = NULL

     if (self$have_siblings) {
       # Find whether any id in the siblings matches the one we are looking
       # for.
       find_sibling <- sapply(private$.siblings, identical, id)

       if (any(find_sibling)) {
         result = private$.siblings[[find_sibling]]
       }
     }

     invisible(result)
   },
   set_root = function(node) {
     private$.root = node
   },
   set_left_child = function(node) {
     if (self$have_child) stop("Already have left child!")

     private$.left_child = node
   },
   set_data = function(data) {
     private$.data = data
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
    siblings = function() {
      invisible(private$.siblings)
    },
    id = function() {
      invisible(private$.id)
    },
    have_child = function() {
      !is.null(private$.left_child)
    },
    have_siblings = function() {
      !is.null(private$.siblings)
    },
    data = function() {
      return(private$.data)
    }
  )
)

