#' GeneralTree is a R6 implementation of a tree that can have multiple
#' childeren per parent.
#' @docType class
#' @importFrom R6 R6Class
#' @export GeneralTree
#' @return Object of \code{\link{R6Class}} with methods for creating a general
#' tree.
#' @format \code{\link{R6Class}} object.
#' @examples
#' root <- GeneralTree$new(0, 'root')
#' child1 <- root$addNode(0, 1, 'child.0.1')
#' child2 <- root$addNode(0, 2, 'child.0.2')
#' child3 <- root$addNode(0, 3, 'child.0.3')
#' child4 <- root$addNode(3, 4, 'child.3.4')
#' root$search(4)
#' @section Methods:
#' \describe{
#'  \item{\code{addNode(parent_id, id, data)}}{Add a new node to the tree. The
#'  new node will be a child of parent_id and have an id and data.}
#' }
GeneralTree <- R6Class('GeneralTree',
  lock_objects = FALSE,
  private = list(
    .data = NULL,
    .left_child = NULL,
    .siblings = NULL,
    .root = NULL,
    .id = NULL,
    .tree_depth = 1,
    .parent = NULL
  ),
  public = list(
   initialize = function(id, data) {
     private$.id = id
     private$.data = data

     invisible(self)
   },
   addNode = function(parent_id, id, data) {
     new_node = NULL

     # Find the parent node.
     parent_node = self$searchNode(parent_id)

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
           parent_node$left_child$addSibling(new_node)
         } else {
           parent_node$set_left_child(new_node)
           new_node$set_root(parent_node$root)
         }
       } else {
         stop("Could not find matching parent node with parent id ", parent_id)
       }
     }

     if (!is.null(new_node)) new_node$set_parent(parent_node)

     invisible(new_node)
   },
   addSibling = function(node) {
     if (!self$have_parent) stop("Cannot add sibling to root")

     private$.siblings = c(private$.siblings, list(node))
     node$set_root(self$root)

     invisible(node)
   },
   search = function(id) {
     self$searchNode(id)$data
   },
   searchNode = function(id) {
     # Determine whether search was called at the root node.
     if (is.null(private$.root))
       result = self$searchNode_starting_at_node(id)
     else
       result = private$.root$searchNode_starting_at_node(id)

     invisible(result)
   },
   searchNode_starting_at_node = function(id) {
     result = NULL
     # Verify whether the current node matches the id.
     if (identical(id, private$.id)) {
       result = self
     }

     if (!is.null(private$.siblings) && is.null(result)) {
       for (s in private$.siblings) {
         result = s$searchNode_starting_at_node(id)
         if (!is.null(result)) break
       }
     }

     if (is.null(result)) {
       # Search the left child if it is present.
       if (!is.null(private$.left_child)) {
         result = private$.left_child$searchNode_starting_at_node(id)
       } else {
         result = NULL
       }
     }

     invisible(result)
   },
   set_root = function(node) {
     private$.root = node
   },
   set_left_child = function(node) {
     if (self$have_child) warning("Already have left child!")

     private$.left_child = node
   },
   set_data = function(data) {
     private$.data = data
   },
   set_parent = function(node) {
     private$.parent = node
   },
   setSiblings = function(siblings) {
     private$.siblings = siblings
   },
   getSiblingNodes = function() {
     sibling_nodes = NULL

     if (self$have_siblings) {
       sibling_nodes = self$parent$left_child$siblings
       identical_to_self <- function(x) identical(x, self)
       sibling_nodes = Filter(Negate(identical_to_self), sibling_nodes)
     }

     invisible(sibling_nodes)
   },
   getSiblingData = function() {
     sibling_data = NULL
     if (self$have_siblings) {
       sibling_data = lapply(self$getSiblingNodes(), function(x) {
                               if (!identical(x, self)) x$data
         })
     }

     return(sibling_data)
   },
   getSiblingId = function() {
     sibling_ids = NULL
     if (self$have_siblings) {
       sibling_ids = lapply(self$getSiblingNodes(), function(x) {
                               if (!identical(x, self)) x$id
         })
     }

     return(sibling_ids)
   },
   getChildNodes = function() {
     child_nodes = NULL
     if (self$have_child) {
       child_nodes = c(list(self$left_child), self$left_child$siblings)
     }
   },
   getChildData = function() {
     child_data = NULL
     if (self$have_child) {
       child_data = lapply(self$getChildNodes(), function(x) x$data)
     }

     return(child_data)
   },
   getChildId = function() {
     child_data = NULL
     if (self$have_child) {
       child_data = lapply(self$getChildNodes(), function(x) x$id)
     }

     return(child_data)
   },
   deleteId = function(id) {
     node = self$searchNode(id)
     node$delete()
   },
   delete = function() {
     if (self$have_child) {
       self$left_child$delete()
     }

     # If we have siblings we need to make sure that only we get deleted and
     # nothing else. In case we have siblings there are two possibilities,
     # 1. we are the most left child, and,
     # 2. we are not the most left child.
     if (self$have_siblings && self$have_parent) {
       # Handle the first case described above.
       if (identical(self$parent$left_child$id, self$id)) {
         # Set the left child of the parent to the first sibling.
         suppressWarnings({
           self$parent$set_left_child(self$siblings[[1]])
         })
         remaining_siblings = self$siblings
         # Remove the first sibling.
         remaining_siblings[[1]] = NULL
         # Set the remaining siblings.
         self$parent$left_child$setSiblings(remaining_siblings)
       } else {
         siblings = self$parent$left_child$siblings
         own_position = sapply(siblings, function(x) identical(x, self))
         siblings = siblings[!own_position]
         self$parent$left_child$setSiblings(siblings)
       }
     } else if (self$have_parent) {
       suppressWarnings({
         self$parent$set_left_child(NULL)
       })
     } else{
       stop("Did not know how to remove myself")
     }
   }
  ),
  active = list(
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
      if (is.null(self$parent))
        return(FALSE)
      else
        self$parent$left_child$have_private_siblings
    },
    have_private_siblings = function() {
      !is.null(private$.siblings)
    },
    have_parent = function() {
      !is.null(private$.parent)
    },
    data = function() {
      return(private$.data)
    },
    is_root = function() {
      is.null(private$.root)
    },
    parent = function() {
      return(private$.parent)
    },
    tree_depth = function() {
      depth = 1
      if (!self$is_root) {
        depth = self$root$tree_depth
      } else {
        depth = self$branch_depth
      }

      return(depth)
    },
    branch_depth = function() {
      depth = 1

      if (self$have_child) {
        depth = max(depth, self$left_child$branch_depth + 1)
      }

      if (self$have_private_siblings) {
        sibling_depth = max(depth, sapply(self$siblings, function(x)
                                          x$branch_depth))
      }

      return(depth)
    }
  )
)

