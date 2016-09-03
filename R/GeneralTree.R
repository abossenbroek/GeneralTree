#' A tree that can have multiple childeren per parent.
#'
#' This class allows to create a tree with multiple childs per node. The data
#' as well as the id are left totally to the choice of the user and can even be
#' different.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export GeneralTree
#' @return Object of \code{\link{R6Class}} with methods for creating a general
#' tree.
#' @format \code{\link{R6Class}} object.
#' @section Methods:
#' \describe{
#'  \item{\code{addNode(parent_id, id, data)}}{Add a new node to the tree. The
#'  new node will be a child of parent_id and have an id and data.}
#'  \item{\code{search(id)}}{Search an node in the tree that has an id equal to
#'  \code{id}. This method returns the data associated with the node.}
#'  \item{\code{searchNode(id)}}{Search an node in the tree that has an id
#'  equal to \code{id}. This method returns the node.}
#'  \item{\code{searchBranch(id)}}{Search for a node in a particular branch of
#'  the tree. The function returns a node.}
#'  \item{\code{getSiblingNodes()}}{Get all the siblings of this node in a list.
#'  The results will not include the node itself.}
#'  \item{\code{getSiblingId()}}{Get all the sibling ids in a list. The results
#'   will not include the node itself.}
#'  \item{\code{getSiblingData()}}{Get all the sibling data in a list. The results
#'   will not include the node itself.}
#'  \item{\code{getChildNodes(recursive = FALSE)}}{Get the child nodes from the
#'   current branch. On default the function will only return one level deep.
#'   If \code{recursive} is set to \code{TRUE}, also childs in nested branches
#'   will be returned. The childs will all be returned in a list.}
#'  \item{\code{getChildId(recursive = FALSE)}}{Get the ids from all the
#'   child nodes. If \code{recursive} is set to \code{TRUE}, also ids from childs in
#'   nested branches will be returned. The ids will all be returned in a
#'   list.}
#'  \item{\code{getChildData(recursive = FALSE)}}{Get the data from all the
#'   child nodes. If \code{recursive} is set to \code{TRUE}, also data from childs in
#'   nested branches will be returned. The data will all be returned in a
#'   list.}
#'  \item{\code{deleteId(id)}}{Delete a node with id equal to \code{id}. All
#'   child nodes will also be deleted.}
#'  \item{\code{delete()}}{Delete the current node and all childs. Should not
#'   be called directly.}
#'  \item{\code{iterator()}}{Get an iterator to iterate through the tree in a
#'   depth first search.}
#'  \item{\code{nextElem()}}{Get the next element in a depth first search.
#'   Before using this function always create an iterator.}
#'  \item{\code{toString(what = c("id", "data"), string_prepend = "")}}{Creates
#'    a string representation of the node. Note that id and data should work
#'    with paste to work correctly. All branches will also be returned to the
#'    string.}
#'  }
#' @section Active methods:
#' \describe{
#'  \item{\code{root}}{Returns the root of a node.}
#'  \item{\code{left_child}}{Returns the left child of a node.}
#'  \item{\code{siblings}}{Returns the left sibling of a node.}
#'  \item{\code{id}}{Returns the id of a node.}
#'  \item{\code{have_child}}{Returns \code{TRUE} if the node has childs and
#'   \code{FALSE} otherwise.}
#'  \item{\code{have_siblings}}{Returns \code{TRUE} if the node has siblings and
#'   \code{FALSE} otherwise.}
#'  \item{\code{is_last_sibling}}{Returns \code{TRUE} if the node is the last siblings and
#'   \code{FALSE} otherwise.}
#'  \item{\code{have_private_siblings}}{Returns \code{TRUE} if the node has a
#'    private field siblings set and \code{FALSE} otherwise.}
#'  \item{\code{have_parent}}{Returns \code{TRUE} if the node has a
#'    parent field set and \code{FALSE} otherwise.}
#'  \item{\code{data}}{Returns the data of the node.}
#'  \item{\code{id}}{Returns the id of the node.}
#'  \item{\code{is_root}}{Returns \code{TRUE} if the node is the
#'    root and \code{FALSE} otherwise.}
#'  \item{\code{parent}}{Return the parent of the node.}
#'  \item{\code{tree_depth}}{Returns the depth of the tree.}
#'  \item{\code{branch_depth}}{Returns the depth of the branch.}
#'  \item{\code{isSingletonTree}}{Returns \code{TRUE} if the tree contains only
#'    a single element and \code{FALSE} otherwise.}
#' }
#' @examples
#' # Create a tree
#' tree <- GeneralTree$new(0, "root")
#' tree$addNode(0, 1, "child.0.1")
#' tree$addNode(0, 2, "child.0.2")
#' tree$addNode(0, 3, "child.0.3")
#' tree$addNode(3, 4, "child.3.4")
#' tree$search(4)
#'
#' #
#' # Print the tree
#' tree
#'
#' #
#' # Example how to iterate through the tree in a depth first iteration.
#' i <- tree$iterator()
#' while (!is.null(i)) {
#'    i$setData(paste("id:", i$id, " : data", i$data))
#'    i <- tryCatch(i$nextElem(), error = function(e) NULL)
#' }
#'
#' # An example with the foreach package.
#' require(iterators)
#' require(foreach)
#' itx <- iter(tree, by = "id")
#' numbers_in_tree <- foreach(i = itx, .combine = c) %do% c(i)
#'
#' itx <- iter(tree, by = "data")
#' data_in_tree <- foreach(i = itx, .combine = c) %do% c(i)
#'
GeneralTree <- R6Class("GeneralTree",
  lock_objects = FALSE,
  private = list(
    .data = NULL,
    .left_child = NULL,
    .siblings = NULL,
    .root = NULL,
    .id = NULL,
    .parent = NULL,
    .is_discovered = FALSE,
    .is_root_discovered = FALSE
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

     if (is.null(parent_node)) stop("Could not find the parent node with id ", parent_id)

     new_node = GeneralTree$new(id, data)

     if (self$isSingletonTree ) {
         # Add the child and set up all the references in the child correctly.
         private$.left_child = new_node
         private$.left_child$setRoot(parent_node)
         private$.left_child$setParent(parent_node)
     } else {
         new_node = parent_node$addChild(new_node)
     }

     invisible(new_node)
   },
   addChild = function(node) {
     node$setParent(self)
     node$setRoot(self$root)

     if (self$have_child) {
       self$left_child$addSibling(node)
     } else {
       self$setLeftChild(node)
     }
     invisible(node)
   },
   addSibling = function(node) {
     if (self$is_root) stop("Cannot add sibling to root")

     private$.siblings = c(private$.siblings, list(node))
     node$setRoot(self$root)

     invisible(node)
   },
   search = function(id) {
     self$searchNode(id)$data
   },
   searchNode = function(id) {
     # Determine whether search was called at the root node.
     if (is.null(private$.root))
       result = self$searchBranch(id)
     else
       result = private$.root$searchBranch(id)

     invisible(result)
   },
   searchBranch = function(id) {
     result = NULL
     # Verify whether the current node matches the id.
     if (identical(id, private$.id)) {
       result = self
     }

     if (!is.null(private$.siblings) && is.null(result)) {
       for (s in private$.siblings) {
         result = s$searchBranch(id)
         if (!is.null(result)) break
       }
     }

     if (is.null(result)) {
       # Search the left child if it is present.
       if (!is.null(private$.left_child)) {
         result = private$.left_child$searchBranch(id)
       } else {
         result = NULL
       }
     }

     invisible(result)
   },
   setRoot = function(node) {
     private$.root = node
   },
   setLeftChild = function(node) {
     if (self$have_child) warning("Already have left child!")

     private$.left_child = node
   },
   setData = function(data) {
     private$.data = data
   },
   setParent = function(node) {
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
       sibling_data = lapply(self$getSiblingNodes(), function(x) x$data)
     }

     return(sibling_data)
   },
   getSiblingId = function() {
     sibling_ids = NULL
     if (self$have_siblings) {
       sibling_ids = lapply(self$getSiblingNodes(), function(x) x$id)
     }

     return(sibling_ids)
   },
   getChildNodes = function(recursive = FALSE) {
     child_nodes = NULL
     if (self$have_child) {
       child_nodes = c(list(self$left_child), self$left_child$siblings)
       if (recursive) {
         child_nodes = c(child_nodes, sapply(child_nodes, function(x) x$getChildNodes(recursive)))
         child_nodes = unlist(child_nodes)
       }
     }
     return(child_nodes)
   },
   getChildData = function(recursive = FALSE) {
     child_data = NULL
     if (self$have_child) {
       child_data = lapply(self$getChildNodes(recursive), function(x) x$data)
     }

     return(child_data)
   },
   getChildId = function(recursive = FALSE) {
     child_data = NULL
     if (self$have_child) {
       child_data = lapply(self$getChildNodes(recursive), function(x) x$id)
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
           self$parent$setLeftChild(self$siblings[[1]])
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
         self$parent$setLeftChild(NULL)
       })
     }
   },
   nextElem = function(set_discover = TRUE, include_root = TRUE) {
     next_element = NULL
     candidates = NULL

     if (self$is_root) {
       if (!self$isRootDiscovered && include_root) {
         next_element = self
         self$setRootDiscovered(set_discover)
       } else {
         candidates <- self$getChildNodes(recursive = TRUE)
       }
     } else {
       candidates <- c(list(self$left_child), self$getSiblingNodes())
     }

     if (is.null(next_element) && !is.null(unlist(candidates))) {
       # Remove all NULL values.
       candidates <- Filter(Negate(is.null), candidates)
       # Remove all nodes that were already discovered.
       not_discovered <- Filter(Negate(function(x) x$isDiscovered), candidates)
       if (length(not_discovered) > 0)
         next_element = not_discovered[[1]]
     }

     if (is.null(next_element) && !self$is_root && self$have_parent) {
       next_element = self$parent$nextElem()
     }

     if (!is.null(next_element))
        next_element$setDiscovered(set_discover)

     # If this was the last node, reset the root discovery.
     if (is.null(next_element) && self$is_root)
       self$setRootDiscovered(set_discover)


     if (is.null(next_element)) {
       next_element = self
       stop("StopIteration")
     }

     invisible(next_element)
   },
   iterator = function() {
     if (self$is_root) {
       self$resetDiscoveredOnBranch()
       self$setRootDiscovered(FALSE)
       return(self$nextElem())
     } else {
       return(self$root$iterator())
     }
   },
   resetDiscovered = function() {
     if (!self$is_root) {
       private$.root$resetDiscoveredOnBranch()
     } else {
       self$resetDiscoveredOnBranch()
       self$setDiscovered(FALSE)
     }
   },
   resetDiscoveredOnBranch = function() {
     reset_status = sapply(self$getChildNodes(recursive = TRUE), function(x)
                            x$setDiscovered(FALSE))
     invisible(reset_status)
   },
   setDiscovered = function(is_discovered) {
     private$.is_discovered = is_discovered
   },
   setRootDiscovered = function(is_root_discovered) {
     private$.is_root_discovered = is_root_discovered
   },
   nodeInfoToString = function(what = c("id", "data")) {
     what = match.arg(what, several.ok = TRUE)

     get_id = any("id" %in% what)
     get_data = any("data" %in% what)

     node_id = ""
     if (get_id)
       node_id = as.character(self$id)

     node_data = ""
     if (get_data)
       node_data = as.character(self$data)

     sep = ""
     if(get_id && get_data) {
       sep = " : "
     }
     node_string = paste(node_id, node_data, sep = sep)

     return(node_string)
   },
   toString = function(what = c("id", "data"), string_prepend = "") {
     what = match.arg(what, several.ok = TRUE)

     initiateEmptyString = function(length = 1) {
       paste0(rep(" ", length), collapse = "")
     }

     if (self$is_root) {
       string = self$nodeInfoToString(what)
       if (self$have_child) {
         space = nchar(string)
         child_nodes = self$getChildNodes(recursive = FALSE)
         string_prepend = initiateEmptyString(length = space)
         result = paste0(sapply(child_nodes, function(x) x$toString(what, string_prepend)), collapse = "\n")
         string = paste0(string, result)
       }
     } else {
       tree_sep = string_prepend

       if (identical(self$parent$left_child, self)) {
         node_sep = paste0(" --> ")
       } else if (self$is_last_sibling) {
         node_sep = paste0(tree_sep, " \\-> ")
       } else {
         node_sep = paste0(tree_sep, " |-> ")
       }

       if (self$have_child) {
         max_space = max(sapply(self$parent$getChildNodes(), function(x)
                                nchar(x$nodeInfoToString(what))))

         branch_symbol = "|"
         if (self$is_last_sibling || !self$have_siblings)
           branch_symbol = " "

         tree_sep = paste0(tree_sep, " ", branch_symbol, "   ", initiateEmptyString(length = max_space))

         child_nodes = self$getChildNodes(recursive = FALSE)
         result = paste0(sapply(child_nodes, function(x) x$toString(what, tree_sep)), collapse = "\n")
         string = paste0(node_sep, self$nodeInfoToString(what), result, collapse = "\n")
       } else {
         string = paste0(node_sep, self$nodeInfoToString(what))
       }
     }

     return(string)
   }
  ),
  active = list(
    root = function() {
      if (is.null(private$.root)) {
        invisible(self)
      } else {
        invisible(private$.root)
      }
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
    is_last_sibling = function() {
      if (self$have_siblings) {
        siblings = self$parent$left_child$getSiblingNodes()
        return(identical(siblings[[length(siblings)]], self))
      }
      return(FALSE)
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
      if (!self$is_root) {
        depth = self$root$tree_depth
      } else {
        depth = self$branch_depth
      }

      return(depth)
    },
    isDiscovered = function() {
      return(private$.is_discovered)
    },
    isRootDiscovered = function() {
      return(private$.is_root_discovered)
    },
    branch_depth = function() {
      depth = 1

      if (self$have_child) {
        depth = max(depth, self$left_child$branch_depth + 1)
      }

      if (self$have_private_siblings) {
        depth = max(depth, sapply(self$siblings, function(x)
                                          x$branch_depth))
      }

      return(depth)
    },
    isSingletonTree = function() {
      return(is.null(private$.root) && is.null(private$.siblings) &&
         is.null(private$.left_child))
    }
  )
)
