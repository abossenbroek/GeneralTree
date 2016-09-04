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
#'  \item{\code{searchData(id)}}{Search an node in the tree that has an id equal to
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
#'  \item{\code{nextElem()}}{Get the
#'       next element in a depth first search.  Before using this function
#'       always create an iterator.}
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
#'  \item{\code{treeDepth}}{Returns the depth of the tree.}
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
#' tree$searchData(4)
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
   initialize = function(id, data)
     initialize(self, private, id, data)
   ,
   addNode = function(parent_id, id, data)
     addNode(self, private, parent_id, id, data)
   ,
   addChild = function(id, data)
       addChild(self, private, id, data)
   ,
   addChildNode = function(node)
     addChildNode(self, private, node)
   ,
   addSibling = function(id, data)
     addSibling(self, private, id, data)
   ,
   addSiblingNode = function(node)
     addSiblingNode(self, private, node)
   ,
   searchData = function(id)
     searchData(self, id)
   ,
   searchNode = function(id)
     searchNode(self, id)
   ,
   searchBranch = function(id)
     searchBranch(self, id)
   ,
   setRoot = function(node)
     setRoot(self, private, node)
   ,
   setLeftChild = function(node)
     setLeftChild(self, private, node)
   ,
   setData = function(data)
     setData(self, private, data)
   ,
   setParent = function(node)
     setParent(self, private, node)
   ,
   setSiblings = function(siblings)
     setSiblings(self, private, siblings)
   ,
   getSiblingNodes = function()
     getSiblingNodes(self)
   ,
   getSiblingData = function()
     getSiblingData(self)
   ,
   getSiblingId = function()
     getSiblingId(self)
   ,
   getChildNodes = function(recursive = FALSE)
     getChildNodes(self, recursive)
   ,
   getChildData = function(recursive = FALSE)
     getChildData(self, recursive)
   ,
   getChildId = function(recursive = FALSE)
     getChildId(self, recursive)
   ,
   deleteId = function(id)
     deleteId(self, id)
   ,
   delete = function()
     delete(self, private)
   ,
   nextElem = function()
       self$nextElemWorker()
   ,
   nextElemWorker = function(set_discover = TRUE, include_root = TRUE)
     nextElemWorkerImpl(self, private, set_discover, include_root)
   ,
   iterator = function()
     iteratorImpl(self, private)
   ,
   resetDiscovered = function()
     resetDiscovered(self, private)
   ,
   resetDiscoveredOnBranch = function()
     resetDiscoveredOnBranch(self, private)
   ,
   setDiscovered = function(is_discovered)
    setDiscovered(self, private, is_discovered)
   ,
   setRootDiscovered = function(is_root_discovered)
    setRootDiscovered(self, private, is_root_discovered)
   ,
   nodeInfoToString = function(what = c("id", "data"))
     nodeInfoToString(self, what)
   ,
   toString = function(what = c("id", "data"), string_prepend = "")
     toString(self, what, string_prepend)
  ),
  active = list(
    root = function()
      root(self, private)
    ,
    left_child = function()
      left_child(self, private)
    ,
    siblings = function()
      siblings(self, private)
    ,
    id = function()
      id(self, private)
    ,
    have_child = function()
      have_child(self, private)
    ,
    have_siblings = function()
      have_siblings(self, private)
    ,
    is_last_sibling = function()
      is_last_sibling(self, private)
    ,
    have_private_siblings = function()
      have_private_siblings(self, private)
    ,
    have_parent = function()
      have_parent(self, private)
    ,
    data = function()
      data(self, private)
    ,
    is_root = function()
      is_root(self, private)
    ,
    parent = function()
      parent(self, private)
    ,
    treeDepth = function()
      treeDepth(self, private)
    ,
    isDiscovered = function()
      isDiscovered(self, private)
    ,
    isRootDiscovered = function()
      isRootDiscovered(self, private)
    ,
    branch_depth = function()
      branch_depth(self, private)
    ,
    isSingletonTree = function()
      isSingletonTree(self, private)
  )
)

#' Initialize a General Tree object.
#'
#' @param self    the GeneralTree
#' @param private the private members of the GeneralTree.
#' @param id      the id of the new node.
#' @param data    the data of the new node.
#'
#' @keywords internal
initialize <- function(self, private, id, data) {
  private$.id = id
  private$.data = data

  invisible(self)
}

#'
#' @keywords internal
addNode <- function (self, private, parent_id, id, data) {
  new_node <- NULL

  # Find the parent node.
  parent_node <- self$searchNode(parent_id)

  if (is.null(parent_node)) stop("Could not find the parent node with id ", parent_id)

  new_node <- GeneralTree$new(id, data)

  if (self$isSingletonTree) {
    # Add the child and set up all the references in the child correctly.
    private$.left_child = new_node
    private$.left_child$setRoot(parent_node)
    private$.left_child$setParent(parent_node)
  } else {
    new_node = parent_node$addChildNode(new_node)
  }

  invisible(new_node)
}

#'
#' @keywords internal
addChildNode <- function (self, private, node) {
  node$setParent(self)
  node$setRoot(self$root)

  if (self$have_child) {
    self$left_child$addSiblingNode(node)
  } else {
    self$setLeftChild(node)
  }
  invisible(node)
}

#' Add a child at a point in the tree.
#'
#' @param self    The point in the tree where the child should be added.
#' @param private The private part of the tree.
#' @param id      The id of the node that should be added.
#' @param data    The data of the node that should be added.
#' @return invisible the new node that was created.
#' @keywords internal
addChild <- function (self, private, id, data) {
  new_node <- GeneralTree$new(id, data)

  if (self$isSingletonTree) {
    # Add the child and set up all the references in the child correctly.
    private$.left_child = new_node
    private$.left_child$setRoot(self)
    private$.left_child$setParent(self)
  } else {
    new_node = self$addChildNode(new_node)
  }

  invisible(new_node)
}

#' Add a sibling to the current node.
#'
#' @param self    The point in the tree where the sibling should be added.
#' @param private The private part of the tree.
#' @param id      The id of the node that should be added.
#' @param data    The data of the node that should be added.
#' @return invisible the new node that was created.
#' @keywords internal
addSibling <- function (self, private, id, data) {
  if (self$is_root) stop("Cannot add sibling to root")

  new_node <- GeneralTree$new(id, data)
  new_node <- self$parent$addChildNode(new_node)

  invisible(new_node)
}

#' Add a node to the list of siblings of the current node.
#'
#' @param self    The point in the tree where the sibling should be added.
#' @param private The private part of the tree.
#' @keywords internal
addSiblingNode <- function (self, private, node) {
  if (self$is_root) stop("Cannot add sibling to root")

  private$.siblings = c(private$.siblings, list(node))
  node$setRoot(self$root)

  invisible(node)
}

#' Search for an id in starting at a point in the tree and return the data
#' matching the id.
#'
#' @param self the node where to start searching.
#' @param id the id to look for.
#' @return The data associated with an id.
#' @export
searchData <- function (self, id) {
  return(self$searchNode(id)$data)
}

#' Search for an id in starting at a point in the tree and return the node
#' matching the id.
#'
#' @param self the node where to start searching.
#' @param id the id to look for.
#' @return The data associated with an id.
#' @export
searchNode <- function (self, id) {
  # Determine whether search was called at the root node.
  if (self$is_root)
    result <- self$searchBranch(id)
  else
    result <- self$root$searchBranch(id)

  return(result)
}

#'
#' @keywords internal
searchBranch <- function (self, id) {
  result <- NULL
  # Verify whether the current node matches the id.
  if (identical(id, self$id)) {
    result <- self
  } else {

    if (self$have_private_siblings) {
      for (s in self$siblings) {
        result <- s$searchBranch(id)
        if (!is.null(result)) break
      }
    }

    if (is.null(result)) {
      # Search the left child if it is present.
      if (self$have_child) {
        result <- self$left_child$searchBranch(id)
      } else {
        result <- NULL
      }
    }
  }

  invisible(result)
}

#'
#' @keywords internal
getSiblingNodes <- function (self) {
  sibling_nodes <- NULL

  if (self$have_siblings) {
    sibling_nodes <- self$parent$left_child$siblings
    identical_to_self <- function(x) identical(x, self)
    sibling_nodes <- Filter(Negate(identical_to_self), sibling_nodes)
  }

  invisible(sibling_nodes)
}

#'
#' @keywords internal
getSiblingData <- function (self) {
  sibling_data <- NULL
  if (self$have_siblings) {
    sibling_data <- lapply(self$getSiblingNodes(), function(x) x$data)
  }

  return(sibling_data)
}

#'
#' @keywords internal
getSiblingId <- function (self) {
  sibling_ids <- NULL
  if (self$have_siblings) {
    sibling_ids = lapply(self$getSiblingNodes(), function(x) x$id)
  }

  return(sibling_ids)
}


#'
#' @keywords internal
setRoot <- function (self, private, node) {
  private$.root = node
}

#'
#' @keywords internal
setLeftChild <- function (self, private, node) {
  if (self$have_child) warning("Already have left child!")

  private$.left_child = node
}

#'
#' @keywords internal
setData <- function (self, private, data) {
  private$.data = data
}

#'
#' @keywords internal
setParent <- function (self, private, node) {
  private$.parent = node
}

#'
#' @keywords internal
setSiblings <- function (self, private, siblings) {
  private$.siblings = siblings
}

#' Get all the child nodes below the current node.
#'
#' @param self The node where to start
#' @param recursive Should the function be called on all child nodes too?
#' @return a list of child nodes.
#' @export
getChildNodes <- function (self, recursive = FALSE) {
  child_nodes <- NULL
  if (self$have_child) {
    child_nodes <- c(list(self$left_child), self$left_child$siblings)
    if (recursive) {
      child_nodes <- c(child_nodes, sapply(child_nodes, getChildNodes, recursive))
      child_nodes <- unlist(child_nodes)
    }
  }
  return(child_nodes)
}

#' Get the data of the child nodes below the current node.
#'
#' @param self The node where to start.
#' @param recursive Should the function be called on all child nodes too?
#' @return the data associated with child nodes.
#' @export
getChildData <- function (self, recursive = FALSE) {
  child_data <- NULL
  if (self$have_child) {
    child_data <- lapply(getChildNodes(self, recursive), function(x) x$data)
  }

  return(child_data)
}

#' Get the ids of the child nodes below the current node.
#'
#' @param self The node where to start.
#' @param recursive Should the function be called on all child nodes too?
#' @return the ids associated with child nodes.
#' @export
getChildId <- function (self, recursive = FALSE) {
  child_data <- NULL
  if (self$have_child) {
    child_data <- lapply(getChildNodes(self, recursive), function(x) x$id)
  }

  return(child_data)
}

#' Delete a node with a given id.
#'
#' @param self The reference to the tree where the id should be searched.
#' @export
deleteId <- function (self, id) {
  node <- self$searchNode(id)
  node$delete()
  invisible(self)
}

#' Delete all a node and all nodes below that node.
#' @param self    the GeneralTree
#' @param private the private members of the GeneralTree.
#'
#' @keywords internal
delete <- function(self, private) {
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
      remaining_siblings <- self$siblings
      # Remove the first sibling.
      remaining_siblings[[1]] <- NULL
      # Set the remaining siblings.
      self$parent$left_child$setSiblings(remaining_siblings)
    } else {
      siblings <- self$parent$left_child$siblings
      own_position <- sapply(siblings, function(x) identical(x, self))
      siblings <- siblings[!own_position]
      self$parent$left_child$setSiblings(siblings)
    }
  } else if (self$have_parent) {
    suppressWarnings({
      self$parent$setLeftChild(NULL)
    })
  }
}

#' The implementation of the GeneralTree method nextElem.
#'
#' @param self    the GeneralTree
#' @param private the private members of the GeneralTree.
#' @param set_discover Whether the discover flag should be set when seeking a
#'                     next element.
#' @param include_root Should the root be included in the discovery?
#' @return the next element in the tree that has the discover flag not set
#'         searched in a depth first search.
#'
#' @keywords internal
nextElemWorkerImpl <- function (self, private, set_discover = TRUE,
                                    include_root = TRUE) {
  next_element <- NULL
  candidates <- NULL

  if (self$is_root) {
    if (!self$isRootDiscovered && include_root) {
      next_element <- self
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
      next_element <- not_discovered[[1]]
  }

  if (is.null(next_element) && !self$is_root && self$have_parent) {
    next_element <- self$parent$nextElem()
  }

  if (!is.null(next_element))
    next_element$setDiscovered(set_discover)

  # If this was the last node, reset the root discovery.
  if (is.null(next_element) && self$is_root)
    self$setRootDiscovered(set_discover)


  if (is.null(next_element)) {
    next_element <- self
    stop("StopIteration")
  }

  invisible(next_element)
}

#' The implementation of the GeneralTree method iterator.
#'
#' @param self    the GeneralTree
#' @param private the private members of the GeneralTree.
#' @return An iterator for the general tree.
#'
#' @keywords internal
iteratorImpl <- function (self, private) {
  if (self$is_root) {
    self$resetDiscoveredOnBranch()
    self$setRootDiscovered(FALSE)
    return(self$nextElem())
  } else {
    return(self$root$iterator())
  }
}

#' The implementation of the GeneralTree method iterator.
#'
#' @param self    the GeneralTree
#' @param private the private members of the GeneralTree.
#' @return An iterator for the general tree.
#'
#' @keywords internal
resetDiscovered <- function(self, private) {
  if (!self$is_root) {
    private$.root$resetDiscoveredOnBranch()
  } else {
    self$resetDiscoveredOnBranch()
    self$setDiscovered(FALSE)
  }
}

#' Reset the discover bit of all the nodes on the branch to FALSE.
#'
#' @param self    the GeneralTree
#' @param private the private members of the GeneralTree.
#'
#' @keywords internal
resetDiscoveredOnBranch <- function(self, private) {
  sapply(self$getChildNodes(recursive = TRUE), function(x)
                        x$setDiscovered(FALSE))
  invisible(self)
}

#' Set the discovered bit to a certain state.
#'
#' @param self    the GeneralTree
#' @param private the private members of the GeneralTree.
#' @param is_discovered the state to which the discover bit should be set.
#'
#' @keywords internal
setDiscovered <- function(self, private, is_discovered) {
  private$.is_discovered <- is_discovered
  invisible(self)
}

#' Set the root discovered bit to a certain state.
#'
#' @param self    the GeneralTree
#' @param private the private members of the GeneralTree.
#' @param is_root_discovered the state to which the bit should be set.
#' @return An iterator for the general tree.
#'
#' @keywords internal
setRootDiscovered <- function(self, private, is_root_discovered) {
  private$.is_root_discovered = is_root_discovered
  invisible(self)
}

#' Convert a node to string.
#'
#' @param self    the GeneralTree node that should be converted to a string.
#' @param what    what should be converted to a string.
#' @return A string object that represents the node.
#'
#' @keywords internal
nodeInfoToString <- function(self, what = c("id", "data")) {
  what <- match.arg(what, several.ok = TRUE)

  get_id <- any("id" %in% what)
  get_data <- any("data" %in% what)

  node_id <- ""
  if (get_id)
    node_id <- as.character(self$id)

  node_data <- ""
  if (get_data)
    node_data <- as.character(self$data)

  sep <- ""
  if(get_id && get_data) {
    sep <- " : "
  }
  node_string <- paste(node_id, node_data, sep = sep)

  return(node_string)
}

#' Convert a branch to a string.
#'
#' @param self           the GeneralTree node from where the branch should be
#'                       converted to a string.
#' @param what           what should be converted to a string.
#' @param string_prepend which string should be prepended to the string.
#' @return A string object that represents the node.
#'
#' @keywords internal
toString <- function (self, what = c("id", "data"), string_prepend = "") {
  what <- match.arg(what, several.ok = TRUE)

  initiateEmptyString <- function(length = 1) {
    paste0(rep(" ", length), collapse = "")
  }

  if (self$is_root) {
    string <- self$nodeInfoToString(what)
    if (self$have_child) {
      space <- nchar(string)
      child_nodes <- self$getChildNodes(recursive = FALSE)
      string_prepend <- initiateEmptyString(length = space)
      result <- paste0(sapply(child_nodes, function(x) x$toString(what, string_prepend)), collapse = "\n")
      string <- paste0(string, result)
    }
  } else {
    tree_sep <- string_prepend

    if (identical(self$parent$left_child, self)) {
      node_sep <- paste0(" --> ")
    } else if (self$is_last_sibling) {
      node_sep <- paste0(tree_sep, " \\-> ")
    } else {
      node_sep <- paste0(tree_sep, " |-> ")
    }

    if (self$have_child) {
      max_space <- max(sapply(self$parent$getChildNodes(), function(x)
                             nchar(x$nodeInfoToString(what))))

      branch_symbol <- "|"
      if (self$is_last_sibling || !self$have_siblings)
        branch_symbol <- " "

      tree_sep <- paste0(tree_sep, " ", branch_symbol, "   ", initiateEmptyString(length = max_space))

      child_nodes <- self$getChildNodes(recursive = FALSE)
      result <- paste0(sapply(child_nodes, function(x) x$toString(what, tree_sep)), collapse = "\n")
      string <- paste0(node_sep, self$nodeInfoToString(what), result, collapse = "\n")
    } else {
      string <- paste0(node_sep, self$nodeInfoToString(what))
    }
  }

  return(string)
}

#' Return the root of the node.
#'
#' @param self      The node that should be inspected.
#' @param private   The private members of a node.
#' @return The root node.
#' @keywords internal
root <- function (self, private) {
  if (is.null(private$.root)) {
    invisible(self)
  } else {
    invisible(private$.root)
  }
}

#' Return the left child of the node.
#'
#' @param self      The node that should be inspected.
#' @param private   The private members of a node.
#' @return The left child of the node or \code{NULL} otherwise.
#' @keywords internal
left_child <- function (self, private) {
  invisible(private$.left_child)
}

#' Return the private siblings of the node.
#'
#' @param self      The node that should be inspected.
#' @param private   The private members of a node.
#' @return A list that contains the private siblings of the node.
#' @keywords internal
siblings <- function (self, private) {
  invisible(private$.siblings)
}

#' Return the id of the current node.
#'
#' @param self      The node that should be inspected.
#' @param private   The private members of a node.
#' @return the id of the node.
#' @keywords internal
id <- function (self, private) {
  return(private$.id)
}

#' Tell whether the current node has childeren.
#'
#' @param self      The node that should be inspected.
#' @param private   The private members of a node.
#' @return \code{TRUE} when the node has children.
#' @keywords internal
have_child <- function (self, private) {
  !is.null(private$.left_child)
}

#' Tell whether the current node has siblings.
#'
#' @param self      The node that should be inspected.
#' @param private   The private members of a node.
#' @return \code{TRUE} when the node has siblings.
#' @keywords internal
have_siblings <- function (self, private) {
  if (is.null(self$parent))
    return(FALSE)
  else
    self$parent$left_child$have_private_siblings
}

#' Tell whether the current node is the last sibling.
#'
#' @param self      The node that should be inspected.
#' @param private   The private members of a node.
#' @return \code{TRUE} when the node is the last sibling.
#' @keywords internal
is_last_sibling <- function (self, private) {
  if (self$have_siblings) {
    siblings = self$parent$left_child$getSiblingNodes()
    return(identical(siblings[[length(siblings)]], self))
  }
  return(FALSE)
}

#' Tell whether the current node has private siblings.
#'
#' @param self      The node that should be inspected.
#' @param private   The private members of a node.
#' @return \code{TRUE} when the node has private siblings.
#' @keywords internal
have_private_siblings <- function (self, private) {
  !is.null(private$.siblings)
}

#' Returns true when the node has a parent.
#'
#' @param self      The node that should be inspected.
#' @param private   The private members of a node.
#' @return \code{TRUE} when the node has a parent.
#' @keywords internal
have_parent <- function (self, private) {
  !is.null(private$.parent)
}

#' Return the data associated with a node.
#'
#' @param self      The node which data should be retrieved.
#' @param private   The private members of a node.
#' @return the data of the node.
#' @keywords internal
data <- function (self, private) {
  return(private$.data)
}

#' Tell whether the passed node is the root of the tree.
#'
#' @param self      The node that refers to the tree.
#' @param private   The private members of a node.
#' @return \code{TRUE} when the node is the root of the tree.
#' @keywords internal
is_root <- function (self, private) {
  is.null(private$.root)
}

#' Get the parent of a node.
#'
#' @param self      The node that refers to the tree.
#' @param private   The private members of a node.
#' @return the parent of the node.
#' @keywords internal
parent <- function (self, private) {
  return(private$.parent)
}

#' Get the depth of a tree.
#'
#' @param self      The node that refers to the tree.
#' @param private   The private members of a node.
#' @return A numeric value that indicates the number of layers in the tree.
#' @keywords internal
treeDepth <- function (self, private) {
  if (!self$is_root) {
    depth = self$root$treeDepth
  } else {
    depth = self$branch_depth
  }

  return(depth)
}

#' Informs whether the node has the discovered bit set.
#'
#' @param self      The node where to start.
#' @param private   The private members of a node.
#' @return \code{TRUE} if the node has the discovered bit set.
#' @keywords internal
isDiscovered <- function (self, private) {
  return(private$.is_discovered)
}

#' Returns whether the node has the is_root_discovered bit set.
#'
#' @param self      The node where to start.
#' @param private   The private members of a node.
#' @return \code{TRUE} if the node has the is_root_discovered bit set.
#' @keywords internal
isRootDiscovered <- function (self, private) {
  return(private$.is_root_discovered)
}

#' Get the depth of a branch.
#'
#' @param self      The node where to start.
#' @param private   The private members of a node.
#' @return A numeric value that indicates the number of layers in the branch.
#' @keywords internal
branch_depth <- function (self, private) {
  depth = 1

  if (self$have_child) {
    depth = max(depth, self$left_child$branch_depth + 1)
  }

  if (self$have_private_siblings) {
    depth = max(depth, sapply(self$siblings, function(x)
                              x$branch_depth))
  }

  return(depth)
}

#' Informs whether the tree is a singleton tree.
#'
#' @param self      The node where to start.
#' @param private   The private members of a node.
#' @return \code{TRUE} if the node contains only a single element i.e. the root
#'  node.
#' @keywords internal
isSingletonTree <- function (self, private) {
  return(is.null(private$.root) && is.null(private$.siblings) &&
         is.null(private$.left_child))
}

