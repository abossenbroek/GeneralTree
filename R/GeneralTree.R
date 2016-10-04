#
# Copyright (c) 2016-2016 Anton Bossenbroek
#
# GeneralTree is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This file is part of GeneralTree.
#
# GeneralTree is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GeneralTree.  If not, see <http://www.gnu.org/licenses/>.
#

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
#' \dontrun{
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
#' }
GeneralTree <- R6Class("GeneralTree",
  lock_objects = FALSE,
  private = list(
    .xptr = NULL,
    .ref_uid = NULL,
    deep_clone = function(name, value) {
      if (name == ".xptr")
        return(copy(value, private$.ref_uid))
      if (name == ".ref_uid")
        return(0)
       else
        return(value)
    }
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
   addSibling = function(id, data)
     addSibling(self, private, id, data)
   ,
   travelUp = function(id, data)
     travelUp(self, private)
   ,
   searchData = function(key)
     searchData(self, private, key)
   ,
   searchNode = function(key)
     searchNode(self, private, key)
   ,
   setKey = function(key)
     setKey(self, private, key)
   ,
   setData = function(data)
     setData(self, private, data)
   ,
   getSiblingData = function()
     getSiblingData(self, private)
   ,
   getSiblingId = function()
     getSiblingId(self, private)
   ,
   getChildData = function(recursive = FALSE)
     getChildData(self, private, recursive)
   ,
   getChildId = function(recursive = FALSE)
     getChildId(self, private, recursive)
   ,
   getChildrenKeys = function(recursive = FALSE)
     getChildrenKeys(self, private, recursive = recursive)
   ,
   getChildrenData = function(recursive = FALSE)
     getChildrenData(self, private, recursive = recursive)
   ,
   getChildrenKeysByKey = function(key, recursive = FALSE)
     getChildrenKeys(self, private, key = key, recursive = recursive)
   ,
   getChildrenDataByKey = function(key, recursive = FALSE)
     getChildrenData(self, private, key = key, recursive = recursive)
   ,
   getSiblingsKeys = function()
     getSiblingsKeys(self, private)
   ,
   getSiblingsData = function()
     getSiblingsData(self, private)
   ,
   getSiblingsKeysByKey = function(key)
     getSiblingsKeys(self, private, key)
   ,
   getSiblingsDataByKey = function(key)
     getSiblingsData(self, private, key)
   ,
   getBranchKeys = function()
     getBranchKeys(self, private)
   ,
   getBranchData = function()
     getBranchData(self, private)
   ,
   getBranchKeysByKey = function(key)
     getBranchKeys(self, private, key)
   ,
   getBranchDataByKey = function(key)
     getBranchData(self, private, key)
   ,
   getLeafsKeys = function()
     getLeafsKeys(self, private)
   ,
   getLeafsData = function()
     getLeafsData(self, private)
   ,
   getLeafsKeysByKey = function(key)
     getLeafsKeys(self, private, key)
   ,
   getLeafsDataByKey = function(key)
     getLeafsData(self, private, key)
   ,
   deleteId = function(key)
     deleteId(self, private, key)
   ,
   delete = function()
     deleteId(self, private)
   ,
   setRefUID = function(uid)
     setRefUID(self, private, uid)
   ,
   changeRef = function()
      changeRef(self, private)
   ,
   print = function(limit = 50)
     printTree(self, private, limit = limit)
   ,
   cmp = function(val)
     cmp(self, private, val)
   ,
   cmpMemory = function(val)
     cmpMemory(self, private, val)
   ,
   getXptr = function()
     private$.xptr
   ,
   getRefUID = function()
     private$.ref_uid
   ,
   updateKey = function(new_key)
     updateKey(self, private, new_key = new_key)
   ,
   updateKeyByKey = function(key, new_key)
     updateKey(self, private, key = key, new_key = new_key)
   ,
   updateData = function(new_data)
     updateData(self, private, new_data = new_data)
   ,
   updateDataByKey = function(key, new_data)
     updateData(self, private, key = key, new_data = new_data)
   ,
   applyOnBranch = function(f)
     applyOnBranch(self, private, f = f)
   ,
   applyOnBranchByKey = function(key, f)
     applyOnBranch(self, private, key = key, f = f)
  ),
  active = list(
    depth = function()
      depth(self, private)
    ,
    have_siblings = function()
      have_siblings(self, private)
    ,
    have_ref_uid = function()
      have_ref_uid(self, private)
    ,
    is_last_sibling = function()
      isLastSibling(self, private)
    ,
    root = function()
      getRoot(self, private)
    ,
    parent = function() {
      self$changeRef()
      parent_uid <- find_uid(private$.xptr,
                             get_parent_at_ref(private$.xptr)$key)

      result <- self$clone()
      result$setRefUID(parent_uid)

      invisible(result)
    },
    key = function() {
      self$changeRef()
      return(get_ref(private$.xptr)$key)
    },
    data = function() {
      self$changeRef()
      return(get_ref(private$.xptr)$data)
    },
    xptr = function() {
      return(private$.xptr)
    }
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
  private$.xptr <- initialize_tree(id, data)
  self$setRefUID(find_uid(private$.xptr, id))

  invisible(self)
}

#'
#' @keywords internal
addNode <- function (self, private, parent_id, key, data) {
  private$.xptr <- add_node(private$.xptr, parent_id, key, data)

  new_uid <- find_uid(private$.xptr, key)
  result <- self$clone()
  result$setRefUID(new_uid)

  invisible(result)
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
  self$changeRef()
  add_child(private$.xptr, id, data)

  result <- self$clone()
  new_uid <- find_uid(private$.xptr, id)
  result$setRefUID(new_uid)

  invisible(result)
}

#' Add a sibling to the current node.
#'
#' @param self    The point in the tree where the sibling should be added.
#' @param private The private part of the tree.
#' @param id      The id of the node that should be added.
#' @param data    The data of the node that should be added.
#' @return invisible reference to ourself.
#' @keywords internal
addSibling <- function (self, private, id, data) {
  self$changeRef()
  self$.xptr <- add_sibling(private$.xptr, id, data)

  result <- self$clone()
  new_uid <- find_uid(private$.xptr, id)
  result$setRefUID(new_uid)

  invisible(result)
}

#' Travel one level up in the tree.
#'
#' @param self    The point in the tree where the sibling should be added.
#' @param private The private part of the tree.
#' @return invisible the new node that was created.
#' @keywords internal
travelUp <- function (self, private) {
  self$changeRef()
  self$.xptr <- travel_up(self$.xptr)

  invisible(self)
}

#' Search for an id in starting at a point in the tree and return the data
#' matching the id.
#'
#' @param self the node where to start searching.
#' @param id the id to look for.
#' @return The data associated with an id.
#' @keywords internal
searchData <- function (self, private, key) {
  self$changeRef()
  return(get_data(private$.xptr, key))
}

#'
#' @keywords internal
getSiblingData <- function (self, private) {
  warning("DEPRECATED: getSiblingData will be replaced with getSiblingsData in future releases.")
  return(getSiblingsData(self, private))
}

#'
#' @keywords internal
getSiblingId <- function (self, private) {
  warning("DEPRECATED: getSiblingId will be replaced with getSiblingsKey in future releases.")
  return(getSiblingsKeys(self, private))
}

#'
#' @keywords internal
getSiblingsKeys <- function (self, private, key) {
  if (missing(key)) {
    self$changeRef()
    return(get_siblings_keys_at_ref(private$.xptr))
  }

  return(get_siblings_keys(private$.xptr, key))
}

#'
#' @keywords internal
getSiblingsData <- function (self, private, key) {
  if (missing(key)) {
    self$changeRef()
    return(get_siblings_data_at_ref(private$.xptr))
  }
  return(get_siblings_data(private$.xptr, key))
}

#'
#' @keywords internal
setKey <- function (self, private, new_key) {
  private$.xptr <- set_key(private$.xptr, new_key)

  invisible(self)
}

#'
#' @keywords internal
setData <- function (self, private, new_data) {
  self$changeRef()
  private$.xptr <- set_data(private$.xptr, new_data)

  invisible(self)
}

#' Get the data of the child nodes below the current node.
#'
#' @param self The node where to start.
#' @param recursive Should the function be called on all child nodes too?
#' @return the data associated with child nodes.
#' @keywords internal
getChildData <- function (self, private, recursive = FALSE) {
  warning("DEPRECATED: getChildData will be replaced with getChildrenData in future releases")
  return(getChildrenData(self, private, recursive = recursive))
}

#' Get the data of the child nodes below the current node.
#'
#' @param self The node where to start.
#' @param recursive Should the function be called on all child nodes too?
#' @return the data associated with child nodes.
#' @keywords internal
getChildrenData <- function (self, private, key, recursive = FALSE) {
  if (missing(key)) {
    self$changeRef()
    return(get_children_data_at_ref(private$.xptr, recursive))
  }

  return(get_children_data(private$.xptr, key, recursive))
}

#' Get the ids of the child nodes below the current node.
#'
#' @param self The node where to start.
#' @param recursive Should the function be called on all child nodes too?
#' @return the ids associated with child nodes.
#' @keywords internal
getChildId <- function (self, private, recursive = FALSE) {
  warning("DEPRECATED: getChildId will be replaced with getChildrenKeys in future releases")
  return(getChildrenKeys(self, private, recursive = recursive))
}

#' Get the keys of the child nodes below the current node.
#'
#' @param self The node where to start.
#' @param recursive Should the function be called on all child nodes too?
#' @return the keys associated with child nodes.
#' @keywords internal
getChildrenKeys <- function (self, private, key, recursive = FALSE) {
  if (missing(key)) {
    self$changeRef()
    return(get_children_keys_at_ref(private$.xptr, recursive))
  }

  return(get_children_keys(private$.xptr, key, recursive))
}

#' Get the keys of the branch nodes below the current node.
#'
#' @param self The node where to start.
#' @param private the private members of the GeneralTree.
#' @return the keys associated with branch nodes.
#' @keywords internal
getBranchKeys <- function (self, private, key) {
  if (missing(key)) {
    self$changeRef()
    return(get_branch_keys_at_ref(private$.xptr))
  }

  return(get_branch_keys(private$.xptr, key))
}

#' Get the data of the branch nodes below the current node.
#'
#' @param self The node where to start.
#' @param private the private members of the GeneralTree.
#' @param key The key for which the branch should be returned
#' @return the data associated with branch nodes.
#' @keywords internal
getBranchData <- function (self, private, key) {
  if (missing(key)) {
    self$changeRef()
    return(get_branch_data_at_ref(private$.xptr))
  }
  return(get_branch_data(private$.xptr, key))
}

#' Get the keys of the leafs nodes below the current node.
#'
#' @param self The node where to start.
#' @param private the private members of the GeneralTree.
#' @param key The key for which the leafs should be returned
#' @return the keys associated with leafs nodes.
#' @keywords internal
getLeafsKeys <- function (self, private, key) {
  if (missing(key)) {
    self$changeRef()
    return(get_leafs_keys_at_ref(private$.xptr))
  }
  return(get_leafs_keys(private$.xptr, key))
}

#' Get the data of the leafs nodes below the current node.
#'
#' @param self The node where to start.
#' @param private the private members of the GeneralTree.
#' @param key The key for which the leafs should be returned
#' @return the data associated with leafs nodes.
#' @keywords internal
getLeafsData <- function (self, private, key) {
  if (missing(key)) {
    self$changeRef()
    return(get_leafs_data_at_ref(private$.xptr))
  }

  return(get_leafs_data(private$.xptr, key))
}

#' Delete last referenced key.
#'
#' @param self The reference to the tree where the id should be searched.
#' @param private the private members of the GeneralTree.
#' @param id The id that should be deleted.
#' @keywords internal
deleteId <- function (self, private, key) {
  replacement_uid <- NULL
  self$changeRef()

  if (!missing(key)) {
    replacement_uid <- delete_node(private$.xptr, key)
  } else {
    replacement_uid <- delete_node_at_ref(private$.xptr)
  }

  result <- self$clone()
  result$setRefUID(replacement_uid)

  invisible(result)
}

#' Returns the depth of the tree.
#'
#' @param self The reference to the tree where the id should be searched.
#' @param private the private members of the GeneralTree.
#' @keywords internal
depth <- function (self, private) {
  self$changeRef()

  return(get_tree_depth_at_ref(private$.xptr))
}

#' @keywords internal
have_siblings <- function (self, private) {
  self$changeRef()

  return(have_siblings_at_ref(private$.xptr))
}

#' @keywords internal
setRefUID <- function (self, private, uid) {
  private$.ref_uid = uid

  invisible(self)
}

#' @keywords internal
searchNode <- function (self, private, key) {
  if (missing(key))
    stop("searchNode: Need a key to search for.")

  new_uid <- find_uid(private$.xptr, key)
  result <- self$clone()
  result$setRefUID(new_uid)

  invisible(result)
}

#' @keywords internal
have_ref_uid <- function (self, private) {
  return(!is.null(private$.ref_uid))
}

#' @keywords internal
changeRef <- function (self, private) {
  if (self$have_ref_uid)
    change_ref(private$.xptr, private$.ref_uid)
}

#' @keywords internal
isLastSibling <- function (self, private) {
  self$changeRef()

  return(is_last_sibling_at_ref(private$.xptr))
}

#' @keywords internal
getRoot <- function (self, private) {
  root_uid <- find_uid(private$.xptr, get_root(private$.xptr)$key)

  result <- self$clone()
  result$setRefUID(root_uid)

  invisible(result)
}

#' @keywords internal
cmp <- function (self, private, val)
{
  result <- cmp_gti(private$.xptr, val$getXptr())

  # cmp_gti() will only return success if the keys and data of nodes point to
  # the same objects. If this is not the case, which would be the case if we
  # have a true copy of an object we still may have the same object and data
  # but true copies. We still need to verify for that case
  if (!result)
    result <- result && (serialize(private$.xptr) == serialize(val$getXptr()))

  result <- result && private$.ref_uid == val$getRefUID()

  return(result)
}

#' @keywords internal
cmpMemory <- function (self, private, val)
{
  result <- cmp(self, private, val)
  result <- result && cmp_gti_mem(private$.xptr, val$xptr)

  return(result)
}

#' @keywords internal
updateKey <- function (self, private, key, new_key)
{
  if (missing(key)) {
    self$changeRef()
    return(update_key_at_ref(private$.xptr, new_key))
  }

  return(update_key(private$.xptr, key, new_key))
}

#' @keywords internal
updateData <- function (self, private, key, new_data)
{
  if (missing(key)) {
    self$changeRef()
    return(update_data_at_ref(private$.xptr, new_data))
  }

  return(update_data(private$.xptr, key, new_data))
}

#' @keywords internal
applyOnBranch <- function (self, private, key, f)
{
  if (missing(key)) {
    self$changeRef()
    return(apply_on_branch(private$.xptr, f))
  }

  return(apply_on_branch_at_ref(private$.xptr, key, f))
}

#' @keywords internal
printTree <- function (self, private, key, limit = 50)
{
  tree_info <- NULL

  node_info <- function(tn) {
    list(key = tn$key, data = tn$data,
         is_last_sibling = tn$is_last_sibling,
         parents_above = tn$parents_above)
  }
  generate_line <- function (left, right) {
    padding <- paste0(rep(" ", getOption("width") - nchar(left) - nchar(right)),
                      collapse = "")
    paste0(left, padding, right, "\n")
  }

  if (missing(key)) {
    self$changeRef()
    tree_info <- apply_on_branch(private$.xptr, node_info)
  } else {
    tree_info <- apply_on_branch_at_ref(init, key, node_info)
  }

  capped <- FALSE
  if (length(tree_info) > limit) {
    tree_info <- tree_info[1 : limit]
    capped <- TRUE
  }

  cat(generate_line("TREE", "DATA"))

  is_root <- TRUE
  pre_list <- list()
  for (t in tree_info) {
    pre <- paste0(Filter(Negate(is.null), pre_list[1 : t$parents_above]) , collapse = "")
    sep <- ""
    if (!t$is_last_sibling) {
      if (is_root) {
        is_root <- FALSE
        pre_list[t$parents_above + 1] <- ""
      } else {
        pre_list[t$parents_above + 1] <- "|  "
        sep <- "|- "
      }
    } else {
      sep <- "\\- "
      pre_list[t$parents_above + 1] <- "   "
    }
    tree_structure <- paste0(pre, sep, t$key, collapse = "")
    data_str <- substring(toString(t$data), 1, 10)
    cat(generate_line(tree_structure,  data_str))
  }

  if (capped)
    cat("Maximum entries reached, change limit to a higher number to see more")
}
