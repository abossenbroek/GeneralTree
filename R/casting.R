#
# Copyright (c) 2016-2016 Anton Bossenbroek
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

#' Convert a GeneralTree to a data frame.
#'
#' @param x GeneralTree to convert to a data frame.
#' @param row.names Ignored.
#' @param optional Ignored.
#' @param ... Ignored.
#' @export
as.data.frame.GeneralTree <- function(x, row.names = NULL, optional = NULL,
                                      ...) {
  i <- x$iterator()

  id <- list()
  data <- list()
  parent <- list()

  while (!is.null(i)) {
    id <- c(id, i$id)
    data <- c(data, i$data)
    parent_id <- i$parent$id
    if (is.null(parent_id))
      parent_id <- NA
    parent <- c(parent, parent_id)

    i <- tryCatch(i$nextElem(), error = function(e) NULL)
  }

  return(data.frame(id = unlist(id),
                    data = unlist(data),
                    parent = unlist(parent), stringsAsFactors = FALSE))
}

#' Convert an object to a GeneralTree.
#' @param x The object that should be converted.
#' @param ... passed to underlying functions.
#' @export
as.GeneralTree <- function(x, ...) UseMethod("as.GeneralTree")

#' Convert a data frame to a GeneralTree.
#' @param x       The data frame that should be converted to a tree.
#' @param ...     id      The column name of the column that holds the ids of each node.
#'                data    The column name of the column that holds the data of
#'                        each node.
#'                parent  The column name of the column that holds the parent
#'                        of each node, NA indicates a node is the root.
#' @examples
#'   test_tree_df <- data.frame(
#'       ID = c("root", "child1", "child2", "child3"),
#'       DATA = c("parent1", "data3.1", "data1.2", "data1.3"),
#'      PARENT = c(NA, "child3", "root", "root"), stringsAsFactors = FALSE)
#' as.GeneralTree(test_tree_df, id = "ID", data = "DATA", parent = "PARENT")
#'
#' @export
as.GeneralTree.data.frame <- function(x, ...) {
  dots <- list(...)

  id_colname = "id"
  data_colname = "data"
  parent_colname = "parent"
  parent_node = NULL

  if ("id" %in% names(dots))
    id_colname = dots$id
  if ("data" %in% names(dots))
    data_colname = dots$data
  if ("parent" %in% names(dots))
    parent_colname = dots$parent
  if ("parent_node" %in% names(dots))
    parent_node = dots$parent_node


  if (!(id_colname %in% names(x)))
      stop("Could not find id column ", id_colname)
  if (!(data_colname %in% names(x)))
      stop("Could not find data column ", data_colname)
  if (!(parent_colname %in% names(x)))
      stop("Could not find product column ", parent_colname)

  if (any(sapply(x[c(id_colname, data_colname, parent_colname)], is.factor)))
    warning("Some columns are encoded as factors which could lead to errors.")

  if ((sum(is.na(x[parent_colname][, 1])) != 1) && is.null(parent_node))
    stop(paste0("Multiple entries with NA parent where found.",
                "Make sure to have only one entry with parent NA."))

  new_tree = NULL

  if (is.null(parent_node)) {
    root_id = x[id_colname][is.na(x[parent_colname]), 1]
    root_data = x[data_colname][is.na(x[parent_colname]), 1]

    new_tree = GeneralTree$new(root_id, root_data)
  } else {
    if (inherits(parent_node, "GeneralTree")) {
        if (parent_node$isSingletonTree) {
            new_tree = parent_node
        } else {
            stop("the passed parent_node is not a singleton tree.")
        }
    } else {
        stop("the passed parent_node was not a GeneralTree object.")
    }
  }

  # Select the remaining data that needs to be converted into the tree.
  remaining_data = x[!is.na(x[parent_colname])[, 1],]

  if (nrow(remaining_data) > 0) {
    idx_to_push = 1 : nrow(remaining_data)
    idx_not_found = NULL

    i = 0

    while(length(idx_to_push) > 0) {
      if (i == idx_to_push[1]) {
        stop("Could not find parent: ", remaining_data[parent_colname][i, 1])
      }
      i = idx_to_push[1]
      current_id = remaining_data[id_colname][i, 1]
      current_data = remaining_data[data_colname][i, 1]
      current_parent = remaining_data[parent_colname][i, 1]

      new_node = tryCatch(new_tree$addNode(current_parent, current_id,
                                        current_data),
                           error = function(e) NULL)

      # The new node was successfully added so we remove the current node from
      # the stack of nodes that needed to be added.
      if (!is.null(new_node)) {
        idx_to_push = idx_to_push[-1]

        # Remove the element from the not found list.
        if (i %in% idx_not_found)
          idx_not_found = idx_not_found[idx_not_found != i]
      } else {
        # Exit if the list of indices is the same as the list that was not
        # found.
        if (identical(intersect(idx_to_push, idx_not_found), idx_to_push))
          break

        if (!(current_parent %in% remaining_data[parent_colname][, 1]))
          stop("Could not find parent ", current_parent)

        parent_location = which(remaining_data[id_colname][, 1] %in%
                                current_parent)
        pivot = match(parent_location, idx_to_push)

        tmp_idx = idx_to_push[1]
        idx_to_push[1] = idx_to_push[pivot]
        idx_to_push[pivot] = tmp_idx

        # Record the index that we were not able to find.
        idx_not_found <- c(idx_not_found, i)
      }
    }

    if (length(idx_not_found) > 0)
      stop("Could not find all parents in the data frame.")
  }

  return(new_tree)
}

#' Convert a R parsed expression to a GeneralTree.
#' @param x The expression that should be converted.
#' @param ...  what = "token" fill the tree with tokens as the data field.
#'             what = "text" fill the tree with text as the data field.
#' @examples
#' p <- parse(text = "
#'                    tree <- GeneralTree$new(1, 'parent1')
#'                    tree$addNode(1, 2, 'child.1.2')
#'                    tree$addNode(2, 3, 'child.2.3')",
#'            keep.source = TRUE)
#' as.GeneralTree(p, what = "token")
#' as.GeneralTree(p, what = "text")
#' as.GeneralTree(p, what = c("text", "token"))
#' @export
as.GeneralTree.expression <- function(x, ...) {

  parsed_data <- utils::getParseData(x)

  if (is.null(parsed_data))
    stop("Could not find parsed data, make sure to set keep.source = TRUE in",
         " your call to parse.")

  dots <- list(...)

  what <- "text"
  if ("what" %in% names(dots)) {
    if (all(dots$what == "token")) {
      what <- "token"
    } else if (all(dots$what == "text")) {
      what <- "text"
    } else if (all(dots$what %in% c("text", "token"))) {
      parsed_data$DATA <- paste(parsed_data$token, parsed_data$text, sep = ": ")
      what <- "DATA"
    } else {
      stop("Do not know how to process ", dots$what)
    }
  }

  tree <- GeneralTree$new(0L, "BaseEnvironment")

  return(GeneralTree::as.GeneralTree(parsed_data, data = what,
                                     parent_node = tree))
}
