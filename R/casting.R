#' Convert a GeneralTree to a data frame.
#' @export
as.data.frame.GeneralTree <- function(x) {
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
                    parent = unlist(parent)))
}

#' Convert an object to a GeneralTree.
#' @param x The object that should be converted.
#' @export
as.GeneralTree <- function(x) UseMethod("as.GeneralTree", x)

#' Convert a data frame to a GeneralTree.
#' @export
as.GeneralTree.data.frame <- function(x, id = "id", data = "data",
                                      parent = "parent") {

  if (sum(is.na(x$parent)) != 1)
    stop(paste0("Multiple entries with NA parent where found.",
                "Make sure to have only one entry with parent NA."))

  root_id = x$id[is.na(x$parent)]
  root_data = x$data[is.na(x$parent)]

  new_tree = GeneralTree$new(root_id, root_data)

  # Select the remaining data that needs to be converted into the tree.
  remaining_data = x[!is.na(x$parent),]

  ids_in_tree <- NULL

  if (nrow(remaining_data) > 0) {
    idx_to_push <- 1 : nrow(remaining_data)

    i = 0

    while(length(idx_to_push) > 0) {
      if (i == idx_to_push[1]) {
        stop("Could not find parent: ", remaining_data$parent[i])
      }
      i = idx_to_push[1]
      current_id = remaining_data$id[i]
      current_data = remaining_data$data[i]
      current_parent = remaining_data$parent[i]

      new_node = tryCatch(new_tree$addNode(current_parent, current_id,
                                        current_data), 
                           error = function(e) NULL)

      # The new node was successfully added so we remove the current node from
      # the stack of nodes that needed to be added.
      if (!is.null(new_node)) {
        idx_to_push = idx_to_push[-1]
      } else {
        # Swap the element that we could not add with the a pivot. We take the
        # pivot as the center, plus one to ensure that we have only two
        # elements left, the next element in the list will be used.
        pivot = ceiling(length(idx_to_push) / 2) + 1
        tmp_idx = idx_to_push[1]
        idx_to_push[1] = idx_to_push[pivot]
        idx_to_push[pivot] = tmp_idx
      }


    }
  }

  return(new_tree)
}
