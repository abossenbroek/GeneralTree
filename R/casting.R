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

  return(new_tree)
}
