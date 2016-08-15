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
