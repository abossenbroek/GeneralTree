#' Print a GeneralTree object.
#' @param x tree to print.
#' @param ... arguments passed to underlying functions.
#' @export
print.GeneralTree <- function(x, ...) {
  dots = list(...)

  if (!("what" %in% names(dots)))
    dots$what = "id"

  cat(x$toString(...))
}


