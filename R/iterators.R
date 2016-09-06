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

#' Internal function heavily inspired by iterators package.
#' @keywords internal
#' @export
nextElem.GeneralTreeIter <- function (obj, ...) {
  repeat {
    tryCatch({
      value <- getIterVal.GeneralTreeIter(obj, 1L)
      obj$state$i <- obj$state$i + 1L

      if (obj$checkFunc(value)) {
          return(value)
      }
    }, error = function(e) {
      if (any(nzchar(e$message))) {
        if (identical(e$message, "StopIteration")) {
          if (obj$recycle) {
            obj$state$i <- 0L
          }
          else {
            stop("StopIteration", call. = FALSE)
          }
        }
        else {
          stop(e$message, call. = FALSE)
        }
      }
      else {
        stop("Abort", call. = e)
      }
    })
  }
}

#' Internal function heavily inspired by iterators package.
#' @keywords internal
#' @export
nextElem.GeneralTree <- function(obj, ...) {
  obj$nextElem()
}


#' Internal function heavily inspired by iterators package.
#' @keywords internal
#' @export
iter.GeneralTree <- function (obj, by = c("data"),
                              checkFunc = function(...) TRUE,
                              recycle = FALSE,
                              ...) {
  state <- new.env()
  state$i <- 0L
  nodes <- obj$branchToList()

  state$obj <- lapply(nodes, function(x) {
    switch(by, data = x$data, id = x$id,
           eval(parse(file = NULL, text = paste0("x$", by))))
                              })
  n <- length(state$obj)
  it <- list(state = state, by = by, length = n, checkFunc = checkFunc,
             recycle = recycle)
  class(it) <- c("GeneralTreeIter", "iter")
  it
}

#' Function heavily inspired by iterators package.
#' @keywords internal
#' @export
getIterVal <- function (obj, plus, ...) {
    UseMethod("getIterVal")
}

#' Function heavily inspired by iterators package.
#' @keywords internal
#' @export
getIterVal.GeneralTreeIter <- function (obj, plus = 0L, check = TRUE, ...) {
    i <- obj$state$i + plus
    n <- obj$length

    iter_object <- obj$state$obj

    if (i > n)
        stop("StopIteration", call. = FALSE)

    return(obj$state$obj[[i]])
}
