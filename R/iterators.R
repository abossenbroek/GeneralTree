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
nextElem.GeneralTreeIter <- function(obj, ...) {
  repeat {
    tryCatch({
      if (obj$checkFunc(getIterVal(obj, 1L))) {
        obj$state$obj <- obj$state$obj$nextElem()
        obj$state$i <- obj$state$i + 1L
        return(getIterVal(obj))
      }
      obj$state$obj <- obj$state$obj$nextElem()
      obj$state$i <- obj$state$i + 1L
    }, error = function(e) {
      if (any(nzchar(e$message))) {
        if (identical(e$message, "StopIteration")) {
          if (obj$recycle) {
            obj$state$i <- 0L
            obj$state$obj$resetDiscovered()
            obj$state$obj <- obj$state$obj$root
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
#' @importFrom R6 object_summaries
#' @export
iter.GeneralTree <- function (obj, by = c("data"),
                              checkFunc = function(...) TRUE,
                              recycle = FALSE,
                              ...) {
  if (!(by %in% gsub("([a-zA-Z0-9]*):.*", "\\1",
                     R6:::object_summaries(obj, exclude = ".__enclos_env__"))))
    stop("Could not find", by, "as a member of ", setdiff(class(obj), "R6"))

  state <- new.env()
  state$i <- 0L
  state$obj <- obj
  obj$resetDiscoveredOnBranch()
  # Add one to compensate for parent node.
  n <- length(obj$getChildNodes(recursive = TRUE)) + 1
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
    if (i > n)
        stop("StopIteration", call. = FALSE)
    switch(obj$by, "data" = obj$state$obj$data, "id" = obj$state$obj$id,
           eval(parse(file = NULL, text = paste0("obj$state$obj$", obj$by))))
}
