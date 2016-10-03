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

#' Deep clone a General Tree.
#'
#' @param x The target to where the tree should be copied.
#' @param value The general tree that should be cloned into.
#' @return a clone of the tree.
#' @usage `<-.GeneralTree`(x, value)
#' @export
#' @rdname assign
"<-.GeneralTree" <- function(x, value) {
    x <- value$clone(deep = TRUE)
    return(x)
}

#' Deep clone a General Tree.
#'
#' @param x The target to where the tree should be copied.
#' @param value The general tree that should be cloned into.
#' @return a clone of the tree.
#' @usage `=.GeneralTree`(x, value)
#' @export
#' @rdname set
"=.GeneralTree" <- function(x, value) {
    x <- value$clone(deep = TRUE)
    return(x)
}

#' Compare two General Trees.
#'
#' @param x The target to where the tree should be copied.
#' @param value The general tree that should be cloned into.
#' @return the result of the comparison.
#' @aliases ==
#' @usage `==.GeneralTree`(x, value)
#' @rdname equals
"==.GeneralTree" <- function(x, value) {
  x$cmp(value) && value$cmp(x)
}

