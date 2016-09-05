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

#' Deep clone a General tree
#' @param x The object that should be converted.
#' @param ... passed to underlying functions.
#' @export
deep_clone <- function(x) UseMethod("deep_clone")


#' Deep clone a General Tree.
#'
#' @param x     The general tree that should be deep cloned.
#' @return a clone of the tree.
#' @export
deep_clone.GeneralTree <- function(x) {
    # Create a list of all the childeren.
    childeren <- x$getChildNodes(recursive = TRUE)

    new_tree <- GeneralTree$new(id = x$id, data = x$data)

    sapply(childeren, function(x) {
               new_tree$addNode(parent = x$parent$id, id = x$id, data = x$data)
                })

    return(new_tree)
}

#' Deep clone a General Tree.
#'
#' @param x     The general tree that should be deep cloned.
#' @return a clone of the tree.
#' @export
"<-.GeneralTree" <- function(x)
    deep_clone.GeneralTree(x)

#' Deep clone a General Tree.
#'
#' @param x     The general tree that should be deep cloned.
#' @return a clone of the tree.
#' @export
"=.GeneralTree" <- function(x)
    deep_clone.GeneralTree(x)


