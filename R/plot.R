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

#' Plot a GeneralTree object.
#' @param x tree to plot.
#' @param ... arguments passed to underlying functions.
#' @export
plot.GeneralTree <- function(x, ...) {
  tree_grGraph <- generate_grViz(x, ...)

  dots <- list(...)
  if ("what" %in% names(dots))
    dots$what <- NULL
  if ("color" %in% names(dots))
    dots$color <- NULL
  if ("shape" %in% names(dots))
    dots$shape <- NULL
  if ("style" %in% names(dots))
    dots$style <- NULL

  dots$diagram <- tree_grGraph$dot_code

  do.call(DiagrammeR::grViz, dots)
}

#' Create a DiagrammeR graph that represents the tree.
#'
#' @param what select what to draw in the tree.
#' @param ... Additional arguments passed to create_nodes
#' @seealso \code{\link{DiagrammeR::create_nodes}}
generate_grViz <- function(obj, what = c("id", "data"), ...) {
  i <- obj$iterator()
  what <- match.arg(what, several.ok = FALSE)
  get_id <- any("id" %in% what)
  get_data <- any("data" %in% what)

  ids <- c()
  data <- c()
  edges_from <- c()
  edges_to <- c()

  while (!is.null(i)) {
    if (get_id) ids <- c(ids, i$id)
    if (get_data) data <- c(data, i$data)

    # If i has childeren we need to create the edges.
    if (i$have_child) {
      if (get_data && !get_id) {
        node_id <- i$data
        childeren <- i$getChildData()
      } else {
        node_id <- i$id
        childeren <- i$getChildId()
      }

      edges_from <- c(edges_from, rep(node_id, length(childeren)))
      edges_to <- c(edges_to, childeren)
    }

    i <- tryCatch(i$nextElem(), error = function(e) NULL)
  }

  if (get_data && !get_id)
    node_ids <- data
  else
    node_ids <- ids

  dots <- list(...)
  if (!("style" %in% names(dots))) dots$style <- "filled"
  if (!("color" %in% names(dots))) dots$color <- "gray"
  if (!("shape" %in% names(dots))) dots$shape <- "rectangle"

  create_nodes_call <- dots
  create_nodes_call$nodes <- node_ids
  create_nodes_call$label <- TRUE
  create_nodes_call$type <- "lower"


  nodes <- do.call(DiagrammeR::create_nodes, create_nodes_call)


  edges <- DiagrammeR::create_edges(
                from = edges_from,
                to = edges_to,
                rel = "related")

  graph <- DiagrammeR::create_graph(
    nodes_df = nodes,
    edges_df = edges)

  return(graph)
}

