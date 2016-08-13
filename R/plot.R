#' Plot a GeneralTree object.
#' @export
plot.GeneralTree <- function(obj, ...) {
  tree_grGraph <- generate_grViz(obj, ...)

  DiagrammeR::grViz(tree_grGraph, ...)
  invisible(tree_grGraph)
}

generate_grViz <- function(obj, what = c('id', 'data'), ...) {
  i <- tree$iterator()
  what <- match.arg(what, several.ok = TRUE)
  get_id <- any('id' %in% what)
  get_data <- any('data' %in% what)

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
    node_id <- ids

  if (xor(get_id, get_data)) {
  nodes <- DiagrammeR::create_nodes(
                nodes = node_id,
                label = TRUE,
                type = 'lower',
                style = 'filled',
                color = 'aqua',
                shape = 'circle')
  } else {
    nodes <- DiagrammeR::create_nodes(
                nodes = node_id,
                label = FALSE,
                type = 'lower',
                style = 'filled',
                color = 'aqua',
                shape = 'circle',
                data = paste(ids, data, sep = '|'))
  }

  edges <- DiagrammeR::create_edges(
                from = edges_from,
                to = edges_to,
                rel = 'related')

  graph <- DiagrammeR::create_graph(
    nodes_df = nodes,
    edges_df = edges)

  return(graph)
}

