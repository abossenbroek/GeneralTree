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

#' Benchmark function that measures the speed of the tree implementation.
#'
#' The current implementation tests tree creation, search and iteration and
#' returns the results in a data frame.
#'
#' @param depth the depth of the test trees.
#' @param number_of_splits the number of splits that should be benchmarked.
#'                         This can either be a single number or vector.
#' @param fraction_to_search the fraction ids in the tree that should be
#'                           searched.
#' @param times           The number of times a benchmark should be run.
#' @return a data frame with the mean and median of each test.
#' @export
benchmarkGeneralTree <- function (depth = 4,
                                  number_of_splits = c(2, 4, 16),
                                  fraction_to_search = 0.5,
                                  times = 100L) {

  if (!is.numeric(depth))
    stop("depth should be numeric")
  else if (depth < 1)
    stop("depth should be larger than 1")

  if (!all(is.numeric(number_of_splits)))
    stop("number_of_splits should be numeric or a vector of numerics")

  if (!is.numeric(fraction_to_search))
    stop("fraction_to_search should be numeric")
  else if (fraction_to_search < 0.01 || fraction_to_search > 1)
    stop("fraction_to_search should be within the 0.01 and 1 range")

  if (!is.numeric(times))
    stop("times should be numeric")
  else if (times < 1)
    stop("times should be strict positive")

  # Verify whether microbenchmark is installed.
  if (!("microbenchmark" %in% installed.packages()[, "Package"]))
    stop("Could not perform benchmark without the microbenchmark package.")

  create_tree <- function(number_of_childeren = 2) {
    idx <- 0
    tree <- GeneralTree$new(id = idx, data = idx)
    for (i in 1 : (depth - 1)) {
      parents <- sapply(tree$branchToList(), function(x) {
                          if (!x$have_child)
                            return (x$id)
                          else
                            return (NULL)
                        })
      parents <- unlist(Filter(Negate(is.null), parents))
      for (p in parents) {
        for (n in 1 : number_of_childeren) {
          idx <- idx + 1
          tree$addNode(parent = p, id = idx, data = idx)
        }
      }
    }
    return(tree)
  }

  search_tree <- function(tree) {
    all_ids <- sapply(tree$branchToList(), function(x) x$id)
    ids_to_search <- all_ids[1 : ceiling(length(all_ids) *
                                                fraction_to_search)]
    if (length(ids_to_search) > 2)
        ids_to_search <- sample(ids_to_search)
    for (i in ids_to_search)
      tree$searchData(i)
  }

  native_iterate <- function(tree) {
    i <- tree$iterator()
    ids_in_tree <- c()
    while (!is.null(i)) {
      ids_in_tree <- c(ids_in_tree, i$id)
      i <- tryCatch(i$nextElem(), error = function(e) NULL)
    }
  }

  foreach_iterate <- function(tree) {
      return(NULL)
  }

  if (all(c("foreach", "iterator") %in% installed.packages()[, "Package"])) {
      foreach_iterate <- function(tree) {
          itx <- iterator::iter(tree, by = "id")
          ids_in_tree <- foreach::foreach(i = itx, .combine = c) %do% c(i)
      }
  }


  raw_results <- list()

  set.seed(10)

  for (splits in number_of_splits) {
    # Create a perfect binary tree.
    create_res <- list(summary(microbenchmark::microbenchmark({
      create_tree(number_of_childeren = splits)
    }), times = times))
    names(create_res) <- paste0(splits, "-create")

    tree <- create_tree(number_of_childeren = splits)

    search_res <- list(summary(microbenchmark::microbenchmark({
      search_tree(tree)
    }), times = times))
    names(search_res) <-  paste0(splits, "-search")

    native_iter_res <- list(summary(microbenchmark::microbenchmark({
      native_iterate(tree)
    }), times = times))
    names(native_iter_res) <-  paste0(splits, "-native-iter")

    foreach_iter_res <- list(summary(microbenchmark::microbenchmark({
      foreach_iterate(tree)
    }), times = times))
    names(foreach_iter_res) <-  paste0(splits, "-foreach-iter")

    casting_df_res <- list(summary(microbenchmark::microbenchmark({
      as.data.frame(tree)
    }), times = times))
    names(casting_df_res) <-  paste0(splits, "-casting-df")

    tree_in_df <- as.data.frame(tree)
    casting_gt_df_res <- list(summary(microbenchmark::microbenchmark({
      as.GeneralTree(tree_in_df)
    }), times = times))
    names(casting_gt_df_res) <-  paste0(splits, "-casting-gt-df")

    raw_results <- c(raw_results, create_res, search_res, native_iter_res,
                     foreach_iter_res, casting_df_res, casting_gt_df_res)

    rm(list = "tree")
  }

  result <- data.frame(row.names = names(raw_results),
                        mean = sapply(raw_results, function(x) x$mean),
                        median = sapply(raw_results, function(x) x$median))


  return (result)
}
