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
#' @export
benchmarkGeneralTree <- function (depth = 4,
                                  number_of_splits = c(2, 4, 16),
                                  fraction_to_search = 0.5,
                                  times = 100L) {
# nocov start


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
    ids_to_search <- sample(all_ids[1 : ceiling(length(all_ids) *
                                                fraction_to_search)])
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


    raw_results <- c(raw_results, create_res, search_res, native_iter_res)
  }

  result <- data.frame(row.names = names(raw_results),
                        mean = sapply(raw_results, function(x) x$mean),
                        median = sapply(raw_results, function(x) x$median))


  return (result)
# nocov end
}
