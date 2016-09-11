
| CI Tool        | Status for Master     | Status for Develop  |
|:---------------|:----------------------|:--------------------|
| Travis         |[![Build Status](https://travis-ci.org/abossenbroek/GeneralTree.svg?branch=master)](https://travis-ci.org/abossenbroek/GeneralTree) | [![Build Status](https://travis-ci.org/abossenbroek/GeneralTree.svg?branch=develop)](https://travis-ci.org/abossenbroek/GeneralTree) |
| AppVeyor       |[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/abossenbroek/GeneralTree?branch=develop&svg=true)](https://ci.appveyor.com/project/abossenbroek/GeneralTree) | [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/abossenbroek/GeneralTree?branch=master&svg=true)](https://ci.appveyor.com/project/abossenbroek/GeneralTree) |
| Codecov        |[![Coverage Status](https://img.shields.io/codecov/c/github/abossenbroek/GeneralTree/master.svg)](https://codecov.io/github/abossenbroek/GeneralTree?branch=master) | [![Coverage Status](https://img.shields.io/codecov/c/github/abossenbroek/GeneralTree/develop.svg)](https://codecov.io/github/abossenbroek/GeneralTree?branch=develop) |
| Coveralls     |[![Coverage Status](https://coveralls.io/repos/github/abossenbroek/GeneralTree/badge.svg?branch=master)](https://coveralls.io/github/abossenbroek/GeneralTree?branch=master) | [![Coverage Status](https://coveralls.io/repos/github/abossenbroek/GeneralTree/badge.svg?branch=develop)](https://coveralls.io/github/abossenbroek/GeneralTree?branch=develop) |

[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html) [![CRAN](http://www.r-pkg.org/badges/version/GeneralTree)](http://cran.r-project.org/package=GeneralTree)[![Downloads](http://cranlogs.r-pkg.org/badges/GeneralTree?color=brightgreen)](http://www.r-pkg.org/pkg/GeneralTree)

# GeneralTree
This R package allows you to create trees with an arbitrary number of child
nodes per parent node. It includes an depth first iterator, a function to plot
the tree and a function to print the tree.

The current main benefit is that it allows to convert a R parsed object to a
tree.

## Example

### General use
```{r}
require(GeneralTree)
# Initialize the tree.
tree <- GeneralTree$new('root', '1.1')

# Add nodes.
tree$addNode('root', 'child1', '2.1')
tree$addNode('root', 'child2', '2.2')
tree$addNode('root', 'child3', '2.3')

# Print the tree
tree
```

The output would be:
```
root : 1.1 --> child1 : 2.1
           |-> child2 : 2.2
           \-> child3 : 2.3
```

### Iteration
There are two ways to iterate through the tree depth first. The first uses an
internal mechanism whereas the second allows the data structure to be hooked in
the foreach and iterator packages.

#### Internal
The benefit of this approach is that you do not require dependencies on foreach
and iterator.
```{r}
i <- tree$iterator()
while (!is.null(i)) {
    print(i$id)
    i <- tryCatch(i$nextElem(), error = function(e) NULL)
}
```

#### Foreach
Using the foreach and iterator packages permits you to write shorter code, as
the following example shows:

```{r}
require(iterators)
require(foreach)
itx <- iter(tree, by = "id")
numbers_in_tree <- foreach(i = itx, .combine = c) %do% c(i)
```

_Note that the package has not yet been tested in a parallel environment._

# How to install
The easiest way to install the package is by means of the devtools,

```{r}
require(codetools)
install_github('GeneralTree', username = 'abossenbroek')
```

# License
The GeneralTree package is licensed under the GPL V2.0 license.

# Code of conduct
Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to abide by
its terms.
