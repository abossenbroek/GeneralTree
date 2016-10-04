#include <iostream>

#include <Rcpp.h>

#include "tree_types.h"
#include "ListFunctors.h"
#include "TreeNode.h"

using namespace Rcpp;

uid
GetUIDFunctor::Process(const TreeNode& tn) const {
  return tn.get_uid();
}

SEXP
SEXPApplyFunctor::Process(const TreeNode& tn) const {
  return wrap(f(as<SEXP>(tn)));
}


