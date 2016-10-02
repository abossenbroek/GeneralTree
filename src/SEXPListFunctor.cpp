#include <Rcpp.h>

#include "SEXPListFunctor.h"
#include "TreeNode.h"

using namespace Rcpp;

SEXP
GetDataFunctor::Process(const TreeNode& tn) const {
  return tn.get_data();
}

SEXP
GetKeyFunctor::Process(const TreeNode& tn) const {
  return tn.get_key();
}

SEXP
GetUIDFunctor::Process(const TreeNode& tn) const {
  return wrap(tn.get_uid());
}

SEXP
ApplyFunctor::Process(const TreeNode& tn) const {
    return wrap(f(as<TreeNode>(tn)));
}


