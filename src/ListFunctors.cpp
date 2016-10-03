#include <iostream>

#include <Rcpp.h>

#include "ListFunctors.h"
#include "TreeNode.h"

using namespace Rcpp;

SEXP
SEXPGetDataFunctor::Process(const TreeNode& tn) const {
  return tn.get_data();
}

SEXP
SEXPGetKeyFunctor::Process(const TreeNode& tn) const {
  return tn.get_key();
}

SEXP
SEXPGetUIDFunctor::Process(const TreeNode& tn) const {
  return wrap(tn.get_uid());
}

SEXP
SEXPApplyFunctor::Process(const TreeNode& tn) const {
  SEXP res;
  try {
    res = wrap(f(as<SEXP>(tn)));
  } catch(std::exception &ex) {
    forward_exception_to_r(ex);
  } catch (...) {
    ::Rf_error("c++ exception (unknown reason)");
  }

  return res;
}


