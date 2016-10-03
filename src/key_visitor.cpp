// [[Rcpp::depends(BH)]]
// [[Rcpp::plugins(cpp11)]]

#include <memory>

#include <boost/variant.hpp>

#include <Rcpp.h>

#include "key_visitor.h"
#include "tree_types.h"

using namespace Rcpp;

tree_key_sp
tree_key_cast_SEXP(const SEXP& key)
{
  switch (TYPEOF(key)) {
    case REALSXP: {
        std::shared_ptr<tree_key> result(new tree_key(as<NumericVector>(key)[0]));
        return result;
    }

    case INTSXP: {
       std::shared_ptr<tree_key> result(new tree_key(as<IntegerVector>(key)[0]));
       return result;
    }

    case STRSXP: {
       std::shared_ptr<tree_key> result(new tree_key(as<String>(key)));
       return result;
    }

    default: {
      stop("tree_key_cast_SEXP: incompatible SEXP encoutered. Currently only int,"
          " numeric and string are supported.");
    }
  }

  return nullptr;
}

