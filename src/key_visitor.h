// [[Rcpp::depends(BH)]]
// [[Rcpp::plugins(cpp11)]]
#ifndef _KEY_VISITOR_H_
#define _KEY_VISITOR_H_

#include <boost/variant.hpp>

#include <Rcpp.h>

using namespace Rcpp;

#include "GeneralTreeInternal.h"


/*
 * This function stores an mapping from key to SEXP.
 *
 * The purpose of this function is that at a later stage we can call the
 * right visitor on our variant to return the intended type from the tree.
 */
static std::shared_ptr<tree_key> add_mapping(uid& key, SEXP& value) {
  switch (TYPEOF(value)) {
    case REALSXP: {
        std::shared_ptr<tree_key> result(new tree_key(as<NumericVector>(value)[0]));
        return result;
    }

    case INTSXP: {
       std::shared_ptr<tree_key> result(new tree_key(as<IntegerVector>(value)[0]));
       return result;
    }

    case STRSXP: {
       std::shared_ptr<tree_key> result(new tree_key(as<String>(value)));
       return result;
    }

    default: {
      stop("add_mapping: incompatible SEXP encoutered. Currently only int,"
          " numeric and string are supported.");
    }
  }

  return nullptr;
}

static std::shared_ptr<tree_key> tree_key_cast_SEXP(SEXP& key)
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
      stop("add_mapping: incompatible SEXP encoutered. Currently only int,"
          " numeric and string are supported.");
    }
  }

  return nullptr;
}

class key_visitor
  : public boost::static_visitor<SEXP>
{
public:
  SEXP operator()(int& i) const
  {
    wrap(i);
  }

  SEXP operator()(std::string& s) const
  {
    wrap(s);
  }

  SEXP operator()(double& d) const
  {
    wrap(d);
  }
};

#endif // _KEY_VISITOR_H_
