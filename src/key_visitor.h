// [[Rcpp::depends(BH)]]
// [[Rcpp::plugins(cpp11)]]
#ifndef _KEY_VISITOR_H_
#define _KEY_VISITOR_H_

#include <algorithm>
#include <stdexcept>

#include <boost/variant.hpp>

#include <Rcpp.h>

using namespace Rcpp;

#include "tree_types.h"
#include "GeneralTreeInternal.h"

class key_visitor
  : public boost::static_visitor<SEXP>
{
public:
  SEXP operator()(int& i) const
  {
    return wrap(i);
  }

  SEXP operator()(std::string& s) const
  {
    return wrap(s);
  }

  SEXP operator()(double& d) const
  {
    return wrap(d);
  }
};

class key_int_visitor
  : public boost::static_visitor<int>
{
public:
  int operator()(int& i) const
  {
    return i;
  }

  int operator()(std::string& s) const
  {
    throw std::invalid_argument("Could not cast int to string");
    return 0;
  }

  int operator()(double& d) const
  {
    throw std::invalid_argument("Could not cast double to string");
    return 0;
  }
};


/*
 * This function stores an mapping from key to SEXP.
 *
 * The purpose of this function is that at a later stage we can call the
 * right visitor on our variant to return the intended type from the tree.
 */
static std::shared_ptr<tree_key>
add_mapping(uid& key, SEXP& value) {
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
      // Rcerr << "found value: " << TYPEOF(value) << std::endl;
      stop("add_mapping: incompatible SEXP encoutered. Currently only int,"
          " numeric and string are supported.");
    }
  }

  return nullptr;
}

static std::shared_ptr<tree_key>
tree_key_cast_SEXP(SEXP& key)
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

static shared_ptr_SEXP_vec
tree_key_cast_SEXP_vec(shared_ptr_key_vec vec)
{
  shared_ptr_SEXP_vec result(new std::vector<SEXP>());
  key_visitor* v = new key_visitor();

  result->reserve(vec->size());

  transform(vec->begin(), vec->end(), back_inserter(*result),
      [&](tree_key& x){ return boost::apply_visitor(*v, x); } );

  delete(v);

  return result;
}



#endif // _KEY_VISITOR_H_
