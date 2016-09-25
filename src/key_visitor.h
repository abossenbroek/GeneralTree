// [[Rcpp::depends(BH)]]
// [[Rcpp::plugins(cpp11)]]
#ifndef _KEY_VISITOR_H_
#define _KEY_VISITOR_H_

#include <algorithm>
#include <stdexcept>
#include <string>

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

class key_string_visitor
  : public boost::static_visitor<std::string>
{
public:
  std::string operator()(int& i) const
  {
    return std::to_string(i);
  }

  std::string operator()(std::string& s) const
  {
    return s;
  }

  std::string operator()(double& d) const
  {
    return std::to_string(d);
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

static tree_key_sp
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
      stop("add_mapping: incompatible SEXP encoutered. Currently only int,"
          " numeric and string are supported.");
    }
  }

  return nullptr;
}

#endif // _KEY_VISITOR_H_
