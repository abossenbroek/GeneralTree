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
static std::shared_ptr<tree_key> add_mapping(uid& key, SEXP& value,
    uid_to_SEXP_map& type_mapping) {
  type_mapping.insert(uid_SEXP_pair(key, value));

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

/*
 * Delete a mapping from the table.
 */
static void delete_mapping(uid& key, uid_to_SEXP_map& type_mapping) {
  uid_to_SEXP_map::iterator uid_it = type_mapping.find(key);

  if (uid_it != type_mapping.end())
    type_mapping.erase(uid_it);
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
