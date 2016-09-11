// [[Rcpp::depends(BH)]]
#include <Rcpp.h>

#include <boost/shared_ptr.hpp>

#include <string>


#include "GeneralTreeInternal.h"

using namespace Rcpp;
using namespace std;


// [[Rcpp::export]]
SEXP
initialize_tree(SEXP id, SEXP data)
{
  GeneralTreeInternal* gti = new GeneralTreeInternal(id, data);
  gti_xptr p(gti, true);

  return p;
}

// [[Rcpp::export]]
SEXP
pass_gti_xptr(SEXP gti)
{
  gti_xptr p(gti);

  return p;
}

// [[Rcpp::export]]
LogicalVector
cmp(SEXP gti_lhs, SEXP gti_rhs)
{
  gti_xptr lhs(gti_lhs);
  gti_xptr rhs(gti_rhs);

  return lhs->cmp(*(GeneralTreeInternal*)rhs);
}

// [[Rcpp::export]]
SEXP
add_node(SEXP gti_sexp, SEXP parent_id, SEXP id, SEXP data)
{
  gti_xptr gti(gti_sexp);
  gti->add_node(parent_id, id, data);

  return gti;
}

// [[Rcpp::export]]
SEXP
get_value(SEXP gti_sexp, SEXP key)
{
  gti_xptr gti(gti_sexp);

  return gti->get_value(key);
}


