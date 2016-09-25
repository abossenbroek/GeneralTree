// [[Rcpp::depends(BH)]]
#include <Rcpp.h>

#include <boost/shared_ptr.hpp>

#include <string>


#include "tree_types.h"
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

  return *lhs == *rhs;
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
get_data(SEXP gti_sexp, SEXP key)
{
  gti_xptr gti(gti_sexp);

  return gti->get_data(key);
}

// [[Rcpp::export]]
std::vector<SEXP>
get_children_keys(SEXP gti_sexp, SEXP parent_id, bool recursive = false)
{
  gti_xptr gti(gti_sexp);
  SEXP_vec_sp c_keys = gti->get_children_keys(parent_id, recursive);

  return(*c_keys);
}

// [[Rcpp::export]]
std::vector<SEXP>
get_children_data(SEXP gti_sexp, SEXP parent_id, bool recursive = false)
{
  gti_xptr gti(gti_sexp);
  SEXP_vec_sp c_sexp = gti->get_children_data(parent_id, recursive);

  return *c_sexp ;
}

// [[Rcpp::export]]
std::vector<SEXP>
get_siblings_keys(SEXP gti_sexp, SEXP node_id)
{
  gti_xptr gti(gti_sexp);
  SEXP_vec_sp c_keys = gti->get_siblings_keys(node_id);

  return(*c_keys);
}

// [[Rcpp::export]]
std::vector<SEXP>
get_siblings_data(SEXP gti_sexp, SEXP node_id)
{
  gti_xptr gti(gti_sexp);
  SEXP_vec_sp c_sexp = gti->get_siblings_data(node_id);

  return *c_sexp ;
}

