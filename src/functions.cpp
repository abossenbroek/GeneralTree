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

// [[Rcpp::export]]
std::vector<SEXP>
get_children_keys(SEXP gti_sexp, SEXP parent_id)
{
  gti_xptr gti(gti_sexp);
  uid parent_uid = gti->find_uid(parent_id);
  shared_ptr_key_vec c_keys = gti->get_children_keys(parent_uid);

  std::vector<SEXP> c_sexp(*tree_key_cast_SEXP_vec(c_keys));

  return(c_sexp);
}

// [[Rcpp::export]]
std::vector<SEXP>
get_siblings_keys(SEXP gti_sexp, SEXP node_id)
{
  gti_xptr gti(gti_sexp);
  uid node_uid = gti->find_uid(node_id);
  shared_ptr_key_vec s_keys = gti->get_siblings_keys(node_uid);

  std::vector<SEXP> s_sexp(*tree_key_cast_SEXP_vec(s_keys));

  return(s_sexp);
}

// [[Rcpp::export]]
std::vector<SEXP>
get_children_values(SEXP gti_sexp, SEXP parent_id)
{
  gti_xptr gti(gti_sexp);
  uid parent_uid = gti->find_uid(parent_id);
  shared_ptr_SEXP_vec c_sexp = gti->get_children_values(parent_uid);

  return *c_sexp ;
}

// [[Rcpp::export]]
std::vector<SEXP>
get_siblings_values(SEXP gti_sexp, SEXP node_id)
{
  gti_xptr gti(gti_sexp);
  uid node_uid = gti->find_uid(node_id);
  shared_ptr_SEXP_vec s_sexp = gti->get_siblings_values(node_uid);

  return *s_sexp ;
}

// [[Rcpp::export]]
std::vector<SEXP>
get_branch_keys(SEXP gti_sexp, SEXP parent_id, bool recursive)
{
  gti_xptr gti(gti_sexp);

  uid parent_uid = gti->find_uid(parent_id);
  shared_ptr_SEXP_vec result(new SEXP_vec());

  if (!gti->has_child(parent_uid)) {
    return *result;
  }

  shared_ptr_uid_vec branch_uids(gti->branch_uid_to_list(parent_uid, recursive));

  result = gti->get_value(branch_uids);

  return *result;

}

// [[Rcpp::export]]
bool
has_child(SEXP gti_sexp, SEXP node_id)
{
  gti_xptr gti(gti_sexp);

  uid node_uid = gti->find_uid(node_id);

  return gti->has_child(node_uid);
}
