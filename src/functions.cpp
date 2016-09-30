// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>

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
deserialize_tree(SEXP tree)
{
  GeneralTreeInternal* gti = new GeneralTreeInternal(tree);
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
bool
cmp_gti(SEXP gti_lhs, SEXP gti_rhs)
{
  gti_xptr lhs(gti_lhs);
  gti_xptr rhs(gti_rhs);

  return *lhs == *rhs;
}

// [[Rcpp::export]]
bool
cmp_gti_mem(SEXP gti_lhs, SEXP gti_rhs)
{
  gti_xptr lhs(gti_lhs);
  gti_xptr rhs(gti_rhs);

  return &(*lhs) == &(*rhs);
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
int
delete_node(SEXP gti_sexp, SEXP to_delete)
{
  gti_xptr gti(gti_sexp);
  return (int)gti->delete_node(to_delete);
}

// [[Rcpp::export]]
int
delete_node_at_ref(SEXP gti_sexp)
{
  gti_xptr gti(gti_sexp);
  return (int)gti->delete_node();
}

// [[Rcpp::export]]
SEXP
add_child(SEXP gti_sexp, SEXP id, SEXP data)
{
  gti_xptr gti(gti_sexp);
  gti->add_child(id, data);

  return gti;
}

// [[Rcpp::export]]
SEXP
add_sibling(SEXP gti_sexp, SEXP id, SEXP data)
{
  gti_xptr gti(gti_sexp);
  gti->add_sibling(id, data);

  return gti;
}

// [[Rcpp::export]]
SEXP
travel_up(SEXP gti_sexp)
{
  gti_xptr gti(gti_sexp);
  gti->travel_up();

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
SEXP
set_key(SEXP gti_sexp, SEXP new_key)
{
  gti_xptr gti(gti_sexp);
  gti->set_key(new_key);

  return gti;
}


// [[Rcpp::export]]
SEXP
set_data(SEXP gti_sexp, SEXP new_data)
{
  gti_xptr gti(gti_sexp);
  gti->set_data(new_data);

  return gti;
}

// [[Rcpp::export]]
SEXP
copy(SEXP gti_sexp, int new_uid)
{
  gti_xptr gti(gti_sexp);
  GeneralTreeInternal* new_tree = new GeneralTreeInternal(*(gti.get()), (uid)new_uid);
  gti_xptr p(new_tree, true);

  return p;
}

// [[Rcpp::export]]
SEXP
serialize(SEXP gti_sexp)
{
  gti_xptr gti(gti_sexp);

  return wrap(*gti);
}

// [[Rcpp::export]]
std::vector<SEXP>
get_children_keys(SEXP gti_sexp, SEXP parent_id, bool recursive = false)
{
  gti_xptr gti(gti_sexp);
  SEXP_vec_sp c_keys = gti->get_children_keys(parent_id, recursive);

  return *c_keys;
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

  return *c_keys;
}

// [[Rcpp::export]]
std::vector<SEXP>
get_siblings_data(SEXP gti_sexp, SEXP node_id)
{
  gti_xptr gti(gti_sexp);
  SEXP_vec_sp c_sexp = gti->get_siblings_data(node_id);

  return *c_sexp ;
}



// [[Rcpp::export]]
std::vector<SEXP>
get_branch_data(SEXP gti_sexp, SEXP node_id)
{
  gti_xptr gti(gti_sexp);
  SEXP_vec_sp c_keys = gti->get_branch_data(node_id);

  return *c_keys;
}

// [[Rcpp::export]]
std::vector<SEXP>
get_branch_keys(SEXP gti_sexp, SEXP node_id)
{
  gti_xptr gti(gti_sexp);
  SEXP_vec_sp c_keys = gti->get_branch_keys(node_id);

  return *c_keys;
}

// [[Rcpp::export]]
std::vector<SEXP>
get_leafs_data(SEXP gti_sexp, SEXP node_id)
{
  gti_xptr gti(gti_sexp);
  SEXP_vec_sp c_keys = gti->get_leafs_data(node_id);

  return *c_keys;
}

// [[Rcpp::export]]
std::vector<SEXP>
get_leafs_keys(SEXP gti_sexp, SEXP node_id)
{
  gti_xptr gti(gti_sexp);
  SEXP_vec_sp c_keys = gti->get_leafs_keys(node_id);

  return *c_keys;
}

// [[Rcpp::export]]
std::vector<SEXP>
get_children_keys_at_ref(SEXP gti_sexp, bool recursive = false)
{
  gti_xptr gti(gti_sexp);
  SEXP_vec_sp c_keys = gti->get_children_keys(recursive);

  return *c_keys;
}

// [[Rcpp::export]]
std::vector<SEXP>
get_children_data_at_ref(SEXP gti_sexp, bool recursive = false)
{
  gti_xptr gti(gti_sexp);
  SEXP_vec_sp c_sexp = gti->get_children_data(recursive);

  return *c_sexp ;
}

// [[Rcpp::export]]
std::vector<SEXP>
get_siblings_keys_at_ref(SEXP gti_sexp)
{
  gti_xptr gti(gti_sexp);
  SEXP_vec_sp c_keys = gti->get_siblings_keys();

  return *c_keys;
}

// [[Rcpp::export]]
std::vector<SEXP>
get_siblings_data_at_ref(SEXP gti_sexp)
{
  gti_xptr gti(gti_sexp);
  SEXP_vec_sp c_sexp = gti->get_siblings_data();

  return *c_sexp ;
}

// [[Rcpp::export]]
std::vector<SEXP>
get_branch_data_at_ref(SEXP gti_sexp)
{
  gti_xptr gti(gti_sexp);
  SEXP_vec_sp c_keys = gti->get_branch_data();

  return *c_keys;
}

// [[Rcpp::export]]
std::vector<SEXP>
get_branch_keys_at_ref(SEXP gti_sexp)
{
  gti_xptr gti(gti_sexp);
  SEXP_vec_sp c_keys = gti->get_branch_keys();

  return *c_keys;
}

// [[Rcpp::export]]
std::vector<SEXP>
get_leafs_data_at_ref(SEXP gti_sexp)
{
  gti_xptr gti(gti_sexp);
  SEXP_vec_sp c_keys = gti->get_leafs_data();

  return *c_keys;
}

// [[Rcpp::export]]
std::vector<SEXP>
get_leafs_keys_at_ref(SEXP gti_sexp)
{
  gti_xptr gti(gti_sexp);
  SEXP_vec_sp c_keys = gti->get_leafs_keys();

  return *c_keys;
}

// [[Rcpp::export]]
int
get_tree_depth_at_ref(SEXP gti_sexp)
{
  gti_xptr gti(gti_sexp);

  return gti->tree_depth_at_ref();
}

// [[Rcpp::export]]
int
get_tree_depth(SEXP gti_sexp)
{
  gti_xptr gti(gti_sexp);

  return gti->tree_depth();
}

// [[Rcpp::export]]
bool
have_siblings_at_ref(SEXP gti_sexp)
{
  gti_xptr gti(gti_sexp);

  return gti->have_siblings();
}

// [[Rcpp::export]]
SEXP
change_ref(SEXP gti_sexp, int key)
{
  gti_xptr gti(gti_sexp);
  gti->change_ref((uid)key);

  return gti;
}

// [[Rcpp::export]]
int
find_uid(SEXP gti_sexp, SEXP key)
{
  gti_xptr gti(gti_sexp);

  return (int)gti->find_uid(key);
}

// [[Rcpp::export]]
bool
is_last_sibling_at_ref(SEXP gti_sexp)
{
  gti_xptr gti(gti_sexp);
  return gti->is_last_sibling();
}

// [[Rcpp::export]]
bool
is_last_sibling(SEXP gti_sexp, SEXP key)
{
  gti_xptr gti(gti_sexp);
  return gti->is_last_sibling(key);
}

// [[Rcpp::export]]
SEXP
get_data_at_ref(SEXP gti_sexp)
{
  gti_xptr gti(gti_sexp);
  return gti->get_data();
}

// [[Rcpp::export]]
SEXP
get_root(SEXP gti_sexp)
{
  gti_xptr gti(gti_sexp);

  return wrap(*gti->get_root());
}

// [[Rcpp::export]]
SEXP
get_parent_at_ref(SEXP gti_sexp)
{
  gti_xptr gti(gti_sexp);

  return wrap(*gti->get_parent());
}

// [[Rcpp::export]]
SEXP
get_ref(SEXP gti_sexp)
{
  gti_xptr gti(gti_sexp);

  return wrap(*gti->get_ref());
}
