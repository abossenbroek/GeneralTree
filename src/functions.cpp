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
add_node(SEXP gti_sexp, SEXP parent_key, SEXP id, SEXP data)
{
  gti_xptr gti(gti_sexp);
  gti->add_node(parent_key, id, data);

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

// [[Rcpp::export]]
SEXP
update_key_at_ref(SEXP gti_sexp, SEXP new_key)
{
  gti_xptr gti(gti_sexp);

  return wrap(gti->update_key(new_key));
}

// [[Rcpp::export]]
SEXP
update_key(SEXP gti_sexp, SEXP old_key, SEXP new_key)
{
  gti_xptr gti(gti_sexp);

  return wrap(gti->update_key(old_key, new_key));
}

// [[Rcpp::export]]
SEXP
update_data_at_ref(SEXP gti_sexp, SEXP new_data)
{
  gti_xptr gti(gti_sexp);

  return wrap(gti->update_data(new_data));
}

// [[Rcpp::export]]
SEXP
update_data(SEXP gti_sexp, SEXP key, SEXP new_data)
{
  gti_xptr gti(gti_sexp);

  return wrap(gti->update_data(key, new_data));
}

// [[Rcpp::export]]
std::vector<SEXP>
apply_on_branch(SEXP gti_sexp, Function& f)
{
  gti_xptr gti(gti_sexp);
  SEXP_vec_sp c_res = gti->apply_branch(f);

  return *c_res;
}

// [[Rcpp::export]]
std::vector<SEXP>
apply_on_branch_at_ref(SEXP& gti_sexp, SEXP& key, Function& f)
{
  gti_xptr gti(gti_sexp);
  SEXP_vec_sp c_res = gti->apply_branch(key, f);

  return *c_res;
}

// [[Rcpp::export]]
List
get_children(SEXP& gti_sexp, SEXP& key, bool recursive = false)
{
  gti_xptr gti(gti_sexp);
  tree_node_c_sp_vec_sp c_res = gti->get_children(key, recursive);

  return wrap(*c_res);
}

// [[Rcpp::export]]
List
get_branch(SEXP& gti_sexp, SEXP& key)
{
  gti_xptr gti(gti_sexp);
  tree_node_c_sp_vec_sp c_res = gti->get_branch(key);

  return wrap(*c_res);
}

// [[Rcpp::export]]
List
get_leafs(SEXP& gti_sexp, SEXP& key)
{
  gti_xptr gti(gti_sexp);
  tree_node_c_sp_vec_sp c_res = gti->get_leafs(key);

  return wrap(*c_res);
}

// [[Rcpp::export]]
List
get_siblings(SEXP& gti_sexp, SEXP& key)
{
  gti_xptr gti(gti_sexp);
  tree_node_c_sp_vec_sp c_res = gti->get_siblings(key);

  return wrap(*c_res);
}

// [[Rcpp::export]]
List
get_children_at_ref(SEXP& gti_sexp, bool recursive = false)
{
  gti_xptr gti(gti_sexp);
  tree_node_c_sp_vec_sp c_res = gti->get_children(recursive);

  return wrap(*c_res);
}

// [[Rcpp::export]]
List
get_branch_at_ref(SEXP& gti_sexp)
{
  gti_xptr gti(gti_sexp);
  tree_node_c_sp_vec_sp c_res = gti->get_branch();

  return wrap(*c_res);
}

// [[Rcpp::export]]
List
get_leafs_at_ref(SEXP& gti_sexp)
{
  gti_xptr gti(gti_sexp);
  tree_node_c_sp_vec_sp c_res = gti->get_leafs();

  return wrap(*c_res);
}

// [[Rcpp::export]]
List
get_siblings_at_ref(SEXP& gti_sexp)
{
  gti_xptr gti(gti_sexp);
  tree_node_c_sp_vec_sp c_res = gti->get_siblings();

  return wrap(*c_res);
}
