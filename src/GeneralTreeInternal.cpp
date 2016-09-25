// [[Rcpp::depends(BH)]]
// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>

#include <memory>
#include <algorithm>
#include <string>

#include "GeneralTreeInternal.h"
#include "key_visitor.h"

using namespace Rcpp;
using namespace std;

GeneralTreeInternal::GeneralTreeInternal(const SEXP& root_id, const SEXP&
    root_data)
{
  /* Create a root node without a child or parent. */
  shared_ptr<TreeNode> root_node = make_shared<TreeNode>(root_id, root_data);

  insert_node(root_node);
}

GeneralTreeInternal::GeneralTreeInternal()
{
}

void
GeneralTreeInternal::add_node(const SEXP& parent_id, const SEXP& child_id,
    const SEXP& data)
{
  /* resolve the uid of the parent so that we can set pointers. */
  tree_node_sp parent;
  try {
    parent = nodes[find_uid(parent_id)];
  } catch (std::exception &ex) {
    forward_exception_to_r(ex);
  } catch (...) {
    ::Rf_error("c++ exception (unknown reason)");
  }

  tree_node_sp child = make_shared<TreeNode>(child_id, data);

  /* Add child to parent. */
  parent->add_child(child);

  /* Add child to internal storage. */
  insert_node(child);
}

uid
GeneralTreeInternal::find_uid(const SEXP& id) const
{
  shared_ptr<tree_key> search_key = tree_key_cast_SEXP(id);

  uid_id_bimap::right_const_iterator id_iter =
    this->uid_to_key.right.find(*search_key);

  if (id_iter == this->uid_to_key.right.end())
    throw std::invalid_argument("Could not find id in tree.");

  return id_iter->second;
}

tree_node_sp
GeneralTreeInternal::find_node(const SEXP& id) const
{
  uid node_uid = find_uid(id);

  return nodes[node_uid];
}

uid
GeneralTreeInternal::get_uid() const
{
  return nodes.size();
}

uid
GeneralTreeInternal::insert_node(tree_node_sp& new_node)
{
  SEXP new_node_key = new_node->get_key();
  shared_ptr<tree_key> search_key = tree_key_cast_SEXP(new_node_key);

  nodes.push_back(new_node);
  int new_uid = nodes.size() - 1;
  new_node->set_uid(new_uid);

  /* Store in a bimap to allow big-oh log(n) search. */
  uid_to_key.insert(uid_id_pair(new_uid, *search_key));

  return new_uid;
}

SEXP
GeneralTreeInternal::get_data(const SEXP& id) const
{
  tree_node_sp node_found = find_node(id);
  return node_found->get_data();
}

bool
GeneralTreeInternal::has_child(const SEXP& id) const
{
  tree_node_sp node_found = find_node(id);
  return node_found->has_left_child();
}

bool
GeneralTreeInternal::has_siblings(const SEXP& id) const
{
  tree_node_sp node_found = find_node(id);
  return node_found->has_siblings();
}

tree_node_sp
GeneralTreeInternal::get_parent(const SEXP& id) const
{
  tree_node_sp node_found = find_node(id);
  return node_found->get_parent();
}

shared_ptr<tree_node_sp_vec>
GeneralTreeInternal::get_children(const SEXP& parent_id, bool recursive)
{
  /* Retrieve the parent node. */
  tree_node_sp parent_node_found = find_node(parent_id);

  return parent_node_found->get_children(recursive);
}


SEXP_vec_sp
GeneralTreeInternal::get_children_keys(const SEXP& parent_id, bool recursive) const
{
  tree_node_c_sp parent_node_found = find_node(parent_id);

  SEXP_vec_sp result(new SEXP_vec());
  tree_node_c_sp_vec_sp children = parent_node_found->get_children(recursive);

  result->reserve(children->size());

  transform(children->begin(), children->end(), back_inserter(*result),
      [](shared_ptr<const TreeNode> x){ return x->get_key(); });

  return result;
}

SEXP_vec_sp
GeneralTreeInternal::get_children_data(const SEXP& parent_id, bool recursive) const
{
  tree_node_c_sp parent_node_found = find_node(parent_id);

  SEXP_vec_sp result(new SEXP_vec());
  tree_node_c_sp_vec_sp children = parent_node_found->get_children(recursive);

  result->reserve(children->size());

  transform(children->begin(), children->end(), back_inserter(*result),
      [](shared_ptr<const TreeNode> x){ return x->get_data(); });

  return result;
}

tree_node_c_sp_vec_sp
GeneralTreeInternal::get_children(const SEXP& parent_id, bool recursive) const
{
  /* Retrieve the parent node. */
  tree_node_c_sp parent_node_found = find_node(parent_id);

  return parent_node_found->get_children(recursive);
}

std::shared_ptr<tree_node_sp_vec>
GeneralTreeInternal::get_siblings(const SEXP& node_id)
{
  /* Retrieve the parent node. */
  tree_node_sp node_found = find_node(node_id);

  return node_found->get_tree_siblings();
}

tree_node_c_sp_vec_sp
GeneralTreeInternal::get_siblings(const SEXP& node_id) const
{
  /* Retrieve the parent node. */
  tree_node_c_sp node_found = find_node(node_id);

  return node_found->get_tree_siblings();
}


