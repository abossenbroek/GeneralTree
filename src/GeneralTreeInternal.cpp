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
  root = root_node;
}

GeneralTreeInternal::GeneralTreeInternal(const GeneralTreeInternal& to_clone)
{
  /* First copy the root. */
  shared_ptr<TreeNode> root_node = make_shared<TreeNode>(to_clone.get_root()->get_key(),
      to_clone.get_root()->get_data());

  insert_node(root_node);
  root = root_node;

  /* Get complete list of children. */
  tree_node_c_sp_vec_sp tree = const_pointer_cast<const TreeNode>(to_clone.get_root())->get_children(true);

  for (auto it = tree->begin(); it != tree->end(); ++it)
    add_node((*it)->get_parent()->get_uid(), (*it)->get_key(), (*it)->get_data());
}

GeneralTreeInternal::GeneralTreeInternal()
{
}

uid
GeneralTreeInternal::add_node(const SEXP& parent_id, const SEXP& child_key,
    const SEXP& child_data)
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

  tree_node_sp child = make_shared<TreeNode>(child_key, child_data);

  /* Add child to parent. */
  parent->add_child(child);

  /* Add child to internal storage. */
  return insert_node(child);
}

uid
GeneralTreeInternal::add_node(const uid& parent_uid, const SEXP& child_key,
    const SEXP& child_data)
{
  /* resolve the uid of the parent so that we can set pointers. */
  tree_node_sp parent;
  try {
    parent = nodes[parent_uid];
  } catch (std::exception &ex) {
    forward_exception_to_r(ex);
  } catch (...) {
    ::Rf_error("c++ exception (unknown reason)");
  }

  tree_node_sp child = make_shared<TreeNode>(child_key, child_data);

  /* Add child to parent. */
  parent->add_child(child);

  /* Add child to internal storage. */
  return insert_node(child);
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

template<typename Container_source, typename Container_dest>
void
get_info(const Container_source& src, Container_dest& dst, const ListFunctor& lf)
{
    transform(begin(*src), end(*src), back_inserter(*dst),
        [&](shared_ptr<const TreeNode> x){ return lf.Process(*x); });
}

SEXP_vec_sp
GeneralTreeInternal::access_tree_node_vec(const SEXP& node_id,
    const AccessFunctor &af,
    const ListFunctor& lf) const
{
  tree_node_c_sp node_found = find_node(node_id);

  SEXP_vec_sp result(new SEXP_vec());
  /* Get the nodes using the access functor. */
  tree_node_c_sp_vec_sp tn_vec = af.tree_accessor(*node_found);

  result->reserve(tn_vec->size());

  get_info(tn_vec, result, lf);

  return result;
}

SEXP_vec_sp
GeneralTreeInternal::get_children_keys(const SEXP& parent_id,
    bool recursive) const
{
  return access_tree_node_vec(parent_id, AccessChildrenFunctor(recursive),
      GetKeyFunctor());
}

SEXP_vec_sp
GeneralTreeInternal::get_children_data(const SEXP& parent_id,
    bool recursive) const
{
  return access_tree_node_vec(parent_id, AccessChildrenFunctor(recursive),
      GetDataFunctor());
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

SEXP_vec_sp
GeneralTreeInternal::get_siblings_keys(const SEXP& node_id) const
{
  return access_tree_node_vec(node_id, AccessSiblingsFunctor(), GetKeyFunctor());
}

SEXP_vec_sp
GeneralTreeInternal::get_siblings_data(const SEXP& node_id) const
{
  return access_tree_node_vec(node_id, AccessSiblingsFunctor(), GetDataFunctor());
}

GeneralTreeInternal::operator SEXP() const
{
  List serialization;

  serialization["root"] = wrap(*root);

  List node_uids(no_init(nodes.size()));
  List node_keys(no_init(nodes.size()));
  List node_data(no_init(nodes.size()));
  List node_parents(no_init(nodes.size()));

  for (int i = 0; i < nodes.size(); ++i) {
    node_uids[i] = nodes.at(i)->get_uid();
    node_keys[i] = nodes.at(i)->get_key();
    node_data[i] = nodes.at(i)->get_data();
    node_parents[i] = nodes.at(i)->get_parent_uid();
  }

  serialization["uids"] = node_uids;
  serialization["keys"] = node_keys;
  serialization["data"] = node_data;
  serialization["parents"] = node_parents;

  return serialization;
}

std::shared_ptr<tree_node_sp_vec>
GeneralTreeInternal::get_branch(const SEXP& node_id)
{
  tree_node_sp node_found = find_node(node_id);

  return node_found->get_branch();
}

tree_node_c_sp_vec_sp
GeneralTreeInternal::get_branch(const SEXP& node_id) const
{
  tree_node_c_sp node_found = find_node(node_id);

  return node_found->get_branch();
}

SEXP_vec_sp
GeneralTreeInternal::get_branch_keys(const SEXP& node_id) const
{
  return access_tree_node_vec(node_id, AccessBranchFunctor(), GetKeyFunctor());
}

SEXP_vec_sp
GeneralTreeInternal::get_branch_data(const SEXP& node_id) const
{
  return access_tree_node_vec(node_id, AccessBranchFunctor(), GetDataFunctor());
}

