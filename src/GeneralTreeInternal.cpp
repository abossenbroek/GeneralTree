// [[Rcpp::depends(BH)]]
// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>

#include <memory>
#include <algorithm>
#include <string>
#include <limits>
#include <map>

#if defined(__clang__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wredeclared-class-member"
#endif

#include <boost/variant.hpp>

#include <boost/bimap.hpp>
#include <boost/bimap/multiset_of.hpp>
#include <boost/bimap/support/lambda.hpp>

#if defined(__clang__)
# pragma clang diagnostic pop
#endif

#include "GeneralTreeInternal.h"
#include "key_visitor.h"

using namespace Rcpp;
using namespace std;
using namespace boost::bimaps;

GeneralTreeInternal::GeneralTreeInternal(const SEXP& root_id,
    const SEXP& root_data)
{
  /* Create a root node without a child or parent. */
  shared_ptr<TreeNode> root_node = make_shared<TreeNode>(root_id, root_data);

  internal_storage_insert(root_node);
  root = root_node;
  last_ref_node = root_node;
}

GeneralTreeInternal::GeneralTreeInternal(GeneralTreeInternal& to_clone, const uid& new_root_uid)
{
  tree_node_sp new_root;

  try {
    new_root = to_clone.get_nodes()->at(new_root_uid);
  } catch (std::invalid_argument &ex) {
    throw std::invalid_argument("GeneralTreeInternal: Could not find new_root_uid in tree.");
  } catch (std::exception &ex) {
    forward_exception_to_r(ex);
  } catch (...) {
    ::Rf_error("c++ exception (unknown reason)");
  }

  /* First copy the root. */
  shared_ptr<TreeNode> root_node = make_shared<TreeNode>(*new_root);

  /*  Assign the root of this object to newly created root. */
  root = root_node;

  /* Add the root to the internal storage. */
  internal_storage_insert(root_node);

  /* We may need to copy only a subset of the tree. The complexity that arises
   * when doing this is that the original object will contain parent uids that
   * will not exist in the copy. */

  /*  Store the current ref of the object. */
  uid old_ref = to_clone.get_ref()->get_uid();

  /*  Change the ref to the uid of the new root. */
  to_clone.change_ref(new_root_uid);

  /* Get complete list of children. */
  tree_node_c_sp_vec_sp tree = const_pointer_cast<const TreeNode>(new_root)->get_children(true);

  /* Retrieve the nodes that are in the branch. */
  SEXP_vec_sp branch_uids = to_clone.get_branch_uids();

  /* Convert it to uid vector. */
  map<uid, uid> uid_mapping;
  uid uid_offset = numeric_limits<uid>::max();

  /* Convert the SEXP to an uid and determine the value of the lowest uid since
   * that will be the offset of the new root and finally add the uid to the
   * mapping table. */
  for (int i = 0; i < branch_uids->size(); ++i) {
    SEXP old_uid = branch_uids->at(i);
    uid retrieved_uid = Rcpp::as<int >(old_uid);
    uid_offset = min(retrieved_uid, uid_offset);
    uid_mapping.insert(pair<uid, uid>(retrieved_uid, retrieved_uid));
  }

  /* Adjust the mapping table with the offset.  */
  for (auto &it : uid_mapping)
    it.second -= uid_offset;

  /* Add each node with new new uid. */
  for (auto it = tree->begin(); it != tree->end(); ++it) {
    if (*it == root)
      continue;

    /* Experimental, use a mapping table to determine the parent of each node
     * in the new tree.
     * TODO: test whether this works in all scenarios! */
    add_node(uid_mapping[(*it)->get_parent()->get_uid()],
        (*it)->get_key(), (*it)->get_data());
  }

  /* Set the reference back to its original value. */
  to_clone.change_ref(old_ref);
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
  } catch (std::invalid_argument &ex) {
    throw std::invalid_argument("add_node: Could not find parent in tree.");
  } catch (std::exception &ex) {
    forward_exception_to_r(ex);
  } catch (...) {
    ::Rf_error("c++ exception (unknown reason)");
  }

  tree_node_sp child = make_shared<TreeNode>(child_key, child_data);

  /* Add child to parent. */
  parent->add_child(child);

  /* Add child to internal storage. */
  return internal_storage_insert(child);
}

uid
GeneralTreeInternal::add_node(const uid& parent_uid, const SEXP& child_key,
    const SEXP& child_data)
{
  if (parent_uid >= nodes.size())
    throw std::runtime_error("add_node: trying to add a node with a non existent parent_node.");

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
  return internal_storage_insert(child);
}


uid
GeneralTreeInternal::add_child(const SEXP& child_key, const SEXP& child_data)
{
  tree_node_sp child = make_shared<TreeNode>(child_key, child_data);

  if (last_ref_node.get() == nullptr)
    throw std::runtime_error("add_child: Do not have a last_add_node defined");

  /* Add child to last added node. */
  last_ref_node->add_child(child);

  last_ref_node = child;

  /* Add child to internal storage. */
  return internal_storage_insert(child);
}

uid
GeneralTreeInternal::add_sibling(const SEXP& sibling_key, const SEXP& sibling_data)
{
  tree_node_sp sibling = make_shared<TreeNode>(sibling_key, sibling_data);

  if (last_ref_node.get() == nullptr)
    std::runtime_error("add_sibling: Do not have a last_add_node defined");

  /* Add sibling to last added node. */
  last_ref_node->get_parent()->get_left_child()->add_sibling(sibling);

  last_ref_node = sibling;
  /* Add sibling to internal storage. */
  return internal_storage_insert(sibling);
}

uid
GeneralTreeInternal::find_uid(const SEXP& id) const
{
  shared_ptr<tree_key> search_key = tree_key_cast_SEXP(id);

  uid_id_bimap::right_const_iterator id_iter =
    this->uid_to_key.right.find(*search_key);

  if (id_iter == this->uid_to_key.right.end())
    throw std::invalid_argument("Could not find key in tree.");

  return id_iter->second;
}

tree_node_sp
GeneralTreeInternal::find_node(const SEXP& id) const
{
  uid node_uid = find_uid(id);

  return nodes[node_uid];
}

tree_node_sp
GeneralTreeInternal::find_node(const uid& uid_) const
{
  uid_id_bimap::left_const_iterator uid_iter =
    this->uid_to_key.left.find(uid_);

  if (uid_iter == this->uid_to_key.left.end())
    throw std::invalid_argument("Could not find uid in tree.");

  return nodes[uid_];
}

uid
GeneralTreeInternal::get_uid() const
{
  return nodes.size();
}

uid
GeneralTreeInternal::internal_storage_insert(tree_node_sp& new_node)
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

void
GeneralTreeInternal::internal_storage_update(const uid& current_uid, const SEXP& new_key)
{
  auto it = uid_to_key.left.find(current_uid);

  if (it == uid_to_key.left.end())
    throw std::runtime_error("internal_storage_update: Could not find uid in storage.");

  shared_ptr<tree_key> new_search_key = tree_key_cast_SEXP(new_key);

  bool result = uid_to_key.left.modify_data(it, boost::bimaps::_data = *new_search_key);
  if (!result)
    throw runtime_error("internal_storage_update: could not update the key.");
}

void
GeneralTreeInternal::internal_storage_delete(const uid& to_delete)
{
  nodes.erase(nodes.begin() + to_delete);
  /* At this time we are required to remove the entries from the bimap because
   * the bimap modify methods do not guarantee an update. */
  uid_id_bimap::left_iterator it = uid_to_key.left.find(to_delete);
  uid_to_key.left.erase(it, uid_to_key.left.end());

  /* Since the nodes are stored in a vector we need to iterate through the
   * vector to update the uids of each individual node. Additionally we need to
   * update the bimap. */
  for (int i = (nodes.size() - 1); i >= to_delete; --i) {
    uid old_uid = nodes[i]->get_uid();
    nodes[i]->set_uid(i);
    uid_to_key.insert(uid_id_pair(i, *tree_key_cast_SEXP(nodes[i]->get_key())));
  }
}

uid
GeneralTreeInternal::travel_up()
{
  if (!(last_ref_node->has_parent()))
    throw std::out_of_range("travel_up: last added node does not have a"
        " parent so cannot travel up");

  last_ref_node = last_ref_node->get_parent();

  return last_ref_node->get_uid();
}

SEXP
GeneralTreeInternal::get_data(const SEXP& id) const
{
  tree_node_sp node_found = find_node(id);
  return node_found->get_data();
}


const SEXP
GeneralTreeInternal::get_data() const
{
  return last_ref_node->get_data();
}

bool
GeneralTreeInternal::has_child(const SEXP& id) const
{
  tree_node_sp node_found = find_node(id);
  return node_found->have_left_child();
}

bool
GeneralTreeInternal::have_siblings(const SEXP& id) const
{
  tree_node_sp node_found = find_node(id);
  return node_found->have_tree_siblings();
}

const tree_node_sp
GeneralTreeInternal::get_parent(const SEXP& id) const
{
  tree_node_sp node_found = find_node(id);
  return node_found->get_parent();
}

const tree_node_sp
GeneralTreeInternal::get_parent() const
{
  return last_ref_node->get_parent();
}

const tree_node_sp
GeneralTreeInternal::get_ref() const
{
  if (last_ref_node.get() == nullptr)
    throw std::runtime_error("get_ref: last_ref_node points to nullptr.");

  return last_ref_node;
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
get_info(const Container_source& src, Container_dest& dst, const SEXPListFunctor& lf)
{
    transform(begin(*src), end(*src), back_inserter(*dst),
        [&](shared_ptr<const TreeNode> x){ return lf.Process(*x); });
}

SEXP_vec_sp
GeneralTreeInternal::access_tree_node_vec(const SEXP& node_id,
    const AccessFunctor &af,
    const SEXPListFunctor& lf) const
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
GeneralTreeInternal::access_tree_node_vec(const AccessFunctor &af,
    const SEXPListFunctor& lf) const
{
  SEXP_vec_sp result(new SEXP_vec());
  /* Get the nodes using the access functor. */
  tree_node_c_sp_vec_sp tn_vec = af.tree_accessor(*last_ref_node);

  result->reserve(tn_vec->size());

  get_info(tn_vec, result, lf);

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

GeneralTreeInternal::GeneralTreeInternal(SEXP gti_exp)
{
  try {
    List gti_list = as<List>(gti_exp);
    List parents = as<List>(gti_list["parents"]);
    List keys = as<List>(gti_list["keys"]);
    List data = as<List>(gti_list["data"]);
    SEXP root_spec = gti_list["root"];

    /* Add the root node. */
    shared_ptr<TreeNode> root_node = make_shared<TreeNode>(root_spec);

    internal_storage_insert(root_node);
    root = root_node;

    /* Handle rest of serialization. Start at one so that we skip root. */
    for (int i = 1; i < parents.size(); ++i)
      add_node(as<NumericVector>(parents[i])[0], keys[i], data[i]);

  } catch (std::exception &ex) {
    forward_exception_to_r(ex);
  } catch (...) {
    ::Rf_error("c++ exception (unknown reason)");
  }
}

GeneralTreeInternal::operator SEXP() const
{
  List serialization;

  serialization["root"] = wrap(*root);

  List node_keys(no_init(nodes.size()));
  List node_data(no_init(nodes.size()));
  List node_parents(no_init(nodes.size()));

  for (int i = 0; i < nodes.size(); ++i) {
    node_keys[i] = nodes.at(i)->get_key();
    node_data[i] = nodes.at(i)->get_data();
    node_parents[i] = nodes.at(i)->get_parent_uid();
  }

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

tree_node_sp_vec_sp
GeneralTreeInternal::get_leafs(const SEXP& node_id)
{
  tree_node_sp node_found = find_node(node_id);

  return node_found->get_leafs();
}

tree_node_c_sp_vec_sp
GeneralTreeInternal::get_leafs(const SEXP& node_id) const
{
  tree_node_c_sp node_found = find_node(node_id);

  return node_found->get_leafs();
}

void
GeneralTreeInternal::set_key(const SEXP& new_key)
{
  last_ref_node->set_key(new_key);
  internal_storage_update(last_ref_node->get_uid(), new_key);
}

void
GeneralTreeInternal::set_data(const SEXP& new_data)
{
  last_ref_node->set_data(new_data);
}

const uid
GeneralTreeInternal::delete_node(const SEXP& node_id)
{
  tree_node_sp node_found = find_node(node_id);
  if (*node_found == *root)
    throw invalid_argument("delete_node: Cannot delete root node.");

  uid replacement_uid = node_found->get_parent_uid();

  if (*node_found == *root && node_found->have_siblings())
    replacement_uid = root->get_siblings()->at(0)->get_uid();

  /* Get the entire vector of nodes that we need to delete. */
  tree_node_sp_vec_sp to_delete = node_found->get_branch();

  for (tree_node_sp_vec::reverse_iterator it = to_delete->rbegin();
      it != to_delete->rend(); ++it)
    internal_storage_delete((*it)->get_uid());

  /* In case the root node is deleted we need the replacement node to be the
   * root. */
  if (node_found == root)
    root = node_found->delete_node();
  else
    node_found->delete_node();

  return replacement_uid;
}

const uid
GeneralTreeInternal::delete_node()
{
  if (*last_ref_node == *root)
    throw invalid_argument("delete_node: Cannot delete root node.");

  /* Get the entire vector of nodes that we need to delete. */
  tree_node_sp_vec_sp to_delete = last_ref_node->get_branch();

  uid replacement_uid = last_ref_node->get_parent_uid();

  if (*last_ref_node == *root && last_ref_node->have_siblings())
    replacement_uid = root->get_siblings()->at(0)->get_uid();

  for (tree_node_sp_vec::reverse_iterator it = to_delete->rbegin();
      it != to_delete->rend(); ++it)
    internal_storage_delete((*it)->get_uid());

  /* In case the root node is deleted we need the replacement node to be the
   * root. */
  if (last_ref_node == root) {
    last_ref_node = last_ref_node->delete_node();
    root = last_ref_node;
  } else {
    last_ref_node = last_ref_node->delete_node();
  }

  return replacement_uid;
}

void
GeneralTreeInternal::change_ref(const uid& new_uid)
{
  try {
    last_ref_node = find_node(new_uid);
  } catch (std::exception &ex) {
    forward_exception_to_r(ex);
  } catch (...) {
    ::Rf_error("c++ exception (unknown reason)");
  }
}

const bool
GeneralTreeInternal::is_last_sibling(const SEXP& id) const
{
  tree_node_sp node = find_node(id);

  return is_last_sibling(node);
}

const bool
GeneralTreeInternal::is_last_sibling() const
{
  return is_last_sibling(last_ref_node);
}


const bool
GeneralTreeInternal::is_last_sibling(const tree_node_sp& tn) const
{
  if (tn->has_parent()) {
    /* Verify whether the first child of the parent of the node has siblings. */
    if (tn->get_parent()->get_left_child()->have_siblings()) {
      /* Verify whether the last sibling in the list is equal to the element
       * that was passed. */
      return *tn->get_parent()->get_left_child()->get_siblings()->back() == *tn;
    } else {
      /* It seems that this node is the left child of its parent and it has no
       * siblings so we return true since we are the only node. */
      return true;
    }
  }

  /* We are currently looking at siblings of the root. If we have only have a
   * single node we are the last sibling. */
  if (!root->have_siblings())
    return true;

  return  *root->get_siblings()->back() == *tn;
}


SEXP
GeneralTreeInternal::update_key(const SEXP& old_key, const SEXP& new_key)
{
  return update_key(find_uid(old_key), new_key);
}

SEXP
GeneralTreeInternal::update_key(const uid& uid_, const SEXP& new_key)
{
  tree_node_sp to_change;
  try {
    to_change = nodes[uid_];
  } catch (std::exception &ex) {
    forward_exception_to_r(ex);
  } catch (...) {
    ::Rf_error("c++ exception (unknown reason)");
  }

  SEXP key_found = to_change->get_key();
  to_change->set_key(new_key);

  internal_storage_update(uid_, new_key);

  return key_found;
}

SEXP
GeneralTreeInternal::update_key(const SEXP& new_key)
{
  return update_key(last_ref_node->get_uid(), new_key);
}

SEXP
GeneralTreeInternal::update_data(const uid& uid_, const SEXP& new_data)
{
  tree_node_sp to_change;
  try {
    to_change = nodes[uid_];
  } catch (std::exception &ex) {
    forward_exception_to_r(ex);
  } catch (...) {
    ::Rf_error("c++ exception (unknown reason)");
  }

  SEXP old_data = to_change->get_data();
  to_change->set_data(new_data);

  return old_data;
}

SEXP
GeneralTreeInternal::update_data(const SEXP& key, const SEXP& new_data)
{
  return update_data(find_uid(key), new_data);
}

SEXP
GeneralTreeInternal::update_data(const SEXP& new_key)
{
  return update_data(last_ref_node->get_uid(), new_key);
}

const unsigned int
GeneralTreeInternal::tree_depth() const
{
  return root->tree_depth();
}

SEXP_vec_sp
GeneralTreeInternal::get_children_keys(const SEXP& parent_id, bool recursive)
  const
{
  return access_tree_node_vec(parent_id, AccessChildrenFunctor(recursive),
      GetKeyFunctor());
}

SEXP_vec_sp
GeneralTreeInternal::get_children_data(const SEXP& parent_id, bool recursive)
  const
{
  return access_tree_node_vec(parent_id, AccessChildrenFunctor(recursive),
      GetDataFunctor());
}

SEXP_vec_sp
GeneralTreeInternal::get_children_keys(bool recursive) const
{
  return access_tree_node_vec(AccessChildrenFunctor(recursive),
      GetKeyFunctor());
}

SEXP_vec_sp
GeneralTreeInternal::get_children_data(bool recursive) const
{
  return access_tree_node_vec(AccessChildrenFunctor(recursive),
      GetDataFunctor());
}

SEXP_vec_sp
GeneralTreeInternal::get_siblings_keys(const SEXP& node_id) const
{
  return access_tree_node_vec(node_id, AccessSiblingsFunctor(),
      GetKeyFunctor());
}

SEXP_vec_sp
GeneralTreeInternal::get_siblings_data(const SEXP& node_id) const
{
  return access_tree_node_vec(node_id, AccessSiblingsFunctor(),
      GetDataFunctor());
}

SEXP_vec_sp
GeneralTreeInternal::get_siblings_keys() const
{
  return access_tree_node_vec(AccessSiblingsFunctor(), GetKeyFunctor());
}

SEXP_vec_sp
GeneralTreeInternal::get_siblings_data() const
{
  return access_tree_node_vec(AccessSiblingsFunctor(), GetDataFunctor());
}

SEXP_vec_sp
GeneralTreeInternal::get_branch_keys(const SEXP& node_id) const
{
  return access_tree_node_vec(node_id, AccessBranchFunctor(),
      GetKeyFunctor());
}

SEXP_vec_sp
GeneralTreeInternal::get_branch_data(const SEXP& node_id) const
{
  return access_tree_node_vec(node_id, AccessBranchFunctor(),
      GetDataFunctor());
}

SEXP_vec_sp
GeneralTreeInternal::get_branch_uids() const
{
  return access_tree_node_vec(AccessBranchFunctor(), GetUIDFunctor());
}

SEXP_vec_sp
GeneralTreeInternal::get_branch_keys() const
{
  return access_tree_node_vec(AccessBranchFunctor(), GetKeyFunctor());
}

SEXP_vec_sp
GeneralTreeInternal::get_branch_data() const
{
  return access_tree_node_vec(AccessBranchFunctor(), GetDataFunctor());
}

SEXP_vec_sp
GeneralTreeInternal::get_leafs_keys(const SEXP& node_id) const
{
  return access_tree_node_vec(node_id, AccessLeafsFunctor(),
      GetKeyFunctor());
}

SEXP_vec_sp
GeneralTreeInternal::get_leafs_data(const SEXP& node_id) const
{
  return access_tree_node_vec(node_id, AccessLeafsFunctor(),
      GetDataFunctor());
}

SEXP_vec_sp
GeneralTreeInternal::get_leafs_keys() const
{
  return access_tree_node_vec(AccessLeafsFunctor(), GetKeyFunctor());
}

SEXP_vec_sp
GeneralTreeInternal::get_leafs_data() const
{
  return access_tree_node_vec(AccessLeafsFunctor(), GetDataFunctor());
}


