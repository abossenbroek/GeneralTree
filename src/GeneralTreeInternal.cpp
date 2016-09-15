// [[Rcpp::depends(BH)]]
// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>

#include <memory>
#include <algorithm>
#include <string>

#include <boost/phoenix/phoenix.hpp>



#include "GeneralTreeInternal.h"
#include "key_visitor.h"

using namespace boost::phoenix::placeholders;

using namespace Rcpp;
using namespace std;

GeneralTreeInternal::GeneralTreeInternal(SEXP root_id, SEXP root_data)
{
  uid_counter = 0;
  shared_ptr<tree_key> root_key = add_mapping(uid_counter, root_id);
  this->uid_to_id.insert(uid_id_pair(this->uid_counter, *root_key));
  this->uid_to_data.insert(uid_SEXP_pair(this->uid_counter, root_data));

  this->uid_counter++;
}

GeneralTreeInternal::GeneralTreeInternal()
{
}

SEXP
GeneralTreeInternal::get_value(SEXP key)
{
  if (!is_id_in_tree(key))
    throw std::invalid_argument("get_value: Could not find id in tree.");

  uid found_uid = find_uid(key);
  uid_to_SEXP_map::iterator value = uid_to_data.find(found_uid);

  if (value == uid_to_data.end())
    throw std::runtime_error("get_value: key was found in child but uid not"
        " in data. Possible inconsistency");

  return value->second;
}

bool
GeneralTreeInternal::is_id_in_tree(SEXP id)
{
  shared_ptr<tree_key> search_key = tree_key_cast_SEXP(id);

  uid_id_bimap::right_const_iterator id_iter =
    this->uid_to_id.right.find(*search_key);

  return id_iter != this->uid_to_id.right.end();
}

void
GeneralTreeInternal::add_node(SEXP parent_id, SEXP child_id, SEXP data)
{
  // Resolve the uid
  uid parent_uid = 0;
  try {
    parent_uid = this->find_uid(parent_id);
  } catch (std::exception &ex) {
    forward_exception_to_r(ex);
  } catch (...) {
    ::Rf_error("c++ exception (unknown reason)");
  }
  shared_ptr<tree_key> child_key = add_mapping(uid_counter, child_id);

  this->uid_to_id.insert(uid_id_pair(this->uid_counter, *child_key));
  this->uid_to_data.insert(uid_SEXP_pair(this->uid_counter, data));
  this->uid_to_parent.insert(uid_uid_pair(this->uid_counter, parent_uid));

  // Check whether the parent has a child.
  if (this->has_child(parent_uid)) {
    // Find the left child of this parent.
    uid left_child = this->get_lchild(parent_uid);
    // Add the new node to the existing child.
    this->add_sibling(left_child, this->uid_counter);
  } else {
    // Set the parent.
    this->uid_to_parent.insert(uid_uid_pair(this->uid_counter, parent_uid));
    // Set the child.
    this->uid_to_child.insert(uid_uid_pair(parent_uid, this->uid_counter));
  }

  // Finally increase the counter.
  this->uid_counter++;
}

uid
GeneralTreeInternal::find_uid(SEXP id)
{
  shared_ptr<tree_key> search_key = tree_key_cast_SEXP(id);

  uid_id_bimap::right_const_iterator id_iter =
    this->uid_to_id.right.find(*search_key);

  if (id_iter == this->uid_to_id.right.end()) {
    throw std::invalid_argument("Could not find id in tree.");
  }

  return id_iter->second;
}

uid
GeneralTreeInternal::get_lchild(uid parent_uid)
{
  uid_to_uid_map::iterator child_iter =
    this->uid_to_child.find(parent_uid);

  if (child_iter == this->uid_to_child.end()) {
    throw std::invalid_argument("Could not find child in tree.");
  }

  return child_iter->second;
}

bool
GeneralTreeInternal::has_child(uid parent_uid) {

  uid_to_uid_map::iterator child_iter =
    this->uid_to_child.find(parent_uid);

  return child_iter != this->uid_to_child.end();
}

bool
GeneralTreeInternal::has_siblings(uid node_uid) {

  // First try to find whether this node is present in uid_to_siblings.
  uid_to_uids_map::iterator sib_iter =
    this->uid_to_siblings.find(node_uid);

  if (sib_iter != this->uid_to_siblings.end()) {
    return true;
  }


  if (has_parent(node_uid)) {
    uid parent_uid = get_parent(node_uid);
    uid child = get_lchild(parent_uid);

    // If the child has a different uid than the one we got passed we can safely
    // conclude that this node has a sibling.
    return child != node_uid;
  }

  return false;
}

void
GeneralTreeInternal::add_sibling(uid origin_uid, uid sibling_uid)
{
  if (origin_uid == sibling_uid)
    throw std::invalid_argument("Do not know how to add a sibling to the"
        " same origin.");

  uid_to_uids_map::iterator origin_iter =
    this->uid_to_siblings.find(origin_uid);

  if (origin_iter == this->uid_to_siblings.end()) {
    origin_iter = (this->uid_to_siblings.insert(uid_uids_pair(origin_uid,
          uid_vec()))).first;
  }

  origin_iter->second.push_back(sibling_uid);
  uid origin_parent_id = this->get_parent(origin_uid);
  this->set_parent(origin_parent_id, sibling_uid);
}

void
GeneralTreeInternal::set_parent(uid parent_uid, uid child_uid)
{
  uid_to_uid_map::iterator par_iter =
    this->uid_to_parent.find(child_uid);

  // Verify whether we should create a new setting or update the reference.
  if (par_iter == this->uid_to_parent.end()) {
    this->uid_to_parent.insert(uid_uid_pair(child_uid, parent_uid));
  } else {
    par_iter->second = parent_uid;
  }
}

uid
GeneralTreeInternal::get_parent(uid child_uid)
{
  uid_to_uid_map::iterator par_iter =
    this->uid_to_parent.find(child_uid);

  // Verify whether we should create a new setting or update the reference.
  if (par_iter == this->uid_to_parent.end()) {
    throw std::invalid_argument("Could not find id with this parent id.");
  }

  return par_iter->second;
}

bool
GeneralTreeInternal::has_parent(uid child_uid)
{
  uid_to_uid_map::iterator par_iter =
    this->uid_to_parent.find(child_uid);

  return par_iter != this->uid_to_parent.end();
}

bool
GeneralTreeInternal::cmp(const GeneralTreeInternal& gti)
{
  bool status = true;

  status = status && (this->uid_counter == gti.uid_counter);

  // TODO: finish comparison implementation.
  return status;
}


shared_ptr_uid_vec
GeneralTreeInternal::get_children_uid(uid parent_uid)
{
  shared_ptr<uid_vec> result(new uid_vec());

  // Return empty list if this node does not have any children.
  if (!has_child(parent_uid))
    return result;

  uid child_uid = get_lchild(parent_uid);
  // Create a list with child and possible siblings.
  result->push_back(child_uid);

  // Add the siblings to the list if the node has any siblings.
  if (has_siblings(child_uid)) {
    shared_ptr<uid_vec> sibling_uids = get_siblings_uid(child_uid);
    result->insert(result->end(), sibling_uids->begin(), sibling_uids->end());
  }

  return result;
}

shared_ptr_uid_vec
GeneralTreeInternal::get_siblings_uid(uid node_uid)
{
  shared_ptr<uid_vec> result(new uid_vec());

  if (!has_siblings(node_uid))
    return result;

  // Find the left most child of the parent of the node.
  uid lchild_uid = get_lchild(get_parent(node_uid));

  uid_to_uids_map::iterator uid_it = uid_to_siblings.find(lchild_uid);

  // This should probably never happen but we want to make sure.
  if (uid_it == uid_to_siblings.end())
    throw std::runtime_error("get_siblings_uid: uid was found as having siblings"
        " but no data was found. Possible inconsistency.");

  // Make sure that the result contains all the nodes under the parent.
  result->push_back(lchild_uid);
  result->insert(result->end(), uid_it->second.begin(), uid_it->second.end());

  // Remove the node with which this function was called from the vector.
  int node_position = 0;
  for (unsigned int i = 0; i < result->size(); ++i) {
    if (result->at(i) == node_uid)
      node_position = i;
  }
  result->erase(result->begin() + node_position);

  return result;
}

shared_ptr_key_vec
GeneralTreeInternal::get_children_keys(uid parent_uid)
{
  shared_ptr<std::vector<tree_key> > result(new std::vector<tree_key>);

  if (!has_child(parent_uid))
    return result;

  shared_ptr<uid_vec> children = get_children_uid(parent_uid);
  result->reserve(children->size());

  transform(children->begin(), children->end(), back_inserter(*result),
      [this](uid& x){ return find_key(x); } );

  return result;
}

shared_ptr_key_vec
GeneralTreeInternal::get_siblings_keys(uid node_uid)
{
  shared_ptr<std::vector<tree_key> > result(new std::vector<tree_key>);

  if (!has_siblings(node_uid))
    return result;

  shared_ptr<uid_vec> siblings = get_siblings_uid(node_uid);
  result->reserve(siblings->size());

  transform(siblings->begin(), siblings->end(), back_inserter(*result),
      [this](uid& x){ return find_key(x); } );

  return result;
}

tree_key
GeneralTreeInternal::find_key(uid node_uid)
{
  uid_id_bimap::left_const_iterator id_iter =
    this->uid_to_id.left.find(node_uid);

  if (id_iter == uid_to_id.left.end())
    throw std::invalid_argument("find_key: Could not find node_uid in tree.");

  return id_iter->second;
}

SEXP
GeneralTreeInternal::get_value(uid node_uid)
{
  uid_to_SEXP_map::iterator value = uid_to_data.find(node_uid);

  if (value == uid_to_data.end())
    throw std::runtime_error("get_value: key was found in child but uid not"
        " in data. Possible inconsistency");

  return value->second;
}



shared_ptr_SEXP_vec
GeneralTreeInternal::get_children_values(uid parent_uid)
{
  shared_ptr_SEXP_vec result(new std::vector<SEXP>);

  if (!has_child(parent_uid))
    return result;

  shared_ptr<uid_vec> children = get_children_uid(parent_uid);
  result->reserve(children->size());

  transform(children->begin(), children->end(), back_inserter(*result),
      [this](uid& x){ return get_value(x); } );

  return result;
}

shared_ptr_SEXP_vec
GeneralTreeInternal::get_siblings_values(uid node_uid)
{
  shared_ptr_SEXP_vec result(new std::vector<SEXP>);

  if (!has_siblings(node_uid))
    return result;

  shared_ptr<uid_vec> siblings = get_siblings_uid(node_uid);
  result->reserve(siblings->size());

  transform(siblings->begin(), siblings->end(), back_inserter(*result),
      [this](uid& x){ return get_value(x); } );

  return result;
}

unsigned int
GeneralTreeInternal::count_child_nodes(uid parent_uid, bool recursive)
{
  if (!has_child(parent_uid))
    return 0;

  /* To create a final list we need to reserve space in advance. To achieve
   * this we will first sum the number of children in this branch. */
  shared_ptr_uid_vec c_nodes = get_children_uid(parent_uid);

  unsigned int branch_size = c_nodes->size();

  /* If we have a recursive call we need to call this function on all children
   * with children.  */
  if (recursive) {
    for (const uid& child_uid : *c_nodes) {
      branch_size += count_child_nodes(child_uid, recursive);
    }
  }

  return branch_size;
}


shared_ptr_key_vec
GeneralTreeInternal::branch_keys_to_list(uid parent_uid, bool recursive)
{
  shared_ptr_key_vec result(new key_vec());
  return result;
}


shared_ptr_uid_vec
GeneralTreeInternal::branch_uid_to_list(uid parent_uid, bool recursive)
{
  shared_ptr_uid_vec result(new uid_vec());

  if (!has_child(parent_uid))
    return result;

  /* Reserve sufficient space to store all the child nodes. */
  result->reserve(count_child_nodes(parent_uid, recursive));

  shared_ptr_uid_vec children(get_children_uid(parent_uid));

  for (uid c : *children) {
    //TODO: add code.
  }

  return result;

}
