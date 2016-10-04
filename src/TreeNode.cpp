// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>

#include <memory>
#include <algorithm>
#include <iterator>

#include "TreeNode.h"
#include "key_visitor.h"

using namespace std;
using namespace Rcpp;

template<typename Container, class T>
Container find_erase (Container& vec, const T& val) {

  auto start = begin(*vec);
  auto last = end(*vec);

  while (start != last) {
    if (**start == *val)
      break;
    ++start;
  }

  if (start != last)
    vec->erase(start);

  return vec;
}

TreeNode::TreeNode(SEXP tn)
{
  try {
    List object = as<List>(tn);
    key_ = object["key"];
    data = object["data"];
    my_uid_ = INVALID_UID;
  } catch (std::exception &ex) {
    forward_exception_to_r(ex);
  } catch (...) {
    ::Rf_error("c++ exception (unknown reason)");
  }
}

TreeNode::TreeNode(const TreeNode& tn)
{
  key_ = tn.get_key();
  data = tn.get_data();
  my_uid_ = INVALID_UID;
}

void
TreeNode::add_child(const std::shared_ptr<TreeNode>& new_child) {
  new_child->set_parent(shared_from_this());

  if (have_left_child()) {
    left_child->add_sibling(new_child);
  } else {
    left_child = new_child;
  }
}

tree_node_c_sp_vec_sp
TreeNode::get_children(bool recursive) const {
  /* Will be used to reserve the size of the final array. */
  size_t num_children = 0;
  /* Create a vector to store the results */
  tree_node_c_sp_vec_sp children(new tree_node_c_sp_vec());

  /* If the curent node has a child we add the const to the list. */
  if (have_left_child()) {
    children->push_back(const_pointer_cast<const TreeNode>(left_child));

    /* If the child has siblings we add those to the list too. */
    if (left_child->have_siblings()) {
      tree_node_sp_vec* siblings = left_child->get_siblings();
      num_children = 1 + siblings->size();

      children->reserve(num_children);
      /* Add all the siblings to children. */
      transform(siblings->begin(), siblings->end(), back_inserter(*children),
          [](const tree_node_sp& x){ return const_pointer_cast<const TreeNode>(x); } );
    }

    /* Finally, if we were called recursively we need to call the function on
     * all child nodes. */
    if (recursive) {
      tree_node_c_sp_vec_sp current_children(new tree_node_c_sp_vec(*children));

      int last_position = 0;

      for (auto c : *current_children) {
        /* If c has children we call the function and add the results. */
        if (c->have_left_child()) {
          tree_node_c_sp_vec_sp sub_children = c->get_children(recursive);
          num_children += sub_children->size();
          children->reserve(num_children);

          /* We want to insert the sub children after the child so that the
           * results are depth first. */
          tree_node_c_sp_vec::iterator iter = (children->begin() + last_position);
          for (int i = 0; iter != children->end(); ++iter) {
            if (*c == **iter) {
              last_position += i;
              break;
            }
            ++i;
          }
          iter++;

          children->insert(iter, begin(*sub_children), end(*sub_children));
          /* We add the size of sub_children to the number that we can skip
           * since we are sure that in the next iteration the child for which
           * we will be looking will not be in that part of the vector. */
          last_position += sub_children->size();
        }
      }
    }
  }

  return children;
}

tree_node_sp_vec_sp
TreeNode::get_children(bool recursive) {
  /* Will be used to reserve the size of the final array. */
  size_t num_children = 0;
  /* Create a vector to store the results */
  tree_node_sp_vec_sp children(new tree_node_sp_vec());

  /* If the curent node has a child we add it to the list. */
  if (have_left_child()) {
    children->push_back(left_child);

    /* If the child has siblings we add those to the list too. */
    if (left_child->have_siblings()) {
      tree_node_sp_vec* siblings = left_child->get_siblings();
      num_children = 1 + siblings->size();

      children->reserve(num_children);
      children->insert(end(*children), begin(*siblings), end(*siblings));
    }

    /* Finally, if we were called recursively we need to call the function on
     * all child nodes. */
    if (recursive) {
      tree_node_sp_vec_sp current_children(new tree_node_sp_vec(*children));


      for (auto c : *current_children) {
        int last_position = 0;
        /* If c has children we call the function and add the results. */
        if (c->have_left_child()) {
          tree_node_sp_vec_sp sub_children = c->get_children(recursive);
          num_children += sub_children->size();
          children->reserve(num_children);
          /* We want to insert the sub children after the child so that the
           * results are depth first. */
          tree_node_sp_vec::iterator iter = (children->begin() + last_position);
          for (int i = 0; iter != children->end(); ++iter) {
            if (*c == **iter) {
              last_position += i;
              break;
            }
            ++i;
          }
          iter++;

          children->insert(iter, begin(*sub_children), end(*sub_children));
          /* We add the size of sub_children to the number that we can skip
           * since we are sure that in the next iteration the child for which
           * we will be looking will not be in that part of the vector. */
          last_position += sub_children->size();
        }
      }
    }
  }

  return children;
}

tree_node_sp_vec_sp
TreeNode::get_tree_siblings()
{
  if(!has_parent())
      throw std::runtime_error("Node does not have a parent.");

  tree_node_sp_vec_sp results = parent->get_children();

  /* Remove the current node from the list of children and return the result. */
  return find_erase(results, this);
}

tree_node_c_sp_vec_sp
TreeNode::get_tree_siblings() const
{
  if(!has_parent())
      throw std::runtime_error("Node does not have a parent.");

  tree_node_c_sp c_parent = const_pointer_cast<const TreeNode>(parent);

  tree_node_c_sp_vec_sp results = c_parent->get_children();

  /* Remove the current node from the list of children and return the result. */
  return find_erase(results, this);
}

TreeNode::operator SEXP() const
{
  List serialization;

  serialization["uid"] = my_uid_;
  serialization["key"] = key_;
  serialization["data"] = data;
  serialization["parent_uid"] = get_parent_uid();
  serialization["is_last_sibling"] = is_last_sibling();
  serialization["parents_above"] = parents_above();

  return serialization;
}

const unsigned int
TreeNode::tree_depth() const
{
  unsigned int result = 0;

  if (have_siblings()) {
    for (auto it = begin(siblings); it != end(siblings); ++it)
      result = max(result, (*it)->tree_depth());
  }

  if (have_left_child())
    result = max(result, 1 + left_child->tree_depth());

  return result;
}

tree_node_sp_vec_sp
TreeNode::get_branch()
{
  tree_node_sp_vec_sp branch(new tree_node_sp_vec());

  branch->push_back(shared_from_this());

  if (have_left_child()) {
    tree_node_sp_vec_sp children(get_children(true));

    branch->reserve(children->size() + 1);
    branch->insert(end(*branch), begin(*children), end(*children));
  }

  return branch;
}

tree_node_c_sp_vec_sp
TreeNode::get_branch() const
{
  tree_node_c_sp_vec_sp branch(new tree_node_c_sp_vec());

  branch->push_back(const_pointer_cast<const TreeNode>(shared_from_this()));

  if (have_left_child()) {
    tree_node_c_sp_vec_sp children(get_children(true));

    branch->reserve(children->size() + 1);
    branch->insert(end(*branch), begin(*children), end(*children));
  }

  return branch;
}


tree_node_sp_vec_sp
TreeNode::get_leafs()
{
  tree_node_sp_vec_sp branch = get_branch();
  tree_node_sp_vec_sp result(new tree_node_sp_vec());

  result->reserve(branch->size());

  for (auto it = begin(*branch); it != end(*branch); ++it)
    if (!((*it)->have_left_child()))
      result->push_back(*it);

  return result;
}

tree_node_c_sp_vec_sp
TreeNode::get_leafs() const
{
  tree_node_c_sp_vec_sp branch = get_branch();
  tree_node_c_sp_vec_sp result(new tree_node_c_sp_vec());

  result->reserve(branch->size());

  for (auto it = begin(*branch); it != end(*branch); ++it)
    if (!((*it)->have_left_child()))
      result->push_back(*it);

  return result;
}

tree_node_sp
TreeNode::delete_node()
{
  tree_node_sp replacement = nullptr;

  if (have_left_child()) {
    left_child->delete_node();
    left_child = nullptr;
  }

  /* If we have siblings we need to promote the first sibling to the parent's
   * left child and add the remaining siblings to that left child. */
  if (have_siblings()) {
    replacement = siblings[0];
    if (has_parent())
      parent->set_left_child(replacement);

    /* Add all the remaining siblings to the new left child. */
    for (auto it = (siblings.begin() + 1); it != siblings.end(); ++it)
      replacement->add_sibling(*it);

    /* Remove the siblings from the vector part of this child. */
    siblings.clear();
  }

  return replacement;
}

const unsigned int
TreeNode::parents_above() const
{
  tree_node_c_wp current_node_w(shared_from_this());
  unsigned int depth = 0;

  auto sp = current_node_w.lock();
  while (sp->has_parent()) {
    tree_node_c_wp parent_wp(sp->get_parent());
    current_node_w.swap(parent_wp);
    sp = current_node_w.lock();
    ++depth;
  }

  return depth;
}

const bool
TreeNode::is_last_sibling() const
{
  if (!has_parent())
    return false;

  tree_node_c_sp_vec_sp children = const_pointer_cast<const TreeNode>(parent)->get_children();

  if (children->size() == 1)
    return true;
  else
    return *(children->back()) == *this;
}

bool operator== (const TreeNode& lhs, const TreeNode& rhs)
{
  bool result = true;

  if (lhs.have_left_child() && rhs.have_left_child()) {
    result = result && *lhs.get_left_child() == *rhs.get_left_child();
  } else if (!lhs.have_left_child() != !rhs.have_left_child()) {
    /* Either lhs or rhs does not have a child so result is false. */
    return false;
  }

  if (lhs.have_siblings() && rhs.have_siblings()) {
    /* Verify whether the size of both are equal. */
    if (lhs.get_siblings()->size() != rhs.get_siblings()->size()) {
      return false;
    }

    /* Compare all the siblings. */
    for (int i = 0; i < lhs.get_siblings()->size(); ++i)
      result = result && *lhs.get_siblings()->at(i) ==
        *rhs.get_siblings()->at(i);
  } else if (!lhs.have_siblings() != !rhs.have_siblings()) {
    /* Either lhs or rhs does not have siblings so result is false. */
    return false;
  }

  return result && lhs.get_key() == rhs.get_key() && lhs.get_data() == rhs.get_data();
}

namespace Rcpp {
  template <> SEXP wrap( const tree_node_sp& tn) {
    return wrap(*tn);
  }
  template <> SEXP wrap( const tree_node_c_sp& tn) {
    return wrap(*tn);
  }
}
