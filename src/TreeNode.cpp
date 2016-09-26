#include "TreeNode.h"
#include <Rcpp.h>

#include <memory>
#include <algorithm>

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

void
TreeNode::add_child(const std::shared_ptr<TreeNode>& new_child) {
  new_child->set_parent(shared_from_this());

  if (has_left_child()) {
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
  if (has_left_child()) {
    children->push_back(const_pointer_cast<const TreeNode>(left_child));

    /* If the child has siblings we add those to the list too. */
    if (left_child->has_siblings()) {
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
        if (c->has_left_child()) {
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
  if (has_left_child()) {
    children->push_back(left_child);

    /* If the child has siblings we add those to the list too. */
    if (left_child->has_siblings()) {
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
        if (c->has_left_child()) {
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

  serialization["key"] = key;
  serialization["data"] = data;
  serialization["uid"] = wrap(my_uid);
  serialization["parent_uid"] = wrap(get_parent_uid());

  return serialization;
}

const unsigned int
TreeNode::tree_depth() const
{
  unsigned int result = 0;

  if (has_siblings()) {
    for (auto it = begin(siblings); it != end(siblings); ++it)
      result = max(result, (*it)->tree_depth());
  }

  if (has_left_child())
    result = max(result, 1 + left_child->tree_depth());

  return result;
}
