#ifndef _LISTFUNCTORS_H_
#define _LISTFUNCTORS_H_
#pragma once

#include <string>

#include <RcppCommon.h>

#include "tree_types.h"
#include "TreeNode.h"

using namespace Rcpp;

template<typename T>
struct ListFunctor {
  virtual T Process(const TreeNode& tn) const = 0;
};

struct GetUIDFunctor : public ListFunctor<uid> {
  uid Process(const TreeNode& tn) const;
};

class SEXPApplyFunctor : public ListFunctor<SEXP> {
private:
  Function f;

public:
  SEXPApplyFunctor(const Function& f_) : f(f_) {}

  SEXP Process(const TreeNode& tn) const;
};

#endif /* _LISTFUNCTOR_H_ */
