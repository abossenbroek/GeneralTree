// [[Rcpp::depends(BH)]]
// [[Rcpp::plugins(cpp11)]]
#ifndef _KEY_VISITOR_H_
#define _KEY_VISITOR_H_
#pragma once

#include <algorithm>
#include <stdexcept>
#include <string>

#include <boost/variant.hpp>

#include <Rcpp.h>

using namespace Rcpp;

#include "tree_types.h"
#include "GeneralTreeInternal.h"

class key_visitor
  : public boost::static_visitor<SEXP>
{
public:
  SEXP operator()(int& i) const
  {
    return wrap(i);
  }

  SEXP operator()(std::string& s) const
  {
    return wrap(s);
  }

  SEXP operator()(double& d) const
  {
    return wrap(d);
  }
};

class key_int_visitor
  : public boost::static_visitor<int>
{
public:
  int operator()(int& i) const
  {
    return i;
  }

  int operator()(std::string& s) const
  {
    throw std::invalid_argument("Could not cast int to string");
    return 0;
  }

  int operator()(double& d) const
  {
    throw std::invalid_argument("Could not cast double to string");
    return 0;
  }
};

class key_string_visitor
  : public boost::static_visitor<std::string>
{
public:
  std::string operator()(int& i) const
  {
    return std::to_string(i);
  }

  std::string operator()(std::string& s) const
  {
    return s;
  }

  std::string operator()(double& d) const
  {
    return std::to_string(d);
  }
};

tree_key_sp tree_key_cast_SEXP(const SEXP& key);

#endif // _KEY_VISITOR_H_
