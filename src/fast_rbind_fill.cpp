#include <Rcpp.h>
#include <utility>
#include <unordered_map>

using namespace Rcpp;

enum SupportedType { BLNK_NUMERIC, BLNK_INTEGER, BLNK_CHARACTER, BLNK_LOGICAL };

SupportedType get_type(SEXP x);
GenericVector get_correct_NA(SupportedType x);

// [[Rcpp::export]]
List fast_rbind_fill(List lst) {
  int lst_size  = lst.size();
  std::unordered_map<std::string,SupportedType> col_names_types;
  DataFrame out = DataFrame::create();
  out["b"] = CharacterVector::create("foo");

  // First loop: Get all possible column names and column types
  for (auto i = 0; i < lst.size(); ++i) {
    Rcpp::checkUserInterrupt();
    List df = as<List>(lst[i]);
    CharacterVector names = df.names();
    for (auto nm : names) {
      auto key = as<std::string>(nm);
      col_names_types.insert(std::make_pair(key, get_type(df[key])));
    }
  }

  std::vector<std::string> all_keys;
  std::vector<GenericVector> values;
  for (auto k : col_names_types) {
    all_keys.push_back(k.first);
    values.push_back(get_correct_NA(k.second));
  }

  return List::create(Named("keys") = all_keys, Named("nas") = values);
}


/**
* helper functions
*/

bool is_numeric(SEXP x) {
  return TYPEOF(x) == REALSXP;
}

bool is_integer(SEXP x) {
  return TYPEOF(x) == INTSXP;
}

bool is_character(SEXP x) {
  return TYPEOF(x) == STRSXP;
}

bool is_logical(SEXP x) {
  return TYPEOF(x) == LGLSXP;
}

SupportedType get_type(SEXP x) {
  if (is_logical(x)) {
    return BLNK_LOGICAL;
  } else if (is_integer(x)) {
    return BLNK_INTEGER;
  } else if (is_numeric(x)) {
    return BLNK_NUMERIC;
  } else {
    return BLNK_LOGICAL;
  }
}

GenericVector get_correct_NA(SupportedType x) {
  switch(x) {
    case BLNK_LOGICAL :
      return GenericVector::create(NA_LOGICAL);
    case BLNK_INTEGER :
      return GenericVector::create(NA_INTEGER);
    case BLNK_NUMERIC :
      return GenericVector::create(NA_REAL);
    case BLNK_CHARACTER :
      return GenericVector::create(NA_STRING);
    default :
      return GenericVector::create(NA_STRING);
  }
}
