#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::IntegerVector find_volume_groups(Rcpp::NumericVector x, int volume) {
  std::vector<int> groups;
  groups.reserve(x.length());
  int cumulative_volume = 0;
  int group = 1;
  for (size_t i = 0; i < x.length(); i++) {
    cumulative_volume += x[i];
    groups.push_back(group);
    if (cumulative_volume >= volume) {
      cumulative_volume = 0;
      group++;
    }
  }
  return Rcpp::wrap(groups);
}
