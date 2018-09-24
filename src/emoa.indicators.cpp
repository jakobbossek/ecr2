#include <Rcpp.h>
#include <math.h>
#include <limits>

using namespace Rcpp;

// [[Rcpp::export]]
double computeGenerationalDistanceC(NumericMatrix points, NumericMatrix refPoints, double p) {
  int n = points.ncol();
  int m = refPoints.ncol();
  int o = points.nrow();

  double gd = 0.0;

  for (int i = 0; i < n; ++i) {
    double minpDist = std::numeric_limits<double>::max();
    for (int j = 0; j < m; ++j) {
      double pdist = 0.0;
      for (int k = 0; k < o; ++k) {
        pdist += (points(k, i) - refPoints(k, j)) * (points(k, i) - refPoints(k, j));
      }
      pdist = sqrt(pdist);
      if (pdist < minpDist) {
        minpDist = pdist;
      }
    }
    gd += pow(minpDist, p);
  }
  gd = pow(gd / n, 1.0 / p);
  return gd;
}
