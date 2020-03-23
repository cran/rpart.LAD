#include <Rcpp.h>
#include <vector>
#ifdef _OPENMP
# include <omp.h>
#endif

using namespace Rcpp;
using namespace std;

int getMedian(vector<double> const& weights) {
  int tmp;
  double r, l;
  int ret=-1;
  double left=0, right=0;
  int offset=0, layer=2;
  int move = weights.size()-3;

  while(move >= 0) {
    tmp = move+offset;
    l = weights[tmp];
    r = weights[tmp+1];

    layer *= 2;

    if(left+l >= right+r) {
      if(move == 0) {
        ret = offset;
        break;
      } else {
        move -= layer;
        offset *= 2;
        right += r;
      }
    } else {
      if(move == 0) {
        ret = offset+1;
        break;
      } else {
        move -= layer;
        offset = (offset+1)*2;
        left += l;
      }
    }
  }

  return ret;
}


// [[Rcpp::export]]
Rcpp::NumericVector getMedians(Rcpp::NumericVector const& x, Rcpp::NumericVector const& wt, Rcpp::IntegerVector const& idx) {
  int n = LENGTH(x);
  int layers = (int)ceil(log2(n));
  if (layers > 15) Rcpp::stop("Fehler, zu viele Layer");
  int n2 = exp2(layers);
  if (n2 < n) Rcpp::stop("Fehler n2 < n");
  std::vector<double> weights(2*n2-1, 0);
  std::vector<double> weights2(2*n2-1, 0);

  for(int i=0; i < n; i++) weights[i] = wt[i];

  int offset = 0; int laenge = exp2(layers); int tmp;
  for(int l=layers-1; l>=0; l--) {
    for(int i=0; 2*i<laenge; i++) {
      tmp = offset + 2*i;
      weights[offset+laenge+i] = weights[tmp] + weights[tmp+1];
    }
    offset += laenge;
    laenge = laenge / 2;
  }

  weights2 = weights;

  Rcpp::NumericVector result(2*n-2);

  int myidx; int myidx2;

  for(int i = 0; i < n-1; i++) {
    myidx = idx[n-i-1]-1;
    weights[myidx] = 0;
    myidx2 = myidx;
    offset = 0;
    laenge = n2;
    for(int l=layers-1; l>=0; l--) {
      myidx2 = floor(myidx2/2);
      offset += laenge;
      laenge = laenge/2;
      weights[offset+myidx2] -= wt[myidx];
    }
    result[n-2-i] = x[getMedian(weights)];
  }

  for(int i = 0; i < n-1; i++) {
    myidx = idx[i]-1;
    weights2[myidx] = 0;
    myidx2 = myidx;
    offset = 0;
    laenge = n2;
    for(int l=layers-1; l>=0; l--) {
      myidx2 = floor(myidx2/2);
      offset += laenge;
      laenge = laenge/2;
      weights2[offset+myidx2] -= wt[myidx];
    }
    result[n-1+i] = x[getMedian(weights2)];
  }

  return result;
}


// [[Rcpp::export]]
Rcpp::NumericVector getGoodnessOMP(Rcpp::NumericVector const& x,
                                   Rcpp::NumericVector const& wt,
                                   Rcpp::NumericVector const& medians) {
  int n = LENGTH(x);
  Rcpp::NumericVector splits(n-1);

  double tmpsum = 0;

  #pragma omp parallel for reduction(+:tmpsum)
  for(int j=0; j<n; j++) tmpsum += wt[j];

  #pragma omp parallel for
  for(int pos=0; pos<n-1; pos++) {
    double tmp = 0;
    double m = medians[pos];
    for(int j=0; j<pos+1; j++) {
      tmp += wt[j] * fabs(x[j] - m);
    }

    m = medians[n-1+pos];
    for(int j=pos+1; j<n; j++) {
      tmp += wt[j] * fabs(x[j] - m);
    }

    splits[pos] = tmp / tmpsum;
  }

  return splits;
}

// [[Rcpp::export]]
Rcpp::NumericVector getGoodness(Rcpp::NumericVector const& x,
                                Rcpp::NumericVector const& wt,
                                Rcpp::NumericVector const& medians) {
  int n = LENGTH(x);
  Rcpp::NumericVector splits(n-1);

  double tmpsum = 0;
  for(int j=0; j<n; j++) tmpsum += wt[j];

  for(int pos=0; pos<n-1; pos++) {
    double tmp = 0;
    double m = medians[pos];
    for(int j=0; j<pos+1; j++) {
      tmp += wt[j] * fabs(x[j] - m);
    }

    m = medians[n-1+pos];
    for(int j=pos+1; j<n; j++) {
      tmp += wt[j] * fabs(x[j] - m);
    }

    splits[pos] = tmp / tmpsum;
  }

  return splits;
}
