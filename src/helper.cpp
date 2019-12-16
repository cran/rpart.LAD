#include <Rcpp.h>

using namespace Rcpp;
using namespace std;

bool s_indx_asc(const pair<int,double>& a, const pair<int,double>& b) {
  return a.second<b.second;
}

//[[Rcpp::export]]
Rcpp::IntegerVector sort_index(Rcpp::NumericVector x){
  std::vector< pair<int,double> > f(LENGTH(x));
  std::vector< pair<int,double> >::iterator ff=f.begin();
  Rcpp::IntegerVector F(LENGTH(x));
  Rcpp::IntegerVector::iterator FF=F.begin();
  Rcpp::NumericVector::iterator xx=x.begin();
  for(int i=0; xx!=x.end(); ++xx,++i,++ff) *ff=pair<int,double>(i,*xx);
  sort(f.begin(),f.end(), s_indx_asc);
  for(ff=f.begin();ff!=f.end();++ff,++FF) *FF=ff->first;
  return F;
}

//[[Rcpp::export]]
double wmedian(Rcpp::NumericVector x, Rcpp::NumericVector weight){
  Rcpp::IntegerVector sx = sort_index(x);

  std::vector<double> cweights(sx.size());
  cweights[0] = weight[sx[0]];
  for(int i=1; i<sx.size(); i++) {
    cweights[i] = cweights[i-1] + weight[sx[i]];
  }
  double sweights = cweights[sx.size()-1]/2;

  return(x[sx[std::distance(cweights.begin(), lower_bound(cweights.begin(), cweights.end(), sweights))]]);
}
