#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]

double DisF(NumericVector x,NumericVector y) {
  
  int n=x.length();
  double Dis=0;
  
  for ( int i=0;i<n;i++ ){
    
    Dis+=pow(x[i]-y[i],2);
    
  }
  
  Dis=pow(Dis,0.5);
  
  return(Dis);
  
}
