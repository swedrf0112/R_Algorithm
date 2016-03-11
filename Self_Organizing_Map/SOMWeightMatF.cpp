#include <RcppArmadilloExtensions/sample.h>
#include <Rcpp.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix SOMWeightMatF(NumericMatrix DataD, NumericMatrix WeiM,NumericMatrix NodeM,int Itertimes,double IniNeiDis,double IniLearnRate){

  int DataDRowNum=DataD.nrow();
  int WeiMRowNum=WeiM.nrow();
  IntegerVector RandV(DataDRowNum);
  IntegerVector SampleIndV=seq_len(DataDRowNum);
  double NeiDis=0;
  double LearnRate=0;
  Function DisF("DisF");
  Function which("which");
  
  for ( int i=1;i<=Itertimes;i++ ){
    
       Rcpp::Rcout<<i<<std::endl;
    
       if ( i==1 ){
         
          NeiDis=IniNeiDis;
          LearnRate=IniLearnRate;
          
       }else{
         
          NeiDis=IniNeiDis*exp(-i/(Itertimes/log(IniNeiDis)));
          LearnRate=IniLearnRate*exp(-i/Itertimes);
          
       }
    
      for ( int j=0;j<DataDRowNum;j++ ){
    
          RandV=RcppArmadillo::sample( SampleIndV , DataDRowNum , FALSE);  
          int TempRandLocat=as<int>(which(RandV==(j+1)));
  
          NumericVector TempDV(WeiMRowNum,0.0);
          NumericVector TempDataDRow(DataD.ncol(),0.0);
          NumericVector TempWeiMRow(WeiM.ncol(),0.0);
  
      for ( int k=0;k<WeiMRowNum;k++ ){
        
          TempDataDRow=DataD.row(TempRandLocat-1);
          TempWeiMRow=WeiM.row(k);
          TempDV[k]=as<double>(DisF(TempDataDRow,TempWeiMRow));
          
      }
  
      int MinWLoc=which_min(TempDV);
  
      NumericVector ChangeWDisV(WeiMRowNum,0.0);
      for ( int l=0;l<WeiMRowNum;l++ ) {
        
          ChangeWDisV[l]=as<double>(DisF(NodeM.row(MinWLoc),NodeM.row(l)));
        
      }
      IntegerVector ChangeWLocV(WeiMRowNum,0);
      ChangeWLocV=which( (ChangeWDisV<NeiDis)==TRUE );
      ChangeWLocV=ChangeWLocV-1;
  
  
      for ( int m=0;m<ChangeWLocV.length();m++){
    
          WeiM.row(ChangeWLocV[m])=WeiM.row(ChangeWLocV[m])+( exp( - pow(ChangeWDisV[ChangeWLocV[m]],2)/(2*pow(NeiDis,2))) * LearnRate )*( DataD.row(TempRandLocat-1)-WeiM.row(ChangeWLocV[m]) );
 
      }
 
  }
    
    
  }
  
  
  return WeiM;
}
