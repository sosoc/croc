// source taken with permission from Norman Kuring (NASA)
// http://oceancolor.gsfc.nasa.gov/DOCS/OCSSW/l3bindump_8c_source.html
// and from smigen.cpp (NASA, Seadas)
// http://oceancolor.gsfc.nasa.gov/DOCS/OCSSW/smigen_8cpp_source.html

#include <math.h>

#include <Rcpp.h>
using namespace Rcpp;

//' Longitude and latitude from bin number.  
//'
//' Generate longitude and latitude coordinates from bin number. 
//' @param bins bin number
//' @param nrows number of rows in this grid
//' @export
// [[Rcpp::export]]
List bin2lonlat(IntegerVector bins, IntegerVector nrows){
  int NUMROWS = nrows[0];
  int row;
  int *basebin = new int[NUMROWS];
  //IntegerVector basebin(NUMROWS);
  short *numbin = new short[NUMROWS];
  double *latbin = new double[NUMROWS];
  int totbins;
  
  basebin[0] = 1;
  for(row=0; row<NUMROWS; row++){
    latbin[row] = ((row + 0.5)*180.0/NUMROWS) - 90.0;
    //numbin[row] = (int16)(2*NUMROWS*cos(latbin[row]*PI/180.0) + 0.5);
    numbin[row] = (short)(2*NUMROWS*cos(latbin[row]*PI/180.0) + 0.5);
    if(row > 0){
      basebin[row] = basebin[row - 1] + numbin[row - 1];
    }
  }
  totbins = basebin[NUMROWS - 1] + numbin[NUMROWS - 1] - 1;
  
  int bin, ibin;
  int n = bins.size();
  NumericVector clon(n);
  NumericVector clat(n);
  
  for (ibin = 0; ibin < n; ibin++) {
    row = NUMROWS - 1;
    //row = numrows[1] - 1;
    bin = bins[ibin];
    while(bin < basebin[row]) row--;
    clat[ibin] = latbin[row];
    clon[ibin] = 360.0*(bin - basebin[row] + 0.5)/numbin[row] - 180.0;
  }
  delete[] basebin;
  delete[] numbin;
  delete[] latbin;
  
  List lonlat = List::create();
  lonlat["x"] = clon;
  lonlat["y"] = clat;
  return lonlat;
}

//int16 lat2row(double lat){
//  int16 row;
//  
//  row = (int16)((90 + lat)*NUMROWS/180.0);
//  if(row >= NUMROWS) row = NUMROWS - 1;
//  return(row);
//}
//
//int32 rowlon2bin(int16 row, double lon){
//  int16 col;
//  int32 bin;
//  
//  lon = constrain_lon(lon);
//  col = (int16)((lon + 180.0)*numbin[row]/360.0);
//  if(col >= numbin[row]) col = numbin[row] - 1;
//  bin = basebin[row] + col;
//  return(bin);
//}

//double constrain_lat(double lat){
//   if(lat >  90) lat =  90;
//   if(lat < -90) lat = -90;
//   return(lat);
// }
// 
// double constrain_lon(double lon){
//   while(lon < -180) lon += 360;
//   while(lon >  180) lon -= 360;
// return(lon);
// }

//
//
//
//char *bitstr16(int16 n){
//  static char   str[17];
//  int       i;
//  
//  str[16]=0;
//  for(i = 0; i < 16; i++){
//    if(n & (1 << i)) str[i] = '1';
//    else             str[i] = '0';
//  }
//  return(str);
//}
//
//char *bitstr32(int32 n){
//  static char   str[33];
//  int       i;
//  
//  str[32] = 0;
//  for(i = 0; i < 32; i++){
//    if(n & (1 << i)) str[i] = '1';
//    else             str[i] = '0';
//  } 
//  return(str);
//}
//


////' Parameters from initbin. 
////'
////' Initbin. 
////' @export
//// [[Rcpp::export]]
//List initlist() {
//  initbin();
//  List alist = List::create(); 
//  alist["totbins"] = totbins; 
//  alist["NUMROWS"] = NUMROWS;
////  alist["basebin"] = basebin;
////  alist["latbin"] = latbin;
//  return alist;
//}


//  /* The entire globe */
//  binnums = (int *)realloc(binnums,totbins*sizeof(int));
//  if(binnums == NULL){
//    fprintf(stderr,"-E- %s line %d: Memory allocation failed.\n", __FILE__,__LINE__);
//    return(EXIT_FAILURE);
//  }
//  for(b = 1; b <= totbins; b++) { 
//    binnums[numbins++] = b;
//  }
//  
//

/*
The following functions are based on the pseudocode found in Appendix A of:

Campbell, J.W., J.M. Blaisdell, and M. Darzi, 1995:
Level-3 SeaWiFS Data Products: Spatial and Temporal Binning Algorithms.
NASA Tech. Memo. 104566, Vol. 32,
S.B. Hooker, E.R. Firestone, and J.G. Acker, Eds.,
NASA Goddard Space Flight Center, Greenbelt, Maryland
*/
//
//List binparameters(int NUMROWS){
//  int   row;
//  basebin[0] = 1;
//  for(row=0; row<NUMROWS; row++){
//    latbin[row] = ((row + 0.5)*180.0/NUMROWS) - 90.0;
//    //numbin[row] = (int16)(2*NUMROWS*cos(latbin[row]*PI/180.0) + 0.5);
//    numbin[row] = (short)(2*NUMROWS*cos(latbin[row]*PI/180.0) + 0.5);
//    if(row > 0){
//      basebin[row] = basebin[row - 1] + numbin[row - 1];
//    }
//  }
//  totbins = basebin[NUMROWS - 1] + numbin[NUMROWS - 1] - 1;
//}

