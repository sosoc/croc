// source taken with permission from Norman Kuring (NASA)
// http://oceancolor.gsfc.nasa.gov/DOCS/OCSSW/l3bindump_8c_source.html
// and from smigen.cpp (NASA, Seadas)
// http://oceancolor.gsfc.nasa.gov/DOCS/OCSSW/smigen_8cpp_source.html

#include <math.h>
#include "hdf.h"

#include <Rcpp.h>
using namespace Rcpp;

#define BLIST_FIELDS "bin_num,nobs,nscenes,time_rec,weights,sel_cat,flags_set"



//' Basic L3 bin files 
//'
//' Read from L3 bin. 
//' @param filename path to L3 bin OC file (HDF4)
//' @param vname names of VData parameters to read (will read both _sum and _ssq)
//' @param bins read out the bin number and other metadata (default TRUE)
//' @export
// [[Rcpp::export]]
List binlist(CharacterVector filename, CharacterVector vname, LogicalVector bins) {
  // file name
  std::string fname = Rcpp::as<std::string>(filename);
  
  // collect Rcpp vectors in a list
  List z  = List::create() ;
  
  
  /* counters */
  int i, jvar, nvar, numrecs, *binnums = NULL;
  /* file refs */
  int file_id, vdata_ref, vdata_id,pvd_id;
  
  // updated MDS 2014-08-09
  //const char * c_filename = fname.c_str();
  file_id = Hopen(fname.c_str(), DFACC_READ, 0);
  /* Initialize the Vdata interface. */
  Vstart(file_id);
  
  
  vdata_ref = VSfind(file_id,"BinIndex");
  vdata_id = VSattach(file_id, vdata_ref, "r");
  /* Find out how many rows */
  int numrs = VSelts(vdata_id);
  //IntegerVector xl;
  z["NUMROWS"] = numrs;
  
  // finish up, close the file
  VSdetach(vdata_id);
  
  
  
  /* Open the "BinList" Vdata. */
  vdata_ref = VSfind(file_id,"BinList");
  
  vdata_id = VSattach(file_id, vdata_ref, "r");
  /* Find out how many bins are stored in this file. */
  numrecs = VSelts(vdata_id);
  
  if (bins[0]) {
    
    /* Rcpp structures for shared binlist */
    IntegerVector bin(numrecs);
    IntegerVector nobservations(numrecs);
    IntegerVector nsc(numrecs);
    NumericVector wghts(numrecs);
    
    /* Set up to read the fields in the BinList Vdata records. */
    VSsetfields(vdata_id,BLIST_FIELDS);
    
    int * bin_num = new int[numrecs];
    int * flags_set = new int[numrecs];
    short * nobs = new short[numrecs];
    short * nscenes = new short[numrecs];
    short * time_rec = new short[numrecs];
    
    float32 * weights = new float32[numrecs];
    uint8 *  sel_cat = new uint8[numrecs];
    VOIDP    bufptrs[] = {
      &bin_num[0],&nobs[0],&nscenes[0],&time_rec[0],&weights[0],&sel_cat[0],&flags_set[0]
    };
    
    int bibuf = 19 * numrecs;
    uint8 * blistrec = new uint8[bibuf];
    
    // pretty sure this is not needed
    //VSseek(vdata_id,i); 
    VSread(vdata_id,blistrec,numrecs,FULL_INTERLACE); 
    /*
    VSfpack() sets the global bin_num variable (and others)
    via the bufptrs pointer array.
    */
    
    VSfpack(vdata_id,_HDF_VSUNPACK,BLIST_FIELDS,blistrec, bibuf, numrecs,NULL,bufptrs); 
    
    // separate loops for vdatat_id and pvd_id, because they seek to different places
    for (i = 0; i < numrecs; i++ ) {
      // populate the Rcpp vectors
      bin[i] = bin_num[i];
      nobservations[i] = nobs[i];
      nsc[i] = nscenes[i];
      wghts[i] = weights[i];
      
    }   
    
    //  
    z["bin_num"] = bin;
    z["nobs"] = nobservations;
    z["nscenes"] = nsc;
    z["weights"] = wghts;
    
  } 
  // loop over all parameters, get the sum/ssq for each
  nvar = vname.size();
  for (jvar = 0; jvar < nvar; jvar++) {
    // reset the sum/ssq Rcpp vectors each time
    NumericVector parsum(numrecs);
    NumericVector parssq(numrecs);
    
    std::string vstrname  = Rcpp::as<std::string>(vname[jvar]);   
    
    int PREC_SIZE = 8 * numrecs;
    //int * bin_num = new int[numrecs];
    uint8 * paramrec = new uint8[PREC_SIZE];
    char *param_fields;
    float32 * summ = new float32[numrecs];
    float32 * sum_sq = new float32[numrecs];
    VOIDP    paramptrs[] = {&summ[0], &sum_sq[0]};
    
    
    /* Open the parameter-specific Vdata. */
    // updated MDS 2014-08-09
    //const char * PARAM = vstrname.c_str();
    vdata_ref = VSfind(file_id,vstrname.c_str());
    pvd_id = VSattach(file_id, vdata_ref, "r");
    /* Set up to read the fields in the parameter-specific Vdata records. */
    {
      int len;
      //len = 2*strlen(PARAM) + strlen("_sum,") + strlen("_sum_sq") + 1;
      len = 2*vstrname.size() + strlen("_sum,") + strlen("_sum_sq") + 1;
      param_fields = (char *)malloc(len);
      strcpy(param_fields,vstrname.c_str());
      strcat(param_fields,"_sum,");
      strcat(param_fields,vstrname.c_str());
      strcat(param_fields,"_sum_sq");
    }
    
    VSsetfields(pvd_id,param_fields); 
    
    /*
    Read the sum and sum-of-squares for the
    the specified parameter for this bin.
    */
    
    //VSseek(pvd_id,i); 
    VSread(pvd_id,paramrec, numrecs, FULL_INTERLACE); 
    /*
    VSfpack() sets the global sum and sum_sq variables
    via the paramptrs pointer array.
    */
    VSfpack(pvd_id,_HDF_VSUNPACK,param_fields,paramrec,PREC_SIZE, numrecs, NULL,paramptrs); 
    
    
    for (i = 0; i < numrecs; i++ ) { 
      parsum[i] = summ[i];
      parssq[i] = sum_sq[i];
      
    }
    // this has to come *after* the Rcpp loop just above
    free(param_fields);
    
    
    //removal of recursive list, all at one level
    //List z2  = List::create();
    String nm1 = vstrname;
    nm1 += "_sum";
    String nm2 = vstrname;
    nm2 += "_ssq";
    z[nm1] = parsum;
    z[nm2] = parssq; 
    
    VSdetach(pvd_id);
    
  }
  
  // finish up, close the file
  VSdetach(vdata_id); 
  Vend(file_id);
  Hclose(file_id); 
  
  free(binnums);
  
  return z;
}



//' Longitude and latitude from bin number.  
//'
//' Generate longitude and latitude coordinates from bin number. 
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

