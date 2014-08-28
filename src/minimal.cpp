// source taken with permission from Norman Kuring (NASA)
// http://oceancolor.gsfc.nasa.gov/DOCS/OCSSW/l3bindump_8c_source.html
// and from smigen.cpp (NASA, Seadas)
#include <math.h>
#include "hdf.h"
#include <Rcpp.h>
using namespace Rcpp;

#define BLIST_FIELDS "bin_num,nobs,nscenes,time_rec,weights,sel_cat,flags_set"
#define BLIST_SIZE  19
//static uint8    blistrec[BLIST_SIZE];
//static int    bin_num;
//static int flags_set;
//static short    nobs,nscenes,time_rec;
//static float32  weights;
//static uint8    sel_cat;
//static VOIDP    bufptrs[] = {
//  &bin_num,&nobs,&nscenes,&time_rec,&weights,&sel_cat,&flags_set
//};

//
//#define PREC_SIZE   8
//static uint8    paramrec[PREC_SIZE];
//static char *param_fields;
//static float32  summ,sum_sq;
//static VOIDP    paramptrs[] = {&summ,&sum_sq};


//' Basic L3 bin files 
//'
//' Read from L3 bin. 
//' @param filename path to L3 bin OC file (HDF4)
//' @param vname names of VData parameters to read (will read both _sum and _ssq)
//' @export
// [[Rcpp::export]]
List minlist(CharacterVector filename, CharacterVector vname) {
  // file name
  std::string fname = Rcpp::as<std::string>(filename);
  const char * c_filename = fname.c_str();

  /* counters */
  int b, i, jvar, nvar, numrecs, numbins = 0, *binnums = NULL;
  /* file refs */
  int file_id, vdata_ref, vdata_id,pvd_id;
  
  file_id = Hopen(c_filename, DFACC_READ, 0);
  /* Initialize the Vdata interface. */
  Vstart(file_id);
  vdata_ref = VSfind(file_id,"BinIndex");
  vdata_id = VSattach(file_id, vdata_ref, "r");
  /* Find out how many rows */
  int numrs = VSelts(vdata_id);
  
  
  // collect Rcpp vectors in a list
  List z  = List::create();
  z["NUMROWS"] = numrs;
  
  // finish up, close the file
  VSdetach(vdata_id);
  
  

  /* Open the "BinList" Vdata. */
  vdata_ref = VSfind(file_id,"BinList");
  
  vdata_id = VSattach(file_id, vdata_ref, "r");
  /* Find out how many bins are stored in this file. */
  numrecs = VSelts(vdata_id);
  

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
  
    VSseek(vdata_id,i); 
    VSread(vdata_id,blistrec,numrecs,FULL_INTERLACE); 
    /*
    VSfpack() sets the global bin_num variable (and others)
    via the bufptrs pointer array.
    */
 
    VSfpack(vdata_id,_HDF_VSUNPACK,BLIST_FIELDS,blistrec, bibuf, numrecs,NULL,bufptrs); 
    fprintf(stderr,"bin: %d\n",bin_num[0]);
    fprintf(stderr,"bin: %d\n",bin_num[4]);
    fprintf(stderr,"wgt: %f\n",weights[0]);
    fprintf(stderr,"wgt: %f\n",weights[1000]);
   
   
   
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
 // z["bin_num"] = bin_num;
  z["nobs"] = nobservations;
  z["nscenes"] = nsc;
  z["weights"] = wghts;
  


  // finish up, close the file
  VSdetach(vdata_id); 
  Vend(file_id);
  Hclose(file_id); 
 // free(param_fields);
  free(binnums);  
  return z;
}



