 /*
 This program extracts data from level-3 HDF bin files and 
 writes them out as an ASCII table.
 
 Regions of interest can be specified by latitude and longitude
 boundaries or by a central coordinate and a radius in kilometers.
 
 Norman Kuring       14-Feb-2003 Original development
 Norman Kuring       11-Dec-2007 Fix memory-overrun bug and add a
                     couple of calls to free().
 Norman Kuring       21-Dec-2011 Give a precision when printing out
                     bit strings to avoid unwanted printing
                     of uninitialized memory.
 Norman Kuring       21-Mar-2013 Change the latbin array from 32-bit
                     floats to 64-bit to reduce rounding-
                     error effects on bin numbers at smaller
                     bin sizes.  Thanks to George White for
                     pointing this one out.
 */
 
 #include <stdio.h>
 #include <stdlib.h>
 #include <string.h>
 #include <math.h>
 #include "hdf.h"
 
 #define PI  3.1415926535897932384626433832795029L
 
 double sqrt(double x);
 double asin(double x);
 double sin(double x);
 double cos(double x);
 double fabs(double x);
 
 #define INFILE  argv[1]
 #define PARAM   argv[2]
 
 #define USAGE(prog) fprintf(stderr,"\
 Usage:\n\
 %s main_file_path parameter bin_number\n\
 or\n\
 %s main_file_path parameter lat lon radius\n\
 or\n\
 %s main_file_path parameter north south west east\n\
 ",prog,prog,prog)
 
 #define NUMROWS     2160
 #define EARTH_RADIUS    6371.229
 
 static int32    basebin[NUMROWS];
 static int16    numbin[NUMROWS];
 static float64  latbin[NUMROWS];
 static int32    totbins;
 
 #define BLIST_FIELDS "bin_num,nobs,nscenes,time_rec,weights,sel_cat,flags_set"
 #define BLIST_SIZE  19
 static uint8    blistrec[BLIST_SIZE];
 static int32    bin_num,flags_set;
 static int16    nobs,nscenes,time_rec;
 static float32  weights;
 static uint8    sel_cat;
 static VOIDP    bufptrs[] = {
         &bin_num,&nobs,&nscenes,&time_rec,&weights,&sel_cat,&flags_set
         };
 
 #define PREC_SIZE   8
 static uint8    paramrec[PREC_SIZE];
 static char *param_fields;
 static float32  sum,sum_sq;
 static VOIDP    paramptrs[] = {&sum,&sum_sq};
 
 void    initbin(void);
 int32   latlon2bin(double lat, double lon);
 void    bin2latlon(int32 bin, double *clat, double *clon);
 void    bin2bounds(int32 bin, double *n, double *s, double *w, double *e);
 int16   lat2row(double lat);
 int32   rowlon2bin(int16 row, double lon);
 int32   binsearch(int32 bin, int32 vdata_id, int32 numrecs);
 double  constrain_lat(double lat);
 double  constrain_lon(double lon);
 char    *bitstr16(int16 n);
 char    *bitstr32(int32 n);
 
 int main(int argc, char *argv[]){
 
   int32     numbins = 0, *binnums = NULL;
   int32     i;
   int32     file_id,vdata_ref,vdata_id,numrecs,pvd_id;
 
   initbin();
 
   if(argc == 4){
     /* Input arguments are: main_file_path parameter bin_number. */
     int32   bin_number;
     bin_number = atol(argv[3]);
     numbins = 1;
     binnums = (int32 *)malloc(sizeof(int32));
     if(binnums == NULL){
       fprintf(stderr,"-E- %s line %d: Memory allocation failed.\n",
       __FILE__,__LINE__);
       return(EXIT_FAILURE);
     }
     binnums[0] = bin_number;
   }
 
   else if(argc == 6){
     /* Input arguments are: main_file_path parameter lat lon radius. */
     double  clat, clon, radius, radius_degrees;
 
     clat    = constrain_lat(atof(argv[3]));
     clon    = constrain_lon(atof(argv[4]));
     radius = atof(argv[5]);
 
     radius_degrees = (radius/EARTH_RADIUS) * (180.0/PI);
     if(radius_degrees > 180){
       int32 b;
 
       /* The entire globe has been selected. */
       binnums = (int32 *)realloc(binnums,totbins*sizeof(int32));
       if(binnums == NULL){
         fprintf(stderr,"-E- %s line %d: Memory allocation failed.\n",
     __FILE__,__LINE__);
     return(EXIT_FAILURE);
       }
       for(b = 1; b <= totbins; b++){
     binnums[numbins++] = b;
       }
     }
     else{
       double    north, south;
       double    sin_c_over_2;
       int16 n_row, s_row, row;
 
       sin_c_over_2 = sin((radius/EARTH_RADIUS)/2);
 
       north  = clat + radius_degrees;
       south  = clat - radius_degrees;
 
       if(north > 90){
     /*
         Chosen radius extends north to pole and over the other side to some
     latitude south of the north pole.  Set the new north to this new
         latitude.  All bins north of this latitude get selected.
     */
         int32   n, b;
 
     north = 180 - north;
     n_row = lat2row(north);
         n = totbins - basebin[n_row] + 1;
         binnums = (int32 *)realloc(binnums,(numbins + n)*sizeof(int32));
         if(binnums == NULL){
           fprintf(stderr,"-E- %s line %d: Memory allocation failed.\n",
           __FILE__,__LINE__);
           return(EXIT_FAILURE);
         }
         for(b = basebin[n_row]; b <= totbins; b++){
           binnums[numbins++] = b;
         }
       }
       else{
         n_row = lat2row(north) + 1;
       }
 
       if(south < -90){
     /*
     Chosen radius extends south to pole and over the other side to some
     latitude north of the south pole.  Set the new south to this new
         latitude.  All bins south of this latitude get selected.
     */
         int32   n, b;
 
         south = -180 - south;
     s_row = lat2row(south);
         n = basebin[s_row] + numbin[s_row];
         binnums = (int32 *)realloc(binnums,(numbins + n)*sizeof(int32));
         if(binnums == NULL){
           fprintf(stderr,"-E- %s line %d: Memory allocation failed.\n",
           __FILE__,__LINE__);
           return(EXIT_FAILURE);
         }
         for(b = 1; b <= n; b++){
           binnums[numbins++] = b;
         }
       }
       else{
     s_row = lat2row(south) - 1;
       }
 
       for(row = s_row + 1; row < n_row; row++){
     double  deltalon;
     double  sin_deltalat_over_2;
         double  west, east;
 
     sin_deltalat_over_2 = sin((latbin[row] - clat)*(PI/180.0) / 2);
 
         /*
         The following equation is a rearranged version of
         Equation 5-3a on page 30 of
         Map Projections -- A Working Manual
         by John P. Snyder
         U.S. Geological Survey Professional Paper 1395
         1987
         (Fourth printing 1997)
         */
     deltalon = 2 * asin( sqrt( fabs(
         (sin_c_over_2*sin_c_over_2 - sin_deltalat_over_2*sin_deltalat_over_2)
         /( cos(latbin[row]*(PI/180.0)) * cos(clat*(PI/180.0)) )
         )));
 
         deltalon *= 180.0/PI;
 
         west = constrain_lon(clon - deltalon);
         east = constrain_lon(clon + deltalon);
 
     if(east < west){
           /* User's region of interest spans the 180-degree meridian. */
           int32   b,b1,b2,b3,b4,n1,n2;
           b1 = rowlon2bin(row,west);
           b2 = rowlon2bin(row, 180);
           b3 = rowlon2bin(row,-180);
           b4 = rowlon2bin(row,east);
           n1 = b2 - b1 + 1;
           n2 = b4 - b3 + 1;
           binnums = (int32 *)realloc(binnums,(numbins + n1 + n2)*sizeof(int32));
           if(binnums == NULL){
             fprintf(stderr,"-E- %s line %d: Memory allocation failed.\n",
             __FILE__,__LINE__);
             return(EXIT_FAILURE);
           }
           for(b = b1; b <= b2; b++){
             binnums[numbins++] = b;
           }
           for(b = b3; b <= b4; b++){
             binnums[numbins++] = b;
           }
         }
         else{
           /* User's region of interest does not span the 180-degree meridian. */
           int32   b,b1,bn,n;
   
           b1 = rowlon2bin(row,west);
           bn = rowlon2bin(row,east);
           n = bn - b1 + 1;
           binnums = (int32 *)realloc(binnums,(numbins + n)*sizeof(int32));
           if(binnums == NULL){
             fprintf(stderr,"-E- %s line %d: Memory allocation failed.\n",
             __FILE__,__LINE__);
             return(EXIT_FAILURE);
           }
           for(b = b1; b <= bn; b++){
             binnums[numbins++] = b;
           }
         }
       }
     }
   }
 
   else if(argc == 7){
     /* Input arguments are: main_file_path parameter north south west east. */
     double  north, south, west, east;
     int16   n_row, s_row, row;
 
     north = constrain_lat(atof(argv[3]));
     south = constrain_lat(atof(argv[4]));
     west  = constrain_lon(atof(argv[5]));
     east  = constrain_lon(atof(argv[6]));
 
     if(south > north){
       double    tmp;
 
       fprintf(stderr,"-W- %s line %d: ",__FILE__,__LINE__);
       fprintf(stderr,
       "Specified south latitude is greater than specified north latitude.\n");
       fprintf(stderr,"I will swap the two.\n");
       tmp = north;
       north = south;
       south = tmp;
     }
 
     n_row = lat2row(north);
     s_row = lat2row(south);
 
     for(row = s_row; row <= n_row; row++){
 
       if(east < west){
         /* User's region of interest spans the 180-degree meridian. */
         int32   b,b1,b2,b3,b4,n1,n2;
         b1 = rowlon2bin(row,west);
     b2 = rowlon2bin(row, 180);
     b3 = rowlon2bin(row,-180);
     b4 = rowlon2bin(row,east);
     n1 = b2 - b1 + 1;
     n2 = b4 - b3 + 1;
     binnums = (int32 *)realloc(binnums,(numbins + n1 + n2)*sizeof(int32));
     if(binnums == NULL){
           fprintf(stderr,"-E- %s line %d: Memory allocation failed.\n",
           __FILE__,__LINE__);
       return(EXIT_FAILURE);
     }
         for(b = b1; b <= b2; b++){
           binnums[numbins++] = b;
     }
     for(b = b3; b <= b4; b++){
           binnums[numbins++] = b;
     }
       }
       else{
         /* User's region of interest does not span the 180-degree meridian. */
         int32   b,b1,bn,n;
 
         b1 = rowlon2bin(row,west);
         bn = rowlon2bin(row,east);
         n = bn - b1 + 1;
         binnums = (int32 *)realloc(binnums,(numbins + n)*sizeof(int32));
         if(binnums == NULL){
           fprintf(stderr,"-E- %s line %d: Memory allocation failed.\n",
           __FILE__,__LINE__);
           return(EXIT_FAILURE);
         }
         for(b = b1; b <= bn; b++){
           binnums[numbins++] = b;
         }
       }
     }
   }
 
   else{
     USAGE(argv[0]);
     return(EXIT_FAILURE);
   }
 
   /*
   Now that I have a list of desired bins, I can extract the
   corresponding data from the level-3 bin files.
   */
 
   /* Open the HDF file. */
   file_id = Hopen(INFILE, DFACC_READ, 0);
   if(file_id == FAIL){
     fprintf(stderr,"-E- %s line %d: Hopen(%s,DFACC_READ,0) failed.\n",
     __FILE__,__LINE__,INFILE);
     return(EXIT_FAILURE);
   }
   /* Initialize the Vdata interface. */
   if(Vstart(file_id) == FAIL){
     fprintf(stderr,"-E- %s line %d: Vstart(%d) failed.\n",
     __FILE__,__LINE__,file_id);
     return(EXIT_FAILURE);
   }
 
   /* Open the "BinList" Vdata. */
   vdata_ref = VSfind(file_id,"BinList");
   if(vdata_ref == FAIL){
     fprintf(stderr,"-E- %s line %d: VSfind(%d,\"BinList\") failed.\n",
     __FILE__,__LINE__,file_id);
     return(EXIT_FAILURE);
   }
   vdata_id = VSattach(file_id, vdata_ref, "r");
   if(vdata_id == FAIL){
     fprintf(stderr,"-E- %s line %d: VSattach(%d,%d,\"r\") failed.\n",
     __FILE__,__LINE__,file_id,vdata_ref);
     return(EXIT_FAILURE);
   }
   /* Find out how many bins are stored in this file. */
   numrecs = VSelts(vdata_id);
   if(numrecs == FAIL){
     fprintf(stderr,"-E- %s line %d: VSelts(%d) failed.\n",
     __FILE__,__LINE__,vdata_id);
     return(EXIT_FAILURE);
   }
   /* Set up to read the fields in the BinList Vdata records. */
   if(VSsetfields(vdata_id,BLIST_FIELDS) == FAIL){
     fprintf(stderr,"-E- %s line %d: VSsetfields(%d,%s) failed.\n",
     __FILE__,__LINE__,vdata_id,BLIST_FIELDS);
     return(EXIT_FAILURE);
   }
 
   /* Open the parameter-specific Vdata. */
   vdata_ref = VSfind(file_id,PARAM);
   if(vdata_ref == 0){
     fprintf(stderr,"-E- %s line %d: VSfind(%d,\"%s\") failed.\n",
     __FILE__,__LINE__,file_id,PARAM);
     return(EXIT_FAILURE);
   }
   pvd_id = VSattach(file_id, vdata_ref, "r");
   if(pvd_id == FAIL){
     fprintf(stderr,"-E- %s line %d: VSattach(%d,%d,\"r\") failed.\n",
     __FILE__,__LINE__,file_id,vdata_ref);
     return(EXIT_FAILURE);
   }
   /* Set up to read the fields in the parameter-specific Vdata records. */
   {
     int len;
     len = 2*strlen(PARAM) + strlen("_sum,") + strlen("_sum_sq") + 1;
     param_fields = (char *)malloc(len);
     if(param_fields == NULL){
       fprintf(stderr,"-E- %s line %d: Memory allocation failed.\n",
       __FILE__,__LINE__);
       return(EXIT_FAILURE);
     }
     strcpy(param_fields,PARAM);
     strcat(param_fields,"_sum,");
     strcat(param_fields,PARAM);
     strcat(param_fields,"_sum_sq");
   }
   if(VSsetfields(pvd_id,param_fields) == FAIL){
     fprintf(stderr,"-E- %s line %d: VSsetfields(%d,%s) failed.\n",
     __FILE__,__LINE__,pvd_id,param_fields);
     return(EXIT_FAILURE);
   }
 
   /* Output a header record to identify the fields written out below. */
   printf("%80s%15.15s %15.15s\n"," ",PARAM,PARAM);
   printf("    bin centerlat  centerlon");
   printf("     north     south       west       east");
   printf("    n   N         sum_obs sum_squared_obs          weight");
   printf("  time_trend_bits                     l2_flag_bits sel\n");
   printf("------- --------- ----------");
   printf(" --------- --------- ---------- ----------");
   printf(" ---- --- --------------- --------------- ---------------");
   printf(" ---------------- -------------------------------- ---\n");
 
   for(i = 0; i < numbins; i++){
     int32   recno;
 
     recno = binsearch(binnums[i],vdata_id,numrecs);
     if(recno >= 0){
       double    n,s,w,e,clat,clon;
 
       /*
       Read the sum and sum-of-squares for the
       the specified parameter for this bin.
       */
       if(VSseek(pvd_id,recno) == FAIL){
         fprintf(stderr,"-E- %s line %d: VSseek(%d,%d) failed.\n",
         __FILE__,__LINE__,pvd_id,recno);
     return(EXIT_FAILURE);
       }
       if(VSread(pvd_id,paramrec,1,FULL_INTERLACE) != 1){
         fprintf(stderr,"-E- %s line %d: ",__FILE__,__LINE__);
     fprintf(stderr,"VSread(%d,paramrec,1,FULL_INTERLACE) failed.\n",
     pvd_id);
         return(EXIT_FAILURE);
       }
       /*
       VSfpack() sets the global sum and sum_sq variables
       via the paramptrs pointer array.
       */
       if(
       VSfpack(
       pvd_id,_HDF_VSUNPACK,param_fields,paramrec,PREC_SIZE,1,NULL,paramptrs
       )
       == FAIL){
         fprintf(stderr,"-E- %s line %d: ",__FILE__,__LINE__);
     fprintf(stderr,"VSfpack(%d, ...) failed.\n", pvd_id);
     return(EXIT_FAILURE);
       }
 
       /* Get the geographical coordinates associated with this bin. */
       bin2latlon(binnums[i],&clat,&clon);
       bin2bounds(binnums[i],&n,&s,&w,&e);
 
       /* Output the results. */
       printf("%7d %9.5f %10.5f %9.5f %9.5f %10.5f %10.5f ",
       binnums[i],clat,clon,n,s,w,e);
       printf("%4d %3d ",nobs,nscenes);
       printf("% .8e % .8e % .8e ",sum,sum_sq,weights);
       printf("%.16s %.32s ",bitstr16(time_rec),bitstr32(flags_set));
       printf("%3d",sel_cat);
       printf("\n");
     }
 
   }
 
   if(VSdetach(pvd_id) == FAIL){
     fprintf(stderr,"-E- %s line %d: VSdetach(%d) failed.\n",
     __FILE__,__LINE__,pvd_id);
     return(EXIT_FAILURE);
   }
   if(VSdetach(vdata_id) == FAIL){
     fprintf(stderr,"-E- %s line %d: VSdetach(%d) failed.\n",
     __FILE__,__LINE__,vdata_id);
     return(EXIT_FAILURE);
   }
   if(Vend(file_id) == FAIL){
     fprintf(stderr,"-E- %s line %d: Vend(%d) failed.\n",
     __FILE__,__LINE__,file_id);
     return(EXIT_FAILURE);
   }
   if(Hclose(file_id) == FAIL){
     fprintf(stderr,"-E- %s line %d: Hclose(%d) failed.\n",
     __FILE__,__LINE__,file_id);
     return(EXIT_FAILURE);
   }
 
   free(param_fields);
   free(binnums);
 
   return(EXIT_SUCCESS);
 } /* End of main() function */
 
 int32 binsearch(int32 bin, int32 vdata_id, int32 numrecs){
   int32 lo, hi, mid;
 
   lo = 0;
   hi = numrecs - 1;
   while(lo <= hi){
     mid = (lo + hi)/2;
     if(VSseek(vdata_id,mid) == FAIL){
       fprintf(stderr,"-E- %s line %d: VSseek(%d,%d) failed.\n",
       __FILE__,__LINE__,vdata_id,mid);
       exit(EXIT_FAILURE);
     }
     if(VSread(vdata_id,blistrec,1,FULL_INTERLACE) != 1){
       fprintf(stderr,"-E- %s line %d: ",__FILE__,__LINE__);
       fprintf(stderr,"VSread(%d,blistrec,1,FULL_INTERLACE) failed.\n",
       vdata_id);
       exit(EXIT_FAILURE);
     }
     /*
     VSfpack() sets the global bin_num variable (and others)
     via the bufptrs pointer array.
     */
     if(
     VSfpack(
     vdata_id,_HDF_VSUNPACK,BLIST_FIELDS,blistrec,BLIST_SIZE,1,NULL,bufptrs
     )
     == FAIL){
       fprintf(stderr,"-E- %s line %d: ",__FILE__,__LINE__);
       fprintf(stderr,"VSfpack(%d, ...) failed.\n", vdata_id);
       exit(EXIT_FAILURE);
     }
     if     (bin < bin_num) hi = mid - 1;
     else if(bin > bin_num) lo = mid + 1;
     else                   return(mid);
   }
   return(-1);
 }
 
 char *bitstr16(int16 n){
   static char   str[17];
   int       i;
 
   str[16]=0;
   for(i = 0; i < 16; i++){
     if(n & (1 << i)) str[i] = '1';
     else             str[i] = '0';
   }
   return(str);
 }
 
 char *bitstr32(int32 n){
   static char   str[33];
   int       i;
 
   str[32] = 0;
   for(i = 0; i < 32; i++){
     if(n & (1 << i)) str[i] = '1';
     else             str[i] = '0';
   } 
   return(str);
 }
 
 /*
 The following functions are based on the pseudocode found in Appendix A of:
 
 Campbell, J.W., J.M. Blaisdell, and M. Darzi, 1995:
 Level-3 SeaWiFS Data Products: Spatial and Temporal Binning Algorithms.
 NASA Tech. Memo. 104566, Vol. 32,
 S.B. Hooker, E.R. Firestone, and J.G. Acker, Eds.,
 NASA Goddard Space Flight Center, Greenbelt, Maryland
 */
 
 void initbin(void){
   int   row;
 
   basebin[0] = 1;
   for(row=0; row<NUMROWS; row++){
     latbin[row] = ((row + 0.5)*180.0/NUMROWS) - 90.0;
     numbin[row] = (int16)(2*NUMROWS*cos(latbin[row]*PI/180.0) + 0.5);
     if(row > 0){
       basebin[row] = basebin[row - 1] + numbin[row - 1];
     }
   }
   totbins = basebin[NUMROWS - 1] + numbin[NUMROWS - 1] - 1;
 }
 
 int16 lat2row(double lat){
   int16 row;
 
   row = (int16)((90 + lat)*NUMROWS/180.0);
   if(row >= NUMROWS) row = NUMROWS - 1;
   return(row);
 }
 
 int32 rowlon2bin(int16 row, double lon){
   int16 col;
   int32 bin;
 
   lon = constrain_lon(lon);
   col = (int16)((lon + 180.0)*numbin[row]/360.0);
   if(col >= numbin[row]) col = numbin[row] - 1;
   bin = basebin[row] + col;
   return(bin);
 }
 
 int32 latlon2bin(double lat, double lon){
   int16 row, col;
   int32 bin;
 
   /* Constrain latitudes to [-90,90] and longitudes to [-180,180]. */
   lat = constrain_lat(lat);
   lon = constrain_lon(lon);
 
   row = lat2row(lat);
   col = (int16)((lon + 180.0)*numbin[row]/360.0);
   if(col >= numbin[row]) col = numbin[row] - 1;
   bin = basebin[row] + col;
   return(bin);
 }
 
 void bin2latlon(int32 bin, double *clat, double *clon){
   int16 row;
 
   row = NUMROWS - 1;
   if(bin < 1) bin = 1;
   while(bin < basebin[row]) row--;
   *clat = latbin[row];
   *clon = 360.0*(bin - basebin[row] + 0.5)/numbin[row] - 180.0;
 }
 
 void bin2bounds(
 int32   bin,
 double  *north,
 double  *south,
 double  *west,
 double  *east
 ){
   int16     row;
   double    lon;
 
   row = NUMROWS - 1;
   if(bin < 1) bin = 1;
   while(bin < basebin[row]) row--;
   *north = latbin[row] + 90.0/NUMROWS;
   *south = latbin[row] - 90.0/NUMROWS;
   lon = 360.0*(bin - basebin[row] + 0.5)/numbin[row] - 180.0;
   *west = lon - 180.0/numbin[row];
   *east = lon + 180.0/numbin[row];
 }
 
 double constrain_lat(double lat){
   if(lat >  90) lat =  90;
   if(lat < -90) lat = -90;
   return(lat);
 }
 
 double constrain_lon(double lon){
   while(lon < -180) lon += 360;
   while(lon >  180) lon -= 360;
   return(lon);
 }
