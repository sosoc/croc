//get_vdata_info.c
//bingo  http://www.hdfgroup.org/training/HDFtraining/UsersGuide/Example_Vd.fm.html#928503
#include "hdf.h"
#define  FIELD_SIZE     80         /* maximum length of all the field names */

#include <Rcpp.h>
using namespace Rcpp;

//' Vdata names and field names in HDF4. 
//' 
//' Retrieve vdata names and their field names from HDF4. 
//' @param file name of HDF4 file containing Vdata
//' @return Named list, each element with the field names of the named Vdata. 
//' @export
//' @examples
//' \dontrun{
//' f <- "A2014202.L3b_DAY_PIC.main"
//' if (!file.exists(f)) {
//'   download.file(sprintf("http://oceandata.sci.gsfc.nasa.gov/cgi/getfile/%s.bz2", f), sprintf("%s.bz2", f), mode = "wb")
//'   system(sprintf("bunzip2 %s.bz2", f))
//' } 
//'   vdatainfo(f)
//' 
//' 
//' }
// [[Rcpp::export]]
List vdatainfo(CharacterVector file) {
  
  /************************* Variable declaration **************************/
    intn  status_n;      /* returned status for functions returning an intn  */
    int32 status_32,     /* returned status for functions returning an int32 */
          n_records,     /* to retrieve the number of records in the vdata   */
          interlace_mode,/* to retrieve the interlace mode of the vdata      */
          vdata_size,    /* to retrieve the size of all specified fields     */
          file_id, vdata_ref, vdata_id;
    char  fieldname_list[FIELD_SIZE], /* buffer to retrieve the vdata data   */
          vdata_name[VSNAMELENMAX];   /* buffer to retrieve the vdata name   */
    /********************** End of variable declaration **********************/
    
    
   std::string fname = Rcpp::as<std::string>(file);
   const char * FILE_NAME = fname.c_str();
  
  List VDataNames = List::create();
  //int iVDataNames = 0;
    /*
    * Open the HDF file for reading. 
    */
    file_id = Hopen (FILE_NAME, DFACC_READ, 0);
    /*
    * Initialize the VS interface. 
    */
    status_n = Vstart (file_id);
    /*
    * Set vdata_ref to -1 to start the search from the beginning of file.
    */
    vdata_ref = -1;
    /*
    * Use VSgetid to obtain each vdata by its reference number then attach 
    * to the vdata and get its information.  The loop terminates when 
    * the last vdata is reached.
    */
    while ((vdata_ref = VSgetid (file_id, vdata_ref)) != FAIL)
    {
       /*
       * Attach to the current vdata for reading.
       */
       vdata_id = VSattach (file_id, vdata_ref, "r");
       
       /*
       * Test whether the current vdata is not a storage of an attribute, then
       * obtain and display its information.
       */
       if( VSisattr (vdata_id) != TRUE )
       {
          status_n = VSinquire (vdata_id, &n_records, &interlace_mode, 
                             fieldname_list, &vdata_size, vdata_name);
          VDataNames[vdata_name] = fieldname_list;
       }
      
       /*
       * Detach from the current vdata.
       */
       status_32 = VSdetach (vdata_id);
    } 
    /*
    * Terminate access to the VS interface and close the HDF file. 
    */
    status_n = Vend (file_id);
    status_32 = Hclose (file_id);
    
    return(VDataNames);
 }
