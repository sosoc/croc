#include <Rcpp.h>
#include <math.h>

using namespace Rcpp;


// copied 2018-04-26 http://www.science.oregonstate.edu/ocean.productivity/vgpm.code.php
double opp_befa( double chl,
                 double irr,
                 double sst,
                 double dayL ) {
  /*
  !C--------------------------------------------------------------------------*\
  
  !Description:     opp_befa - computes daily primary productivity using
  the Behrenfeld-Falkowski (BeFa) algorithm.  The BeFa
  algorithm estimates productivity using surface chl
  (mg m-3), surface irradiance (Einsteins m-2 d-1),
  sea surface temperature (C), and day length (hours).
  Pb_opt is modelled as a polynomial function of SST.
  
  !Input Parameters:  
  chl            Chlorophyll_a surface concentration in milligrams
  chlorophyl per cubic meter
  irr            Photosynthetically available radiation in Einsteins per
  day per square meter
  sst            Sea surface temperature in degrees Centigrade
  dayL           Length day in decimal hours.
  
  !Output Parameters: 
  <return>       Primary productivity in milligrams Carbon per square meter
  per day
  
  !Revision History:  
  
  First programmed up by Monica Chen at Rutgers
  (1996)
  
  Revised by K. Turpie at NASA 
  (August 1997)
  
  Maintained by Don Shea at NASA
  
  Now maintained by Robert O'Malley at Oregon State University  
  (April, 2005 - present)
  
  !References and Credits
  
  Behrenfeld,M.J; Falkowski,P.G.; 1997. Photosynthetic Rates Derived
  from Satellite-Based Chlorophyll Concentration.  Limnology and 
  Oceanography, Volume 42, Number 1
  
  !END------------------------------------------------------------------------*\
  */
  double chl_tot,
  z_eu,
  pb_opt,
  irrFunc,
  npp;
  
  
  /* Calculate euphotic depth (z_eu) with Morel's Case I model.            */
  /* Calculate chl_tot from Satellite Surface Chlorophyll Data.            */
  
  if (chl <  1.0L) 
    chl_tot = 38.0L * pow( chl, 0.425L );
  else
    chl_tot = 40.2L * pow( chl, 0.507L );
  
  
  z_eu = 200.0L * pow( chl_tot, (-.293L) );
  
  if (z_eu <= 102.0L) 
    z_eu = 568.2L * pow( chl_tot, (-.746L) );
  
  
  /* Calculate the Pb_opt from satellite sea surface temperature (sst).    */
  
  if (sst < -10.0L) 
    pb_opt = 0.00L; 
  else if (sst <  -1.0L) 
    pb_opt = 1.13L; 
  else if (sst >  28.5L) 
    pb_opt = 4.00L;
  else {
    pb_opt = 1.2956 + 2.749e-1*sst + 6.17e-2*pow(sst,2) - 2.05e-2*pow(sst, 3)
    + 2.462e-3*pow(sst,4) - 1.348e-4*pow(sst,5) + 3.4132e-6*pow(sst,6) 
    - 3.27e-8*pow(sst,7);
  }
  
  
  /* calculate the irradiance function */
  
  irrFunc = 0.66125L * irr / ( irr + 4.1L );
  
  
  /* Return the primary production calculation.                            */
  
  npp = pb_opt * chl * dayL * irrFunc * z_eu;
  
  return npp;
}

//' Primary production
//' 
//' @export
// [[Rcpp::export]]
NumericVector prod_BeFa(NumericVector chla, NumericVector irrad, NumericVector stemp, NumericVector daylength) {
  NumericVector out(chla.length()); 
  for (int i = 0; i < out.length(); i++) {
    out[i] = opp_befa(chla[i], irrad[i], stemp[i], daylength[i]); 
  }
  return out; 
}