#' Day length
#'
#' @param lat latitude
#' @param doy day of year
#'
#' @return hours 
#' @export
#'
daylength <- function(lat, doy) {

  # get lat in radians
  gamma = lat / 180.0 * pi
  
  ## convert date into an angle
  psi = doy / 365.0 * 2.0 * pi
  
  # calc solar declination
  # Kirk page 35
  solarDec = (0.39637
                     - 22.9133 * cos(psi)
                     + 4.02543 * sin(psi)
                     - 0.38720 * cos(2*psi)
                     + 0.05200 * sin(2*psi)) * pi / 180.0;
  
  r = -tan(gamma) * tan(solarDec);
  r[r <= -1] <- 24
  r[abs(r) < 1] <- 24 * acos(r[abs(r) < 1]) / pi
  r[r > 24] <- 24
r
  }
