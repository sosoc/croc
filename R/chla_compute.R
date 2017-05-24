#' MODISA chlorophyll-a in the southern hemisphere. 
#' 
#' Compute chlorophyll data in the L3 bins, using both Johnson and Oceancolor algorithms. 
#' 
#' Use `raadtools::ocfiles(product = "MODISA", varname = "RRS", type = "L3b")` to get all daily MODISA L3 bin RRS files, and
#' explore the `date` column to generate a set of `days` for this function. 
#' @param days dates to calculate
#' @param maxbin a maximum bin to use, for specialist use only
#' @return tibble of bins and chlorophyll-a values
#' @export
#' @importFrom tibble tibble
#' @importFrom progress progress_bar
#' @importFrom dplyr mutate filter select transmute
#' @importFrom rlang .data
chla_compute <- function(days,  maxbin = NULL) {
  ## data frame of all L3 RRS files for MODISA
  files <- ocfiles(time.resolution = "daily", product = "MODISA", varname = "RRS", type = "L3b", bz2.rm = TRUE, ext = "nc") 
  days <- as.Date(as.POSIXct(days, tz = "GMT"))
  
  files <- files[as.Date(files$date) %in% days, ]
  
  
  ## initialize the bin logic for MODISA
  init <- initbin(NUMROWS = 4320)
  ## counts up from the south
  if (is.null(maxbin)) maxbin <- init$totbin/2
  ## accumulate sum and count for mean
  count_rj <- cumul_rj <- numeric(maxbin)
  count_nasa <- cumul_nasa <- numeric(maxbin)
  
  ## track progress
  pb <- progress_bar$new(total = nrow(files))
  for (ifile in seq(nrow(files))) {
    d <- read_L3_file(files$fullname[ifile], compound_vars = c("Rrs_443", "Rrs_488", "Rrs_555", "Rrs_547")) %>% 
      filter(.data$bin_num <= maxbin) %>% 
      mutate(chla_johnson = chla(.data, sensor = "MODISA", algo = "johnson")) %>% 
      mutate(chla_nasa = chla(.data, sensor = "MODISA", algo = "oceancolor")) %>% 
      dplyr::select(.data$bin_num, .data$chla_johnson, .data$chla_nasa) %>% 
      filter(.data$chla_johnson > 0)
    
    ## we accumulate outside of the data frame to ensure speed
    ## the bin_num indexes the vector structure directly since the south pole is bin 1
    count_rj[d$bin_num] <- count_rj[d$bin_num] + 1
    cumul_rj[d$bin_num] <- cumul_rj[d$bin_num] + d$chla_johnson
    
    count_nasa[d$bin_num] <- count_nasa[d$bin_num] + 1
    cumul_nasa[d$bin_num] <- cumul_nasa[d$bin_num] + d$chla_nasa
    ## track progress
    pb$tick()
  }
  
  tibble(bin_num = seq_len(maxbin), count_nasa = count_nasa, cumul_nasa = cumul_nasa, 
         count_rj = count_rj, cumul_rj = cumul_rj) %>% filter(.data$count_rj > 0)
}
