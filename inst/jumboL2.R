
fs <- file.path("/rdsi/PRIVATE/data/oceandata.sci.gsfc.nasa.gov/SeaWiFS/L2/1998/001", "S1998001180514.L2_GAC_OC")

x <- readL2(fs, data.frame = FALSE)

##alpha <- inclination - 90
## gamma <- lonc - orbit node longitude

## alpha 8.2
## onl -93.06278229 
## lonc -96.08347321
## gamma -96.08347321 - -93.06278229


## alpha 3.06
## gamma -3.020691

prj <- "+proj=omerc +y_0=0 +x_0=0 +k=1 +units=m +alpha=8.2 +gamma=-3.020691 +lonc=-96.08347321 +lat_0=-13.356072"

xmat0 <- as.matrix(x[["longitude"]])
ymat0 <- as.matrix(x[["latitude"]])
xy <- cbind(as.vector(xmat0), as.vector(ymat0))
xfl <- matrix(as.logical(intToBits(values(x$l2_flags))), nrow(x), byrow = TRUE)
xyp <- project(xy, prj)
graphics.off()
plot(xyp, pch = ".", main = sample(letters, 1))

template <- prj <- "+proj=omerc +y_0=0 +x_0=0 +k=1 +units=m +alpha=%s +gamma=%s +lonc=-96.083473 +lat_0=-13.356072"
alpha <- seq(5, 12, by = 1)
gamma <- seq(-5, 3, by = 1)

ysd <- xsd <- matrix(NA_real_, length(alpha), length(gamma))

for (ia in seq_along(alpha))
  for (ig in seq_along(gamma)) {
    prj <- sprintf(template, alpha[ia], gamma[ia])
    pxy <- try(project(xy, prj))
    if (!inherits(pxy, "try-error")) {
    xmat0[] <- pxy[,1]
    ymat0[] <- pxy[,2]
    xsd[ia, ig] <- sum(apply(xmat0, 1, sd))
    ysd[ia, ig] <- sum(apply(ymat0, 2, sd))
}
  }

    
    
    
    
xy <- x %>% 
  filter(chlor_a > 0) %>%
  dplyr::select(longitude, latitude) %>% as.matrix() %>%  project(prj)
##alpha <- Orbit node longitude - ifelse(Descending, 90, 0)
## gamma <- lonc - orbit node longitude

xy <- project(as.matrix(x[x$chlor_a > 0, c("longitude", "latitude")]), prj)

scl <- function(x) (x - min(x))/diff(range(x))
pal <- chl.pal(palette = TRUE)
##plot(xy, pch = 16, cex = 0.5, col = pal$cols[findInterval(ifelse(x$chlor_a > 0 & x$chlor_a < 100, x$chlor_a, 0), pal$breaks)], asp = 1)

plot(xy, pch = ".",  col = chl.pal(x$chlor_a[x$chlor_a > 0]), asp = "")


r <- rasterFromXYZ(cbind(xy, x$chlor_a))
