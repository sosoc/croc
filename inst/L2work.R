library(roc)
fs <- list.files("/rdsi/PRIVATE/data/oceandata.sci.gsfc.nasa.gov/SeaWiFS/L2/1998/001", pattern = "L2_GAC_OC$", full.names = TRUE)
i <- 1
x <- roc:::readL2(fs[i]) %>% filter(Rrs_443 > 0)

#pdf()
library(maps)

ice <- NULL
day <- NULL
## 3 and 4 the same
cols <- rainbow(20)[1:15]
for (i in seq_along(fs)) {
 # map()
#  title(basename(fs[i]))
x <- roc:::readL2(fs[i])
x$SEAICE <- as.logical(matrix(intToBits(x$l2_flags), nrow = 32)[25, ])
day <- bind_rows(day, x %>% filter(chlor_a > 0))
#with(x %>% filter(Rrs_443 > -32767), points(longitude, latitude, pch = "." ,, col = cols[i]))
#ice <- bind_rows(ice, x %>% filter(SEAICE))
}
#dev.off()


day$bin <- lonlat2bin(day$longitude, day$latitude, 2160)

bin <- day %>% group_by(bin) %>% summarize(chl = mean(chl), n = n())

ice1 <- ice %>% filter(latitude < 0)
xy <- project(cbind(ice1$longitude, ice1$latitude), projection(readice()))
plot(xy, pch = ".")


x <- roc:::readL2(fs[14])
ice <- readice("1998-01-01")
library(rgdal)
xy <- project(cbind(x$longitude, x$latitude), projection(ice))
xy <- coordinates(spTransform(SpatialPoints(cbind(x$longitude, x$latitude), CRS("+proj=longlat +ellps=sphere")), 
                  projection(ice)))

x$X <- xy[,1]
x$Y <- xy[,2]


e <- new("Extent"
         , xmin = -532719.133309823
         , xmax = 825192.382970118
         , ymin = -2541936.71556527
         , ymax = -1288479.93130686
)

plot(e, asp = 1, type = "n")
plot(readice("1998-01-01"), add = TRUE, zlim = c(.1, 100), col = rev(terrain.colors(46))[-c(1:20)])

points(project(do.call(cbind, bin2lonlat(bin$bin[bin$bin %in% binex], 2160)), projection(readice())), pch = 16, col = palr::chlPal(bin$chl[bin$bin %in% binex]), cex = 0.6)

plot(e, asp = 1, type = "n")
plot(readice("1998-01-01"), add = TRUE, zlim = c(.1, 100), col = rev(terrain.colors(46))[-c(1:20)])

points(project(do.call(cbind,bin2lonlat(l3$bin_num[l3$bin_num %in% binex], 2160)),
                       projection(readice())), pch = 16, col = palr::chlPal(l3$chl[l3$bin_num %in% binex]), cex = 0.6)



with(x %>% filter(chlor_a > 0), points(X, Y, pch = 16, cex = .2, col = palr::chlPal(chlor_a)))
with(x %>% filter(Rrs_443 > -32767), points(X, Y, pch = 16))

x$SEAICE <- as.logical(matrix(intToBits(x$l2_flags), nrow = 32)[25, ])

with(x %>% filter(SEAICE), points(X, Y, pch = 16, cex = 0.2))


with(x %>% filter(SEAICE), plot(X, Y, pch = "."))
