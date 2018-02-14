#f <- list.files(u, pattern = "las$", full.names = TRUE)

library(rlas)
library(lidR)
fld <- lidR::catalog(u)
d <- lidR::as.spatial(fld)
library(raster)
projection(d) <- "+proj=utm +zone=55 +south +datum=WGS84"

a <- mapedit::selectFeatures(d)
b <- mapedit::editFeatures(d)
f <- c("5205247.las", "5215247.las", "5205246.las", "5215246.las")
ws = seq(3,21, 3)
th = seq(0.1, 2, length.out = length(ws))

library(lidR)
x <- readLAS(file.path(u, f), select = "xyz")
ws = seq(3,21, 3)
th = seq(0.1, 2, length.out = length(ws))
library(sf)
pp <- sp::spTransform(as(as(b[nrow(b), ], "Spatial"), "SpatialPolygons"), projection(d))
res <- over(SpatialPoints(cbind(x@data$X, x@data$Y), proj4string = CRS(projection(d))), pp)
las <- x
las@data <- las@data[!is.na(res), ]
#frac <- 0.5
#las@data <- las@data[seq(1, nrow(las@data), length = nrow(las@data) * frac)]
lasground(las, "pmf", ws, th)
las <- lasfilter(las, Classification == 2)
library(rgl)
plot3d(as.data.frame(las@data)[c("X", "Y", "Z")], col = "grey", pch = ".")
bg3d("black")
aspect3d("iso")


library(RTriangle)
coords <- as.data.frame(las@data[, c("X", "Y", "Z")])
coords <- coords[!duplicated(coords[, 1:2]), ]
tr <- triangulate(pslg(P = coords[, 1:2], PA = coords[, 3, drop = FALSE]))
rgl.triangles(cbind(tr$P, tr$PA)[t(tr$T), ])

saveRDS(tr, file = "inst/examples/lidar_terrain.rds", compress = "bzip2")
