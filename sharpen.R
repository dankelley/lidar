# https://nsgi.novascotia.ca/datalocator/elevation/
name <- "public_gardens"
L <- 400 # extent in m
d <- "~/Downloads/1044600063500_201901_RAW_DEM"
centre <- data.frame(
    ID = 1,
    Y = 44.6428, X = -63.5825
)
width <- 7 # same for height

# no need to edit below this line
label <- sprintf("Centred at %.3fW %.3fN", -centre$X, centre$Y)
library(raster) # to read image
library(oce) # for imagep
library(sf) # for coordinate calculations
library(magick)

# Load data
f <- list.files(d, "*tif$", full.names = TRUE)
l <- raster(f)
getLidarMatrix <- function(l) {
    rval <- getValues(l, format = "matrix") |> t()
    rval <- rval[, rev(seq_len(dim(rval)[2]))]
    rval
}
h <- getLidarMatrix(l)
# Set a focus region (square)
projection <- l@srs
coordinates(centre) <- c("X", "Y")
proj4string(centre) <- CRS("+proj=longlat +datum=WGS84")
C <- spTransform(centre, CRS(projection))
C <- c(xmin(C) - xmin(l), ymin(C) - ymin(l))
pin <- function(x) {
    x <- as.integer(x)
    ifelse(x < 1, 1, x)
}
look <- pin(c(C[1] - L / 2, C[1] + L / 2, C[2] - L / 2, C[2] + L / 2))
H <- h[look[1]:look[2], look[3]:look[4]]
H3 <- array(dim = c(nrow(H), ncol(H), 3))
H3[, , 1] <- H
H3[, , 2] <- H
H3[, , 3] <- H
H3 <- H3 - min(H3)
H3 <- H3 / max(H3)

A <- image_read(H3)
Aedge <- image_edge(A, radius = 5)
plot(image_composite(A, Aedge))
plot(A)
plot(Aedge)
#plot(image_shadow(A))
#plot(image_sharp(A))
plot(Aedge)
methods(class=class(A))
plot(A + Aedge)
range(as.double(A))
str(as.matrix(as.raster(Aedge)))
stop()

imagep(H, asp = 1)
H <- H - min(H)
H <- H / max(H)
H <- as.raster(H)
image_edge(H)
imagep(image_edge(H), asp = 1)

res <- 2 * dim(H)[1] / 7

# Good starting point (ignore the above)
# zlim <- 40 + c(-10, 10)
zlim <- quantile(H, c(0.01, 0.99))


# Turbo colourscheme (not perceptive, but it reveals features quite well)
png(paste0("lidar_", name, "_turbo.png"),
    width = width, height = 6.6 / 7 * width, unit = "in", res = res
)
imagep(H,
    asp = 1, col = oceColorsTurbo, decimate = FALSE,
    zlim = "histogram",
    drawTriangles = TRUE,
)
# points(L / 2, L / 2, pch = 1, lwd = 1, cex = 2)
mtext(label)
dev.off()


# Viridis colourscheme
png(paste0("lidar_", name, ".png"),
    width = width, height = 6.6 / 7 * width, unit = "in", res = res
)
imagep(H,
    asp = 1, decimate = FALSE,
    zlim = zlim,
    drawTriangles = TRUE
)
# points(L / 2, L / 2, pch = 1, lwd = 1, cex = 2)
mtext(label)
dev.off()
