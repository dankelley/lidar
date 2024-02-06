# Data source
#     https://nsgi.novascotia.ca/datalocator/elevation/

# Part 1: things to edit
dir <- "~/Downloads/1044600063500_201901_DEM"
name <- "citadel"
L <- 500 # extent in m (cannot be much larger since at image edge)
centre <- data.frame(ID = 1, X = -63.5804, Y = 44.6475)
Q <- 0.99

# Part 2: things that likely do not need editing
width <- 7
height <- 7
res <- round(2 * L / width)
library(raster) # to read image
library(oce) # for imagep
library(sf) # for coordinate calculations

# Load data
l <- list.files(dir, "*tif$", full.names = TRUE) |> raster()
getLidarMatrix <- function(l) {
    rval <- getValues(l, format = "matrix") |> t()
    rval[, dim(rval)[2] |> seq_len() |> rev()]
}
h <- getLidarMatrix(l)

# Set a focus region (square)
projection <- l@srs
coordinates(centre) <- c("X", "Y")
proj4string(centre) <- CRS("+proj=longlat +datum=WGS84")
C <- spTransform(centre, CRS(projection))
C <- c(xmin(C) - xmin(l), ymin(C) - ymin(l))
pin <- function(x) {
    # FIXME: should also check if too large
    x <- as.integer(x)
    ifelse(x < 1, 1, x)
}
look <- pin(c(C[1] - L / 2, C[1] + L / 2, C[2] - L / 2, C[2] + L / 2))
H <- h[look[1]:look[2], look[3]:look[4]]
dimH <- dim(H)
x <- seq_len(dimH[1]) # metres
y <- seq_len(dimH[2])
g <- oce::grad(H, x, y)
G <- g$gx - g$gy # FIXME: allow for illumination azimuth and altitude
water <- H <= 0
q <- as.numeric(quantile(G[!water], Q))
GG <- G
GG[water] <- NA

if (!interactive()) {
    png(paste0("lidar_", name, "_shaded.png"),
        width = width, height = height, unit = "in", res = res,
        type = "cairo", antialias = "none", family = "Arial"
    )
}
imagep(x, y, GG,
    zlim = c(-q, q),
    asp = 1,
    col = cmocean::cmocean("solar"), # golden hues
    decimate = FALSE,
    missingColor = "blue", # water
    mar = c(2.0, 2.0, 1.0, 1.0),
    drawPalette = FALSE
)
mtext(sprintf("Centre at %.3fW %.3fN", -centre$X, centre$Y))
if (!interactive()) {
    dev.off()
}
message("Created ", paste0("lidar_", name, "_shaded.png"))
