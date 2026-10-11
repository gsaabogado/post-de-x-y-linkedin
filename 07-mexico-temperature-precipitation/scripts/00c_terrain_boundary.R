# 00c_terrain_boundary.R --------------------------------------------------------
# Acquisition step 3 of 3, on any machine: the national boundary (GADM 4.1,
# through geodata) and the terrain (Terrain Tiles on AWS, through elevatr,
# zoom 7, about 1.2 km). Terrain Tiles are seamless and void-filled; the
# 30 arc-second SRTM mosaic from geodata left tile seams and blurry patches.
#
# Run from the post root: Rscript scripts/00c_terrain_boundary.R
# OUTPUT: in/gadm/gadm41_MEX_0_pk.rds, in/dem/mex_dem_aws.tif

library(terra)
library(sf)
library(elevatr)
library(geodata)

IN <- "in"
Z <- as.integer(Sys.getenv("DEM_Z", "7"))     # 7 is about 1.2 km, 8 about 600 m
dir.create(file.path(IN, "dem"), recursive = TRUE, showWarnings = FALSE)

mx <- geodata::gadm("MEX", level = 0, path = IN) |> st_as_sf()
message("fetching terrain tiles, zoom ", Z)
dem <- get_elev_raster(locations = mx, z = Z, clip = "locations") |> rast()
message("grid ", paste(dim(dem), collapse = " x "), ", resolution ", round(res(dem)[1], 4), " degrees")
writeRaster(dem, file.path(IN, "dem", "mex_dem_aws.tif"), overwrite = TRUE)
message("DONE!")
