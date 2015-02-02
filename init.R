# Initialize libraries
library(sp)
library(maptools)
library(rgdal)
library(raster)
library(rgeos)
library(mgcv)
library(RgoogleMaps)
library(calibrate)
library(hash)
library(classInt)
library(IDPmisc)
library(grDevices)
library(plotGoogleMaps)
#library(foreach)

source("./mylibrary.R")
source("./files.R")
source("./core.R")
source("./externalFunctions.R")
source("./visualization.R")
source("./operations.R")


# Create a class Spatial Resolution
# This class allows to store the spatial objects associated to the grid at which they are described
setClass("SpatialResolution", sealed=F, representation(meta = "matrix", grid = "list",  spatialObjects = "list"))
setClass("Context", sealed=F, representation(spatialResolutions = "list", props = "matrix", typeCoords="character", previousSlices="list"))

e1 <- new.env(parent = baseenv())
options(digits= 15)
#setRefClass("Context", fields = list(spatialResolutions = "list", props = "matrix", typeCoords="character"))

