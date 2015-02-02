
# RETURNS COORDS FROM SPATIAL RESOLUTION THAT WHERE WITHIN BBOX
# receives bbox 
# returns list of coords within bbox 
withinSpatialSlice <- function( bbox, context, res) {
  auxSpatialSlice(context, res, function(x) {   
    # dá para pontos, testar com outros?
    if(auxWithin(bbox, x$coords)) { x } else { print(x); NULL }
  })
}

# RETURNS COORDS FROM SPATIAL RESOLUTION THAT WHERE WITHIN a BUFFER OF RADIUS FROM SPEFIC POINTS (COORDS)
# receives list of coords (rows of 2 colls - can be only 1) and radius for buffer
# returns list of coords within buffers
bufferSpatialSlice <- function( coords, radius, context, res ) {
  auxSpatialSlice(context, res, function(x) {
    # dá para pontos, testar com outros?
    if(auxRadialBuffer(coords,radius,x$coords)) { x } else { NULL }
  })
}

# returns true if coord within bbox
# bbox should be matrix of 2x2 - rows as coords (southwest to northeast), first col as long, second col as lat
# coord should be long/lat
auxWithin <- function(bbox, coord) {
#    long lat
# sw   m   m
# ne   M   M
  return (!any( ((bbox[1,] <= coord) & (coord <= bbox[2,])) == FALSE ))
}

# returns true if coord within any of radial buffers of coords
auxRadialBuffer <- function(coords, radius, coord) {
  # coord é center
  # nao e' super eficiente, mas funciona ... existem outras forma alem da distancia desta forma
  checkIfInRadius <- apply(coords, 1, function(x) { 
    sum((x-coord)^2) <= (radius^2)
  })
  return(any(checkIfInRadius == TRUE))
}

# aux function eceives function
# returns list of coords within function
auxSpatialSlice <- function(context, res, f, args) {
  
  ind <- findPosResInContext(context, res)
  data <- context@spatialResolutions[[ind]]
  
  result <- sapply(data@spatialObjects, f)
  result <- removeNull(result)
  return(result)
}


###### spatial expression on upper levels #######

# gets coords (a row of long,lat), orgGridSize and toGridSize, 
# and returns new coords at the original res (orgGridSize), that form perfect coordinates at res toGridSize 
# assuming that orgGridSize > toGridSize
extendedCoord <- function(coord, orgGridSize, orgPrecision, toGridSize) {
  
  # gets jumps from org -> to
  jump <- (orgGridSize / toGridSize)*orgPrecision
  print(jump)
  long <- coord[1]
  lat <- coord[2]
  print(long)
  print(lat)
  
  maxLong <- long + ( (jump - (long %% jump)) %% jump)
  maxLat <- lat + ( (jump - (lat %% jump)) %% jump)
  minLong <- maxLong - jump # check if +1 needed?
  minLat <- maxLat - jump # check if +1 needed?
  
  print(maxLong)
  print(minLong)
  print(maxLat)
  print(minLat)
  
  allLongs <- seq(minLong, maxLong, by=orgPrecision)
  allLats <- seq(minLat, maxLat, by=orgPrecision)
  
  print(allLongs)
  print(allLats)
  
  newCoords <- matrix(sapply(allLongs,function(x) {
    rows <- t(cbind(x,allLats))
    return(rows)
  }),ncol=2,byrow=TRUE)
  
  return(newCoords)
  #return(NULL)
}
