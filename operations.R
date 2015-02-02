performSlices <- function(context, gridSize) {

  nrSlices <- length(context@previousSlices)
  ind <- findPosResInContext(context, gridSize)
  print(ind)
  data <- context@spatialResolutions[[ind]]@spatialObjects
  
  for(i in( 1:nrSlices)) {
    slice <- context@previousSlices[[i]];
    functionName <- slice$functionName
    args <- slice$args
    f <- match.fun(functionName)
    
    if(functionName == "semanticSlice")
      data <- do.call(f, list(args[[1]], data))
    if(functionName == "propertySliceGreaterThan")
      data <- do.call(f, list(args[[1]], args[[2]], data))
  
  }
  
  context@spatialResolutions[[ind]]@spatialObjects <- data
  
  return(context)
}

clearSlices <- function(context) {
  context@previousSlices <- list()
  return(context)
}

# Tenho que colocar aqui predicados
attachSemanticSlice <- function(context, t) {
  nrSlices <- length(context@previousSlices)
  context@previousSlices[[nrSlices + 1]] <- list(functionName="semanticSlice", args = list(t))
  return(context)
}


attachPropertySliceGreaterThan <- function(context, property, value) {
  nrSlices <- length(context@previousSlices)
  context@previousSlices[[nrSlices + 1]] <- list(functionName="propertySliceGreaterThan", args = list(property, value))
  return(context)
}

semanticSlice <- function(t, data) {  
  result <- sapply(data, function(x) 
    if(x$type == "Point" || x$type == "Line" || x$type == "Polygon") {
      if(x$attr == t) {
        list(id = x$id, type =x$type, properties =  x$properties, coords = x$coords)
      }
    }
    else {
       if (any(t == rownames(x$synthesis))) {
         temp <- matrix(x$synthesis[t, ], 1, length(x$synthesis[t, ]), byrow = TRUE)
         rownames(temp) <- c(t)
         colnames(temp) <- colnames(x$synthesis)
         list(id = x$id, type =x$type, synthesis =  temp, 
              coords = x$coords, references = x$references)   
       }
     }
  )
  result <- removeNull(result)  
  return(result)
}

propertySliceGreaterThan <- function(property, value, data) {
  
  result <- sapply(data, function(x) 
    if(x$type == "Point" || x$type == "Line" || x$type == "Polygon") {
      if(x$properties[1, property] >= value) {
        list(id = x$id, type =x$type, properties =  x$properties, coords = x$coords)
      }
    }
    else {
       indexes <- which(x$synthesis[, property] >= value)
       if(length(indexes) > 0) {
         
         temp <- matrix(x$synthesis[unname(indexes), ], length(indexes), length(x$synthesis[1, ]), byrow = TRUE)
         
         rownames(temp) <-rownames(x$synthesis)[indexes]
         colnames(temp) <- colnames(x$synthesis)
         list(id = x$id, type =x$type, synthesis =  temp, 
              coords = x$coords, references = x$references)
       }
    }
  )
  result <- removeNull(result)  
  return(result)
  
}

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

# aux function receives function
# returns list of coords within function
auxSpatialSlice <- function(context, res, f, args) {
  
  ind <- findPosResInContext(context, res)
  data <- context@spatialResolutions[[ind]]
  
  result <- sapply(data@spatialObjects, f)
  result <- removeNull(result)
  return(result)
}

reductionRatio <- function(context, ...) {
  
  boundArgs <- list(...)
  # Elements at fine resolution
  if(length(boundArgs) == 1) {
    posFrom <- 1
    toGridSize <- boundArgs[[1]]
    posTo <- findPosResInContext(context, toGridSize)
  }
  
  if(length(boundArgs) == 2) {
    fromGridSize <- boundArgs[[1]]
    toGridSize <- boundArgs[[2]]
    posFrom <- findPosResInContext(context, fromGridSize)
    posTo <- findPosResInContext(context, toGridSize)
  }
  
  nrobjects1 <- length(context@spatialResolutions[[posFrom]]@spatialObjects)
  nrobjects2 <- length(context@spatialResolutions[[posTo]]@spatialObjects)
  
  cat("Numero de objectos from:", nrobjects1, "\n")
  cat("Numero de objectos to:", nrobjects2, "\n")
  
  
  return(1-(nrobjects2/nrobjects1))
}
