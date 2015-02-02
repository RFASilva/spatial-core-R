# TODO: Refactor some code and perform some comments

up <- function(context) {
  lastResolution <- length(context@spatialResolutions)
  lastRes <- context@spatialResolutions[[lastResolution]]@grid$gridSize
  nextExp <- log2(lastRes) - 1
  
  return(goTo(context,lastRes,2^(nextExp)))
  
}

goTo <- function(context, fromGridSize, toGridSize) {
  
  indFrom <- findPosResInContext(context, fromGridSize)
  assign("indFrom", indFrom, envir=e1)

  indTo <- findRightPos(fromGridSize, toGridSize, indFrom)
  assign("indTo", indTo, envir=e1)
  
  type <- context@typeCoords
  mPropsFunctions <- context@props
  objects <- context@spatialResolutions[[indFrom]]
  numberCells <- toGridSize
  
  startTime <- proc.time()
  cat("Starts ... \n")
  
  ## Converts the spatial expression of each spatial object to the new "spatial" resolution, according
  # to the type of data 
  converted <- convert(type, objects, toGridSize, mPropsFunctions)
  
  fConverted <- proc.time()
  cat("Finishes converted: ",fConverted-startTime, "\n")
    
  points <- sapply(converted, function(x) if(x$type=="Point") x, simplify=FALSE)
  lines <- sapply(converted, function(x) if(x$type =="Line") { if (inOneGranule(x$coords) == TRUE) x}, simplify=FALSE)
  polygons <- sapply(converted, function(x) if(x$type =="Polygon") { if (inOneGranule(x$coords) == TRUE) x}, simplify=FALSE)
  GrS <- sapply(converted, function(x) if(x$type=="GranularSynthesis") x, simplify=FALSE) 
  
  fSeparation <- proc.time()
  cat("Finishes separation: ",fSeparation-fConverted, "\n")
    
  # Extrac all single coordinates (they are the potentially ones to be synthesized)
  aux <- extractSinglePoints(points, lines, polygons, GrS);
  ids <- aux$ids;
  coords <- aux$coords;
  
  #In case of there is nothing to be synthesize
  if(is.null(coords)) {
    converted <- removeNull(converted)
    result <- buildspR(objects@meta, objects@grid, converted, type, toGridSize)
    context <- updateContext(context, result)
    return(context)
  }
  
  fJoining <- proc.time()
  cat("Finishes joining to synth: ",fJoining-fSeparation, "\n")
    
  # Finding the points belonging to the same granules
  u <- uniquecombs(coords); # Same coordinates have the same value in the ind vector
  ind <- attr(u, "index");
  nr_uniques <- unique(ind);
  
  fNumSynth <- proc.time()
  cat("Finishes num of synths (",length(nr_uniques), "): ", fNumSynth-fJoining, "\n")
    
  # Compute Synthesis
  sysID = max(sapply(converted, '[[', 'id')) + 1
  j <- 1;
  sys <- list();
 
  # Compute HashTable
  fhash <- proc.time()
  hashConverted <- buildHashTable(converted)
  fhashend <- proc.time()
  cat("Hash table built: ",fhashend - fhash, "\n")
  
  toRemove <- c();
  
  assign("converted", converted, envir=e1)
  assign("hashConverted", hashConverted, envir=e1)
  assign("context", context, envir=e1)
  
  for(i in nr_uniques) {
    
    objsindex <- which(ind == i);
    nObjs <- length(objsindex)
    
    if(nObjs > 1) { 
      fComputeSynth <- proc.time()
      idsingrs <- as.character(ids[objsindex])
      assign("idsingrs", idsingrs, envir=e1)
      sys[[j]] <- computeGranularSynthesis(coords[objsindex[1],], sysID, toGridSize)
      sysID <- sysID + 1
      j<-j+1
     
      toRemove <- append(toRemove, idsingrs, after = length(toRemove))
      #cat("Synth time: ",proc.time()-fComputeSynth, "\n")
    }
  }
  
  if(!is.null(toRemove))
    delete(toRemove, hashConverted);
  
  result <- as.list(hashConverted);
  result <- append(result, sys, after = length(result));
  spobjs <- buildspR(objects@meta, objects@grid, result, type, toGridSize)  
  context <- updateContext(context, spobjs)
  
  total <- proc.time()
  cat("Total: ",total-startTime, "\n")
    
  return(context);
}

computeGranularSynthesis <- function(spatial_granule, sysID, numberCells ) {
  
  ids <- e1[["idsingrs"]]
  allObjects <- e1[["converted"]]
  hashObjects <- e1[["hashConverted"]]
  context <-e1[["context"]]
  indFrom <- e1[["indFrom"]]
  
  objects <- context@spatialResolutions[[indFrom]]
  prevGrid <-  objects@grid
  
  objectsInGranule <- as.list(hashObjects[ids])
  
  # Distinct Attributes on Objects
  attrs <- sapply(objectsInGranule, '[[', 'attr')
  distinct_attrs <- unlist(removeNull(attrs))
  
  # Distinct attributes already stored on previous syntheses
  prevSynthesis <- sapply(objectsInGranule, function(x) if (x$type == "GranularSynthesis")  x, simplify=FALSE)
  
  temp <- sapply(prevSynthesis, '[[', 'synthesis', simplify =FALSE)
  attrsInSyntheses <- rownames(do.call(rbind, temp))
  distinct_attrs <- append(distinct_attrs, attrsInSyntheses, after = length(distinct_attrs))
  distinct_attrs <- unique(distinct_attrs)
  
  flag2 <- proc.time()
  #cat("atributos distintos", flag2 - flag1, "\n")
  
  # Getting properties to be synthesized
  props <- objectsInGranule[[1]]$properties
  nprops <- length(props)
  if(is.null(props)) {
    props <- objectsInGranule[[1]]$synthesis
    nprops <- ncol(props)
  }
 
  # Create the Partition
  n <- length(distinct_attrs);
  partition <- matrix(NA, length(distinct_attrs), nprops)
  
  colnames(partition) <- colnames(props)
  rownames(partition) <- distinct_attrs
  
  flag3 <- proc.time()
  #cat("particao", flag3 - flag2, "\n")
  
  for(i in (1:length(distinct_attrs))) {
    synAttr <- sapply(objectsInGranule, function(x) if (x$type == "Point" || x$type == "Line" || x$type == "Polygon") {
      if(x$attr == distinct_attrs[i])  x}, simplify=FALSE)
    
    synAttr <- removeNull(synAttr)
    partition[distinct_attrs[i], ] <- applySynOperators(distinct_attrs[i], synAttr, prevSynthesis,colnames(partition), prevGrid, numberCells)
  }
  
  flag4 <- proc.time()
  GrS <- list(id = sysID, type ="GranularSynthesis", synthesis = partition , coords = matrix(spatial_granule,1,2), 
              references = ids);
  
  return (GrS)
}

applySynOperators <- function(t, synAttr, prevSynthesis, properties, prevGrid, numberCells) {
  context <-e1[["context"]]
  mSySFunctions <- context@props
  
  newVector <- rep(NA,length(properties))
  
  # Properties Vectors From Objects
  vectorsObjs <- t(sapply(synAttr, '[[', 'properties'))

  # Properties vectors from synthesis
  temp <- sapply(prevSynthesis, function(x) if (any(t == rownames(x$synthesis))) x$synthesis[t, ], simplify =FALSE)
  vectorSynthesis <- do.call(rbind, temp)
  
  # Joining vectors (from objects and synthesis) to be merged
  if(length(vectorsObjs)!=0) {
    vectors <- vectorsObjs
    if(!is.null(vectorSynthesis)) {
      vectors <- rbind(vectorsObjs, vectorSynthesis)
    }
  }
  else {
    vectors <- vectorSynthesis
  }
  
  for(i in (1:length(properties))) {
    index <- which(properties[i] == mSySFunctions[,1])
    functionName <- mSySFunctions[index, 3]
    f <- match.fun(functionName)
    
    # Functions supported
    if(functionName == "soma")
      newVector[i] <- do.call(f, list(vectors[,i]))
    if(functionName == "computeDensity") {
      assign("context", context, envir=e1)
      newVector[i] <- do.call(f, list(vectors[,1], vectors[,i], numberCells)) #Assuming vectors[,1] is alsways the count property
    }  
  }
  
  return(newVector)
}

convert <- function(type,...) {
  boundArgs <- list(...)
    
  objects <- boundArgs[[1]]
  numberCells <- boundArgs[[2]]
  mPropsFunctions <- boundArgs[[3]]
  
  if(type=="int") {
    prevnumbercells <- unlist(objects@grid$gridSize)
    converted <- sapply(objects@spatialObjects, function(x) mapping(type, x, (prevnumbercells / numberCells), mPropsFunctions))
  }
  else if(type =="geo") {
    #note: gridSize is resolution
    gridSize <- (objects@grid$precision * objects@grid$gridSize) / numberCells
    converted <- sapply(objects@spatialObjects, function(x) mapping(type, x, objects@grid$minCoords, gridSize, mPropsFunctions))  
  }
}

mapping <- function (type, x, ...) {
  
  boundArgs <- list(...)
  
  #mapping coordinates
  if(type == "int") {
    scaleFactor <- boundArgs[[1]]
    mPropsFunctions <- boundArgs[[2]]
    f <- match.fun("convertIntCoords")
    coordsMapped = do.call(f, list(x$coords, scaleFactor))
  }
  else if(type == "geo") {
    f <- match.fun("convertGeoCoords")
    minCoords <- boundArgs[[1]]
    gridSize <-  boundArgs[[2]]
    mPropsFunctions <- boundArgs[[3]]
    coordsMapped = do.call(f, list(x$coords, minCoords, gridSize))
  }
  
  if(x$type != "GranularSynthesis" ) {
    list(list(id = x$id, type = x$type, attr = x$attr, properties = upProperties(x$properties, mPropsFunctions, scaleFactor),
              coords = coordsMapped))
  }
  else if(x$type == "GranularSynthesis") {
    list(list(id = x$id, type = x$type, synthesis = x$synthesis, coords = coordsMapped , references = x$references))
  }
}

convertGeoCoords <- function(coords, minCoods, gridSize) {
  # Nota: gridSize é pixel size (resolution)
  minLat <- unlist(minCoods)[1]
  minLong <- unlist(minCoods)[2]
  
  nr <- nrow(coords);
  
  diffs <- floor(cbind(coords[,1] - minLat, coords[,2] - minLong) / gridSize) * gridSize;
  temp <- cbind(diffs[, 1] + minLat, diffs[, 2] + minLong)
  
  coordsConverted <- temp + (gridSize/2);
  return(coordsConverted)
}

convertIntCoords <- function(coords, res) {
  return(ceiling((coords/res)))
}

upProperties <- function(propertyVector, mPropsFunctions, scaleFactor) {
  propertiesNames <- colnames(propertyVector)
  
  for(i in 1:length(propertiesNames)) {
    
    index <- which(propertiesNames[i] == mPropsFunctions[,1]);
    functionName <- mPropsFunctions[index, 2]
    f <- match.fun(functionName)

    if(functionName == "upCount")
      propertyVector[i] <- do.call(f, list(propertyVector[i]))
    else if(functionName == "upLength")
      propertyVector[i] <- do.call(f, list(propertyVector[i], scaleFactor))
    else if(functionName == "upArea")
      propertyVector[i] <- do.call(f, list(propertyVector[i], scaleFactor)) 
    else if(functionName == "upUnconverted")
      propertyVector[i] <- do.call(f, list(propertyVector[i]))
  }

  return(propertyVector)
}

extractSinglePoints <- function (points, lines, polygons, GrS) {
  ## Compute Granular Syntheses, if necessary  
  ids <- unlist(sapply(points, '[[' , "id"))
  coords <- t(sapply(points, '[[', "coords"))
  coords <- cleanList2Matrix(coords)
  
  # It was guaranteed that the spatial expression of lines and polygons have only one spatial granule
  idsLines <- unlist(sapply(lines, '[[' , "id"))
  coordsLines <- cleanList2Matrix(sapply(lines, function(x) x$coords[1,]))
  
  idsPolygons <- unlist(sapply(polygons, '[[' , "id"))
  coordsPolygons  <- cleanList2Matrix(sapply(polygons, function(x) x$coords[1,]))
  
  idsGr <- unlist(sapply(GrS, '[[' , "id"))
  coordsGr  <- cleanList2Matrix(sapply(GrS, function(x) x$coords))
  
  
  # Join all coordinates and corresponding ids
  if(!is.null(coordsLines)) {
    ids <- append(ids, idsLines, after = length(ids))
    coords <- rbind(coords,coordsLines)
  }
  
  if(!is.null(coordsPolygons)) {
    ids <- append(ids, idsPolygons, after = length(ids))
    coords <- rbind(coords,coordsPolygons)
  }
  
  if(!is.null(coordsGr)) {
    ids <-append(ids, idsGr, after = length(ids))
    coords <- rbind(coords,coordsGr)
  }
  
  return(list(ids = ids, coords = coords))  
}


# Remove consecutive elements in a list. 
# To be used to simply the spatial expression of the spatial objects'
remConsecElems <- function (coords, type) { 
  # Coords is a n * 2 matrix. Diff returns the difference between row_i+1 and row_i.
  #Therefore, if diff returns c(0,0) then row_i+1 and row_i are equal.
  result <- which(apply(diff(coords), 1, function(x) if(all(x == c(0,0))) TRUE else FALSE) ==TRUE)  
  
  if(length(result) > 1) {
      coords <- coords[-result,]
  }
  else coords <- coords;
  

  if(type=="Line") {
    if(length(coords) == 2) {
      coords <- matrix(rep(coords, 2), 2, 2, byrow=TRUE)  
    }
  }
  else if(type=="Polygon") {
    if(length(coords) == 2) {
      coords <- matrix(rep(coords, 4), 4, 2, byrow=TRUE)
    }
  }
  
  return(coords)
}

# delele null/empty entries in a list
removeNull  <-  function(x.list){   
  x.list[unlist(lapply(x.list, length) != 0)]
}

cleanList2Matrix <- function (coords) {
  if(is.list(coords)) {
    coords <- removeNull(coords)
    coords <- do.call(rbind, coords)
  }
  return(coords)
}

inOneGranule <- function(coords) {
  if(length(unique(coords))==2) {
    return(TRUE)
  }
  return(FALSE)
}

updateContext <- function(context, newObjects) {
  indFrom <- e1[["indFrom"]]
  indTo <- e1[["indTo"]]
  context@spatialResolutions[[indTo]] <- newObjects;
  
  return(context)
}

buildspR <- function(meta, grid, spObjs, type, gridSize) {
  # um if desnecessario mas e para alterar no futuro
  if(type=="int") {
    result <- new("SpatialResolution", meta=meta, grid=grid, spatialObjects=spObjs) 
    oldGridSize <- result@grid$gridSize
    newGridSize <- gridSize
    oldPrecision <- result@grid$precision
    result@grid$precision <- (result@grid$precision * oldGridSize) / newGridSize
    result@grid$gridSize <- newGridSize;
    result@grid$minCoords <- floor(result@grid$minCoords / result@grid$precision) * result@grid$precision
    result@grid$maxCoords <- floor(result@grid$maxCoords / result@grid$precision) * result@grid$precision
  }
  else if(type =="geo") {
    result <- new("SpatialResolution", meta=meta, grid=grid, spatialObjects=spObjs) 
    oldGridSize <- result@grid$gridSize
    newGridSize <- gridSize
    result@grid$precision <- (result@grid$precision * oldGridSize) / newGridSize
    result@grid$gridSize <- newGridSize;
    result@grid$minCoords <- floor(result@grid$minCoords / result@grid$precision) * result@grid$precision
    result@grid$maxCoords <- floor(result@grid$maxCoords / result@grid$precision) * result@grid$precision
  }
  
  return(result);
}


buildHashTable <- function(objects) {
  h <- hash()
  sapply(objects, function(x) addEntry(h, x$id, x))
  return(h)
}

addEntry <- function(table, id, element) {
  table[[as.character(id)]] <- element
}

# Find the right position to assign the spatialResolution class in the 
# list of spatialResolutions in the context class 
findRightPos <- function (prevGridSize, newGridSize, currPos) {
  expPrevgridsize <- log2(prevGridSize)
  expnewgridsize <- log2(newGridSize)

  delta <- expPrevgridsize - expnewgridsize
  
  return(currPos + delta)
}


findPosResInContext <- function(context, gridSize) {
  temp <- -1
  n <- length(context@spatialResolutions)
  for(i in (1:n)) {
    spR <- context@spatialResolutions[[i]]
    
    if(!is.null(spR)) {
      if(spR@grid$gridSize == gridSize) {
        temp <- i
        break
      }
    }
  }
  
  return(temp)
}



