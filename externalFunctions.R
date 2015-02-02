upUnconverted <- function(value) {
  if(is.na(value)) {
    return(NA)
  }
  return(value);
}

upCount <- function(value) {
  if(is.na(value)) {
    return(NA)
  }
  return(value);
}

upLength <- function(value, scaleFactor) {
  if(is.na(value)) {
    return(NA)
  }
  
  return (value/scaleFactor)
}

upArea <- function(value, scaleFactor) {
  if(is.na(value)) {
    return(NA)
  }
  return (value/scaleFactor)
}

soma <- function (property) {
  return(sum(property))
}

computeDensity <- function(C, property, numberCells) {  
  context <- e1[["context"]]
  gridSize <- context@spatialResolutions[[1]]@grid$gridSize;
  cells <- gridSize  / numberCells
  cells <- cells*cells
  return(sum(C) / cells)
}

accFunctions <- matrix(c("Count", "upCount", "soma", "CountPersons", "upCount", "soma", "CountFatals", "upCount", "soma", "CountDrunks", "upCount", "soma", "Density", "upUnconverted", "computeDensity"), 5, 3, byrow=TRUE)
intFunctions <- matrix(c("Count", "upCount", "soma",
                         "Length", "upLength", "soma",
                         "Perimeter", "upLength", "soma",
                         "Area", "upArea", "soma",
                         "Density", "upUnconverted", "computeDensity"), 5, 3, byrow=TRUE)
tweetFunctions <- matrix(c("Count", "upCount", "soma",
                           "Density", "upUnconverted", "computeDensity"), 2, 3, byrow=TRUE)




