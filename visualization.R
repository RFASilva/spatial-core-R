
#Possible domain for nameColor: red, yellow, green, cyan, blue, magenta
plotTotalCount <- function(data, res, nameColor, withlabels) {
  plot.new()
  pos <- findPosResInContext(data, res)
  objects <- data@spatialResolutions[[pos]]
  mymap <- createBaseMap(objects, pos, "roadmap")

  llCRS <- CRS("+proj=longlat +ellps=WGS84")
  plotDataPoints(objects@spatialObjects, mymap, nameColor, withlabels)
}

#Possible domain for nameColor: red, yellow, green, cyan, blue, magenta
plotTotalCountInt <- function(data, res, nameColor, zoom, withlabels) {
  plot.new()
  pos <- findPosResInContext(data, res)
  objects <- data@spatialResolutions[[pos]]
  mymap <- createBaseMap(objects, pos, "roadmap")
  
  llCRS <- CRS("+proj=longlat +ellps=WGS84")
  plotDataPointsInt(objects@spatialObjects, mymap, nameColor, zoom, withlabels)
}


plotProperty <- function(data, res, nameColor, propertyName, withlabels) {
  plot.new()
  pos <- findPosResInContext(data, res)
  objects <- data@spatialResolutions[[pos]]
  mymap <- createBaseMap(objects, pos, "roadmap")
  
  llCRS <- CRS("+proj=longlat +ellps=WGS84")
  plotByProperty(objects@spatialObjects, mymap, nameColor, propertyName, withlabels)
}


plotTotalCountbyT <- function(data, res, t, colort, withlabels) {
  plot.new()
  pos <- findPosResInContext(data, res)
  objects <- data@spatialResolutions[[pos]]
  mymap <- createBaseMap(objects, pos, "roadmap")
  
  objects <- semanticSlice(t, data, res)
  plotDataPoints(objects, mymap, colort, withlabels)
}

# Assuming that T distinct values is 3
plotRGB <- function(data, res, colorsbyt) {
  plot.new()
  
  pos <- findPosResInContext(data, res)
  objects <- data@spatialResolutions[[pos]]
  
  mymap <- createBaseMap(objects, pos, "roadmap")
  
  assign("maxCount", 0, envir=e1)
  print("HERE")
  data <- sapply(objects@spatialObjects, function(x) 
    if(x$type == "Point") { 
      c(x$coords, 1, x$attr)
      if(1 > e1[["maxCount"]])
        e1[["maxCount"]] <- 1
    }
    else if(x$type == "GranularSynthesis") {
     n <- length(x$synthesis[,1])
     
     if(sum(x$synthesis[,1]) > e1[["maxCount"]])
       e1[["maxCount"]] <- sum(x$synthesis[,1])
     
     unname(cbind(matrix(rep(x$coords, n), n, 2, byrow=TRUE), x$synthesis[,1], rownames(x$synthesis)))
    }
    
  )
  
  
  data <- do.call(rbind, data)
  numberClasses = 6
  values <- as.numeric(data[,3])  
  
  #distinctattr = unique(data[,4])
  #print(distinctattr)
  #fisher
  if(length(unique(values)) > 1) {
   dataClasses <- classIntervals(values, n=numberClasses, style="fisher")
     
   intensityValue <- seq(50, 255, length = numberClasses)
   print(dataClasses$brks)
   
   colors <- sapply(objects@spatialObjects, function(x) computecolors(colorsbyt,dataClasses, x, intensityValue, e1[["maxCount"]]) )
  }
    
  PlotOnStaticMap(mymap, as.numeric(data[,2]), as.numeric(data[,1]), destfile = "MyTile3.png", cex=1,pch=20, col=colors,  add=FALSE);
  
  #if(withlabels) {
  #  TextOnStaticMap(mymap, data[,2], data[,1], as.numeric(idsdata), cex=0.5, add=TRUE)
  #}
}



############################################################## Auxiliary Functions ##########################################################

computecolors <- function(colorsbyt, dataClasses, x, intensityValue, maxCount) {
  startalpha <- 10
  if(x$type == "Point") {
    colors <- c(0,0,0)
    ind <- which(colorsbyt %in% x$attr)
    colors[ind] <- 255
    return(rgb(colors[1], colors[2], colors[3] , alpha = startalpha, maxColorValue = 255))
  }
  
  if (x$type == "GranularSynthesis") {
    colors <- c(0,0,0)
    ind <- which(colorsbyt %in% rownames(x$synthesis))
    for(i in (1:length(ind))) {
      t <- colorsbyt[ind[i]]
      value <- x$synthesis[t, 1]
      
      temp <- dataClasses$brks[1:(length(dataClasses$brk)-1)]
      n <- length(which(value >= temp))
      colors[i] <- intensityValue[n]
    }
    alphavalue <- (sum(x$synthesis[,1]) * (255- startalpha)) / maxCount # Valor da transparencia proporcional ao numero de objectos na sintese
    return(rgb(colors[1], colors[2], colors[3] , alpha = alphavalue + startalpha, maxColorValue = 255))
  }
  
  
}

createBaseMap <- function(objects, pos, mapType) {
  
  bb <- cbind(objects@grid$minCoords, objects@grid$maxCoords)
  
  temp <- unname(bb[,1] + ((bb[,2] - bb[,1])/2))
  center = c(mean(bb[,2]), mean(bb[,1]))
  
  zoom <- MaxZoom(range(bb[2,]), range(bb[1,]), size=c(640, 640))
  mymap <- GetMap.bbox(lonR=bb[1,], latR= bb[2,], zoom=zoom, destfile = "MyTile3.png", maptype=mapType, sensor="false");
  
  return(mymap)
}

plotDataPoints <- function (objects, mymap, nameColor, withlabels) {
  data <- t(sapply(objects, function(x) 
            if(x$type == "Point") { 
              c(x$coords, unname(x$properties[,"Count"]))
            }
            else if(x$type == "GranularSynthesis") {
              c(x$coords, unname(sum(x$synthesis[,"Count"])))
              
            }
        ))
  
  idsdata <- t(sapply(objects, function(x) 
              if(x$type == "Point") { 
                 c("")
              }
              else if(x$type == "GranularSynthesis") {
                 c(x$id)
              }
  ))
  
  print(data)
  if(length(data) > 0) {
    
    numberClasses <- 6
    if(numberClasses > length(unique(data[, 3]))) {
      numberClasses <- length(unique(data[, 3]))
    }
  
  
    if(length(unique(data[, 3])) > 1) {
      dataClasses <- classIntervals(data[, 3], n=numberClasses, style="fisher")
      
      ramp <- colorRamp(c(nameColor, "black"))
      
      rampcolors<- rgb( ramp(seq(0, 1, length = numberClasses)), alpha = 100, max = 255)
      
      colors <- findColours(dataClasses, rampcolors)
      colors <- paste(colors, "64", sep="")
     
    }
    else {
      colors = hsv(col2hsv(nameColor)[1], 1, 1)
    }
  }
  
  PlotOnStaticMap(mymap, data[,2], data[,1], destfile = "MyTile3.png", cex=0.7,pch=20, col=colors,  add=FALSE);
  
  if(withlabels) {
       TextOnStaticMap(mymap, data[,2], data[,1], as.numeric(idsdata), cex=0.4, add=TRUE)
  }
}

plotByProperty <- function (objects, mymap, nameColor, propertyName, withlabels) {
  data <- t(sapply(objects, function(x) 
    if(x$type == "Point") { 
      c(x$coords, unname(x$properties[,propertyName]))
    }
     else if(x$type == "GranularSynthesis") {
       c(x$coords, sum(x$synthesis[,propertyName]))
       
     }
  ))
  
  idsdata <- t(sapply(objects, function(x) 
    if(x$type == "Point") { 
      c("")
    }
      else if(x$type == "GranularSynthesis") {
        c(x$id)
      }
  ))
  
  numberClasses <- 6
  if(numberClasses > length(unique(data[, 3]))) {
    numberClasses <- length(unique(data[, 3]))
  }
  
  if(length(unique(data[, 3])) > 1) {
    dataClasses <- classIntervals(data[, 3], n=numberClasses, style="fisher")
    cat("Quebras das classes: ", dataClasses$brks, "\n")
    ramp <- colorRamp(c(nameColor, "black"))
    
    rampcolors<- rgb( ramp(seq(0, 1, length = numberClasses)), alpha = 100, max = 255)
    
    colors <- findColours(dataClasses, rampcolors)
    colors <- paste(colors, "64", sep="")
  }
  #  else {
  #    colors = hsv(col2hsv(nameColor)[1], 1, 1)
  #  }
  
  PlotOnStaticMap(mymap, data[,2], data[,1], destfile = "MyTile3.png", cex=0.7,pch=20, col=colors,  add=FALSE);
  
  if(withlabels) {
    TextOnStaticMap(mymap, data[,2], data[,1], as.numeric(idsdata), cex=0.5, add=TRUE)
  } 
}


plotDataPointsInt <- function (objects, mymap, nameColor, zoom,  withlabels) {
  
  llCRS <- CRS("+proj=longlat +ellps=WGS84")
  
  data <- t(sapply(objects, function(x) 
    if(x$type == "Point") { 
      c(x$coords, unname(x$properties[,"Count"]))
    }
                   else if(x$type == "GranularSynthesis") {
                     c(x$coords, unname(sum(x$synthesis[,"Count"])))
                     
                   }
  ))
  
  idsdata <- t(sapply(objects, function(x) 
    if(x$type == "Point") { 
      c("")
    }
                      else if(x$type == "GranularSynthesis") {
                        c(x$id)
                      }
  ))
  
  numberClasses <- 6
  if(numberClasses > length(unique(data[, 3]))) {
    numberClasses <- length(unique(data[, 3]))
  }
  
  if(length(unique(data[, 3])) > 1) {
    dataClasses <- classIntervals(data[, 3], n=numberClasses, style="fisher")
    
    ramp <- colorRamp(c(nameColor, "black"))
    
    rampcolors<- rgb( ramp(seq(0, 1, length = numberClasses)), alpha = 100, max = 255)
    
    colors <- findColours(dataClasses, rampcolors)
    colors <- paste(colors, "64", sep="")
    
  }
  else {
    colors = hsv(col2hsv(nameColor)[1], 1, 1)
  }
  
  matrixPoints <- cbind(data[,1], data[,2])
  rownames(matrixPoints) <- 1:nrow(matrixPoints)
  spPoints <- SpatialPoints(matrixPoints, llCRS)
  
  bla <- SpatialPointsDataFrame(spPoints, data.frame(matrix(nrow=length(spPoints), ncol=1)),  proj4string = llCRS, match.ID = FALSE, bbox = NULL)
  plotGoogleMaps(bla,filename='myMap1.htm', iconMarker='./dataset/marker.png', zoom = zoom)
  
  #PlotOnStaticMap(mymap, data[,2], data[,1], destfile = "MyTile3.png", cex=0.7,pch=20, col=colors,  add=FALSE);
  
  if(withlabels) {
    TextOnStaticMap(mymap, data[,2], data[,1], as.numeric(idsdata), cex=0.0000111, add=TRUE)
  }
}


# Given a context and zoom google maps returns the appropriate spatial resolution level
spRLevel <- function(context, googleMapsZoom) {
  
  #We start to know the bounding box of the original data
  grid_init <- context@spatialResolutions[[1]]@grid
  
  # Longitude delta
  minX <- grid_init$minCoords[1]
  maxX <- grid_init$maxCoords[1]
  
  deltaX <- maxX - minX
  worldDivX <- 360/deltaX # 
  #Latitude Delta
  minY <- grid_init$minCoords[2]
  maxY <- grid_init$maxCoords[2]
  deltaY <- maxY - minY
  worldDivY <- 180/deltaY
  
  worldDifMax <- 0
  deltaMax <- 0
  
  if(worldDivX > worldDivY) {
    worldDifMax <- worldDivX
    deltaMax <- deltaX
  }
  else {
    worldDifMax <- worldDivY
    deltaMax <- deltaY
  }
  
  googleGridSize <- (256 * (2^googleMapsZoom)) # numero de pixeis no google dado um nivel de zoom
  tempNumOfCells <- floor(googleGridSize / worldDifMax)
  
  countPow <- floor(log2(tempNumOfCells))
  powerOfTwo <- 2^countPow #2^0
  while (tempNumOfCells > powerOfTwo) { 
    countPow <- countPow+1
    powerOfTwo <- 2^countPow
  }
  
  print("bla")
  print(powerOfTwo)
  resolutionAppropriate <- powerOfTwo
  

  return(resolutionAppropriate)
}

