####################################### FUNCTIONS TO PLOT THE RESULTS ############################################

plotOnMap <- function(objects, withlabels) {
  
  llCRS <- CRS("+proj=longlat +ellps=WGS84")
  bb <- cbind(objects@grid$minCoords, objects@grid$maxCoords)
  
  print(bb)
  
  temp <- unname(bb[,1] + ((bb[,2] - bb[,1])/2))
  center = c(mean(bb[,2]), mean(bb[,1]))
  
  zoom <- MaxZoom(range(bb[2,]), range(bb[1,]), size=c(640, 640))
  mymap <- GetMap.bbox(lonR=bb[1,], latR= bb[2,], zoom=zoom, destfile = "MyTile3.png", maptype="roadmap");
  
  points <- sapply(objects@spatialObjects, function(x) if(x$type=="Point") x, simplify=FALSE)
  
  if(!is.null(points)) {
    coordsPoints <- t(sapply(points, '[[', "coords"))
    coordsPoints <- cleanList2Matrix(coordsPoints)
      
    tmp <- PlotOnStaticMap(mymap, coordsPoints[,2], coordsPoints[,1], destfile = "MyTile2png", cex=0.0001,pch=20, col=c('blue'), add=FALSE);
  }
  
  GrS <- sapply(objects@spatialObjects, function(x) if(x$type=="GranularSynthesis") x, simplify=FALSE)
  if(!is.null(GrS)) {
    
    idGrS <- t(sapply(GrS, '[[', "id"))
    idGrS <- cleanList2Matrix(idGrS)
    
    coordsGrS <- t(sapply(GrS, '[[', "coords"))
    coordsGrS <- cleanList2Matrix(coordsGrS)
    
    PlotOnStaticMap(mymap, coordsGrS[,2], coordsGrS[,1], destfile = "MyTile2png", cex=0.1,pch=20, col=c('red'),  add=TRUE);
    
    if(withlabels)
      TextOnStaticMap(mymap, coordsGrS[,2], coordsGrS[,1], idGrS, cex=0.1, add=TRUE)
  }
  
} 

plotData <- function(data,pos) {
  plot.new()
  
  llCRS <- CRS("+proj=longlat +ellps=WGS84")
  
  type <- data@typeCoords
  objects <- data@spatialResolutions[[pos]]
  
  if(type=="int")
    res <- unlist(objects@grid$gridSize)
  else res <- 0 

  spObjects <- objects@spatialObjects
  
  # Plot points
  points <- sapply(spObjects, function(x) if(x$type=="Point") x, simplify = FALSE)
  plotPoints(points, type, res)
  
  # Plot lines
  lines <- sapply(spObjects, function(x) if(x$type=="Line") x, simplify = FALSE)
  plotLines(lines, type, res)

  # Plot Polygons
  polygons <- sapply(spObjects, function(x) if(x$type=="Polygon") x, simplify = FALSE)
  plotPolygons(polygons, type, res)
    
  #Plot Granular Synthesis
  sys <- sapply(spObjects, function(x) if(x$type=="GranularSynthesis") x, simplify=FALSE)
  plotSyntheses(sys, type, res)

  if (type== "int") {
    abline(v = seq(-0.5, res + 0.5, by = 1), lty = 1, col = "darkgray")
    abline(h = seq(-0.5, res + 0.5, by = 1), lty = 1, col = "darkgray")
  } 
}

plotPoints <- function(points, type, res) {
  coordsPoints <- t(sapply(points, '[[', "coords"))
  coordsPoints <- cleanList2Matrix(coordsPoints)
  
  if(!is.null(coordsPoints)) {
    if(type == "int") {
      par(new=TRUE)
      plot(coordsPoints[,1],coordsPoints[,2],pch=21, col="black", bg="black",ylab = "", xlab = "", xlim=c(-1, res), ylim=c(-1, res))
    }
    else {
      llCRS <- CRS("+proj=longlat +ellps=WGS84")
      spPoints <- SpatialPoints(coordsPoints, llCRS)
      
    #  bla <- SpatialPointsDataFrame(spPoints, data.frame(matrix(nrow=length(spPoints), ncol=1)), coords.nrs = numeric(0),  proj4string = llCRS, match.ID = FALSE, bbox = NULL)
     # plotGoogleMaps(bla,filename='myMap1.htm')
      plot(spPoints, pch=21, col="black", bg="black")
    }
  }
}

plotLines <- function(lines, type, res) {
  coordsLines <- do.call(rbind,  sapply(lines, '[[', "coords"))
  coordsLines <- cleanList2Matrix(coordsLines)
  
  if(length(coordsLines)!= 0) {
    if(type == "int") {
      par(new=TRUE)
      plot(coordsLines[,1],coordsLines[,2],pch=10,cex = 1.5, col="blue", ylab = "", xlab = "", xlim=c(-1, res), ylim=c(-1, res), yaxt="n", xaxt="n")
      sapply(lines, function(x) if(length(x$coords) > 2) lines(x$coords[,1], x$coords[,2], type="l", lwd=3,ylab = "", xlab = "" ))
    }
    else {
      par(new=TRUE)
      sapply(lines, function(x) 
        if(length(x$coords) > 2) {
          line <- list(Lines(list(Line(x$coords)), ID="b")) 
          plot(SpatialLines(line, proj4string = CRS("+proj=longlat +ellps=WGS84")), add=TRUE)
        })
    }
  }
  
}

plotPolygons <- function(polygons, type, res) {
  coordsPolygons <- do.call(rbind,  sapply(polygons, '[[', "coords"))
  coordsPolygons <- cleanList2Matrix(coordsPolygons)
  
  if(length(coordsPolygons)!= 0) {
     
    if(type == "int") {
      par(new=TRUE)
      plot(coordsPolygons[,1],coordsPolygons[,2],pch=10,cex = 1.5, col="red", yaxt="n", xaxt="n", ylab = "", xlab = "", xlim=c(-1, res), ylim=c(-1, res))
      sapply(polygons, function(x) if(length(x$coords) > 2)  polygon(x$coords[,1], x$coords[,2] , border="green", lwd=1, lty="solid", density = 30, col = "green", ylab = "", xlab = "", yaxt="n", xaxt="n"))
      
    }
    else {
      sapply(polygons, function(x) 
        if(length(x$coords) > 2) {
          pol <- list(Polygons(list(Polygon(x$coords)), 1)) 
          plot(SpatialPolygons(pol, proj4string = CRS("+proj=longlat +ellps=WGS84")), add=TRUE)
        })
    }
  }
}

plotSyntheses <- function(sys, type, res) {
  coordsSys <- t(sapply(sys, '[[', "coords"))
  coordsSys <- cleanList2Matrix(coordsSys)
  
  # CODIGO A SEGUIR NAO LIDA COM A ESTRUTURA ACTUALIZADA, ISTO E, COM ATRIBUTOS SEMANTICOS
  #sys <- removeNull(sys)
  #if(length(sys)!= 0) {
  #  data <- rowSums(do.call(rbind,  sapply(sys, '[[', "synthesis", simplify = FALSE)))
    
    #Set up colors
  #  quant <- quantile(data);
  #  color_palette <- rev(heat.colors(6));
  #  mod_mat <- matrix(findInterval(data, quant, all.inside = TRUE))
    
  #  mod_mat <- rev(mod_mat);
  if(length(coordsSys)!= 0) {
    
    if(type =="int") {
        par(new=TRUE)
        #plot(coordsSys[,1], coordsSys[,2], pch=20, cex = 3, yaxt="n", xaxt="n", ylab = "", xlab = "", xlim=c(-1, res), ylim=c(-1, res), col=color_palette[mod_mat])
        plot(coordsSys[,1], coordsSys[,2], pch=20, cex = 3, yaxt="n", xaxt="n", ylab = "", xlab = "", xlim=c(-1, res), ylim=c(-1, res), col="blue")
      }
      else {
        llCRS <- CRS("+proj=longlat +ellps=WGS84")
        spSys <- SpatialPoints(coordsSys, llCRS)
       # plot(spSys, pch=20, cex = 2, col=color_palette[mod_mat], add=TRUE)
        plot(spSys, pch=20, cex = 2, col="blue", add=TRUE)
      }
  }
  
  
}

########################################## END PLOT FUNCTIONS ###############################################

infobyCoords <- function(c1, c2, objects) {
  summary <- sapply(objects@spatialObjects, function(x) if(x$type=="GranularSynthesis") if((x$coords[1] == c1) && (x$coords[2]== c2))  x);
  summary <- removeNull(summary);
  
  str(summary)
}

infobyID <- function(sysid, objects) {
  summary <- sapply(objects@spatialObjects, function(x) if(x$id==sysid) x)
  summary <- removeNull(summary);
  
  print(summary)
}

########################################### FUNCTIONS TO CREATE EXAMPLES ##########################################

createData <- function(numberCells, props) {
  
  sp <- list();
  attributos <- c("Ponto verde", "Ponto amarelo", "Ponto azul");

  for(i in c(1:30) ) {
    ind <- round(runif(1, 1, 3))
    
    property <- matrix(c(1, NA, NA, NA, 1), 1,5)
    colnames(property) <- c("Count", "Length", "Perimeter", "Area", "Density");
    
    sp <- addSpatialObject(i, "Point", attributos[ind], matrix(sample(1:(numberCells), 2),1,2), property, sp);  
  }
  
  properties1 <- matrix( c(1,7, NA, NA, NA), 1,5)
  colnames(properties1) <- c("Count", "Length", "Perimeter", "Area", "Density");
  sp <- addSpatialObject(31, "Line", "line", matrix(c(1,1,1,8,8,7,8,8),4,2,byrow=TRUE), properties1,  sp);
  
  properties2 <- matrix(c(1,10, NA, NA, NA), 1,5)
  colnames(properties2) <- c("Count", "Length", "Perimeter", "Area", "Density");
  
  sp <- addSpatialObject(32, "Line", "line", matrix(c(10,10,20,20,20,13),3,2,byrow=TRUE), properties2, sp);
  
  properties3 <- matrix(c(1,NA,30,8, NA),1,5)
  colnames(properties3) <- c("Count", "Length", "Perimeter", "Area", "Density");
  
  sp <- addSpatialObject(33, "Polygon", "polygon", matrix(c(1,1,10,10,10,1,1,1),4,2, byrow=TRUE), properties3, sp);
  
  properties4 <- matrix(c(1, NA, 30,7, NA),1,5)
  colnames(properties4) <- c("Count", "Length", "Perimeter", "Area", "Density")
  
  sp <- addSpatialObject(34, "Polygon", "polygon", matrix(c(22,22,15,15,27,15,22,22),4,2,byrow=TRUE), properties4, sp);
  
  #sp <- addGrSynthesis(35, c(4,2), c(1,1,1) , c(100,101,102,103,104,105,106,107,108,109),sp);  
  
  dataObjects <- new("SpatialResolution", meta = matrix(c("OLA",1),2,1), grid=list(gridSize = numberCells), spatialObjects=sp) 
  
  
  result <- new("Context", spatialResolutions=list(), props = props, typeCoords = "int"); 
  result@spatialResolutions[[1]] <- dataObjects;
  
  
  
  return (result);
}

addSpatialObject <- function(ident, tp, attributo, c, props, result) {
  i = length(result);
  result[[i+1]] <- list(id = ident, type = tp, attr = attributo, properties = props, coords = c);
  
  return(result);
}

addGrSynthesis <- function(ident, granule, sys, refs, result) {
  i = length(result);
  result[[i+1]] <- list(id = ident, type = "GranularSynthesis", synthesis = sys, coords = granule, references = refs);
  
  return(result);
}

createDataGeo <- function() {
  result <- list();  
  instalacoes <- read.table("D:/PhD/Papers Produced/IOP in Decision Making/Prototype/instalacao.txt", header = TRUE);
  instalacoes <- cbind(instalacoes[,1],instalacoes[,2])

  for(i in c(1:nrow(instalacoes))) {

    result <- addSpatialObject(i, "Point", matrix(instalacoes[i,],1,2) , result);  
  }
  
  result <- addSpatialObject(400, "Polygon", portugal[[1]]@coords, result);
  
  result <- new("SpatialResolution", grid=computeGrid(instalacoes, NA), spatialObjects=result) 
  
  return (result);
}

teste <- function() {
  data <- input('./accidents-toR-first5000.csv')
  result <- new("Context", spatialResolutions=list(), props = accFunctions, typeCoords = "geo"); 
  result@spatialResolutions[[1]] <- data;
  return(result);
}

# Compute grid automatically from data
# For now the boundaries of the grid is based on data, but on the
# future probably will be defined by an user
computeGrid <- function(data, precisionPerCell) {
  bb <- bbox(data)
  minCoords <- bb[,1]
  inc <- max(abs(bb[,1] - bb[,2]))
  maxCoords = bb[,2]
  
  tmpNumOfCells <- inc / precisionPerCell
  
  # next power of 2, not the most efficient, but it works ...
  countPow <- 1
  powerOfTwo <- 2^countPow #2^0
  while (tmpNumOfCells > powerOfTwo) { 
    countPow <- countPow+1
    powerOfTwo <- 2^countPow
  }
  
  result <- list(minCoords = unname(minCoords), maxCoords = unname(maxCoords), precision = precisionPerCell, gridSize = powerOfTwo)
  
  return(result);
}


