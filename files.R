# Processes a CSV input file with our pre-defined format
#
# Parameters: 
#             file - path to file; 
#             coordsType - "geo" or "int"
#             mPropsFunctions - functions for the properties of this data
#
# Returns: a spatialResolution with gridSize on our internal format (it's our spR0)
#
# Format is CSV with the following structure
# ----------
# MetaField1_Name;MetaField1_Value
# MetaField2_Name;MetaField2_Value
# MetaFieldN_Name;MetaFieldN_Value
# ...
# MetaFieldN_Name;MetaFieldN_Value
# ... white line ...
# #DATA#
# Precision;1 
# ID; SpatialType; SpatialExp (WKT); Atrb; Count; Prop2; Prop3; Prop4; etc.
# 1;Point;POINT(6 10);Point;1
# 2;Line;LINESTRING(3 4,10 50,20 25);Line;1;1
# 3;Polygon;POLYGON((1 1,5 1,5 5,1 5,1 1));Polygon;1;1;1;1
# <MUST END WITH EOF>
#
input <- function (file, coordsType, mPropsFunctions) {
 
 startTime <- proc.time()
 cat("Starts ... \n")
  
 result <- list();
 
 # find where #DATA# is and read headers until it
 fileCon <- file(file, "r")
 beginData <- FALSE
 linesToData <- 0
 tmpfileHeader <- list()
 while (!beginData) {
   oneLine <- readLines(fileCon,n=1,warn=TRUE,encoding="UTF-8")
   if ( (oneLine != "#DATA#") && (oneLine != "") ) {
     tmpfileHeader[[length(tmpfileHeader)+1]] <- (strsplit(oneLine,":"))[[1]]
   }
   linesToData <- linesToData+1
   if (oneLine == "#DATA#") { beginData <- TRUE } 
 }
 
 # file meta-info header ...
 fileHeader <- do.call(rbind,tmpfileHeader)
 
 # reads precision
 oneLine <- readLines(fileCon,n=1,warn=TRUE,encoding="UTF-8")
 precision <- as.numeric(strsplit(oneLine,";")[[1]][2])
 
 # closes the file for now ...
 close(fileCon)
 
 # reads CSV
 tmpFile <- read.csv(file, header = TRUE, sep = ";", quote="\"", dec=".", skip=(linesToData+1), fill=TRUE, encoding = "UTF-8") 
 
 finishesLoad <- proc.time()
 cat("Finishes load: ",finishesLoad-startTime, "\n")
 
 # gets names of new props
 tmpPropsNames <- tail(names(tmpFile),-4)
 # stores in meta
 fileHeader <- rbind(fileHeader,cbind("Properties",paste(tmpPropsNames,collapse=";")))

 # matrix para as granular synthesis ... auxiliar - ID, Atrb, Props ... 
 tmpProps <- subset(tmpFile,SpatialType=="GranularSynthesis", select=c(ID, SpatialType, Atrb, 5:ncol(tmpFile)))
 
 firstProcessing <- proc.time()
 cat("1sts processing: ",firstProcessing-finishesLoad, "\n")

 # conversion to internal format (list of objects)
 tmpResult <- apply(tmpFile, 1, internalMapCSVRowsToFormat, tmpProps)
 
 processing <- proc.time()
 cat("Finishes processing: ",processing-firstProcessing, "\n")
 
 #remove repetitions of granular synthesis
 tmpResult2 <- unique(tmpResult)
 
 # computes coordinates matrix for gridCompute from our structure ...
 tmpCoordsPoints <- matrix(unlist(sapply(tmpResult2, function(x) { return(x$coords) })),ncol=2, byrow=TRUE)

 # tmpCoordsPoints <- NULL 
#  for(i in 1:NROW(tmpResult2)) {
#    x <- (tmpResult2[i])[[1]]
#    coords <- x$coords
#    if(x$type!="Point") {
#      for (j in 1:NROW(coords)) {
#        tmpCoordsPoints <- rbind(tmpCoordsPoints,coords[j,c(1,2)])
#      }
#    } else {
#      tmpCoordsPoints <- rbind(tmpCoordsPoints,coords[,c(1,2)])
#    }
#  }
 tmpCoordsPoints <- unique(tmpCoordsPoints)
 
 # checks if precision is good or we need an UP to ..
 numOfDecimalCases <- sort(sapply(tmpCoordsPoints, function(x) { nchar(strsplit(as.character(x), "\\.")[[1]][2]) } ), decreasing = TRUE)[1]
 maxPrecisionPerCell <- 10^(-numOfDecimalCases)
 
 #print(maxPrecisionPerCell)
 #print(resolution)
 
 if (maxPrecisionPerCell < precision) {
   # temos resolução maior, logo temos de fazer um up.
   print("res é maior")
   tmpResult <- new("SpatialResolution", meta = fileHeader, grid=computeGrid(tmpCoordsPoints, maxPrecisionPerCell), spatialObjects=tmpResult2)
   print(tmpResult@grid$precision)
   print(tmpResult@grid$gridSize)
   tmpNewGrid <- computeGrid(tmpCoordsPoints, precision)
   newGridSize <- tmpNewGrid$gridSize
   
   result <- new("Context", spatialResolutions=list(tmpResult), props = mPropsFunctions, typeCoords = coordsType, previousSlices=list()); 
   print(tmpResult@grid$gridSize);
   
   result <- goTo(result, tmpResult@grid$gridSize, newGridSize)
 } else {
   # resolution pedida é menor, portanto usa essa
   tmpResult <- new("SpatialResolution", meta = fileHeader, grid=computeGrid(tmpCoordsPoints, precision), spatialObjects=tmpResult2)
   result <- new("Context", spatialResolutions=list(tmpResult), props = mPropsFunctions, typeCoords = coordsType, previousSlices=list()); 
 }
 
 finishes <- proc.time()
 cat("Finishes aux + create: ",finishes-processing, "\n")
 
 total <- proc.time()
 cat("Total: ",total-startTime, "\n")
 
 return(result)
}

# Processes a spatialResolution structure and outputs a CSV on our pre-determined format
#
# Parameters: 
#             spR - resolution structure to output
#             file - if NULL, outputs to screen only
#
# Returns: a spatialResolution with gridSize on our internal format (it's our spR0)
#
output <- function(spR, file) {  
  
  startTime <- proc.time()
  cat("Starts ... \n")
    
  meta <- apply(spR@meta[-nrow(spR@meta),],1,function(x) { return(paste(x,collapse=":")) })
  dataSep <- "\n#DATA#"
  tmpHeaderGrid <- paste("Precision",spR@grid$precision, sep=";")
  header <- list(paste("ID;SpatialType;SpatialExp.WKT;Atrb",spR@meta[nrow(spR@meta),2], sep=";"))
  
  finishesHeader <- proc.time()
  cat("Finishes header: ",finishesHeader-startTime, "\n")
  
  csvTmp <- lapply(spR@spatialObjects, internalMapFormatToCSVRows)

  finishesCSVGen <- proc.time()
  cat("Finishes CSV generation: ",finishesCSVGen-finishesHeader, "\n")
  
  csvResult <- append(header,csvTmp)
  
  if (!is.null(file)) {
    file.create(file)
    lapply(meta,write,file, append=TRUE)
    lapply(dataSep,write,file, append=TRUE)
    lapply(tmpHeaderGrid,write,file, append=TRUE)
    lapply(csvResult, write, file, append=TRUE)
  } else {
    lapply(meta,cat,"\n", append=TRUE)
    lapply(dataSep,cat,"\n", append=TRUE)
    lapply(tmpHeaderGrid,cat,"\n", append=TRUE)
    lapply(csvResult, cat, "\n", append=TRUE)
  }
  
  finishesCSVOut <- proc.time()
  cat("Finishes CSV output: ",finishesCSVOut-finishesCSVGen, "\n")  
  
  total <- proc.time()
  cat("Total: ",total-startTime, "\n")
    
  return(TRUE)
}

internalMapCSVRowsToFormat <- function(x, tmpProps) {  
  
  #coords <- readWKT(x[3])
  splitCoordsStr <- sapply(strsplit(gsub("[A-Za-z\\(\\)]","", x[3]),","), function(x) sapply(strsplit(x," "),as.numeric))
  tmpCoords <- matrix(splitCoordsStr,ncol=2, byrow=TRUE)
  
  if(x[2]!="GranularSynthesis") {
    props <- rbind(sapply(x[5:length(x)], as.numeric))
  } else {
    myProps <- tmpProps[tmpProps[,1]==x[1],]
    props <- myProps[3:ncol(myProps)]
  }
  
  if(x[2]=="Point") {    
    # ID, point, atrb, coords, props, result (null)
    #tmpCoords <- unname(coordinates(coords), force=TRUE)
    result <- addSpatialObject(as.numeric(x[1]), "Point", unname(x[4]), tmpCoords, props, NULL);
  }
  if(x[2]=="Line") {
    #firstLineCoords <- attr(coords,"lines")[[1]]@Lines[[1]]@coords
    #tmpCoords <- unname(firstLineCoords, force=TRUE)
    
    # ID, point, atrb, props, coords, result (null)
    result <- addSpatialObject(as.numeric(x[1]), "Line", unname(x[4]), tmpCoords, props, NULL);
  }
  if(x[2]=="Polygon") {
    #firstPolyCoords <- attr(coords,"polygons")[[1]]@Polygons[[1]]@coords
    
    #note - readWKT reads in reverse - last to first point - must reverse ?!?!
    #firstPolyCoords <- firstPolyCoords[nrow(firstPolyCoords):1,]
  
    #tmpCoords <- unname(firstPolyCoords, force=TRUE)
    
    # ID, point, atrb, props, coords, result (null)
    result <- addSpatialObject(as.numeric(x[1]), "Polygon", unname(x[4]), tmpCoords, props, NULL);
  }
  if(x[2]=="GranularSynthesis") {
      # var juntar vários, replicados ... será necessário o unique fora disto ...
      #tmpCoords <- unname(coordinates(coords), force=TRUE)
      result <- addGrSynthesis(as.numeric(x[1]), tmpCoords, props, c(), NULL);
  }
  
  return(result[[1]])
}

internalMapFormatToCSVRows <- function(x) {

  id <- x$id
  ourType <- x$type
  
  if (ourType == "Point") {
    
    # very slow using spatial points
    # points <- matrix(x$coords,1)
    # sp <- SpatialPoints(points)
    
    wktType <- "POINT"
    sp <- paste(c(x$coords), collapse=" ")
    
    atrb <- x$attr
  }
  if (ourType == "Line") {
    # very slow using spatial points
    #tmpSPLine <- Line(x$coords)
    #tmpSPLine2 <- Lines(list(tmpSPLine), ID = "a")
    #sp <- SpatialLines(list(tmpSPLine2))
    
    wktType <- "LINESTRING"
    sp <- paste(apply(x$coords, 1, function(x) {  paste(x, collapse=" ") }),collapse=",")
    
    atrb <- x$attr
  }
  if (ourType == "Polygon") {
    # very slow using spatial points
    #tmpSPPol <- Polygon(x$coords)
    #tmpSPPol2 <- Polygons(list(tmpSPPol), ID = "a")
    #sp <- SpatialPolygons(list(tmpSPPol2))
    
    wktType <- "POLYGON"
    sp <- paste(apply(x$coords, 1, function(x) {  paste(x, collapse=" ") }),collapse=",")
    sp <- paste("(", sp, ")", sep="") # adds () because of first polygon
    
    atrb <- x$attr
  }
  
  if (ourType == "GranularSynthesis") {
    
    # very slow using spatial points
    # points <- matrix(x$coords,1)
    # sp <- SpatialPoints(points)
    # coords <- writeWKT(sp,byid=FALSE)
    sp <- paste(c(x$coords), collapse=" ")
    coords <- paste("POINT(", sp, ")", sep="")
    
    # synthList <- list()    
    # TODO: necessário rever com base nas alterações do modelo
    #     for (i in 1:nrow(x$synthesis)) {
    #       #print("aqui")
    #       #print(rownames(x$synthesis)[i])
    #       synthMerge <- paste(x$synthesis[i,1:ncol(x$synthesis)], collapse=";")
    #       #print(synthMerge)
    #       tmpPropSynth <- paste(id,ourType,coords,rownames(x$synthesis)[i],synthMerge,sep=";")
    #       synthList <- c(synthList, tmpPropSynth)
    #     }
    # output <- paste(synthList, collapse="\n")
    
    # quicker version
    synthMerge <- paste(id,ourType,coords,rownames(x$synthesis), apply(x$synthesis,1,function(x) { paste(x, collapse=";")} ), sep=";")
    #print(synthMerge)
    output <- paste(synthMerge, collapse="\n")
  
  } else {
    # todos os outros
    
    # very slow using spatial points
    #coords <- writeWKT(sp,byid=FALSE)    
    coords <- paste(wktType,"(", sp, ")", sep="")
    
    props <- paste(x$properties,collapse=";")
    output <- paste(id,ourType,coords,atrb,props,sep=";")
  }
  
  return(output)
}
