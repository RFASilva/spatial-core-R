#Example Accidents
accData <- input('./dataset/accidents-toR-first50000.csv',"geo", accFunctions)
accData <- goTo(accData, 17179869184, 2^13)
accData <- goTo(accData,  2^13, 2^12)
accData <- up(accData)
plotTotalCount(accData, 2^11, "red", FALSE)

accData <- clearSlices(accData)
accData <- attachSemanticSlice(accData, "Leve")
accData <- attachPropertySliceGreaterThan(accData, "Count",  30)


objectsSliced <- performSlices(accData, 1024)

plotTotalCount(objectsSliced, 2^10, "red", FALSE)

# Exemplo do zoom automatico
zoom <- 5;
resAppropriate <- spRLevel(accData, zoom)
plotTotalCountInt(accData, resAppropriate, "red",zoom, FALSE )

output(accData@spatialResolutions[[4]],"./tmp1.csv")


#plotOnMap(accData@spatialResolutions[[4]],FALSE)
#eT <- proc.time()
#cat("Time: ",eT-sT, "\n")
#plotTotalCount(accData, 1024, "red", FALSE)
#plotTotalCountbyT(tweetContext, 1024, "Android", "magenta", FALSE)

#withinSliceTest <- withinSpatialSlice(bboxTmp, accData, 1024)
#bufferSliceTest <- bufferSpatialSlice( cbind(-81,42), 5, accData, 1024)

plotTotalCount(accData, 1024, "red", TRUE)

plotTotalCount(accData, 2^15, "red", FALSE)
plotTotalCount(accData, 512, "red", FALSE)

plotTotalCountbyT(accData, 1024, "ChoqueCadeia", "red", FALSE)
plotTotalCountbyT(accData, 1024, "Leve", "green", FALSE)
plotTotalCountbyT(accData, 1024, "Normal", "magenta", FALSE)

colorsAcc <- c("Normal", "Leve", "ChoqueCadeia")
plotRGB(accData, 1024,colorsAcc)
plotRGB(accData, 512, colorsAcc)
plotRGB(accData, 256, colorsAcc)
plotRGB(accData, 128, colorsAcc)

#accDataSliced <- attachSemanticSlice(accData, "ChoqueCadeia")
#accDataSliced <- clearSlices(accDataSliced)
#objectsSliced <- performSlices(accDataSliced, 2048)

#plotTotalCount(objectsSliced, 2^11, "red", FALSE)


# Example Tweets
e1 <- new.env(parent = baseenv())
tweetContext <- input('./dataset/event_tweets.csv',"geo", tweetFunctions)
tweetContext <- goTo(tweetContext, 2^73, 2048)
tweetContext <- up(tweetContext)

# Exemplo de utilizacao da funcao ratio. Quando chamada desta forma esta a ser calculada a taxa de reducao
# de elementos comparativamente com o numero de elementos dos dados originais
ratio <- reductionRatio(tweetContext, 1024)

# Quando a funcao e chamada desta forma esta a ser calculada a taxa de reducao de elementos 
# de uma "resolucao" para a "outra"
ratio <- reductionRatio(tweetContext, 2048, 1024)

tweetContext <- clearSlices(tweetContext)
tweetContext <- attachSemanticSlice(tweetContext, "Android")
tweetContext <- attachPropertySliceGreaterThan(tweetContext, "Count",  100)

## Ira fazer o slice para a resolucao indicada "altera". As restantes resolucoes permanecem inalteradas
objectsSliced <- performSlices(tweetContext, 1024) 
plotTotalCount(objectsSliced, 1024, "red", FALSE)

plotTotalCount(tweetContext, 1024, "red", FALSE)

colorsTweet <- c("iPhone", "Android", "BlackBerry")
plotRGB(tweetContext, 2048, colorsTweet)
plotRGB(tweetContext, 512, colorsTweet)
plotRGB(tweetContext, 256, colorsTweet)

# Exemplo do zoom automatico
zoom <- 4;
gridSizeAppropriate <- spRLevel(tweetContext, zoom)
plotTotalCountInt(tweetContext, gridSizeAppropriate, "red",zoom, FALSE )

#output(tweetContext@spatialResolutions[[3]],"./tmp1.csv")
