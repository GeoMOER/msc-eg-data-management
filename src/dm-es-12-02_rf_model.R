#  Quick and dirty prediction of plant species richness using Landsat 8
#
#  Copyright (C) 2014 Hanna Meyer, Thomas Nauss, Roland Brandl
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#  Please send any comments, suggestions, criticism, or (for our sake) bug
#  reports to admin@environmentalinformatics-marburg.de
#
#  Details
#  The script reads Landsat 8 and species richness data and applies machine
#  learning models to predict the species richness based on the Landsat 
#  spectral information.

#### User setttings ############################################################

setwd("D:/active/moc/dm/examples/")
in_path1 <- paste0(getwd(), "/data_procd/")
in_path2 <- paste0(getwd(), "/data_raw/")
in_path3 <- paste0(getwd(), "/data_procd/spatial_data/")

useAllPredictors=TRUE #use all predictors or only those with highest importance?
method <- "rf"

packages <- c("raster", "rgdal", "caret", "corrplot", "latticeExtra","hydroGOF")
lapply(packages, library, character.only = TRUE)


#### Load data #################################################################
rs_files <- list.files(in_path3, glob2rx("*.tif"), full.names = TRUE)
rs_files <- rs_files[c(1, 3, 4, 8:10)]
raster <- stack(rs_files)
raster <- aggregate(raster, fact=3)

data2007 <- readOGR(paste0(in_path3, "IE_2007_pontos_esp.shp"), 
                    "IE_2007_pontos_esp")
data2007@data$CYD_OBL <- as.numeric(data2007@data$CYD_OBL)
data2007@data$richness<-rowSums(data2007@data[,14:89])
data2007 <- spTransform(data2007, CRS(projection(raster)))


#### Compile Train data set ####################################################
dlr <- data.frame(extract(raster, data2007), 
                  richness = data2007@data$richness)
# comit <- which(colnames(dlr)==c("N")|colnames(dlr)=="E")
# dlr <- dlr[,-comit]
# romit <- which(dlr$LC82100502014328LGN00_B10 < 20000) #remove outliers
# dlr <- dlr[-romit,]

#### Predict species richness from Landsat data set ############################
# Reduce bands based on high auto-correlation 
if (!useAllPredictors){
  dlr.cor <- as.matrix(cor(dlr[,-which( colnames(dlr)==c("rich"))]))
  corrplot(dlr.cor)
  dlr.cor.rm <- findCorrelation(dlr.cor, verbose = TRUE)
  dlr.clean <- dlr[,-(dlr.cor.rm+1)]
  # dlr.clean$Rich <- as.factor(dlr.clean$Rich)
  # dlr.clean <- rbind(dlr.clean, dlr.clean)
}

predPerformance <- data.frame(PRED = factor(), VALD = factor())
importance <- data.frame(band = factor(), importance = numeric())
if (useAllPredictors){
  dlr.clean=dlr 
}
validation=data.frame(matrix(ncol=4,nrow=3))
colnames(validation)=c("rsquared","rmse","me","mae")

model.train=list()

for(x in 1:1){
  trainIndex <- createDataPartition(dlr.clean[,ncol(dlr.clean)], 
                                    p = 0.7, list = FALSE)
  df.train <- dlr.clean[trainIndex,]
  df.test <- dlr.clean[-trainIndex,]
  
  model.train[[x]] <- train(df.train[,-ncol(df.train)], df.train[,ncol(df.train)], 
                            method = method, tuneLength = 5)
  plot(model.train[[x]])

  #### Predict on test samples #################################################
  model.test <- predict(model.train[[x]], df.test[,-ncol(df.train)])
  tmp <- data.frame(PRED = model.test,
                    VALD = df.test[,ncol(df.train)])
  predPerformance <- data.frame(rbind(predPerformance, tmp))
  
  rsquared=summary(lm(predPerformance$VALD ~ predPerformance$PRED))$r.squared
  rmse=rmse(predPerformance$PRED,predPerformance$VALD)
  me=rmse(predPerformance$PRED,predPerformance$VALD)
  mae=rmse(predPerformance$PRED,predPerformance$VALD)
  validation[x,]=data.frame(rsquared,rmse,me,mae)
  #### Determine and Plot Varibale Importance ################################## 
  if(method == "rf"){
    varImpPlot(model.train[[x]]$finalModel)
    tmp <- importance(model.train[[x]]$finalModel)
    tmp <- data.frame(band = rownames(tmp), 
                      importance = tmp[,1])
    importance <- data.frame(rbind(importance, tmp))
  }
}
sort <- aggregate(importance$importance, by = list(importance$band), FUN = mean)
sort <- sort[with(sort, order(x)), ][1]
importance$band <- factor(importance$band, levels=as.character(sort[,1]))
bwplot(band~importance, data = importance)
print(validation) #results of each run

#### Predict on Landsat data ################################################### 
preddata=data.frame(coordinates(raster),
                    sapply(names(dlr.clean[,-ncol(df.train)]), function (x)
                      as.vector(eval(parse(text=paste0("raster$",x))))))

preddata=preddata[!is.na(rowSums(preddata)),]
names(preddata)=c("x","y",names(dlr.clean[,-ncol(df.train)]))

prediction_raster <- predict(raster, model.train[[1]])

clrs.spec <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
spplot(prediction_raster,col.regions =clrs.spec(100),
       scales = list(draw = TRUE),zlab="Species") 

save(model.train, raster, file = paste0(in_path3, "dm-es-12_02_rf.RData"))
