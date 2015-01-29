# Shiny app to illustrate interactive visualization using shiny
#
# Copyright (C) 2014 Thomas Nauss
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# Please send any comments, suggestions, criticism, or (for our sake) bug
# reports to admin@environmentalinformatics-marburg.de

#### General setttings #########################################################
# Hallo
if(Sys.info()["user"] == "shiny"){
  in_path1 <- "/srv/shiny-server/moc/msc-data-management/data_procd/"
  in_path2 <- "/srv/shiny-server/moc/msc-data-management/data_raw/"
  in_path3 <- "/srv/shiny-server/moc/msc-data-management/data_procd/spatial_data/"
} else {
  setwd("D:/active/moc/dm/examples/")
  in_path1 <- paste0(getwd(), "/data_procd/")
  in_path2 <- paste0(getwd(), "/data_raw/")
  in_path3 <- paste0(getwd(), "/data_procd/spatial_data/")
}

packages <- c("shiny", "raster", "rgdal", "latticeExtra", "caret", "gridExtra")
lapply(packages, library, character.only = TRUE)


#### Load necessary datasets ###################################################
wb <- read.table(paste0(in_path1, "wb-db_gnipc_co2_1960-2013.csv"), 
                 header = TRUE, sep = ",")
cntry <- read.table(paste0(in_path2, "wb-db_country_classes.csv"), 
                    header = TRUE, sep = ",")
wbc <- merge(wb, cntry[,!(grepl("Economy|X", colnames(cntry)))], 
             by.x = "Country.Code", by.y = "Code")
wbc$Region[wbc$Region == ".."] <- NA
wbc$Region <- droplevels(wbc$Region)
wbc$co2_log <- log(wbc$co2)
wbc$gni_log <- log(wbc$gni)


#### Initialize shiny server ###################################################
shinyServer(function(input, output) {
  
  wbc <- wbc[complete.cases(wbc),]
    output$histo <- renderPlot({
    hist1 <- histogram(wbc$gni_log, type = "density", 
                       breaks = as.numeric(input$n_breaks), col = "white",
                       xlab = "Logarithm of GNI", ylab = "Density", 
                       main = "World bank data GNI values")
    
    dens1 <- densityplot(wbc$gni_log, adjust = input$bw_adjust, na.rm = TRUE, 
                         col = "darkgreen", lwd = 3.0, plot.points = FALSE)
    
    plot1 <- hist1 + as.layer(dens1)
    
    hist2 <- histogram(wbc$co2_log, type = "density", 
                       breaks = as.numeric(input$n_breaks), col = "white",
                       xlab = "Logarithm of CO2", ylab = "Density", 
                       main = "World bank data CO2 values")
    
    dens2 <- densityplot(wbc$co2_log, adjust = input$bw_adjust, na.rm = TRUE, 
                         col = "blue", lwd = 3.0, plot.points = FALSE)
    
    plot2 <- hist2 + as.layer(dens2)
    
    grid.arrange(plot1, plot2, ncol = 2)
  })
})




