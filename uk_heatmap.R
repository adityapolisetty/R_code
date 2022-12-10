myPaths <- .libPaths()

# myPaths <- c(myPaths, "D:/Dropbox/Dropbox/My_files/r_libs")
myPaths <- c(myPaths, "C:\\Users\\adpol\\Dropbox\\My_files\\r_libs") # Aditya's dir 2
.libPaths(myPaths)  # add new path

setwd("C:\\Users\\adpol\\Dropbox\\ICL\\Tarun_Vimal\\address_mapping\\code")


library(rgdal)
library(ggplot2)
library(broom)
library(gpclib)
library(maptools)
library(readxl)
library(stringr)
library(cowplot)
library(readr)
library(mapproj)

gpclibPermit()


# Heatmap Function - Takes data frame with columns heading [id, value], and region types - 
# RGN, LAD, WD, CTRY as inputs. 
uk_heatmap <- function(data,type,DPI){
  
  if (type=="PCD"){
    data <- read_csv("../data/pcodemap.csv", col_types = NULL)
    data$value<- round(runif(nrow(data), 0, 100), 0)
    
    pcodes<- readOGR("../data/shapefiles/PCD/PCD.shp")
    pcodes <- spTransform(pcodes, CRS("+init=epsg:4326"))
    pcodes <- data.frame(pcodes)
    names(pcodes)[names(pcodes) == "pcd"] <- "PCD"
    names(data)[names(data) == "pcode"] <- "PCD"
    shape_fortified<-merge(pcodes, data, all.x = TRUE, by= "PCD")
    shape_fortified<-shape_fortified[order(shape_fortified$objectid),]
    shape_fortified$value[is.na(shape_fortified$value)] <- 0
    shape_fortified$Data <-shape_fortified$value

    gg<- ggplot()  +
    geom_point(data = shape_fortified, aes( x = long, y = lat,colour = Data ),size = 0.1) +
    scale_colour_distiller(palette = "Reds",direction = 1)
    
    # Changes to plot theme 
    gg<- gg + theme_light()+
      theme(legend.position = 'bottom',legend.key.size = unit(1, "cm"),axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            panel.grid.minor = element_blank())
    
    gg<- gg+ coord_map()
    
    # Save plot to wdir
    ggsave(filename = str_c("../plots/",type,".png"), device='png',height=15, width=10, dpi=DPI)
}
  if (type!="PCD"){
    
    # Creating random data
    data <- read_excel("../data/sample/sample_lad.xlsx", col_types = NULL)
    data$value<-data$value*100
    
    shapefile<- readOGR(str_c("../data/shapefiles/",type,"/",type,".shp"))
    shape_fortified <- tidy(shapefile,region=str_c(type, "21CD"))
    shape_fortified<-merge(shape_fortified, data, all.x = TRUE, by= "id")
    shape_fortified<-shape_fortified[order(shape_fortified$order),]
    shape_fortified$value[is.na(shape_fortified$value)] <- 0
    shape_fortified$Data <-shape_fortified$value


    gg<- ggplot() +
    geom_polygon(data=shape_fortified, aes( x = long, y = lat ,group = group,fill=Data),colour = "black", size = 0.1)+
    scale_fill_distiller(palette = "Reds",direction = 1)

    # Changes to plot theme 
    gg<- gg + theme_light()+
      theme(legend.position = 'bottom',legend.key.size = unit(1, "cm"),axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            panel.grid.minor = element_blank())
    
    gg<- gg+ coord_fixed(1)
    
    # Save plot to wdir
    ggsave(filename = str_c("../plots/",type,".png"), device='png',height=15, width=15, dpi=DPI)
    
  }

}

plot<-uk_heatmap(data,"LAD",1000)






