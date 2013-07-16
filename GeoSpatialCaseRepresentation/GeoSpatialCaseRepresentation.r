###
### Project:  Geo Spatial Case Representation 
###
### Url:      http://www.kalisch.biz
###
### File:     GeoSpatialCaseRepresentation.r
###
### Author:   Dominik P.H. Kalisch (dominik@kalisch.biz)
###           Based on an idea from Barry Rowlingson (barry@rowlingson.com)
###
### Desc.:    This script draws a map representing the amount of cases in a
###           district shown by a rectangle on a map of the federal state
###           Brandenburg, Germany.
###
###
### Modification history
### ----------------------------------------------------------------------------
###
### Date        Version   Who                 Description
### 2011-12-02  0.1       Dominik Kalisch     initial build
###
### Needed R packages: maptools, rgdal
###

## Load needed libraries
library(maptools) # Load library to handle shp files
library(rgdal) # Load library to chage projection of the shape file

## Set some variables
mapLib = "shp" # PATH/TO/YOUR/SHP/FILE
mapfile = "dlm_kreis.shp" # YourFile.shp
sqsize = 3000 # The size depends on the projection!
mbord = "#606060" # Color of the border rectangles
mcol = "#0083FE" # Color of the rectangle filling
bcol = "grey" # Color of the borders of the counties
cases <- c(3,0,0,5,0,3,0,0,0,10,0,7,0,0,0,3,0,5) # Assign the cases to the disdrict

## Build a function to plot squares in the right place
stackbox <- function(x, y, n, size, maxheight=5, ...){
  stackheight = seq(0, n, by = maxheight)
  stackheight=diff(unique(c(stackheight, n)))

  for(col in 1:length(stackheight)){
    xl = rep(x + (col-1) * size, stackheight[col]) - (length(stackheight) / 2) * size
    yb = y + size * ((1:stackheight[col])-1) - (max(stackheight) / 2) * size
    xr = xl + size
    yt = yb + size
    rect(xl, yb, xr, yt, ...)
  }
}

## Read the polygon structure from a shape file
map = readShapeSpatial(file.path(mapLib, mapfile))

# Just Temp
map$cases <- cases # Assign the cases to the disdrict

# Get the coordinates for the map
coords = coordinates(map)


plot(map, border=bcol)
for(i in 1:nrow(map)){
  if(map$cases[i]>0){
    stackbox(coords[i,1],coords[i,2],map$cases[i],sqsize,border=mbord,col=mcol)
  }
}

# Unload used packeges
detach(package:maptools)
detach(package:rgdal)