# Plots card containing the ratio of students receiving BAföG aid by state for a given year 
# and also calculates its development over the years.

###############################################################################################################################################
######### Imports #########

setwd("C:/Users/clari/Desktop/data_prepared/Datenjournalismus_BAfoeG/data")

library(tidyverse) 
library(RColorBrewer) 
library(geosphere)
library(rgdal)
library(OpenStreetMap)
library(ggpolypath)
source(helpers.R)


###############################################################################################################################################
######### Data import + cleaning #########

bafog <- read_csv2("bafog.csv", skip = 7, n_max = 448, col_names = FALSE, locale = locale(encoding = "latin1"), na = c("."))  %>% select(Bundesland = X1, Jahr = X2, gefordertePersonen = X9, Vollforderung = X10, Monatsbestand = X12, Kosten = X13)
studis <- read_csv2("studis.csv", skip = 7, n_max = 343, col_names = FALSE, locale = locale(encoding = "latin1")) %>% select(Bundesland = X1, Jahr = X2, Studis = X5)

###############################################################################################################################################
######### Load map data #########

# Shape of Germany
area2 <- loadShapeGermany()

# Background map
osmmap <- openmap(upperLeft = c(55.154, 5.691), lowerRight = c(47.160, 15.095), zoom = 7, 
                  type = "osm",
                  minNumTiles = 9L, mergeTiles = TRUE)


###############################################################################################################################################
######### Plots #########

# Plot Germany maps
scale_full <- c("#ffffff", "#E1F5FE", "#81D4FA", "#29B6F6", "#0288D1", "#0266b5", "#014880", "#00335c")
scale_greater15 <- scale_full[-1]
scale_greater20 <- scale_greater15[-1]
createRatioPlot(1998, scale_greater15)
createRatioPlot(1999, scale_greater15)
createRatioPlot(2000, scale_greater15)
createRatioPlot(2001, scale_greater15)
createRatioPlot(2002, scale_greater15)
createRatioPlot(2003, scale_greater15)
createRatioPlot(2004, scale_greater20)
createRatioPlot(2005, scale_greater20)
createRatioPlot(2006, scale_greater20)
createRatioPlot(2007, scale_greater20)
createRatioPlot(2008, scale_greater20)
createRatioPlot(2009, scale_greater20)
createRatioPlot(2010, scale_greater20)
createRatioPlot(2011, scale_greater20)
createRatioPlot(2012, scale_greater20)
createRatioPlot(2013, scale_greater20) 
createRatioPlot(2014, scale_greater15)
createRatioPlot(2015, scale_greater15)
createRatioPlot(2016, scale_greater15)
createRatioPlot(2017, scale_greater15)
createRatioPlot(2018, scale_greater15)

# Plot Germany wide development over the years
gefordert <- calcPercantageGermany(bafog, studis)
print(gefordert)
plot(gefordert)