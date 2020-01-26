# Plots card visualizing the ratio of students receiving BAföG by state for a given year 
# and also calculates the development of the ratio over the years.

###############################################################################################################################################
######### Imports #########

# TODO: Adapt to your local path!
setwd("C:/Users/clari/Desktop/Daten_IT_new/data")

library(tidyverse) 
library(RColorBrewer) 
library(geosphere)
library(rgdal)
library(OpenStreetMap)
library(ggpolypath)
require(reshape2)
source("../R/helpers.R")

###############################################################################################################################################
######### Data import + cleaning #########

bafog <- read_csv2("bafog.csv", skip = 7, n_max = 448, col_names = FALSE, locale = locale(encoding = "latin1"), na = c(".")) %>% select(Bundesland = X1, Jahr = X2, gefordertePersonen = X9, Vollforderung = X10, Monatsbestand = X12, Kosten = X13)
studis <- read_csv2("studis.csv", skip = 7, n_max = 343, col_names = FALSE, locale = locale(encoding = "latin1")) %>% select(Bundesland = X1, Jahr = X2, Studis = X5)
compare <- read_csv2("compare.csv")

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

# Define color scales
scale_full <- c("#ffffff", "#E1F5FE", "#81D4FA", "#29B6F6", "#0288D1", "#0266b5", "#014880", "#00335c")
scale_greater15 <- scale_full[-1]
scale_greater20 <- scale_greater15[-1]

# Plot Germany maps
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

#########

# Plot Germany wide development over the years
gefordert <- calcPercantageGermany(bafog, studis)

# Required as input for the website
print(gefordert)

# Only for comprehensibility
gefordert <- data.frame(1998:2018, gefordert)
colnames(gefordert) <- c('year', 'ratio')
x11()
ggplot(gefordert, aes(year, ratio)) +
  geom_line(color = "#03a9f4", size=2) +
  scale_x_continuous(breaks = c(1998:2018)) +
  theme(text = element_text(size=16)) +
  xlab("Jahr") +
  ylab("Anteil der BAföG-Empfänger") +
  ggtitle("Deutschlandweite Entwicklung des Anteils der BAföG-Empfänger")

#########

# Visualize differences between different calculation possibilities

x11()
p <- ggplot(dat = melt(compare, id.var="jahr"), aes(x=jahr, y=value)) + 
  geom_line(aes(colour=variable, group=variable), size=2) + 
  scale_x_continuous(breaks = c(2000:2016), minor_breaks = seq(2000, 2016, 1)) +
  labs(x ="Jahr", y = "Anteil der BAföG-Empfänger", title = "Deutschlandweite Entwicklung des Anteils der BAföG-Empfänger", subtitle = "Verschiedene Berechnungsarten") +
  theme(legend.position="right")

colScale <- scale_colour_manual(name = "Berechnungsart" ,values = c("#29B6F6", "#00335c", "#0266b5"), labels=c("BAfög-Empfänger / deutsche Studierende", "BAföG-Empfänger / alle Studierende", "BAföG-Empfänger / Anspruchsberechtigte"))
p + colScale


################################################

# German students over time
i <- 1
de_studis <- c()
for (year in c(1998:2018)) {
  semester <- calcWS(year)
  currStudis <- studis %>% filter(Jahr == semester)
  de_studis[i] <- sum(currStudis$Studis)
  i <- i + 1
}
Jahr <- c(1998:2018)
de_studis <- data.frame(Jahr, de_studis)

x11()
ggplot(data=de_studis, aes(x=Jahr, y=de_studis)) +
  geom_line(color="#03a9f4") +
  scale_x_continuous(breaks = c(1998:2018))+
  xlab("Jahr") + ylab("Zahl der deutschen Studierenden") + 
  theme(text = element_text(size=16)) +
  ggtitle("Entwicklung der Zahl der deutschen Studierenden über die Zeit")


