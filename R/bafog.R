###############################################################################################################################################
######### Imports #########

# !!!!!!!!!!!!! FARBSKALIERUNG ändert sich zwischen Grafiken
# Farben definieren
# Beschriftungen
# Lineare Modelle
# Geom Rect

setwd("C:/Users/clari/Desktop/data_prepared/Datenjournalismus_BAfoeG/data")

library(tidyverse) # Sammlung an Paketen aus dem tidyerse
library(RColorBrewer) # Sinnvolle Farbpaletten
library(geosphere)
library(rgdal)
library(OpenStreetMap)
library(ggpolypath)
source(helpers.R)


###############################################################################################################################################
######### Data import + cleaning #########

bafog <- read_csv2("bafog.csv", skip = 7, n_max = 448, col_names = FALSE, locale = locale(encoding = "latin1"), na = c("."))  %>% select(Bundesland = X1, Jahr = X2, gefordertePersonen = X9)
studis <- read_csv2("studis.csv", skip = 7, n_max = 343, col_names = FALSE, locale = locale(encoding = "latin1")) %>% select(Bundesland = X1, Jahr = X2, Studis = X5)
nettoeinkommen <- read_csv2("nettoeinkommen.csv", skip = 0, col_names = TRUE, locale = locale(encoding = "latin1"))
wohnformen <- read_csv2("wohnformen.csv", skip = 0, col_names = TRUE, locale = locale(encoding = "latin1")) %>% mutate(Ausserhalb = Privat + WG + Wohnheim)
bildung <- read_csv2("Zensus11_Datensatz_Bevoelkerung.csv", skip = 0, col_names = TRUE, locale = locale(encoding = "latin1"))
bildung[is.na(bildung)] <- 0
bildung <- bildung %>% mutate(Akademiker = (Fachhochschulabschluss + Hochschulabschluss + Promotion)/Insgesamt) %>% select(Akademiker, Bundesland)

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

scale_full <- c("#ffffff", "#E1F5FE", "#81D4FA", "#29B6F6", "#0288D1", "#01579B", "#00335c")
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


###############################################################################################################################################

gefordert <- calcPercantageGermany(bafog, studis)
print(gefordert)


###############################################################################################################################################
# Diagnostics https://data.library.virginia.edu/diagnostic-plots/

curr_bafog <- bafog %>% filter(Jahr == 2009)
curr_studis <- studis %>% filter(Jahr == calcWS(2009))

df_model_Bundesland <- left_join(curr_studis, curr_bafog, by = "Bundesland") %>%
  mutate(Anteil_Bafog = gefordertePersonen / Studis)
df_model_Bundesland <- left_join(nettoeinkommen, df_model_Bundesland, by = "Bundesland")
df_model_Bundesland <- left_join(wohnformen, df_model_Bundesland, by = "Bundesland")
df_model_Bundesland <- left_join(bildung, df_model_Bundesland, by = "Bundesland")

ggplot(data=df_model_Bundesland, aes(y=Anteil_Bafog, x=Nettoeinkommen)) + geom_point()
ggplot(data=df_model_Bundesland, aes(y=Anteil_Bafog, x=Eltern)) + geom_point()
ggplot(data=df_model_Bundesland, aes(y=Anteil_Bafog, x=Privat)) + geom_point()
ggplot(data=df_model_Bundesland, aes(y=Anteil_Bafog, x=WG)) + geom_point()
ggplot(data=df_model_Bundesland, aes(y=Anteil_Bafog, x=Wohnheim)) + geom_point()
ggplot(data=df_model_Bundesland, aes(y=Anteil_Bafog, x=Akademiker)) + geom_point()
ggplot(data=df_model_Bundesland, aes(y=Anteil_Bafog, x=Ausserhalb)) + geom_point()


linearMod <- lm(Anteil_Bafog ~ Nettoeinkommen + Eltern + Privat + WG + Wohnheim + Akademiker + Nettoeinkommen*Eltern, data=df_model_Bundesland)  
summary(linearMod)

# The mean of residuals is zero
mean(linearMod$residuals)

par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(linearMod)
par(mfrow=c(1,1)) # Change back to 1 x 1