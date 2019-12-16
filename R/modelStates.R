###############################################################################################################################################
######### Imports #########

setwd("C:/Users/clari/Desktop/data_prepared/Datenjournalismus_BAfoeG/data")

library(tidyverse)
library(gvlma)
source("../R/helpers.R")


###############################################################################################################################################
######### Data import + cleaning #########

bafog <- read_csv2("bafog.csv", skip = 7, n_max = 448, col_names = FALSE, locale = locale(encoding = "latin1"), na = c("."))  %>% select(Bundesland = X1, Jahr = X2, gefordertePersonen = X9, Vollforderung = X10, Monatsbestand = X12, Kosten = X13)
studis <- read_csv2("studis.csv", skip = 7, n_max = 343, col_names = FALSE, locale = locale(encoding = "latin1")) %>% select(Bundesland = X1, Jahr = X2, Studis = X5)
nettoeinkommen <- read_csv2("nettoeinkommen.csv", skip = 0, col_names = TRUE, locale = locale(encoding = "latin1"))
wohnformen <- read_csv2("wohnformen.csv", skip = 0, col_names = TRUE, locale = locale(encoding = "latin1"))
bildung <- read_csv2("Zensus11_Datensatz_Bevoelkerung.csv", skip = 0, col_names = TRUE, locale = locale(encoding = "latin1"))
bildung[is.na(bildung)] <- 0
bildung <- bildung %>% mutate(Akademiker = (Fachhochschulabschluss + Hochschulabschluss + Promotion)/Insgesamt) %>% select(Akademiker, Bundesland)

curr_bafog <- bafog %>% filter(Jahr == 2009)
curr_studis <- studis %>% filter(Jahr == calcWS(2009))

df_model_Bundesland <- left_join(curr_studis, curr_bafog, by = "Bundesland") %>%
  mutate(Anteil_Bafog = gefordertePersonen / Studis)
df_model_Bundesland <- left_join(nettoeinkommen, df_model_Bundesland, by = "Bundesland")
df_model_Bundesland <- left_join(wohnformen, df_model_Bundesland, by = "Bundesland")
df_model_Bundesland <- left_join(bildung, df_model_Bundesland, by = "Bundesland")

###############################################################################################################################################
######### Model #########

# Full model
linearMod <- lm(Anteil_Bafog ~ Nettoeinkommen + Eltern + WG + Wohnheim + Privat + Akademiker, data=df_model_Bundesland)  
summary(linearMod)

# Stepwise selection
selectedMod <- step(linearMod)
summary(selectedMod)

gvmodel <- gvlma(selectedMod)
x11()
plot(gvmodel)
summary(gvmodel)

# Not all predictors are significant?
# Correlation
cor(df_model_Bundesland$WG, df_model_Bundesland$Eltern)

# Model without WG
linearMod <- lm(Anteil_Bafog ~ Nettoeinkommen + Eltern + Akademiker, data=df_model_Bundesland)  
summary(linearMod)
# Akademiker no longer significant... 
# Cook's distance -> Saarland is an outlier: High rates live with parents.

# Model without Akademiker
linearMod <- lm(Anteil_Bafog ~ Nettoeinkommen + Eltern, data=df_model_Bundesland)  
summary(linearMod)
plot(linearMod)

confint(linearMod, conf.level = 0.95)
