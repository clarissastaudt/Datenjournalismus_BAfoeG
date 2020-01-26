###############################################################################################################################################
######### Imports #########

# TODO: Adapt to your local path!
setwd("C:/Users/clari/Desktop/Daten_IT_new/data")

library(tidyverse)
library(rgdal)
source("../R/helpers.R")


###############################################################################################################################################
######### Data import + cleaning #########

bafog <- read_csv2("bafog.csv", skip = 7, n_max = 448, col_names = FALSE, locale = locale(encoding = "latin1"), na = c("."))  %>% select(Bundesland = X1, Jahr = X2, gefordertePersonen = X9, Vollforderung = X10, Monatsbestand = X12, Kosten = X13)
studis <- read_csv2("studis.csv", skip = 7, n_max = 343, col_names = FALSE, locale = locale(encoding = "latin1")) %>% select(Bundesland = X1, Jahr = X2, Studis = X5)

###############################################################################################################################################
######### Costs and average monthly grant 2018 #########

bafog18 <- bafog %>% filter(Jahr == 2018)

print("Complete cost for student BAföG in 2018 [in thousand]")
sum(bafog18$Kosten)

print("Average monthly grant based on the average amount of monthly supported students")
sum(bafog18$Kosten) * 1000 / sum(bafog18$Monatsbestand) / 12

# This does not make sense, as a student does not necessarily receive BAfoG for the whole year
print("Average monthly grant based on total of supported students for that year")
sum(bafog18$Kosten) * 1000 / sum(bafog18$gefordertePersonen) / 12


###################################

# Average monthly BAföG over time

i <- 1
mean_monthly <- c()
for (year in c(1998:2018)) {
  currBafog <- bafog %>% filter(Jahr == year)
  mean_monthly[i] <- sum(currBafog$Kosten) * 1000 / sum(currBafog$Monatsbestand) / 12
  i <- i + 1
}

# Required as input for the website
print(mean_monthly)

# Only for comprehensibility
mean_monthly <- data.frame(mean_monthly, 1998:2018)
colnames(mean_monthly) <- c('mean','year')
x11()
ggplot(mean_monthly, aes(year, mean)) +
  geom_line(color = "#03a9f4", size=2) +
  scale_x_continuous(breaks = c(1998:2018)) +
  theme(text = element_text(size=16)) +
  xlab("Jahr") +
  ylab("Mittlerer monatlicher BAföG-Satz [???]") +
  ggtitle("Mittlerer monatlicher BAföG-Satz im Zeitverlauf")

###############################################################################################################################################

# Percentage of students receiving the full grant
i <- 1
full <- c()
for (year in c(1998:2018)) {
  semester <- calcWS(year)
  currBafog <- bafog %>% filter(Jahr == year)
  currPercentage <- ((sum(currBafog$Vollforderung)) / sum(currBafog$gefordertePersonen))
  full[i] <- currPercentage
  i <- i + 1
}

Jahr <- c(1998:2018)
full <- data.frame(Jahr, full)

x11()
ggplot(data=full, aes(x=Jahr, y=full)) +
  geom_line(color="#03a9f4") +
  scale_x_continuous(breaks = c(1998:2018))+
  xlab("Jahr") + ylab("Anteil der Vollgeförderten unter den BAföG-Empfängern") + 
  theme(text = element_text(size=16)) +
  ggtitle("Entwicklung des Anteils der Vollgeförderten unter den BAföG-Empfängern über die Zeit")


################################################

# Total cost for student BAföG over time
i <- 1
spent <- c()
for (year in c(1998:2018)) {
  semester <- calcWS(year)
  currBafog <- bafog %>% filter(Jahr == year)
  spent[i] <- sum(currBafog$Kosten)
  i <- i + 1
}
Jahr <- c(1998:2018)
spent <- data.frame(Jahr, spent)

x11()
ggplot(data=spent, aes(x=Jahr, y=spent)) +
  geom_line(color="#03a9f4") +
  scale_x_continuous(breaks = c(1998:2018))+
  xlab("Jahr") + ylab("Gesamtausgaben für studentisches BAföG [Tausend ???]") + 
  theme(text = element_text(size=16)) +
  ggtitle("Gesamtausgaben für studentisches BAföG über die Zeit")

