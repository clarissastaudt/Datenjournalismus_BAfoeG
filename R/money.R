###############################################################################################################################################
######### Imports #########

setwd("C:/Users/clari/Desktop/data_prepared/Datenjournalismus_BAfoeG/data")

library(tidyverse)
library(rgdal)
source(helpers.R)


###############################################################################################################################################
######### Data import + cleaning #########

bafog <- read_csv2("bafog.csv", skip = 7, n_max = 448, col_names = FALSE, locale = locale(encoding = "latin1"), na = c("."))  %>% select(Bundesland = X1, Jahr = X2, gefordertePersonen = X9, Vollforderung = X10, Monatsbestand = X12, Kosten = X13)
studis <- read_csv2("studis.csv", skip = 7, n_max = 343, col_names = FALSE, locale = locale(encoding = "latin1")) %>% select(Bundesland = X1, Jahr = X2, Studis = X5)

###############################################################################################################################################
######### Costs and average monthly grant 2018 #########

bafog18 <- bafog %>% filter(Jahr == 2018)

print("Complete cost for student BAföG in 2018 [in thousand ???]")
sum(bafog18$Kosten)

print("Average monthly grant based on the average amount of monthly supported students")
sum(bafog18$Kosten) * 1000 / sum(bafog18$Monatsbestand) / 12

print("Average monthly grant based on total of supported students for that year")
sum(bafog18$Kosten) * 1000 / sum(bafog18$gefordertePersonen) / 12

###############################################################################################################################################
# Maximum grant

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
  xlab("") + ylab("") + theme(text = element_text(size=16))
