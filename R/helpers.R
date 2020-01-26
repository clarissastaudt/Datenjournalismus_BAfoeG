###############################################################################################################################################

# Calcs WS for given year
# e.g.: 1999 -> WS 1999/00
calcWS <- function(year) {
  next_year <- as.POSIXlt(as.Date(paste(year, "/01/01", sep="")))
  next_year$year <- next_year$year+1
  next_year <- as.Date(next_year)
  next_year <- substring(next_year,3,4) 
  semester <- paste("WS ", year, sep="")
  semester <- paste(semester, next_year, sep="/")  
  return(semester)
}

###############################################################################################################################################

# Loads shape data for Germany
loadShapeGermany <- function() {
  area <- readOGR("gadm36_DEU_1.shp", encoding = "UTF-8", use_iconv = TRUE) %>% 
    fortify() %>%
    mutate(xMercator = mercator(cbind(long, lat))[,1],
           yMercator = mercator(cbind(long, lat))[,2])
  areanames <- readRDS("areanames.rds") # Label fuer die Gebiete laden
  areanames <- areanames %>% select(NAME_1, CC_1) %>% filter(!duplicated(CC_1)) %>% mutate(id = as.character(as.integer(rownames(.)) - 1))
  area2 <- left_join(area, areanames, by = "id")
  return(area2)
}

###############################################################################################################################################

createRatioPlot <- function(year, colorscale) {
  
  # Calc WS for given year
  semester <- calcWS(year)

  # Filter by year
  curr_bafog <- bafog %>% filter(Jahr == year)
  curr_studis <- studis %>% filter(Jahr == semester)
  
  # Join data 
  curr_area2 <- left_join(area2, curr_bafog, by = c("NAME_1" = "Bundesland"))
  curr_area2 <- left_join(curr_area2, curr_studis, by = c("NAME_1" = "Bundesland"))
  
  # Plot map 
  # Axis titles and descriptions are added in html
  # Data is in interval 15 - 45% 
  # -> Dividing this into intervals of five yields seven colors 
  x11()
  autoplot(osmmap) + 
    geom_rect(mapping=aes(xmin=620000, xmax=Inf, ymin=5900000, ymax=Inf), fill="white", color="white", alpha=1) +
    geom_polypath(aes(x = xMercator,
                      y = yMercator,
                      group = group,
                      fill = cut(x = gefordertePersonen/Studis*100, breaks= c(0, 10, 15, 20, 25, 30, 35, 40, 45, 50, 100), include.lowest = TRUE)
    ),
    data = curr_area2,
    color = "black",
    size = 0.5,
    alpha = 1,) +
    labs(x = "", y = "", fill = "") + #, title = year
    scale_fill_manual(values=colorscale)+
    theme_void() +
    theme(legend.position="none")
}

###############################################################################################################################################

# Values for javascript visualisation of Germany wide ratio development over the years
calcPercantageGermany <- function(bafog, studis) {
  i <- 1
  gefordert <- c()
  for (year in c(1998:2018)) {
    
    semester <- calcWS(year)
    currStudis <- studis %>% filter(Jahr == semester)
    currBafog <- bafog %>% filter(Jahr == year)
    currPercentage <- (sum(currBafog$gefordertePersonen))/(sum(currStudis$Studis))
    gefordert[i] <- currPercentage
    i <- i + 1
  }
  return(gefordert)
}