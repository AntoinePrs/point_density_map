rm(list=ls())

library(sf)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggsn)


mun <- read_sf("~/BRMUE250GC_SIR.shp")
pop <- read_excel("~/brasil/pop_mun.xlsx")
#corr <- read_excel("Side_projects/brasil/regioes_geograficas_composicao_por_municipios_2017_20180911(1).xls")


#remove unvalid charaters from pop column
pop$`POPULA플O ESTIMADA` <- gsub("\\([0-9]{1,2}\\)", "", pop$`POPULA플O ESTIMADA`)
pop$`POPULA플O ESTIMADA` <- gsub("\\.", "", pop$`POPULA플O ESTIMADA`)
pop$`POPULA플O ESTIMADA` <- as.numeric(pop$`POPULA플O ESTIMADA`)

#join the population data with the geometries
pop$code <- paste(pop$`COD. UF`, pop$`COD. MUNIC`, sep = "")
mun <- mun[mun$CD_GEOCMU %in% pop$code,]
mun <- left_join(mun, pop, by=c("CD_GEOCMU"="code"))


#function to create random coordinates in geometries according to population data
generate_samples <- function(data){
  suppressMessages(st_sample(data, size = data$`POPULA플O ESTIMADA` / 1000))
} 

points <- generate_samples(mun)

#################################################
#visualisation

g <- ggplot()+
  theme_void()+
  theme(panel.grid = element_line(size = 0),
        plot.background = element_rect(fill = "white",
                                       colour = "white"),
        axis.title = element_blank())+
  geom_sf(data=points, size=.001)+
  scalebar(data = bb2, dist = 500, dd2km = TRUE, model  = "WGS84", 
           location = "topleft", height = .01, st.size = 3)