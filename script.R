rm(list=ls())

library(sf)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggsn)


mun <- read_sf("C:/Users/aperis/Documents/Side_projects/brasil/br_municipios/BRMUE250GC_SIR.shp")
pop <- read_excel("C:/Users/aperis/Documents/Side_projects/brasil/pop_mun.xlsx")
#corr <- read_excel("Side_projects/brasil/regioes_geograficas_composicao_por_municipios_2017_20180911(1).xls")


#remove unvalid charaters from pop column
pop$`POPULAÇÃO ESTIMADA` <- gsub("\\([0-9]{1,2}\\)", "", pop$`POPULAÇÃO ESTIMADA`)
pop$`POPULAÇÃO ESTIMADA` <- gsub("\\.", "", pop$`POPULAÇÃO ESTIMADA`)
pop$`POPULAÇÃO ESTIMADA` <- as.numeric(pop$`POPULAÇÃO ESTIMADA`)

#on ajoute les informations sur les RGI à la base population
pop$code <- paste(pop$`COD. UF`, pop$`COD. MUNIC`, sep = "")
mun <- mun[mun$CD_GEOCMU %in% pop$code,]
mun <- left_join(mun, pop, by=c("CD_GEOCMU"="code"))


#function to create random coordinates in geometries according to population data
generate_samples <- function(data){
  suppressMessages(st_sample(data, size = data$`POPULAÇÃO ESTIMADA` / 1000))
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