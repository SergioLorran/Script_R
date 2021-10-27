


# Autor: SÉRGIO LORRAN SOUZA DA SILVA
# ENGENHEIRO CARTOGRAFO E AGRIMENSOR
# CIENTISTA DE DADOS / ANALISTA DE DADOS
# Titulo: Mapa do Brasil com escala e norte



#------CARREGANDO PACOTES-------//

library(esquisse)
library(ggplot2)
library(geobr)
library(sf)
library(ggspatial)

library(broom)
library(dplyr)
library(GISTools)
library(maps)
library(rgdal)


#--------------//----------------//


#Diretorio
setwd('C:/Users/Sérgio/Documents/R_LORRAN/GitHUB')


#----MAPA DO PARÁ----------//

pa <- read_state(code_state = "PA",year = 2020)
#esquisser(pa)

#-------------------//-------------------//

#--MAPA DO BRASIL-----//

br <- read_state(code_state = "all",year = 2020)
#esquisser(br)

#---MAPA---ESCALA---NORTE---ETC---//

ggplot(br) +
  aes(group = code_state) +
  geom_sf(size = 0.1,fill="#FFF7C2") + 
  geom_sf(aes(group=code_state),data = pa,fill="#77B7F5",col="#77B7F5") +
  labs(x = "Longitude", y = "Latitude", title = "Brasil") +
  annotation_north_arrow(style = north_arrow_fancy_orienteering,location="tr") + 
  annotation_scale(location="br",height = unit(0.15,"cm"))+
  theme_bw() +
  theme(
    plot.title = element_text(size = 16L,
                              face = "bold",
                              hjust = 0.5),
    axis.title.y = element_text(size = 14L),
    axis.title.x = element_text(size = 14L)
  ) +
  xlim (-74,-35) +
  ylim(-34,5)







