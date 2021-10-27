



# Autor: SÉRGIO LORRAN SOUZA DA SILVA
# ENGENHEIRO CARTOGRAFO E AGRIMENSOR
# CIENTISTA DE DADOS / ANALISTA DE DADOS
# Titulo: Processamento Digital de Imagem



#------------------------------#


#------CARREGANDO PACOTES-------//
library(sp)
library(raster)
library(rgdal)


#Diretorio
setwd("C:/Users/Sérgio/Documents/R_LORRAN/GitHUB/pratica_2/rsdata/rs")


list.files()

#azul
b2 <- raster("LC08_044034_20170614_B2.tif")

#verde
b3 <- raster("LC08_044034_20170614_B3.tif")

#vemelho
b4 <- raster("LC08_044034_20170614_B4.tif")

#NIR
b5 <- raster("LC08_044034_20170614_B5.tif")


plot(b2, col = gray(0:100/100))

plot(b3, col = gray(0:100/100))

plot(b4, col = gray(0:100/100))

plot(b5, col = gray(0:100/100))



# #Juntar banda em um unico arquivo
# b2b3b4b5 <- stack(b2,b3,b4,b5)
# 
# plot(b2b3b4b5, col = gray(0:100/100))
# 
# #extenção do b2
# b2
# #extent:594090, 639000, 4190190, 42275
# 
# 
# 
# #recortar imagem
# cropbox = c(609000,629000,4200190,4220000)
# 
# #nova imagem
# subset = crop(b2b3b4b5,cropbox)
# 
# plot(subset,col = gray(0:100/100))
# 
# 
# 
# #outro jeito de fazer um recorte
# plot(b2b3b4b5[[1]])
# cropbox = drawExtent()
# #clicar com o mouse duas vezes
# 
# 
# 
# subset2 = crop(b2b3b4b5,cropbox)
# plot(subset2, col= gray(0:100/100)))
# 
# 
# 
# 



##_________###________

#ler em bloco - juntou todas as bandas 1 ao 11
filenames = paste0("LC08_044034_20170614_B",1:11,".tif")
landsat = stack(filenames)

plot(landsat, col= gray(0:100/100))


#outro jeito de ler junto as bancas de 1 ao 11
#file = list.files(pattern = "LC08_044034_20170614_B*")



landsatRGB = stack(b4,b3,b2)
plotRGB(landsatRGB,axes = T, stretch = "lin", main = "landsat cor verdadeira")



##_______##

#FALSA COR
#vermelho = b5(infra) #verde = b4 #azul =b3

landsatFCC = stack(b5,b4,b3)

plotRGB(landsatFCC,axes = T, stretch = "lin", main = "landsat falsa cor")



#PLOTAR DO LADO
par(mfrow=c(1,2))
plotRGB(landsatRGB,axes = T, stretch = "lin", main = "landsat cor verdadeira")
plotRGB(landsatFCC,axes = T, stretch = "lin", main = "landsat falsa cor")



landsat = subset(landsat,1:7)
names(landsat)

names(landsat) = c('ultra-azul','azul','verde','vermelho','NIR','SWIR1', 'SWIR2')
names(landsat)

e <- extent(624387,635752,4200047,4210939)
landsatcrop = crop(landsat, e)
plot(landsatcrop[[5]])


#relacao de bandas

s = stack(landsatcrop[[1]], landsatcrop[[2]])
pairs(s, main = 'ultra-azul x azul')


s = stack(landsatcrop[[4]], landsatcrop[[5]])
pairs(s, main = 'vermelho x NIR')
