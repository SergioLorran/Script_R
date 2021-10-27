

# Autor: SÉRGIO LORRAN SOUZA DA SILVA
# ENGENHEIRO CARTOGRAFO E AGRIMENSOR
# CIENTISTA DE DADOS / ANALISTA DE DADOS
# Titulo: Processamento Digital de Imagem


#------------------------------#


#------CARREGANDO PACOTES-------//
library(sp)
library(raster)
library(rgdal)
library(lattice)
library(latticeExtra)
library(dismo)
library(Rcpp)
library(rpart)


#diretorio
setwd("C:/Users/Sérgio/Documents/R_LORRAN/GitHUB/pratica_2/rsdata/rs")


nlcd <- brick('nlcd-L1.tif')
names(nlcd) <- c("nlcd2001", "nlcd2011")

# Os nomes das classes e cores para plotagem
nlcdclass <- c("Água", "Desenvolvido", "Estéril", "Floresta", "Arbusto", "Herbacea", "Plantado/Cultivado", "zonas úmidas")
classdf <- data.frame(classvalue1 = c(1,2,3,4,5,7,8,9), classnames1 = nlcdclass)
classdf

# Cores
classcolor <- c("#5475A8", "#B50000", "#D2CDC0", "#38814E", "#AF963C", "#D1D182", "#FBF65D", "#C8E6F8")

## RAT = "Raster Attribute Table" e ncld2011 é define RasterLayer como uma variável categórica.
nlcd2011 <- nlcd[[2]]            
nlcd2011 <- ratify(nlcd2011)
rat <- levels(nlcd2011)[[1]]

#
rat$landcover <- nlcdclass
levels(nlcd2011) <- rat


set.seed(99)



# Amostragem

samp2011 <- sampleStratified(nlcd2011, size = 200, na.rm = TRUE, sp = TRUE) 
samp2011



# Número de amostras em cada classe
table(samp2011$nlcd2011)

ref <- levelplot(nlcd2011, col.regions = classcolor, main = 'referencia')
print(ref)

plt <- levelplot(nlcd2011, col.regions = classcolor, main = 'pontos de treinamento')
print(plt + layer(sp.points(samp2011, pch = 3, cex = 0.5, col = 1)))

landsat5 <- brick("centralvalley-2011LT5.tif") 
names(landsat5) <- c('azul', 'verde', 'vermelho', 'NIR', 'SWIR1', 'SWIR2')

plot(landsat5, col=gray((0:100)/100))

sampvals <- extract(landsat5,samp2011)


sampvals <- sampvals[, -1]


sampdata <- data.frame(classvalue = samp2011@data$nlcd2011, sampvals)
library(terra)
library(rasterVis)


cart <- rpart(as.factor(classvalue)~., data=sampdata, method = 'class', minsplit = 5)


plot(cart, uniform=TRUE, main="Árvore de decisão")
text(cart, cex = 0.8)


pr2011 <- predict(landsat5, cart, type='class')
pr2011


pr2011 <- ratify(pr2011)
rat <- levels(pr2011)[[1]]
rat$legend <- classdf$classnames
levels(pr2011) <- rat
levelplot(pr2011, maxpixels = 1e6,
          col.regions = classcolor,
          scales=list(draw=FALSE),
          main = "Árvore de decição- classificação Landsat 5")



set.seed(99)
j <- kfold(sampdata, k = 5, by=sampdata$classvalue)
table(j)


x <- list()
for (k in 1:5) {
  train <- sampdata[j!= k, ]
  test <- sampdata[j == k, ]
  cart <- rpart(as.factor(classvalue)~., data=train, method = 'class', minsplit = 5)
  pclass <- predict(cart, test, type='class')
  # create a data.frame using the reference and prediction
  x[[k]] <- cbind(test$classvalue, as.integer(pclass))
}


y <- do.call(rbind, x)
y <- data.frame(y)
colnames(y) <- c('observado', 'predito')
conmat <- table(y)
# change the name of the classes
colnames(conmat) <- classdf$classnames
rownames(conmat) <- classdf$classnames
conmat



n <- sum(conmat)
n


diag <- diag(conmat)

OA <- sum(diag) / n
OA


rowsums <- apply(conmat, 1, sum)
p <- rowsums / n

colsums <- apply(conmat, 2, sum)
q <- colsums / n
expAccuracy <- sum(p*q)
kappa <- (OA - expAccuracy) / (1 - expAccuracy)
kappa


PA <- diag / colsums

UA <- diag / rowsums
outAcc <- data.frame(producerAccuracy = PA, userAccuracy = UA)
outAcc

