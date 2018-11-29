##################################################
# MASTER UNIVERSITARIO EN MATEMATICAS            #
# Universidad de Sevilla                         # 
# MINERIA ESTADISTICA DE DATOS                   #
# Rafael González López                          #
# TRABAJO SOBRE LOS CONTENIDOS DEL TEMA 4        #
##################################################

rm(list = ls())

# Importamos los datos del trabajo y otros paquetes que
# pueden resultar útiles
library(dplyr)
library(ggplot2)
library(h2o)
library(nnet)
library(pixmap)

# 1) Leemos y transformamos los datos
##############################################
# Fijamos una semilla para asegurar la reproducibilidad
# del experimento
set.seed(15162342)

# Importamos los datos
train <- read.csv("mnist_train.csv",header=FALSE)
test <- read.csv("mnist_test.csv",header=FALSE)

# Transformamos los datos
train[,1:784] <- train[,1:784]/255
test[,1:784] <- test[,1:784]/255

# 2) Seleccionamos las muestras adecuadamente
##############################################
# Solo trabajamos con 5 digitos elegidos aleatoriamente
digitos = sample(0:9,5)
train = filter(train,V785 %in% digitos)
test = filter(test,V785 %in% digitos)

# Y escogemos una cantidad menor de datos para el modelo
muestratrain = sample(nrow(train),3000)
muestratest = sample(nrow(test),1000)

train = train[muestratrain,]
test = test[muestratest,]

# 3) Pasamos a una representación gráfica
##############################################
# Ajustamos las opciones del plot
parantig<-par(mfrow=c(5,5),mar=c(0,0,0,0)+1,bg="white")

# Ploteamos loas 25 primeros del conjunto de entrenamiento
for (i in 1:25)
{
  matriz<- train[i,1:784]
  x<-pixmapGrey(matriz,28,28,cellres=1)
  x@grey<-t(x@grey)
  plot(x)
}
# A continuación, los correspondientes del conjuno test
for (i in 1:25)
{
matriz<- test[i,1:784]
x<-pixmapGrey(matriz,28,28,cellres=1)
x@grey<-t(x@grey)
plot(x)
}

#Restablece las opciones
par(parantig)  

# 4) Despreciamos las variables constantes
##########################################################
# Naturalmente, estas son aquellas que tengan varianza 0
# Para catalogarlas, tomamos un vector que sea FALSE
# en dichas posiciones
nonconstant = apply(train,2,var) != 0
train = train[,nonconstant]
test = test[,nonconstant]

# 5) Construimos el modelo sobre las componentes principales
############################################################
# Usamos el comando princomp. Como tenemos 607 variables, la 
# número 607 es la que queremos predecir. 
acp<- princomp(train[,-607],cor=FALSE)
summary(acp)

# Seleccionamos las CP con var mayor que 1
m<- sum(acp$sdev^2 > mean(acp$sdev^2))
m

# La variabilidad explicada es 89.22%
expvar <- sum((acp$sdev^2)[1:m])/sum(acp$sdev^2)*100
expvar

# Calculamos las puntuaciones para los conjuntos train y test
puntucp_ent<- acp$scores[,1:m]
puntucp_test<- predict(acp,test[,-607])[,1:m]
digito=c(train[,607],test[,607])
digi_cod=class.ind(digito)
head(digi_cod,n=20)

# Construimos la red
red=nnet(puntucp_ent,digi_cod[1:nrow(train),],size=10,decay=0,linout=TRUE)

# Analizamos el resultado
summary(red)
str(predict(red))

head(predict(red))
# Evaluamos el acierto
pred_ent=apply(predict(red),1,which.max)-1
pred_test=apply(predict(red,puntucp_test),1,which.max)-1

# En el conjunto de entrenamiento
tablaent=table(digito[1:nrow(train)],pred_ent)
tablaent
aciertos_ent=100*diag(prop.table(tablaent,1))
acierto_ent=100*sum(diag(tablaent))/sum(tablaent)
acierto_ent
tabla = data.frame(cbind(tablaent,Acierto_ent=round(aciertos_ent,1)))
colnames(tabla) = c("0","1","2","3","9","Acierto Ent")
tabla

# En el conjunto test
tablatest=table(digito[-c(1:nrow(train))],pred_test)
tablatest
aciertos_test=100*diag(prop.table(tablatest,1))
acierto_test=100*sum(diag(tablatest))/sum(tablatest)
acierto_test
cbind(tablatest,Acierto_test=round(aciertos_test,1))
acierto_ent
tabla = data.frame(cbind(tablatest,Acierto_test=round(aciertos_test,1)))
colnames(tabla) = c("0","1","2","3","9","Acierto Test")
tabla

# 6) Construimos el modelo de Aprendizaje Profundo
###################################################
# Inicializamos el sistema y transformamos los datos 
# para adecuarlos a nuestro entorno
localH2O = h2o.init(nthreads = -1)
train[,607] <- factor(train[,607])
test[,607] <- factor(test[,607])
train.hex <- as.h2o(train)
test.hex<- as.h2o(test)

# Construímos el modelo
modelo_h2o <- h2o.deeplearning(
  x = 1:606, y = 607, 
  training_frame = train.hex,
  validation_frame=test.hex,
  distribution="multinomial",
  activation = 'RectifierWithDropout',
  hidden = c(200, 200),
  hidden_dropout_ratio = c(0.5, 0.5),
  input_dropout_ratio = 0.2,
  epochs = 50,
  l1 = 1e-5,
  l2 = 1e-5,
  rho = 0.99,
  epsilon = 1e-8,
  train_samples_per_iteration = 2000)
summary(modelo_h2o) 

# Obtenemos las predicciones 
predic_test <- h2o.predict(modelo_h2o, newdata = test.hex)
pred <- as.data.frame(predic_test)
head(pred)
tail(pred)

# Resumimos la efectividad
tabla=table(test[,607],pred[,1])
tabla
aciertos=100*diag(prop.table(tabla,1))
acierto=100*sum(diag(tabla))/sum(tabla)
acierto
tabla2 = data.frame(cbind(tabla,Acierto_test=round(aciertos,1)))
colnames(tabla2) = c("0","1","2","3","9","Acierto Test")
tabla2
