##################################################
# MASTER UNIVERSITARIO EN MATEMATICAS            #
# Universidad de Sevilla                         # 
# MINERIA ESTADISTICA DE DATOS                   #
# Rafael González López                          #
# TRABAJO SOBRE LOS CONTENIDOS DEL TEMA 1        #
##################################################

# Importamos los datos del trabajo y otros paquetes que
# pueden resultar útiles
library(mlbench)
library(class)
library(ROCR) 
library(dplyr)
library(e1071)
library(ggplot2)

# I) BreastCancer
##############################################
# Fijamos una semilla para asegurar la reproducibilidad
# del experimento
set.seed(48151623)

# Cargamos el primer conjunto de datos y realizamos una
# pequeña exploración
data(BreastCancer)
summary(BreastCancer) #Observamos que existen varios NA's
datos = na.omit(BreastCancer)
summary(datos)
glimpse(datos)

n <- nrow(datos)
indices<- 1:n

# Seleccionamos un 70% de valores de indices para entrenamiento
# y el resto para test
inditest= sample(indices,ceiling(n*0.30))
indient= setdiff(indices,inditest)

xent=datos[indient,2:10]
xtest=datos[inditest,2:10]
yent=datos[indient,11]
ytest=datos[inditest,11]


# En este caso no tenemos necesidad de tipificar las variables,
# pues todas son factores. Si hubiese algunas de tipo cuantitativo
# la separaríamos para poder 

## i) Modelo de Naive-Bayes.
##############################################
# Construimos el modelo de Naive-Bayes de la siguiente forma
modeloNB<- naiveBayes(Class ~ ., data = datos[indient,-1])
modeloNB      #Medias y d.t. en cada grupo
preditest<- predict(modeloNB,xtest)
confutest<- table(ytest,preditest)
confutest
Acierto=100*(confutest[1,1]+confutest[2,2])/sum(confutest)
Sensit= 100*confutest[2,2]/sum(confutest[2,])
Especif=100*confutest[1,1]/sum(confutest[1,])
tabla = data.frame("Naive-Bayes" = c(Acierto,Sensit,Especif))
rownames(tabla) <- c("Acierto","Sensitividad","Especificidad")
tabla

# Construimos ahora la curva ROC
#pdf("roc1.pdf", height=5, width=7)
probabi<- predict(modeloNB,xtest,type="raw")[,2] #Prob. Sí
prediobj<-prediction(probabi,ytest)
roc1 = performance(prediobj, "tpr","fpr")
rocdf = data.frame(x = slot(roc1,"x.values")[[1]],y=slot(roc1,"y.values")[[1]])
auc<- as.numeric(performance(prediobj,"auc")@y.values)
ggplot(rocdf,aes(x=x,y=y,main="r")) + geom_line(size = 2, alpha = 0.7) + 
  labs(title = "Curva ROC para Naive Bayes", x= "Tasa de falsos positivos",
       y = "Tasa de verdaderos positivos") + geom_abline(linetype = "dashed") +
  annotate("text", x= 0.75, y= 0.31, label = paste("AUC =",round(auc,3)),size=5)

cat("AUC test= ",auc ,"\n")
#dev.off()



## ii) Modelo knn
##############################################
# Aplicamos validación cruzada tal y como hemos visto en clase
# con el fin de encontrar el parámetro más adecuado para el
# modelo
knnvc<- tune.knn(xent,  yent,k=seq(1,15),use.all=FALSE)
knnvc
as = data.frame(knnvc$performances)    #rendimientos en cada división
#pdf("param1.pdf", height=5, width=7)
ggplot(data = as, aes(x = k,y=error)) +scale_x_discrete("k", limits = 1:15) +  geom_line() + geom_point(shape=24,col="red")
knnvc$best.parameters
#Rendimiento en el conjunto test
preditestVC<- knn(train=xent,cl=yent,prob=TRUE,
                  test=xtest,k=knnvc$best.parameters$k,
                  use.all=FALSE)
#dev.off()
# Ilustramos algunas medidas de error del modelo
confutestVC<-table(ytest,preditestVC)
confutestVC

Acierto=100*(confutestVC[1,1]+confutestVC[2,2])/sum(confutestVC)
Sensit= 100*confutestVC[2,2]/sum(confutestVC[2,])
Especif=100*confutestVC[1,1]/sum(confutestVC[1,])
tabla2 = data.frame("knn" = c(Acierto,Sensit,Especif))
rownames(tabla2) <- c("Acierto","Sensitividad","Especificidad")
tabla2
compara = cbind(tabla,tabla2)
compara
# A continuación construimos la curva ROC
proportestVC = attr(preditestVC,"prob") #Prop. clase asignada
clasif_n=which(preditestVC == "benign")
probabitestVC_y= proportestVC   
probabitestVC_y[clasif_n]= 1-proportestVC[clasif_n]
predVC <- prediction(probabitestVC_y, ytest) 
roc1 = performance(predVC,"tpr","fpr") 
rocdf = data.frame(x = slot(roc1,"x.values")[[1]],y=slot(roc1,"y.values")[[1]])
aucVC<- as.numeric(performance(predVC,"auc")@y.values)
#pdf("roc2.pdf", height=5, width=7)
ggplot(rocdf,aes(x=x,y=y,main="r")) + geom_line(size = 2, alpha = 0.7) + 
  labs(title = "Curva ROC para KNN", x= "Tasa de falsos positivos",
       y = "Tasa de verdaderos positivos") + geom_abline(linetype = "dashed") +
  annotate("text", x= 0.75, y= 0.31, label = paste("AUC =",round(aucVC,3)),size=5) 
cat("AUC test= ",auc ,"\n")
#dev.off()

# II) Glass
##############################################
# Fijamos una semilla para asegurar la reproducibilidad
# del experimento
rm(list=ls())
set.seed(48151623)

# Cargamos el primer conjunto de datos y realizamos una
# pequeña exploración
data(Glass)
summary(Glass) #Observamos que existen varios NA's
glimpse(Glass)

n <- nrow(Glass)
indices<- 1:n

## i) Parámetros de knn
#############################################
# Procedemos análogamente a como calculamos el parámetro
# utilizando validación cruzada

datos =  scale(Glass[,-10],center=TRUE,scale=TRUE)
knnvc<- tune.knn(datos,Glass[,10],k=seq(1,15),use.all=FALSE)
knnvc
knnvc$performances    #rendimientos en cada 
ggplot(data = data.frame(knnvc$performances), aes(x=k, y=error)) + geom_line(color="blue") + geom_point(shape=24) + scale_x_discrete("k", limits = 1:15) 
knnvc$best.parameters
parametro = knnvc$best.parameters 
## ii) Errores boostrap
#############################################
## Método KNN

B<- 2000
predi= knn(train=datos,cl=Glass[,10],prob=TRUE,  
                        test=datos,k=parametro,
                        use.all=FALSE)                         
erroremp<- mean(predi!=Glass[,10])
errorboot<-numeric(B)  #Error en datos para cada modelo bootstrap
errorOOB<- numeric(B)  #Error OOB para cada modelo bootstrap
n<- nrow(datos)
indin<- 1:n
for (b in 1:B)
{ if (b %% 25==0) cat("Muestra bootstrap número ",b,"\n")
  indiboot= sample(indin,rep=TRUE) 
  indiOOB= setdiff(indin,indiboot)
  modelo.boot=knn(train=datos[indiboot,],cl=Glass[indiboot,10],prob=TRUE,  
                  test=datos,k=parametro, use.all=FALSE)
  errorboot[b]= mean(modelo.boot!=Glass[,10])
  prediOOB= knn(train=datos[indiboot,],cl=Glass[indiboot,10],prob=TRUE,  
                            test=Glass[indiOOB,-10],k=parametro,use.all=FALSE)           
  errorOOB[b]= mean(prediOOB!=Glass[indiOOB,10])
}
errorB<- mean(errorboot)
errorOOB<- mean(errorOOB)
error632B<-0.368*erroremp+0.632*errorOOB  

matrizL<- matrix(NA,n,n)
for (i in 1:n)
  for (j in 1:n)
  {
    matrizL[i,j]<- (Glass[i,10]!=predi[j]) 
  }
Noinf<- mean(matrizL)
tsr<- (errorOOB-erroremp)/(Noinf-erroremp)
w<- 0.632/(1-0.368*tsr)
error632masB<-min((1-w)*erroremp+w*errorOOB,Noinf)

tabla = data.frame("KNN" = c(erroremp,errorB,errorOOB,error632B,error632masB))
rownames(tabla) <- c("Tasa Error Empírico","Tasa Error Bootstrap","Tasa Error OOB",
                     "Tasa Error 0.632Boot","Tasa Error 0.632+Boot")
tabla


# Método Naive-Bayes
B<- 1000
datos2 = data.frame(datos,type = Glass[,10])
modeloNB<- naiveBayes(type~ ., data = datos2) 
predi<- predict(modeloNB,datos)
erroremp<- mean(predi!=Glass[,10])
errorboot<-numeric(B)  #Error en datos para cada modelo bootstrap
errorOOB<- numeric(B)  #Error OOB para cada modelo bootstrap
n<- nrow(datos)
indin<- 1:n
for (b in 1:B)
{ if (b %% 25==0) cat("Muestra bootstrap número ",b,"\n")
  indiboot= sample(indin,rep=TRUE) 
  indiOOB= setdiff(indin,indiboot)
  datboot = datos2[indiboot,]
  modelo.boot=naiveBayes(type ~ ., data= datboot)
  predi<- predict(modelo.boot,datos)
  errorboot[b]= mean(predi!=Glass[,10])
  prediOOB= predict(modelo.boot,datos[indiOOB,])         
  errorOOB[b]= mean(prediOOB!=Glass[indiOOB,10])
}
errorB<- mean(errorboot)
errorOOB<- mean(errorOOB)
error632B<-0.368*erroremp+0.632*errorOOB  

matrizL<- matrix(NA,n,n)
for (i in 1:n)
  for (j in 1:n)
  {
    matrizL[i,j]<- (Glass[i,10]!=predi[j]) 
  }
Noinf<- mean(matrizL)
tsr<- (errorOOB-erroremp)/(Noinf-erroremp)
w<- 0.632/(1-0.368*tsr)
error632masB<-min((1-w)*erroremp+w*errorOOB,Noinf)

tabla2 = data.frame("Naive-Bayes" = c(erroremp,errorB,errorOOB,error632B,error632masB))
rownames(tabla2) <- c("Tasa Error Empírico","Tasa Error Bootstrap","Tasa Error OOB",
                "Tasa Error 0.632Boot","Tasa Error 0.632+Boot")
cbind(tabla,tabla2)
