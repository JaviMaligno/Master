##########################################################
#EJEMPLO 1. RNAS con nnet. Ejemplo de clasificaci�n
##########################################################
library(nnet)
#1. Leer los datos y dividirlos en entrenamiento/test
#####################################################
spam<- read.table("spam.txt",header=TRUE)
# crl.tot longitud total de las palabras en may�sculas
# dollar  frecuencia de aparici�n del s�mbolo "$" 
# bang frecuencia de aparici�n del s�mbolo "!"
# money frecuencia de aparici�n de la palabra"money"
# n000 frecuencia de aparici�n de la cadena `000'
# make frecuencia de aparici�n de la palabra "make"
# yesno variable respuesta,  'n' no spam, 'y' spam
set.seed(12345)
summary(spam)  
#Matrices x e y
x<-spam[,1:6]
y<- spam$yesno=="y"    #TRUE (1) si spam=yes
#Divisi�n en conjunto de entrenamiento/ conjunto test
n<- nrow(spam)
nent<- ceiling(0.75*n)
ntest<- n-nent
indin<- 1:n
indient<- sample(indin,nent)
inditest<- setdiff(indin,indient)
xent<- x[indient,]; xtest<- x[inditest,]
yent<- y[indient]; ytest<- y[inditest]


#2. Es recomendable transformar las variables predictoras
#####################################################

#Una opci�n es tipificar las variables predictoras
#las observaciones test se "tipifican" con las medias
#y desviaciones t�picas de los datos de entrenamiento
zent<- scale(xent,center=TRUE,scale=TRUE)
str(zent)
medias<- attr(zent,"scaled:center")
dt<- attr(zent,"scaled:scale")
ztest<- scale(xtest,medias,dt)
apply(zent,2,mean)
apply(ztest,2,mean)
apply(zent,2,sd)
apply(ztest,2,sd)

#Otra posible transformaci�n de datos: pasar el rango a 
#[0,1] si se usa la f. log�stica o bien a [-1,1] 
#si se usa tanh en los nodos ocultos
#maxabs<- function(x)
#{  max(abs(x))  }
#maximos<- apply(xent,2,maxabs)
#p<- ncol(xent)  #N�mero de predictoras
#for (j in 1:p)
# { xent[,j]<- xent[,j]/maximos[j]
#   xtest[,j]<- xtest[,j]/maximos[j]   }
#summary(xent)
#summary(xtest)


#3. Construir un perc. multicapas con 6 nodos ocultos
#####################################################
red<- nnet(zent,yent,size=6,maxit=1000,entropy=TRUE)
#entropy=TRUE, recomendado para clasificaci�n
summary(red)
predict(red)[1:10,]


#Si se quieren obtener decisiones y/n, se deben comparar 
#las prob. estimadas con un punto de corte:
#Si la prob. estimada p es >=u, decisi�n= spam
predclase<- function(p,u)  
{ ifelse(p>=u,"y","n")
}

#4. Configurar el par de par�metros (Tama�o capa oculta, Penalizaci�n) con VC
#############################################################################
#install.packages("e1071)
library(e1071)
  #La funci�n tune selecciona H seg�n el criterio error de clasificaci�n
#size: tama�o de la capa oculta
#decay: par�metro de regularizaci�n L2
redtune<- tune(nnet,zent,as.numeric(yent),entropy=TRUE,
 ranges=list(size=5:7,decay=c(0,0.1)),maxit=100)
#Es recomendable una b�squeda m�s extensa, por ejemplo:
#size=1:10,decay=c(0,0.05,0.1)
summary(redtune)
plot(redtune)
#Red con la mejor configuraci�n
redselvc<- redtune$best.model 

#5. Evaluar el rendimiento
##########################
confutestvc<-table(spam$yesno[inditest], 
                   predclase(predict(redselvc, ztest),0.5))
colnames(confutestvc)<- c("n","y")
print("Test para la mejor red seg�n validaci�n cruzada:")
confutestvc
cat(" Tasa de acierto test= \t",
    100*(confutestvc[1,1]+confutestvc[2,2])/ntest,"\n",
    "Sensitividad test= \t",
    100*confutestvc[2,2]/sum(confutestvc[2,]),"\n",
    "Especificidad test=  \t",
     100*confutestvc[1,1]/sum(confutestvc[1,]) ,"\n")

#En el conjunto de entrenamiento:
confuentvc<-table(spam$yesno[indient], 
                  predclase(predict(redselvc),0.5))
colnames(confuentvc)<- c("n","y")

confuentvc
cat(" Tasa de acierto emp�rica= \t",
    100*(confuentvc[1,1]+confuentvc[2,2])/nent,"\n",
    "Sensitividad (entr.)= \t\t",
    100*confuentvc[2,2]/sum(confuentvc[2,]),"\n",
    "Especificidad (entr.)= \t",
    100*confuentvc[1,1]/sum(confuentvc[1,]) ,"\n")


#Curva COR y AUC
library(ROCR)
prediobj<-prediction(predict(redselvc, ztest), 
                     spam$yesno[inditest])
plot(performance(prediobj, "tpr","fpr"),main="CURVA COR TEST")
lines(c(0,1:100)/100,c(0,1:100)/100,col="blue",lty=2)
auc<- as.numeric(performance(prediobj,"auc")@y.values)
cat("�rea bajo la curva COR Test= ",auc,"\n")
legend("bottomright",legend=paste("AUC=",round(auc,3)))
cat("�rea bajo la curva COR Test= ",auc,"\n")


#6. Divisi�n de los datos en entrenamiento, validaci�n y test
#############################################################
n<- nrow(spam)
indices<- 1:n
inditest<- sample(indices,trunc(n*0.25)+1)
indivali<- sample(setdiff(indices,inditest),trunc(n*0.25)+1)
indient<-setdiff(indices,union(inditest,indivali))
xent<- x[indient,]; xvali<- x[indivali,]; xtest<- x[inditest,]
yent<- y[indient]; yvali<- y[indivali]; ytest<- y[inditest]

#Transformaci�n de las x
medias<- apply(xent,2,mean)
dt<- apply(xent,2,sd)
xent<- scale(xent,medias,dt)
xvali<- scale(xvali,medias,dt)
xtest<- scale(xtest,medias,dt)
apply(xent,2,mean); apply(xvali,2,mean); apply(xtest,2,mean)
apply(xent,2,sd); apply(xvali,2,sd); apply(xtest,2,sd)

#Ajuste del valor de H y landa mediante el conjunto de validaci�n
vH<- 5:10
nvH<- length(vH)
vdecay<- c(0,0.05,0.1)
ndecay<- length(vdecay)
tasaerr<- matrix(NA,nvH,ndecay)  #Tasa de error de clasificaci�n
rownames(tasaerr)<-  vH
colnames(tasaerr)<-  vdecay


for (i in 1:nvH)
 for (j in 1:ndecay)
{
 cat("H =",vH[i],"\n")
 cat("landa =",vdecay[j],"\n")
 redH<- nnet(xent,yent,size=vH[i],decay=vdecay[j],entropy=T,maxit=100)
 predivali<- predict(redH, xvali)
 tasaerr[i,j]<-mean(spam$yesno[indivali] !=predclase(predict(redH, xvali),0.5))

}

tasaerr
min(tasaerr)
posic<- which(tasaerr==min(tasaerr),arr.ind=TRUE) #Determinar Hopt y landaopt
(Hopt<- posic[1,1])
(landaopt<- posic[1,2])

#Tambi�n se puede hacer con tune

#Una vez elegido H, se puede entrenar la red 
#sobre la uni�n de ent y val:
#Tambi�n se podr�a tipificar la uni�n de ent. y valid.
#y con sus medias y desv. t�picas se tipifica test
xent<- rbind(xent,xvali)
yent<- c(yent,yvali)
redval<- nnet(xent,yent,size=Hopt,decay=landaopt,
              entropy=T,maxit=1000)
#Resultados test con redmej:
confutestval<-table(spam$yesno[inditest], 
                    predict(redval, xtest)>0.5)
colnames(confutestval)<- c("n","y")                 
print("Test para la mejor red seg�n validaci�n:")
confutestval
cat("Tasa de acierto test= ",
    100*(confutestval[1,1]+confutestval[2,2])/sum(confutestval),"\n")
cat("Sensitividad test= ",
    100*confutestval[2,2]/sum(confutestval[2,]),"\n")
cat("Especificidad test= ",
    100*confutestval[1,1]/sum(confutestval[1,]) ,"\n")

library(ROCR)
prediobj<-prediction(predict(redval, xtest), spam$yesno[inditest])
plot(performance(prediobj, "tpr","fpr"),main="CURVA COR TEST")
lines(c(0,1:100)/100,c(0,1:100)/100,col="blue",lty=2)
auc<- as.numeric(performance(prediobj,"auc")@y.values)
legend("bottomright",legend=paste("AUC=",round(auc,3)))
cat("�rea bajo la curva COR Test= ",auc,"\n")

