##################################################
# MASTER UNIVERSITARIO EN MATEMATICAS            #
# Universidad de Sevilla                         # 
# PROCESOS ESTOCÁSTIOCOS Y APLICACIONES          #
# Rafael González López                          #
##################################################
#install.packages("markovchain")
library(markovchain) 
#install.packages("expm")
library(expm)

# Veamos el problema para distintos valores de p
 
matrizp = function(q) {
  estados <- c("1", "2", "3","4")
  cmatrix <- matrix(data = c(1-q,q,0,0,0,0,q,1-q,0,1,0,0,1-q,0,0,q),
                       byrow = T, nrow = 4,dimnames=list(estados,estados)) 
  maquina<-new("markovchain",states=estados,byrow=T,transitionMatrix=cmatrix,name="Maquinaria")
  mclist<-new("markovchainList",markovchains=list(maquina),name="list")
  return(maquina)
}

distest = function(q) {
  a = (1-q)^2/q^2 + q^2 + 2
  b = 1/sqrt(a)
  c = c((1-q)/q,1,q,1)
  return(b*c)
}

plot(matrizp(1/2))
summary(matrizp(1/2))

plot(matrizp(1/4))
summary(matrizp(1/4))

plot(matrizp(3/4))
summary(matrizp(3/4))

distest(1/2)
distest(1/2)*(matrizp(1/2))
