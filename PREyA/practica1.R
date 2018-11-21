##########################################################
#
#         PROCESOS ESTOCÁSTICOS
#
#        MÁSTER DE MATEMÁTICAS
#
#             UNIVERSIDAD DE SEVILLA
##########################################################
##########################################################
#
#     TEMA 2: CADENAS DE MARKOV DISCRETAS
#     HOJA DE PRÁCTICA NÚMERO 1
#
##########################################################
#install.packages("markovchain")
library(markovchain) 
#install.packages("expm")
library(expm)


##########################################################
#
# PROBLEMA NÚMERO 2. EL ASCENSOR.
#
########################################################
#
# Construir la matriz de transición.
#
########################################################
casa <- c("bajo", "primero", "segundo") 
byRow <- TRUE 
casamatrix <- matrix(data = c(0,0.5,0.5,0.75,0,0.25,1,0,0),byrow = byRow, 
                     nrow = 3,dimnames=list(casa,casa)) 
casamatrix

########################################################
#
# Hay que definirlo como objeto markoviano para hacer operaciones
#
#

cmcasa<-new("markovchain",states=casa,byrow=byRow,transitionMatrix=casamatrix,name="ascensor")
mclist<-new("markovchainList",markovchains=list(cmcasa),name="list")
plot(cmcasa) #dibuja el grafo
summary(cmcasa)



########################################################
#
# Si parte del bajo, ¿Cuál es la probabilidad de que esté
# en cada uno de los pisos después de 10 mov.?
#
#########################################################

inicial<-c(1,0,0)
v2dias<-inicial * (casamatrix %^% 10)
v2dias

##################################################################
#
#   PROBLEMA NÚMERO 3. EL MOVIMIENTO ALEATORIO DEL RATÓN
#
#################################################################
#
# Construir la matriz de transición.
#
########################################################
raton <- c("1","2","3","4","5","6","7","8","9") 
byRow <- TRUE 
ratonmatrix <- matrix(data = c(0,0.5,0,0.5,0,0,0,0,0,1/3,0,1/3,0,1/3,0,0,
                               0,0,0,0.5,0,0,0,0.5,0,0,0,1/3,0,0,0,1/3,0,
                               1/3,0,0,0,.25,0,.25,0,.25,0,.25,0,0,0,1/3,
                               0,1/3,0,0,0,1/3,0,0,0,0.5,0,0,0,.5,0,0,0,0,
                               0,1/3,0,1/3,0,1/3,0,0,0,0,0,.5,0,.5,0),
                      byrow = byRow, nrow = 9,dimnames=list(raton,raton)) 
ratonmatrix

########################################################
#
# Hay que definirlo como objeto markoviano para hacer operaciones
#

cmraton<-new("markovchain",states=raton,byrow=byRow,transitionMatrix=ratonmatrix,name="laberinto")
mclist<-new("markovchainList",markovchains=list(cmraton),name="list")
plot(cmraton) #dibuja el grafo

########################################################
#
# Si se suelta al azar, dar las probabilidades de cada celda según 
# el número de movimientos hasta 10 como máximo.
#
#########################################################
celda=vector()
casos=c(1,2,3,4,5,6,7,8,9)
inicial<-c(1/9,1/9,1/9,1/9,1/9,1/9,1/9,1/9,1/9)
tabla=cbind(casos,inicial)
for (i in 1:10){
celda<-inicial * (ratonmatrix %^% i)
tabla=cbind(tabla,t(celda))
}
tabla

#
##################################################################
#
#   PROBLEMA NÚMERO 6. REEMPLAZAMIENTOS DE PIEZAS
#
#################################################################
#
# Construir la matriz de transición.
#
########################################################
pieza <- c("1","2","3","4") 
p=0.5
q=1-p
byRow <- TRUE 
piezamatrix <- matrix(data = c(p,q,0,0,0,0,q,1-q,0,1,0,0,p,q,0,0),byrow = byRow, 
                      nrow = 4,dimnames=list(pieza,pieza)) 
piezamatrix

########################################################
#
# Hay que definirlo como objeto markoviano para hacer operaciones
#
#

cmpieza<-new("markovchain",states=pieza,byrow=byRow,transitionMatrix=piezamatrix,name="fiabilidad")
mclist<-new("markovchainList",markovchains=list(cmpieza),name="list")
plot(cmpieza) #dibuja el grafo

########################################################
#
# Si incialmente hay dos piezas funcionando, ¿cuál es la 
# probabilidad de que sigan después de 10 instantes.
#
#########################################################
inicial=c(1,0,0,0)
prob=inicial* (piezamatrix %^% 10)
prob


#################################################################
#
#  PROBLEMA NUMERO 5. CADENA DE MARKOV DE UN PARTIDO DE TENIS
#
#################################################################

####################################################
#
# Ejemplo para construir cómodamente una matriz de transición
#
###################################################

mat=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14",
      "15","16","17","18","19","20") # Se definen los nombres de los estados
n=length(mat)  # da el nÃºmero de estados
#mm=read.table("/Users/Josemari-Ferpon/Desktop/datos/matriz.txt",header=F) 
#lee de un fichero de datos la matriz
mm=read.table("C:/Datos/matrix.txt",header=F)


vmatriz=vector(length=n*n) #Define un vector con longitud la dimensión de la matriz al cuadrado
k=0
for (i in 1:n){
  for (j in 1:n){
    k=k+1
    vmatriz[k]=mm[i,j] # este bucle es para convertir la matriz de datos en vector
  }
}
byRow <- TRUE 
vmatrix <- matrix(data = vmatriz,byrow = byRow, nrow = 20, 
                  dimnames=list(mat,mat)) #convierte el vector en matriz
vmatrix

cmmatrix<-new("markovchain",states=mat,byrow=byRow,transitionMatrix=vmatrix,name="prueba")

plot(cmmatrix) #dibuja el grafo
summary(cmmatrix)

##########################################################
#
# PROBABILIDADES DE PRIMERA PASADA
#
#

firstPassage(cmmatrix, "1",20)
