#__________________________________________________________________________________
###                                                                             ###
###                     Modelado y Predicci�n Estad�stica.                      ### 
###                M�ster en Matem�ticas. Universidad de Sevilla                ###
###                           Juan Manuel Mu�oz Pichardo                        ###
###                                                                             ###
#__________________________________________________________________________________



#__________________________________________________________________________________
#########                SCRIPT  Ridge01.R                                #########
#########                Regresi�n Ridge                                  #########
#########                Orden: lm.ridge                                  #########
#########                Datos: longley (library MASS)                    #########
#__________________________________________________________________________________

######### CARGA DEL PAQUETE MASS (PARA DATOS)
library(MASS)

######### Algunas opciones de los c�lculos
options(digits = 4) # n�mero de decimales en las salidas
options(columns = 40) # numero de columnas en las salidas

######### LECTURA DE DATOS
data(longley)
dim(longley)
names(longley)

# Un dataframe con 7 variables econ�micas observadas anualmente desde 1947 a 1962
# 
# GNP.deflator : GNP implicit price deflator (1954=100)
# GNP          : Gross National Product.
# Unemployed   : number of unemployed.
# Armed.Forces : number of people in the armed forces.
# Population   : 'noninstitutionalized' population ??? 14 years of age.
# Year         : the year (time).
# Employed     : number of people employed.

names(longley)[1] <- "y"
longley[1:3, ]
summary(longley)

######### AJUSTE POR M�NIMOS CUADRADOS ORDINARIOS
### Ajuste por MCO de variable y frente al resto
longley.mco <- lm(y ~ ., longley)
summary(longley.mco)

# Se puede comprobar fuerte multicolinealidad, aparente en los reducidos valores
# de los t-estad�sticos y elevada R2, as� como en la matriz de correlaciones binarias.
cor(longley)
det(vcov(longley.mco))

######### AJUSTE POR REGRESI�N RIDGE 
#### Ajuste por Regresi�n Ridge con valores de lambda variando 
# entre 0 y 0.1 (0.001, 0,002,...,0.100). 
# Para k=0 coincide con MCO 

longley.rr <- lm.ridge(y ~ ., longley,lambda = seq(0, 0.1, 0.001))
summary(longley.rr)
names(longley.rr)

# Matriz de coeficientes estimados (para cada valor de lambda por filas)
# La primera columna almacena el valor de lambda
coef(longley.rr)[1:3, ]
coef(longley.rr)[99:101, ]

# Media de la variable objetivo
longley.rr$ym

# Media de las variables predictoras
longley.rr$xm

# Vector de las "escalas" utilizadas en la estadarizaci�n de las variables
# predictoras (sus desviaciones t�picas)
longley.rr$scales

# Vector de valores de lambda
longley.rr$lambda[1:3]

# Vector de Valores de errores por validaci�n cruzada generalizada (GCV values)
longley.rr$GCV[1:6]
plot(longley.rr$lambda,longley.rr$GCV,col=2)
# Valor del criterio HKM propuesto por Hoerl et al. (1975)  
longley.rr$kHKB
# Valor del criterio HKM propuesto por Lawless and Wang.(1976) 
longley.rr$kLW


# La funci�n "select" aplicada al objeto que proporciona lm.ridge devuelve
# los valores �ptimos de tres de los siguientes criterios:
#   - Criterio HKM propuesto por Hoerl et al. (1975) y tiene una justificaci�n bayesiana. 
#   - Criterio LW propuesto en Lawless and Wang.(1976)
#   - Criterio de validaci�n cruzada generalizada (GCV).
select(longley.rr)


# Selecci�n del lambda �ptimo de acuerdo al criterio GCV y su regresi�n ridge
nGCV <- which.min(longley.rr$GCV)  # n�mero para el cual se alcanza el m�nimo
nGCV
lGCV <- longley.rr$lambda[nGCV]  # valor de lambda para el cual se alcanza el m�nimo
lGCV

lm.ridge(y ~ ., longley, lambda = lGCV)
summary(lm.ridge(y ~ ., longley, lambda = lGCV))
names(lm.ridge(y ~ ., longley, lambda = lGCV))

coef(lm.ridge(y ~ ., longley, lambda = lGCV))
lm.ridge(y ~ ., longley, lambda = lGCV)$scales
lm.ridge(y ~ ., longley, lambda = lGCV)$lambda
lm.ridge(y ~ ., longley, lambda = lGCV)$ym
lm.ridge(y ~ ., longley, lambda = lGCV)$xm
lm.ridge(y ~ ., longley, lambda = lGCV)$GCV


# Gr�ficos de la traza ridge y las medidas del ECM en funci�n de lambda


matplot(longley.rr$lambda, t(longley.rr$coef), type = "l", xlab = expression(lambda),
         ylab = expression(beta[i]))
abline(v = lGCV)
mtext(expression(lambda[GCV]), side = 3, at = lGCV)
title(main = "Trazas ridge")

plot(longley.rr$lambda, longley.rr$GCV, type = "l", xlab = expression(lambda),
       ylab = "GCV", main = "Criterio GCV",col=2)
abline(v = lGCV)
mtext(expression(lambda[GCV]), side = 3, at = lGCV)
abline(v = longley.rr$kLW)
mtext(expression(lambda[LW]), side = 3, at = longley.rr$kLW)


#__________________________________________________________________________________
#
#         CARGA DEL CONJUNTO DE DATOS prostate.txt
#
# Datos de un estudio que analiza la correlaci�n entre el nivel de ant�geno prost�tico espec�fico
# y una serie de medidas cl�nicas en los hombres que estaban a punto de recibir una 
# prostatectom�a radical.
# 
#            Variables: 
# lcavol          log(volumen del cancer)
# lweight         log(peso de la pr�stata)
# age             Edad
# lbph            log(cantidad de hiperplasia prost�tica benigna)
# svi             invasi�n del ves�culo seminal (Si - 1; No - 0)
# lcp             log(penetraci�n capsular)
# gleason         �ndice Gleason 
# pgg45           % �ndice Gleason 4 o 5
# lpsa            log(ant�geno prost�tico espec�fico)
#__________________________________________________________________________________

datos = read.csv("prostate.txt",sep=";",header=TRUE)  # read csv file 
datos
dim(datos)
names(datos)

table(datos$svi,datos$gleason)
svifact = as.factor(datos$svi)
gleasonfact = as.factor(datos$gleason)
table(svifact,gleasonfact)


######### AJUSTE POR M�NIMOS CUADRADOS ORDINARIOS
### Ajuste por MCO de variable y frente al resto
lm.fit <- lm(lpsa~lcavol+lweight+age+lbph+svifact+lcp+gleasonfact+pgg45,data=datos)
summary(lm.fit)

######### AJUSTE POR REGRESI�N RIDGE 
#### Ajuste por Regresi�n Ridge con valores de lambda variando 
# entre 0 y 0.1 (0.001, 0,002,...,0.100). 
# Para k=0 coincide con MCO 

datos.rr <- lm.ridge(lpsa~lcavol+lweight+age+lbph+svifact+lcp+gleasonfact+pgg45, data=datos,
                     lambda = seq(0, 20, 0.01))
summary(datos.rr)
names(datos.rr)

# Matriz de coeficientes estimados (para cada valor de lambda por filas)
# La primera columna almacena el valor de lambda
coef(datos.rr)[1:3, ]
coef(datos.rr)[99:101, ]

# Media de la variable objetivo
datos.rr$ym

# Media de las variables predictoras
datos.rr$xm

# Vector de las "escalas" utilizadas en la estadarizaci�n de las variables
# predictoras (sus desviaciones t�picas)
datos.rr$scales

# Vector de valores de lambda
datos.rr$lambda[1:3]

# Vector de Valores de errores por validaci�n cruzada generalizada (GCV values)
datos.rr$GCV[1:15]


# Valor del criterio HKM propuesto por Hoerl et al. (1975)  
datos.rr$kHKB
# Valor del criterio HKM propuesto por Lawless and Wang.(1976) 
datos.rr$kLW


# La funci�n "select" aplicada al objeto que proporciona lm.ridge devuelve
# los valores �ptimos de tres de los siguientes criterios:
#   - Criterio HKM propuesto por Hoerl et al. (1975) y tiene una justificaci�n bayesiana. 
#   - Criterio LW propuesto en Lawless and Wang.(1976)
#   - Criterio de validaci�n cruzada generalizada (GCV).
select(datos.rr)


# Selecci�n del lambda �ptimo de acuerdo al criterio GCV y su regresi�n ridge
nGCV <- which.min(datos.rr$GCV)  # n�mero para el cual se alcanza el m�nimo
nGCV
lGCV <- datos.rr$lambda[nGCV]  # valor de lambda para el cual se alcanza el m�nimo
lGCV

lm.ridge(lpsa~lcavol+lweight+age+lbph+svifact+lcp+gleasonfact+pgg45, datos, lambda = lGCV)
summary(lm.ridge(lpsa~lcavol+lweight+age+lbph+svifact+lcp+gleasonfact+pgg45, datos, lambda = lGCV))
names(lm.ridge(lpsa~lcavol+lweight+age+lbph+svifact+lcp+gleasonfact+pgg45, datos, lambda = lGCV))

coef(lm.ridge(lpsa~lcavol+lweight+age+lbph+svifact+lcp+gleasonfact+pgg45, data=datos, lambda = lGCV))
coef(lm.fit)
lm.ridge(lpsa~lcavol+lweight+age+lbph+svifact+lcp+gleasonfact+pgg45, data=datos, lambda = lGCV)$scales
lm.ridge(lpsa~lcavol+lweight+age+lbph+svifact+lcp+gleasonfact+pgg45, data=datos, lambda = lGCV)$lambda
lm.ridge(lpsa~lcavol+lweight+age+lbph+svifact+lcp+gleasonfact+pgg45, data=datos, lambda = lGCV)$ym
lm.ridge(lpsa~lcavol+lweight+age+lbph+svifact+lcp+gleasonfact+pgg45, data=datos, lambda = lGCV)$xm
lm.ridge(lpsa~lcavol+lweight+age+lbph+svifact+lcp+gleasonfact+pgg45, data=datos, lambda = lGCV)$GCV


# Gr�ficos de la traza ridge y las medidas del ECM en funci�n de lambda


matplot(datos.rr$lambda, t(datos.rr$coef), type = "l", xlab = expression(lambda),
        ylab = expression(beta[i]))
abline(v = lGCV)
mtext(expression(lambda[GCV]), side = 3, at = lGCV)
title(main = "Trazas ridge")

plot(datos.rr$lambda, datos.rr$GCV, type = "l", xlab = expression(lambda),
     ylab = "GCV", main = "Criterio GCV",col=2)
abline(v = lGCV)
mtext(expression(lambda[GCV]), side = 3, at = lGCV)
abline(v = datos.rr$kLW)
mtext(expression(lambda[LW]), side = 3, at = datos.rr$kLW)

#__________________________________________________________________________________

