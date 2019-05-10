library(xtable)
library(purrr)

xs = read.csv("datos.csv")
xs = xs[,-1]

print(xtable(xs), include.rownames = F)

library(ggplot2)
library(tidyr)
N = c(10,16,18,24,30,40,60,70,80,100,120,150,170,200,220,250,270,290,300,320,350,370,400)

Var=c(21.40,59.40,76.00,139.00 ,225.00,389.00 ,886.20,1193.00,1587.00,2469.80,
      3551.80,5551.20,7164.60,10034.00,12033.00,15529.20,18139.40,
      20955.60,22416.80,25466.80,30601.80,34050.60,39902.80)

bino = function(x){
  a = x*(x-1)/4
  return(a)
}
c = map(N,bino)
s = c()
for (i in 1:(length(c))){
  s = c(s,c[[i]])
}

frame = data.frame(Nodos = N, Aristas = Var, Teoría = s)
frame2 = gather(frame, key = "Tipo", value = "Aristas",-Nodos)


pdf("plot1.pdf", height=5, width=7)
ggplot(frame2,aes(x=Nodos,y=Aristas, color=Tipo )) + geom_point(color = "black") + 
  geom_line()
dev.off()

sgh = c(4.90,8.90,11.50,20.00,19.00,36.50,56.40,67.00,77.40,102.80,109.20,153.40,
        167.80,201.80, 210.40,253.80,286.80,290.00,298.00,345.40,385.80,387.20,426.60)
spe = c(5.60,13.90,16, 25.52, 6.50, 43.50,62.00,72.60,83.80 ,108.80,113.60,160.60,174.40, 
        204.00,219.40,258.00,291.40,300.20,306.80,398.00,390.80,399.00,432.00)

df1 = data.frame(Nodos = N, S.GH = sgh, S.PE = spe)
df2 = gather(df1, key = "Método", value = "Iteraciones",-Nodos)

pdf("plot2.pdf", height=5, width=7)
ggplot(df2,aes(x=Nodos,y=Iteraciones,color=Método)) + geom_point() + geom_line()
dev.off()

tgh = c(0.01,0.01,0.02,0.02,0.03,0.04,0.10,0.12,0.16,0.24,0.33,0.56,0.83,1.31,
        1.50,2.65,4.09,4.67,4.75,10.20,10.84,11.00,17.09)
tpe = c(0.02,0.03,0.03,0.03,0.04,0.05,0.11,0.12,0.15,0.22,0.32,0.45,0.60,0.80,
        0.93,1.28,1.55,1.73,1.78,2.12,2.51,2.73,3.44)


df3 = data.frame(Nodos = N, T.GH = tgh, T.PE = tpe)
df4 = gather(df3, key = "Método", value = "Tiempo",-Nodos)

pdf("plot3.pdf", height=5, width=7)
ggplot(df4,aes(x=Nodos,y=Tiempo,color=Método)) + geom_point() + geom_line()
dev.off()





xs = read.csv("datos.csv")
xs = xs[,-1]
xs






