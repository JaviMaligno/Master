library(xtable)
library(purrr)
library(ggplot2)
library(tidyr)

xs = read.csv("datos.csv")
xs = xs[,-1]


x1 = read.csv("datos1.csv")
x1 = x1[,-1]
x2 = read.csv("datos2.csv")
x2 = x2[,-1]
x3 = read.csv("datos3.csv")
x3 = x3[,-1]
x4 = read.csv("datos4.csv")
x4 = x4[,-1]
x5 = read.csv("datos5.csv")
x5 = x5[,-1]
x6 = read.csv("datos6.csv")
x6 = x6[,-1]
x7 = read.csv("datos7.csv")
x7 = x7[,-1]
x8 = read.csv("datos8.csv")
x8 = x8[,-1]
x9 = c(500,
        5,
        62317.0,
        541.2,
        545.6,
        0.0,
        49.72127842903137,
        5.776909828186035,
        0.0,
        0.6)


print(xtable(df), include.rownames = F)

df = data.frame(rbind(x1,xs,x2,x3,x4,x5,x6,x7,x8))

colnames(df) <- c("Nodos","I","Var","S.GH", "S.PE","S.Match","T.GH","T.PE","T.Match","Exp.B.B")
df

df0 = gather(cbind(df[,c(1,3)],Teoría = bino(df[,1])), key = "Leyenda", value = "Variables",-Nodos)
pdf("plot1.pdf", height=5, width=7)
ggplot(df0,aes(x=Nodos,y=Variables,color=Leyenda)) + geom_point() + geom_line()
dev.off()

df1 = gather(df[,c(1,4,5)] , key = "Método", value = "Iteraciones",-Nodos)
pdf("plot2.pdf", height=5, width=7)
ggplot(df1,aes(x=Nodos,y=Iteraciones,color=Método)) + geom_point() + geom_line()
dev.off()

df2 = gather(df[,c(1,7,8)] , key = "Método", value = "Tiempo",-Nodos)
pdf("plot3.pdf", height=5, width=7)
ggplot(df2,aes(x=Nodos,y=Tiempo,color=Método)) + geom_point()+geom_line()
dev.off()


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



df = data.frame(N=c(14,16,18,20,22,24,26,28,30),
                "Media Total" = c(9.4,15.8,18.8,22.6,30,39.2,40, 53.8,63), 
                "Media No Sop" = c(4.6,10.2,10.4,14.8,19.2,27,29.4,38.6,48.4),
                "Media Porcentaje" = c(45.39,63.72,55.9,65.31,
                                       63.55,68.81,72.81,73.05,76.91))

df2 = gather(df, key = "Leyenda", value = "Valor",-N,-Media.Porcentaje)

pdf("plot10.pdf", height=5, width=7)
ggplot(df2,aes(x=N,y=Valor,color = Leyenda)) + geom_point() + geom_line() 
dev.off()

pdf("plot11.pdf", height=5, width=7)
ggplot(df2,aes(x=N,y=Media.Porcentaje)) + geom_point(color="blue") +
  geom_line(color ="red")+ geom_hline(yintercept=100,lty = 2) 
dev.off()

16 = 5 9/25 6/14 6/21 8/17
20 = 6/16 6/14 11/38 7/28 9/23
22 = 12/37 10/31 10/24 9/29 13/29
24 = 16/56 13/36 15/46 9/29 8/29
26 = 13/48 11/44 8/25 11/36 10/47
28 = 14/48 17/62 18/62 13/49 14/62