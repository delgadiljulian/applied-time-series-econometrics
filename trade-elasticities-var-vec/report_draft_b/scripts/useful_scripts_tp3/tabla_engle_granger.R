# Tabla de Engle y Granger para cointegración
# Versión 21/08/2022      Encoding: UTF-8
# q_series = Cantidad de series que cointegran (2 ó 3)

Tabla_EG=function(q_series){
# c1=c(2,2,2,3,3,3)
c2=c(50,100,200,50,100,200)
c3=c(-4.32,-4.07,-4,-4.84,-4.45,-4.35)
c4=c(-3.67,-3.37,-3.37,-4.11,-3.93,-3.78)
c5=c(3.28,-3.03,-3.02,-3.73,-3.59,-3.47)
Tabla=as.matrix(cbind(c2,c3,c4,c5))
colnames(Tabla)=c("N.Obs.","1 %","5 %", "10 %")
print(paste("Valores críticos Engle-Granger para ",q_series, " series"))
if(q_series==2){print(Tabla[1:3,])}
if(q_series==3){print(Tabla[4:6,])}
}
