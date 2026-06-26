#***********************************************
#     Vectores Autorregresivos VAR
#***********************************************
install.packages("vars")
library(vars)

# Importar lutkepohl2 archivo de Excel
lu=data.frame(lutkepohl2)
level.lu=data.frame(matrix(c(lu$ln_inc,lu$ln_consump,lu$ln_inv), ncol=3))
names(level.lu)=c("ln_inc","ln_consump","ln_inv")
ts.level.lu=ts(level.lu,start=c(1960,1),frequency=4)
windows()
ts.plot(ts.level.lu)
# Vemos que las series tienen tendencia e intercepto
ts.plot(diff(ts.level.lu))
dev.off()
# Queremos encontrar las relaciones entre el Consumo
# y el Ingreso. También utilizaremos la serie de Inversión
# como exógena
# Al ser las 3 series I(1), trabajaremos
# con sus diferencias logarítmicas que serán I(0)
# DEBERÍAMOS COMPROBAR LO ANTERIOR
Test=Test.ADF.Ver.3(ts.level.lu[,1],3,"AIC")
Test=Test.ADF.Ver.3(diff(ts.level.lu[,1]),2,"AIC")

Test=Test.ADF.Ver.3(ts.level.lu[,2],3,"AIC")
Test=Test.ADF.Ver.3(diff(ts.level.lu[,2]),2,"AIC")

Test=Test.ADF.Ver.3(ts.level.lu[,3],3,"AIC")
Test=Test.ADF.Ver.3(diff(ts.level.lu[,3]),2,"AIC")
# OK. Son las 3 I(1)

# Como tenemos que trabajar con las diferencias. generamos
# un data.frame reducido con las series que vamos a usar
# eliminamos los NAs guiandonos con la serie de dln_inc 
# diferencia del logaritmo del PBI, o sea tasa de crecimiento
dif.lu=subset(lu, !is.na(dln_inc))
dif.lu=data.frame(matrix(c(dif.lu$dln_inc,dif.lu$dln_consump,dif.lu$dln_inv), ncol=3))
names(dif.lu)=c("dln_inc","dln_consump","dln_inv")
# Convertimos en un objeto "series de tiempo"
ts.dif.lu=ts(dif.lu,start=c(1960,2),frequency=4)
windows()
plot.ts(ts.dif.lu)
dev.off()

#***********************************************
#*      DISEÑO DEL MODELO
#***********************************************     
# Nuestro modelo será un VAR con las series
# ingreso y consumo en diferencias logarítmicas
# Estimamos un VAR con nro. de lags elegidos con Akaike
# Utilizamos el comando VAR() del paquete "vars"
var1=VAR(ts.dif.lu[,1:2],type="const", lag.max=8, ic="AIC")
summary(var1)    # Eligió 3 lags

# También se puede correr el argumento VARselect que 
# selecciona los lags según un criterio 
ic=VARselect(ts.dif.lu[,1:2],lag.max=8, type="const")
ic$selection

# Cómo es la secuencia de cálculos de VARselect
VARselect(ts.dif.lu[,1:2],lag.max=8, type="const")


#***********************************************
#     Estimación del  VAR
#***********************************************
# Estimamos un VAR con nro. de lags elegidos con Akaike
# o sea que fijamos la cantidad de lags con p=3
# que fue lo indicado por VARselect
var1=VAR(ts.dif.lu[,1:2],type="const", p=3)
summary(var1)

# Se puede agregar una exógena
var2=VAR(ts.dif.lu[,1:2],type="const", p=3,exogen=ts.dif.lu[,3])
summary(var2)

# Se puede anular un lag intermedio completo. El VAR puede
# seguir estimándose por Mínimos Cuadrados
# Hay que usar una herramienta de restricción que luego se
# usará con los VAR Estructurales
# Ejemplo: Anular el lag 2
# Ver la matriz de coeficientes
Acoef(var1)
coef_R <- matrix(c(1,1,0,0,1,1,1,
                     1,1,0,0,1,1,1), nrow=2, byrow=TRUE) 
coef_R
var1_r=restrict(var1, method = "man", resmat = coef_R)
summary(var1_r)

#***********************************************
#              Diagnósticos
#***********************************************
# Correlación serial. Es muy importante porque si no
# se verifica ausencia de correlación serial se deberán
# agregar lags al VAR
# Se utiliza un test Q para el caso multivariado
serial.test(var1,type = "PT.asymptotic",lags.pt=8)
st=serial.test(var1,type = "PT.asymptotic",lags.pt=8)
windows()
plot(st, names="dln_inc")
plot(st, names="dln_consump")
dev.off()

# Mejor cargar vcorr_res que calcula los estadísticos
# para varios lags
vcorr_res(var1,12,"PT.adjusted")


# Otra opción son los estadísticos tipo Breusch y Pagan
vcorr_res(var1,12,"BG")
vcorr_res(var1,12,"ES")

# En realidad, la propuesta del criterio de información AIC
# era solo indicativa. Por ejemplo, ver que con 2 lags alcanza
var1=VAR(ts.dif.lu[,1:2],type="const", p=2)
summary(var1)

# Testeamos con los tests Q
vcorr_res(var1,12,"PT.adjusted")
# Podemos verificar también con los test BG
vcorr_res(var1,12,"BG")
# Nos quedamos entonces con un VAR con 2 lags

# Estabilidad. 
# Reporta las raíces inversas
# Si en módulo son menores que 1 el VAR es estable
roots(var1)

# Testeo de heterocedasticidad ARCH
# No hay en el paquete un testeo de heterocedasticidad
# del tipo White
t.arch<-arch.test(var1,lags.multi=5, multivariate.only=FALSE)
t.arch

##test de normalidad
t.norm<-normality.test(var1, multivariate.only=FALSE)
t.norm
windows()
hist(t.norm$resid[,1])
hist(t.norm$resid[,2])
dev.off()

#***********************************************
# Causalidad en el sentido de Granger
#***********************************************
# Testea Causalidad en el sentido de Granger
# y "causalidad instantanea" que sería correlación 
# contemporánea de las variables
causality(var1,cause="dln_inc")
# rechaza
causality(var1,cause="dln_consump")
# no rechaza
# Por tanto el ingreso "Granger causa" al consumo 
# El consumo no "Granger causa" al ingreso


#***********************************************
#              Predicción
#***********************************************
# Predicción ex-ante
predictions<-predict(var1,n.ahead=25, ci=0.95)
windows()
plot(predictions,names="dln_inc")
plot(predictions,names="dln_consump")
dev.off()

# Predicción ex post
# Extraigo un data set parcial hasta 1980q4
dif.lu.reg=window(ts.dif.lu, start=c(1960,2), end=c(1980,4), frequency=4)

# Estimo el VAR con los datos hasta 1980
var3=VAR(dif.lu.reg[,1:2],type="const", p=2)
summary(var3)

# Realizo una predicción para los 8 trimestres faltantes
pred3<-predict(var3,n.ahead=8)

# Grafico la serie y su pronóstico
# Ingreso
windows()
plot(ts.dif.lu[,1])
plot(pred3,names="dln_inc")
lines(ts.dif.lu[,1])      
dev.off()

#Consumo
windows()
plot(ts.dif.lu[,2])  
plot(pred3,names="dln_consump")
lines(ts.dif.lu[,2])      
dev.off()

#############################################
#       Funciones de impulso respuesta
############################################
# Normalización de Cholesky. Impulso, un 
# error estándar de la variable impulsora
args(irf)

# Calcula la IRF (todos los impulsos
# y todas las respuestas) con Cholesky (ortho=TRUE)
# Bandas de confianza al 95 % para 10 períodos (default)
irf.1=irf(var1)
windows()
plot(irf.1)
dev.off()
print(irf.1)
# Interpretación: Respuesta de las series a un 
# impulso de la serie del título $serie 
# Valor del impulso: 1 desvío estándar de los residuos
# de la ecuación de la serie impulso

# Para que no reporte los valores de las bandas
# de confianza
print(irf.1$irf)

# Fijarse los desvíos estándar de los residuos
res_inc=var1$varresult$dln_inc$residuals
df_res_inc=var1$varresult$dln_inc$df.residual
sqrt(sum(res_inc^2)/df_res_inc)

##########################################
# Cambio el orden de las variables
##########################################
yb=data.frame(cbind(ts.dif.lu[ ,2],ts.dif.lu[ ,1]))
names(yb)=c("dln_consump","dln_inc")
yb=ts(yb,start=c(1960,2),frequency=4) 

# Fijarse que la estimación no cambia (VERIFICAR)
var1b=VAR(yb,type="const", p=3)
summary(var1b)   # no cambia

# La IRF si cambia
irf.1b <- irf(var1b,nahead=10)  
windows()
plot(irf.1b)        # si cambia
dev.off()
print(irf.1b$irf)

##########################################
# Se puede graficar cada irf individualmente
##########################################
irf.11=irf(var1, impulse="dln_inc", 
           response="dln_consump")
windows()
plot(irf.11)
dev.off()
# Se puede elegir otras bandas de confianza
# ci=0.95 son dos errores estándar
# ci=0.68 es un error estándar
irf.12=irf(var1, impulse="dln_inc", 
           response="dln_consump",ci=0.68)
windows()
plot(irf.12)
dev.off()
# Donde las bandas de confianza incluyen al cero
# la IRF es no significativa

##########################################

# Se puede ignorar la covarianza entre los
# errores del modelo reducido. En este caso
# el impulso es de valor = 1 
# No tiene en cuenta el orden de magnitud de
# las variables
irf.1c=irf(var1, ortho=FALSE) 
windows()
plot(irf.1c)
dev.off()
print(irf.1c$irf)
#####################################################
# Funciones de impulso respuesta acumulativa

irf.1d <- irf(var1,impulse="dln_inc", cumulative=FALSE)
windows()
plot(irf.1d)
dev.off()
print(irf.1d)


irf.1d <- irf(var1,impulse="dln_inc", cumulative=TRUE)
windows()
plot(irf.1d)
dev.off()
print(irf.1d)

# Estacionariedad de la respuesta
# Si hay suficientes períodos, se ve que va
# desapareciendo la influencia del shock
# La variable va a cero, o sea a su valor inicial
irf.1e <- irf(var1,impulse="dln_inc", n.ahead=50)
windows()
plot(irf.1e)
dev.off()
print(irf.1e)

# En el caso de la IRF acumulativa va a un valor 
# de largo plazo
irf.1f <- irf(var1,impulse="dln_inc",n.ahead=50,
              cumulative=TRUE)
windows()
plot(irf.1f)
dev.off()
print(irf.1f)

#***********************************************
#    DESCOMPOSICION DE LA VARIANZA 
#***********************************************
# Descomposición de la varianza
des.1<-fevd(var1,n.ahead=10)
windows()
plot(des.1,addbars=2)
dev.off()
print(des.1)

