#***********************************************
#     Vectores Autorregresivos VAR
#***********************************************
# install.packages("vars")
library(vars)

# Importar lutkepohl2 archivo de Excel
lu=data.frame(lutkepohl2)
level.lu=data.frame(matrix(c(lu$ln_inc,lu$ln_consump,lu$ln_inv), ncol=3))
names(level.lu)=c("ln_inc","ln_consump","ln_inv")
ts.level.lu=ts(level.lu,start=c(1960,1),frequency=4)
dif.lu=subset(lu, !is.na(dln_inc))
dif.lu=data.frame(matrix(c(dif.lu$dln_inc,dif.lu$dln_consump,dif.lu$dln_inv), ncol=3))
names(dif.lu)=c("dln_inc","dln_consump","dln_inv")
ts.dif.lu=ts(dif.lu,start=c(1960,2), frequency=4)

#***********************************************
#     Estimación del  VAR
#***********************************************
var1=VAR(ts.dif.lu[,1:2],type="const", p=2)
summary(var1)

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
# Puedo calcular los desvíos estándar de la
# matriz de covarianza de los residuos

# Covariance matrix of residuals:
#   dln_inc dln_consump
# dln_inc     1.316e-04   6.746e-05
# dln_consump 6.746e-05   1.010e-04
#
# sqrt(1.316e-04)= 0.0114717
# > sqrt(1.010e-04)= 0.01004988

# Solo se aprecia en verdadero valor en la exógena

##########################################
# Cambio el orden de las variables
##########################################
yb=data.frame(cbind(ts.dif.lu[ ,2],ts.dif.lu[ ,1]))
names(yb)=c("dln_consump","dln_inc")
yb=ts(yb,start=c(1960,2),frequency=4)

# Fijarse que la estimación no cambia (VERIFICAR)
var1b=VAR(yb,type="const", p=2)
summary(var1b)   # no cambia (CONTROLAR)

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

# Se puede ignorar la covarianza entre los errores
# del modelo reducido. En este caso no hay influencia
# contemporanea de una serie sobre otra
# El impulso es de valor = 1.
irf.1c=irf(var1, ortho=FALSE)
windows()
plot(irf.1c)
dev.off()
print(irf.1c$irf)
#####################################################
# Funciones de impulso respuesta acumulativa
#####################################################
# Cuando tengo variables en diferencias, la IRF acumulativa
# me indica el cambio del nivel de la variable en el largo
# plazo
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

################################################
#       VAR ESTRUCTURAL
################################################
# Con el VAR estructural o SVAR puedo realizar identificaciones
# más creativas
# Realizo las restricciones para reproducir Cholesky
# La matriz Amat es nuestra matriz B
amat <- diag(2)
diag(amat) <- NA
amat[2,1]=NA
amat
# La matriz Bmat es la que plantea la no covarianza
# de los shocks entre ecuaciones estructurales
bmat=diag(2)
bmat
diag(bmat)=NA
bmat
# Lo corremos con el método "scoring". El metodo "direct" da resultados
# distintos a los convencionales
args(SVAR)
svar1 <- SVAR(var1, estmethod = "scoring", Amat = amat, Bmat=bmat)
summary(svar1)

irf_s1=irf(svar1)   # fijarse que es igual que Cholesky (VERIFICAR-TAKE HOME)
windows()
plot(irf_s1)
print(irf_s1$irf)

##########################################
# Ahora realizo las restricciones para suponer que
# ambos(b12 y b21) son cero
amat2 <- diag(2)
diag(amat2) <- NA
# amat[2,1]=NA
amat2
# La matriz Bmat es la que plantea la no covarianza
# de los shocks entre ecuaciones estructuales
bmat2=diag(2)
diag(bmat2)=NA
bmat2
svar2 <- SVAR(var1, estmethod = "direct", Amat = amat2, Bmat=bmat2)
irf_s2=irf(svar2)
windows()
plot(irf_s2)
dev.off()

# Cuando el VAR está sobreidentificado (o sea puse más
# restricciones que las absolutamente necesarias) puedo
# correr un test que compara el VAR exactamente identificado con el
# sobreidentificado (Test LR) y valida o no la sobreidentificación
svar2$LR$p.value       # rechaza

# La irf en este caso en que no  hay correlación entre
# las perturbaciones de la forma reducida
print(irf_s2$irf)

# Esta irf se corresponde con la que conseguiría con el comando
# irf(var1,n.ahead=10, impulse="dln_inc", response="dln_inc", ortho=F)
# Ya lo habíamos hecho y a la IRF le llamamos irf_1c
print(irf.1c$irf)
# Los números no coinciden porque en un caso (svar) se shockea con un
# error estándar y en otro (var con ortho=F) con un shock unitario
# (take home: Verificar la proporcionalidad)
##############################################
# En vez de realizar la inversión del orden de las series
# puedo plantear un Cholesky al revés
amat3 <- diag(2)
diag(amat3) <- NA
amat3[1,2]=NA
amat3
# La matriz Bmat es la que plantea la no covarianza
# de los shocks entre ecuaciones estructuales
bmat3=diag(2)
diag(bmat3)=NA
bmat3
svar3 <- SVAR(var1, estmethod = "scoring", Amat = amat3, Bmat=bmat3)
summary(svar3)
irf_s3=irf(svar3)   #
windows()
plot(irf_s3)
dev.off
print(irf_s3$irf)
# take home: Verificar que da igual que Cholesky al reves
# (puede fallar: Tu Sam)
######################################################
#  VAR ESTRUCTURAL - RESTRICCIONES DE LARGO PLAZO
######################################################
# Identificación de largo plazo (Blanchard-Quah)
# Importar el archivo Blanchard-Quah.xls
BQ=subset(Blanchard_Quah[,2:3] , !is.na(DY))

# Convertimos en un objeto "series de tiempo"
y=ts(BQ,start=c(1948,2),frequency=4)

# Graficamos las series
plot.ts(y)

# Siguiendo el paper BQ estimamos un var con 8 lags
bq_var=VAR(BQ,type="const", p=8)
summary(bq_var)

# Estimo las restricciones de largo plazo
bq_svar=BQ(bq_var)

# Con ellas obtengo las IRF
windows()
plot(irf(bq_svar, impulse="DY",n.ahead = 40))
plot(irf(bq_svar, impulse="U",n.ahead = 40))
dev.off()

# IRFs acumulativas
# La idea es forzar una separación de los shocks de modo tal que
# uno de ellos no tenga influencia sobre el Ingreso (Y)
windows()
plot(irf(bq_svar, impulse="DY",n.ahead = 100, cumulative=T))
plot(irf(bq_svar, impulse="U",n.ahead = 100, cumulative=T))
dev.off()

# lo vemos mejor con una IRF individual del ingreso
windows()
plot(irf(bq_svar, impulse="DY",response="DY",n.ahead = 100, cumulative=T))
plot(irf(bq_svar, impulse="U",response="DY",n.ahead = 100, cumulative=T))
# Fijarse que con uno de los shocks se logra que en el largo
# plazo el efecto sobre la demanda sea cero
# Ese es el shock nominal
dev.off()
