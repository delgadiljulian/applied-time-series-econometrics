#***********************************************
#*    Modelos VEC - Test de Johansen
#*#**********************************************
# install.packages("vars")
library(vars)
# install.packages("urca")
library("urca")
#***********************************************
#*    Carga de datos
#*#**********************************************
# Los datos son los del libro de Johansen. Datos trimestrales de
# la economía de Dinamarca, desde 1974.1 hasta 1987.3
data(denmark)
View(denmark)
# Cargo 4 de las series "LRM", "LRY", "IBO", "IDE"
# M2, PIB, tasa de bonos y tasa de depósitos
data=ts(denmark[, c("LRM", "LRY", "IBO", "IDE")],start=c(1974,1),frequency=4 )
# Grafico
windows()
plot(data)
dev.off()

#***********************************************
#*    Verificar que las 4 series son I(1)
#*#**********************************************
# Cargo el script del test de Dickey Fuller
# Test.ADF.Ver.3.R y lo corro
y=denmark$LRM
summary(lm(y~seq(1,length(y))) ) # Modelo 3
Test=Test.ADF.Ver.3(denmark$LRM,3,"BIC")  # No rechaza
summary(lm(diff(y)~seq(1,length(y)-1)))  # Modelo 1
Test=Test.ADF.Ver.3(diff(y),1,"BIC")  # Rechaza, LRM es I(1)

y=denmark$LRY
summary(lm(y~seq(1,length(y))) ) # Modelo 3
Test=Test.ADF.Ver.3(denmark$LRM,3,"BIC")  # No rechaza
summary(lm(diff(y)~seq(1,length(y)-1)))  # Modelo 1
Test=Test.ADF.Ver.3(diff(y),1,"BIC")  # Rechaza, LRY es I(1)

y=denmark$IBO
summary(lm(y~seq(1,length(y))) ) # Modelo 3
Test=Test.ADF.Ver.3(denmark$LRM,3,"BIC")  # No rechaza
summary(lm(diff(y)~seq(1,length(y)-1)))  # Modelo 1
Test=Test.ADF.Ver.3(diff(y),1,"BIC")  # Rechaza, IBO es I(1)

y=denmark$IDE
summary(lm(y~seq(1,length(y))) ) # Modelo 2
Test=Test.ADF.Ver.3(denmark$LRM,2,"BIC")  # No rechaza
summary(lm(diff(y)~seq(1,length(y)-1)))  # Modelo 1
Test=Test.ADF.Ver.3(diff(y),1,"BIC")  # Rechaza, IDE es I(1)

#**********************************************
#  Creación del VAR subyacente  EN NIVELES
#**********************************************
# Johansen elige el modelo "const" con 2 lags
# y agrega dummies estacionales centradas
VAR=VAR(data, p=2,type="const",  season=4)
summary(VAR)

# Verificación de la ausencia de autocorrelación
# en las perturbaciones:
# Johansen corre un test LM de SIGNIFICATIVIDAD INDIVIDUAL
# para los rezagos 1 y 4 , que indican ausencia de
# autocorrelación EN  esos lags (Verificado con EViews, no disponible en R)
# Luego Johansen calcula el test Q de SIGNIFICATIVIDAD GLOBAL
# para el rezago 13 y le da 197.63 con # p-value=0.13
serial.test(VAR,lags.pt=13,type="PT.adjusted" )

# Si se hace una revisión más estricta, hay autocorrelación
# Cargando el programa vcorr_res
vcorr_res(VAR,14,"PT.adjusted")

# Verificacion de la normalidad
# (necesaria para hacer el test de Johansen)
normality.test(VAR)          # OK

# ***************************************************
#    Test de cointegración de Johansen
#****************************************************
# Se realiza con el comando ca.jo
# Johansen elige el modelo 2 ("const" en el paquete urca)
# La existencia de pendientes en las series de nivel sugeriría
# utilizar modelo 3 ("none" en urca)
# Test de Traza.
test.trace <- ca.jo(data, ecdet = "const", K=2,
      type="trace", spec="transitory",season=4)
summary(test.trace)
# El test no rechaza la H0 r=0, o sea no cointegración

# Test de Máximo Autovalor.
test.eigen <- ca.jo(data, ecdet = "const",K=2,
            type="eigen",spec="transitory",season=4)
summary(test.eigen)
# El test señala la existencia de un vector de cointegración
# Ver que se reportan varios vectores de cointegración. Nos quedamos
# con la primer columna.
# Idem con los alfa (loading coefficients)
#*****************************************************
#*Fijarse que también obtengo cointegración con
#*el modelo 3 ("none" en el paquete urca)
test.trace_B <- ca.jo(data, ecdet = "none", K=2,
                     type="trace", spec="transitory",season=4)
summary(test.trace_B)
# El test no rechaza la H0 r=0, o sea no cointegración

# Test de Máximo Autovalor.
test.eigen_B <- ca.jo(data, ecdet = "none",K=2,
                     type="eigen",spec="transitory",season=4)
summary(test.eigen_B)
# El test señala la existencia de un vector de cointegración

# Johansen diseña un test llamado en este paquete lttest()
# para decidir entre el modelo 2 ("const" en urca) y el
# modelo 3 ("none" en urca).
# La hipótesis nula es que vale el modelo 2 (sin tendencia)
lttest(test.eigen, r=1)  # o idénticamente
lttest(test.eigen_B, r=1)

#****************************************************
#    Estimación del VEC
#****************************************************
# Con el comando cajols estimo el VEC
# el comando cajorls es para el VEC restringido
# o sea que fuerza que valga el/los vector/es
# que determinó el test de Johansen
VEC=cajorls(test.eigen, r=1)  # el argumento es la salida del test
summary(VEC$rlm)

# Tambien puedo indicarle muestre una sola ecuación (a eleccion)
VEC=cajorls(test.eigen, r=1,reg.number=2)
summary(VEC$rlm)

# La salida es ecuación por ecuación. El coeficiente
# ect1 es el coeficiente de ajuste. No aparece el vector
# de cointegración
VEC$beta  # Vector de cointegración

#****************************************************
#    Tests, IRF, Pronósticos, etc. para el VEC
#****************************************************
# La idea es transformar el VEC en un VAR
# (recordar que el VEC es un VAR re-especificado) pero conservando
# la matriz pi , es decir el vector de cointegración
# y los coeficientes de ajuste
# Rescatamos el test de Johansen
# Test de Traza.
test.trace <- ca.jo(data, ecdet = "const", K=2,
                    type="trace", spec="transitory",season=4)
var.from.vec=vec2var(test.trace, r=1)
summary(var.from.vec)

# Test de normalidad
normality.test(var.from.vec)
# Test de autocorrelación
vcorr_res(var.from.vec,12,"PT.adjusted")
# IRF
print(irf(var.from.vec, ortho=T)$irf)




#****************************************************
#    Test de significatividad de los beta: blrtest()
#****************************************************
# La significatividad de los betas se testea con estas restricciones
# Si alguno de los beta = 0, habría que retirarlo del VEC
#* Recordar el vector de cointegración
# $beta
# ect1
# LRM.l1    1.000000
# LRY.l1   -1.032949
# IBO.l1    5.206919
# IDE.l1   -4.215879
# constant -6.059932
#* Por ejemplo para testear que beta(2) es cero
#* beta(2) corresponde a la variable LRY
H2 <- matrix(c(1,0,0,0,0 , 0,0,1,0,0 , 0,0,0,1,0 ,0,0,0,0,1), c(5,4))
H2
test_b2=blrtest(test.trace, H=H2, r=1)
summary(test_b2)
# La salida es muy extensa. Mejor
blrtest(test.trace, H=H2, r=1)@pval[1]
# Rechaza beta(2)=0

# Testeamos el resto
H1 <- matrix(c(0,1,0,0,0 , 0,0,1,0,0 , 0,0,0,1,0 , 0,0,0,0,1), c(5,4))
H1
# Vale el test igual pero la estimación no se reporta
# porque intenta normalizar con beta(1)=1
blrtest(test.trace, H=H1, r=1)@pval[1]

H3 <- matrix(c(1,0,0,0,0 , 0,1,0,0,0 , 0,0,0,1,0 , 0,0,0,0,1), c(5,4))
H3
blrtest(test.trace, H=H3, r=1)@pval[1]

H4 <- matrix(c(1,0,0,0,0 , 0,1,0,0,0 , 0,0,1,0,0 , 0,0,0,0,1), c(5,4))
H4
blrtest(test.trace, H=H4, r=1)@pval[1]

H5 <- matrix(c(1,0,0,0,0 , 0,1,0,0,0 , 0,0,1,0,0 , 0,0,0,1,0), c(5,4))
H5
blrtest(test.trace, H=H5, r=1)@pval[1]

# Todos los elementos del vector son significativos

#****************************************************
#   Restricciones lineales sobre los beta: blrtest()
#****************************************************
#*Si queremos testear que beta(1) = - beta(2)
H6 <- matrix(c(-1,1,0,0,0 , 0,0,1,0,0 , 0,0,0,1,0 ,0,0,0,0,1), c(5,4))
H6
blrtest(test.trace, H=H6, r=1)@pval[1]
# No se rechaza

#*Si queremos testear que beta(3) = - beta(4)
H7 <- matrix(c(1,0,0,0,0 , 0,1,0,0,0 , 0,0,-1,1,0 ,0,0,0,0,1), c(5,4))
H7
blrtest(test.trace, H=H7, r=1)@pval[1]
# No se rechaza
#****************************************************
#     Test de significatividad y restriccionesw
#           sobre los alfa: alrtest()
#****************************************************
# Si queremos restringir un alfa = 0 lo excluímos
# Por ejemplo para hacer alfa(2)=0
# Sería un test de significatividad de alfa(2)
A2 <- matrix(c(1,0,0,0 , 0,0,1,0 , 0,0,0,1), c(4,3))  ; A2
alrtest(test.trace, A=A2, r=1)@pval[1]
# No se rechaza

# Testeamos el resto
A1 <- matrix(c(0,1,0,0 , 0,0,1,0 , 0,0,0,1), c(4,3))  ; A1
alrtest(test.trace, A=A1, r=1)@pval[1]
# Rechaza

A3 <- matrix(c(1,0,0,0 , 0,1,0,0 , 0,0,0,1), c(4,3))  ; A3
alrtest(test.trace, A=A3, r=1)@pval[1]
# No se rechaza

A4 <- matrix(c(1,0,0,0 , 0,1,0,0 , 0,0,1,0), c(4,3))  ; A4
alrtest(test.trace, A=A4, r=1)@pval[1]
# No se rechaza

# Si queremos hacer una restricción de igualdad
# por ejemplo alfa(1) = - alfa (2)
A5 <- matrix(c(1,-1,0,0,0,0,1,0,0,0,0,1), c(4,3))  ; A5
summary(alrtest(test.trace, A=A5, r=1))
# No se rechaza
#****************************************************
#       Test de restricciones sobre alfas y betas:
#                        ablrtest()
#***************************************************
#*Se pueden plantear restricciones sobre los
# alfas y los betas simultáneamente.
# Habrá que ir incorporando las restricciones porque cuando
# se plantea, por ejemplo, beta(1) = - beta(2) si luego se
# suma beta(3) = - beta(4) se trata de un test conjunto

# Johansen luego de estudiar su ejemplo en forma
# meticulosa llega a las siguientes restricciones
# 1) beta(1) = - beta(2) -> Elasticidad unitaria de la demanda de dinero
#    respecto del producto
# 2) beta(3) = - beta(4) -> La demanda de dinero reacciona a la diferencia
#    entre las tasas de interés de bonos y depósitos
# 3) alfa(3) = alfa(4) = 0 -> Ambas tasas de interés son debilmente exógenas

HJo <- matrix(c(1,-1,0,0,0 , 0,0,1,-1,0 ,0,0,0,0,1), c(5,3))  ; HJo
AJo <- matrix(c(1,0,0,0 , 0,1,0,0), c(4,2))   ;  AJo
ablrtest(test.trace, H=HJo, A=AJo, r=1)@pval
# No se rechaza
# La salida completa del test
summary(ablrtest(test.trace, H=HJo, A=AJo, r=1))

#******************************************
#*   Recuperación del VEC restringido
#******************************************
# Para recuperar el VEC restringido hay que poner
# como argumento del comando cajorls la salida
# del test de restricciones

rtest.Jo=ablrtest(test.trace, H=HJo, A=AJo, r=1)
VEC.Final.Jo=cajorls(rtest.Jo)
summary(VEC.Final.Jo$rlm)
# El reporte de los coeficientes de ajuste está mal
# aunque internamente los guarda bien
# todo lo demás está OK
rtest.Jo@V      # beta restringido
rtest.Jo@W      # alpha restringido

#******************************************************
# Para volver del VEC al VAR y poder usar los comandos
# del VAR para tests, IRF, Pronósticos, etc. no es tan
# sencillo por la acción de las restricciones
#******************************************************


# Ver también
# https://www.r-bloggers.com/2021/12/some-interesting-issues-in-vecm-using-r/
# http://jduras.github.io/files/teaching/eco5316/lec21slides.pdf
# https://www.r-econometrics.com/timeseries/vecintro/
