#***********************************************
#*    Modelos VEC - Test de Johansen
#*#**********************************************
# install.packages("vars")
library(vars)
# install.packages("urca")
library("urca")

# Los datos son los del libro de Johansen. Datos trimestrales de
# la economía de Dinamarca, desde 1974.1 hasta 1987.3
data(denmark)
View(denmark)
# Cargo 4 de las series "LRM", "LRY", "IBO", "IDE"
# M2, PIB, tasa de bonos y tasa de depósitos
data=ts(denmark[, c("LRM", "LRY", "IBO", "IDE")],start=c(1974,1),frequency=4 )

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
# Con el comando cajorls estimo el VEC
VEC=cajorls(test.eigen, r=1)  # el argumento es la salida del test
summary(VEC$rlm)
# La salida es ecuación por ecuación. El coeficiente
# ect1 es el coeficiente de ajuste. No aparece el vector
# de cointegración
VEC$beta  # Vector de cointegración

#****************************************************
#    Test de restricciones sobre los beta: blrtest()
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
summary(blrtest(test.trace, H=H2, r=1))
# Rechaza beta(2)=0

# Testeamos el resto
H1 <- matrix(c(0,1,0,0,0 , 0,0,1,0,0 , 0,0,0,1,0 , 0,0,0,0,1), c(5,4))
H1
# Vale el test igual pero la estimación no se reporta
# porque intenta normalizar con beta(1)=1
summary(blrtest(test.trace, H=H1, r=1))

H3 <- matrix(c(1,0,0,0,0 , 0,1,0,0,0 , 0,0,0,1,0 , 0,0,0,0,1), c(5,4))
H3
summary(blrtest(test.trace, H=H3, r=1))

H4 <- matrix(c(1,0,0,0,0 , 0,1,0,0,0 , 0,0,1,0,0 , 0,0,0,0,1), c(5,4))
H4
summary(blrtest(test.trace, H=H4, r=1))

H5 <- matrix(c(1,0,0,0,0 , 0,1,0,0,0 , 0,0,1,0,0 , 0,0,0,1,0), c(5,4))
H5
summary(blrtest(test.trace, H=H5, r=1))

# Todos los elementos del vector son significativos

#****************************************************
#   Restricciones lineales sobre los beta: blrtest()
#****************************************************
#*Si queremos testear que beta(1) = - beta(2)
H6 <- matrix(c(-1,1,0,0,0 , 0,0,1,0,0 , 0,0,0,1,0 ,0,0,0,0,1), c(5,4))
H6
summary(blrtest(test.trace, H=H6, r=1))
# No se rechaza

#*Si queremos testear que beta(3) = - beta(4)
H7 <- matrix(c(1,0,0,0,0 , 0,1,0,0,0 , 0,0,-1,1,0 ,0,0,0,0,1), c(5,4))
H7
summary(blrtest(test.trace, H=H7, r=1))
# No se rechaza
#****************************************************
#     Restricciones sobre los alfa: alrtest()
#****************************************************
# Si queremos restringir un alfa = 0 lo excluímos
# Por ejemplo para hacer alfa(2)=0
# Sería un test de significatividad de alfa(2)
A2 <- matrix(c(1,0,0,0 , 0,0,1,0 , 0,0,0,1), c(4,3))  ; A2
summary(alrtest(test.trace, A=A2, r=1))
# No se rechaza

# Testeamos el resto
A1 <- matrix(c(0,1,0,0 , 0,0,1,0 , 0,0,0,1), c(4,3))  ; A1
summary(alrtest(test.trace, A=A1, r=1))
# Rechaza

A3 <- matrix(c(1,0,0,0 , 0,1,0,0 , 0,0,0,1), c(4,3))  ; A3
summary(alrtest(test.trace, A=A3, r=1))
# No se rechaza

A4 <- matrix(c(1,0,0,0 , 0,1,0,0 , 0,0,1,0), c(4,3))  ; A4
summary(alrtest(test.trace, A=A4, r=1))
# No se rechaza

# Habrá que ir incorporando las restricciones porque cuando
# se plantea, por ejemplo, alfa(3) = 0 todos los tests sobre
# los otros alfa cambian
#****************************************************
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
# Por ejemplo beta(1) = - beta(2) y alfa(3) = - alfa(4)
H8 <- matrix(c(1,-1,0,0,0 , 0,0,1,0,0 , 0,0,0,1,0 , 0,0,0,0,1), c(5,4))
H8
A6 <- matrix(c(1,0,0,0, 0,1,0,0 , 0,0,-1,1), c(4,3))
A6
summary(ablrtest(test.trace, H=H8, A=A6, r=1))


# Johansen luego de estudiar su ejemplo en forma 
# meticulosa llega a las siguientes restricciones
# 1) beta(1) = - beta(2) -> Elasticidad unitaria de la demanda de dinero 
#    respecto del producto
# 2) beta(3) = - beta(4) -> La demanda de dinero reacciona a la diferencia
#    entre las tasas de interés de bonos y depósitos
# 3) alfa(3) = alfa(4) = 0 -> Ambas tasas de interés son debilmente exógenas

HJ <- matrix(c(1,-1,0,0,0 , 0,0,1,-1,0 ,0,0,0,0,1), c(5,3))  ; HJ
AJ <- matrix(c(1,0,0,0 , 0,1,0,0), c(4,2))   ;  AJ
summary(ablrtest(test.trace, H=HJ, A=AJ, r=1))
# No se rechaza

#******************************************
#*   Recuperación del VEC restringido
#******************************************
# Para recuperar el VEC restringido hay que poner 
# como argumento del comando cajorls la salida
# del test de restricciones

rest.J=ablrtest(test.trace, H=HJ, A=AJ, r=1)
VEC.rest=cajorls(rest.J,r=1)
summary(VEC.rest$rlm)
VEC.rest$beta


# Ver también
# https://www.r-bloggers.com/2021/12/some-interesting-issues-in-vecm-using-r/
# http://jduras.github.io/files/teaching/eco5316/lec21slides.pdf
