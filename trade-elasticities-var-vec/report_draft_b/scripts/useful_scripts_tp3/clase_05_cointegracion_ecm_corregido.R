#**************************************************
# Cointegración - Método de Engle-Granger
#**************************************************
# Relación de largo plazo de las series de Consumo, Ingreso y
# Riqueza de UK
library(urca)

data(Raotbl3)
attach(Raotbl3)
# Carga las series macro de UK
# Consumo
lc<-ts(lc,start=c(1966,4),end=c(1991,2), frequency=4)
# Ingreso
li<-ts(li,start=c(1966,4),end=c(1991,2), frequency=4)
# Riqueza
lw<-ts(lw,start=c(1966,4),end=c(1991,2),  frequency=4)

# Genera un subconjunto con las 3 series
ukcons<-window(cbind(lc,li,lw),start=c(1966,4), end=c(1991,2))
# Gráfico de las series y sus diferencias
plot.ts(ukcons)
plot.ts(diff(ukcons))

# Aquí habría que verificar que las 3 series son I(1)
Test.ADF.Ver.3(lc,3,"BIC")         # Hay raiz unitaria
Test.ADF.Ver.3(diff(lc),2,"BIC")   # No hay raiz unitaria

Test.ADF.Ver.3(li,3,"BIC")         # Hay raiz unitaria
Test.ADF.Ver.3(diff(li),2,"BIC")   # No hay raiz unitaria

Test.ADF.Ver.3(lw,3,"BIC")         # Hay raiz unitaria
Test.ADF.Ver.3(diff(lw),2,"BIC")   # No hay raiz unitaria

# Las 3 series son I(1)

library(dynlm)
# Hace las regresiones Engle Granger
lc.eq=dynlm(lc~li+lw,data=ukcons) ; summary(lc.eq)
li.eq=dynlm(li~lc+lw,data=ukcons) ; summary(li.eq)
lw.eq=dynlm(lw~li+lc,data=ukcons) ; summary(lw.eq)

# Guarda los residuos en sendas variables
res.lc<-ts(lc.eq$residuals, start=c(1966,4),frequency=4)
res.li<-ts(li.eq$residuals,start=c(1966,4),frequency=4)
res.lw<-ts(lw.eq$residuals,start=c(1966,4),frequency=4)

#**************************************************
# Test de Cointegración - Método de Engle-Granger
#**************************************************

# Verifica si son estacionarios. Ojo, verificar con los
# Valores Críticos de Engle y Granger
Test.ADF.Ver.3(res.lc,1,"BIC")
length(res.lc)
Tabla_EG(3)

# Es ruido blanco según los valores críticos de E-G

Test.ADF.Ver.3(res.li,1,"BIC")
Tabla_EG(3)
# Es ruido blanco según los valores críticos de E-G

Test.ADF.Ver.3(res.lw,1,"BIC")
Tabla_EG(3)
# NO Es ruido blanco según los valores críticos de E-G
# Por lo tanto puedo construir un modelo ECM, pero
# con variable explicada Consumo ó Ingreso

#**************************************************
# Modelo con término de corrección del error
#**************************************************

# Utilizando las series y regresiones anteriores
ecm1=dynlm(d(lc)~d(li)+d(lw)+L(res.lc))
summary(ecm1)

# Verifico que los residuos de la regresión sean ruido blanco
corr_res(ecm1,12,0)

# Agrego rezagos de la diferencia de alguna/s variables
# para absorber la autocorrelación remanente
# Esta es una regresión de la econometría tradicional
ecm1=dynlm(d(lc)~d(li)+d(lw)+L(res.lc)+L(d(lc),3))
summary(ecm1)
# Verifico que los residuos de la regresión sean ruido blanco
corr_res(ecm1,12,0)

# Elimino la variable d(lw) que es no significativa
ecm1=dynlm(d(lc)~d(li)+L(res.lc)+L(d(lc),3))
summary(ecm1)

# Verifico que los residuos de la regresión sean ruido blanco
corr_res(ecm1,12,0)

# Este es el modelo definitivo

#**************************************************
# Estimación de una etapa
# Método de Wickens y Breusch
#**************************************************
ecm2=dynlm(d(lc)~d(li)+d(lw)+L(lc)+L(li)+L(lw))
summary(ecm2)
# Para recuperar los coeficientes de corto y largo plazo

#**************************************************
# Metodología de Engle-Granger
# ECM para consumo e  ingreso en EEUU
# Según Pindick & Rubinfeld pag. 541 (ver Bibliografía)
#**************************************************
# Cargar archivos de Excel  Coint_PBI.xls
attach(Coint_PBI)
l_cons<-ts(l_cons,start=c(1960,1),frequency=4)
l_inc<-ts(l_inc,start=c(1960,1),frequency=4)
usa_data<-window(cbind(l_cons,l_inc),start=c(1960,1))
# Gráfico de las series y sus diferencias
plot.ts(usa_data)

# Verificar que ambas series son I(1)
Test.ADF.Ver.3(l_cons,3,"BIC")
plot.ts(diff(l_cons))
Test.ADF.Ver.3(diff(cons),2,"BIC")   # OK, es I(1)

Test.ADF.Ver.3(inc,3,"BIC")
plot.ts(diff(inc))
Test.ADF.Ver.3(diff(inc),2,"BIC")   # OK, es I(1)

# Realizo el ejercicio con un período reducido 1960-1986
# 3era edición del libro de Pindyck y Rubinfeld
l_cons1=window(l_cons,start=c(1960,1),end=c(1986,4),frequency=4)
l_inc1=window(l_inc,start=c(1960,1),end=c(1986,4),frequency=4)

l_cons1.eq=dynlm(l_cons1~l_inc1) ; summary(l_cons1.eq)
res.c1=l_cons1.eq$residuals
Test.ADF.Ver.3(res.c1,1,"BIC")
length(res.c1)
Tabla_EG(2)
# OK, cointegran

# Realizo el ejercicio con un período completo 1960-1995
# 4ta edición del libro de Pindyck y Rubinfeld

l_cons2.eq=dynlm(l_cons~l_inc) ; summary(l_cons2.eq)
res.c2=l_cons2.eq$residuals
Test.ADF.Ver.3(res.c2,1,"BIC")
length(res.c2)
Tabla_EG(2)
# Ahora no cointegran ¿explicación?

#***************************************************
#*   Cambio en la relación de cointegración
#***************************************************

cons<-ts(cons,start=c(1960,1),frequency=4)
inc<-ts(inc,start=c(1960,1),frequency=4)

cons_60_86<-window(cons,start=c(1960,1),end=c(1986,4),frequency=4)
inc_60_86<-window(inc,start=c(1960,1),end=c(1986,4),frequency=4)
cons_60_86.eq=dynlm(cons_60_86~inc_60_86) ; summary(cons_60_86.eq)

cons_87_95<-window(cons,start=c(1987,1),end=c(1995,4),frequency=4)
inc_87_95<-window(inc,start=c(1987,1),end=c(1995,4),frequency=4)
cons_87_95.eq=dynlm(cons_87_95~inc_87_95) ; summary(cons_87_95.eq)

# La propensión marginal a consumir pasa de
# 0.89 en la regresión 1960-1986 a 0.99 en la
# regresión 1987-1995

cons_60_95.eq=dynlm(cons~inc) ; summary(cons_60_95.eq)
# En la muestra completa la prop. marginal a consumir
# queda en un valor intermedio  0.93
