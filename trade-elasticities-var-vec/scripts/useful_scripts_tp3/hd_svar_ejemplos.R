# ============================================================
#                       HD_SVAR(): 
#    Historical Decomposition para VAR/SVAR con línea base
#                 Ejemplos de aplicación
# ============================================================

# ============================================================
#       Carga de los datos y de la función
# ============================================================
# Cargar los datos
Trimestral.Completo <- read.delim("D:/Académico 1erC 2026/Maestría en Economía Aplicada/HD/Trabajo Mayo 2026/Trimestral.Completo.txt")
View(Trimestral.Completo)
# Cargar las variables endógenas en el orden correcto 
# para la identificación de Cholesky
y_red=subset(Trimestral.Completo,!is.na(dln_pbi))
y=ts(y_red[,8:10],start=c(1980,2), frequency=4)

# Generar las exógenas determinísticas (const, trend y dummies)
T <- nrow(y)
trend <- seq(1:T) # Tendencia
const =  rep(1,T)  # Constante
x=ts(cbind(const,trend),start=c(1980.2), frequency=4)
View(x)

# Cargar la función HD_SVAR y correrla

# ============================================================
#       Ejemplo 1 - Identificación de Cholesky
# ============================================================

# Inicializo los parámetros correspondientes
Out_Chol=HD_SVAR(endogenas=y,exogenas=x,p=8, ident="Cholesky") 
# Verificar las salidas de cada serie a partir de cada shock
# Out_Chol$y_only_k , donde k es el shock
head(Out_Chol$y_only_dln_pbi,10)
head(Out_Chol$y_only_dln_tcr,10)
head(Out_Chol$y_only_dln_ipc,10)

# ============================================================
#       Ejemplo 2 -  VAR Estructural (SVAR)
# ============================================================
# Cargo las matrices A y B
amat <- matrix(c(NA,0,NA,NA,NA,0,0,0,NA),nrow=3,byrow=T)
amat
# Es un VAR sobreidentificado
bmat=diag(3)
diag(bmat)=NA
bmat

Out_SVAR=HD_SVAR(endogenas=y,exogenas=x,p=8,inicio = start(y),
        frecuencia = frequency(y),ident ="SVAR",Amat = amat,
        Bmat = bmat,estmethod ="scoring", verbose=F)
head(Out_SVAR$y_only_dln_pbi,10)
head(Out_SVAR$y_only_dln_tcr,10)
head(Out_SVAR$y_only_dln_ipc,10)

# ============================================================
#       Ejemplo 3 -  Identificación Blanchard-Quah
# ============================================================