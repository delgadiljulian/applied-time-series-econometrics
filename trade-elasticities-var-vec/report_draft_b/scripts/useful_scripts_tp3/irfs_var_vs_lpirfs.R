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
windows()
ts.plot(ts.level.lu)
# Vemos que las series tienen tendencia e intercepto
ts.plot(diff(ts.level.lu))
dev.off()

#***********************************************
#    PROYECCIONES LOCALES
#***********************************************
library(lpirfs)
# Load data set
dif.lu=subset(lu, !is.na(dln_inc))
endog = dif.lu[,c("dln_inc","dln_consump")]
# Estimate linear model
results_lin <- lp_lin(endog          = endog,
                      lags_endog_lin = 2,
                      trend          = 0,
                      shock_type     = 0,  # Indica shockar con 1 SD
                      confint        = 1.96,
                      hor            = 12)
# Show impulse responses
plot(results_lin)
# Show OLS diagnostics for the first shock of the first horizon
summary(results_lin)[[1]][1]

#***********************************************
#     Estimación del  VAR
#***********************************************
dif.lu=subset(lu, !is.na(dln_inc))
dif.lu=dif.lu[,c("dln_inc","dln_consump","dln_inv")]
# Convertimos en un objeto "series de tiempo"
ts.dif.lu=ts(dif.lu,start=c(1960,2),frequency=4)
var1=VAR(ts.dif.lu[,1:2],type="const", p=2)

#############################################
#       Funciones de impulso respuesta VAR
############################################
irf.1=irf(var1, n.ahead=12)
windows()
plot(irf.1)
dev.off()
