#************************************************
#*        DESESTACIONALIZACIÓN DE SERIES
#************************************************
# Cargar paquete
# install.packages("seasonal")
library(seasonal)

# Cargar serie de PIB de Argentina, de 1980 a 2007
library(readxl)
PIB_Arg_1980_2007 <- read_excel("PIB Arg 1980 - 2007.xlsx")
View(PIB_Arg_1980_2007)
l_pib=ts(PIB_Arg_1980_2007$LOG_PIB,frequency=4, start=c(1980,1))
# Desestacionalizar
modelo <- seas(l_pib)

# Ver resultados
summary(modelo)

# Serie desestacionalizada
l_pib_sa <- final(modelo)

# Graficar
windows()
plot(l_pib, col = "gray", lwd = 2, main = "PIB original vs desestacionalizado")
lines(l_pib_sa, col = "blue", lwd = 2)
legend("topleft", legend = c("Original", "Desestacionalizado"), col = c("gray", "blue"), lty = 1, lwd = 2)
dev.off()
head(cbind(l_pib,l_pib_sa),6)

#************************************************
#*        FILTROS MACROECONÓMICOAS
#************************************************
# install.packages("mFilter")
# Cargo las librerías
library(mFilter)
# 1.1 Utilizamos la serie de PIB a valores constantes de Argentina
# ya desestacionalizada

# Graficamos
plot.ts(l_pib_sa)

# 1.2 Destendenciar con un filtro lineal
# Esto sería regresar la serie contra una constante y una tendencia
lin.mod <- lm(l_pib_sa ~ time(l_pib_sa))
lin.trend <- lin.mod$fitted.values  
# Los valores ajustados pertenecen a una recta
linear <- ts(lin.trend, start = c(1980, 1), frequency = 4)  
# Crea una serie de tiempo para la tendencia
lin.cycle <- l_pib_sa - linear  
# cycle es la diferencia entre los datos y la tendencia lineal
# Graficamos  la tendencia y el ciclo 

windows()
plot.ts(l_pib_sa, ylab = "")  
lines(linear, col = "red")  
legend("topleft", legend = c("data", "trend"), lty = 1, 
       col = c("black", "red"), bty = "n")
dev.off()

windows()
plot.ts(lin.cycle, ylab = "")  
# Genero la linea de base
base=lin.cycle*0
lines(base, col = "red")
legend("topright", legend = c("cycle    "), lty = 1, col = c("black"), 
       bty = "n")
dev.off()

# 1.3 Destendenciar con el filtro de Hodrick-Prescott
# El valor elegido de Lambda es 1600
hp.decom <- hpfilter(l_pib_sa, freq = 1600, type = "lambda")

windows()
plot.ts(l_pib_sa, ylab = "")  # plotea la serie
lines(hp.decom$trend, col = "red")  # incluye la tendencia HP
legend("topleft", legend = c("data", "HPtrend"), lty = 1, 
    col = c("black", "red"), bty = "n")
dev.off()

windows()
plot.ts(hp.decom$cycle, ylab = "")  # plot cycle
base=l_pib_sa*0
lines(base, col = "red")
legend("topleft", legend = c("HPcycle"), lty = 1, col = c("black"), 
    bty = "n")
dev.off()

# 1.4 Destendenciando con el filtro de Baxter-King
# Necesitamos especificar la frecuencia de la banda para el ciclo
# entre 6 y 32
bk.decom <- bkfilter(l_pib_sa, pl = 6, pu = 32)

windows()
plot.ts(l_pib_sa, ylab = "")
lines(bk.decom$trend, col = "red")
legend("topleft", legend = c("data", "BKtrend"), lty = 1, 
    col = c("black", "red"), bty = "n")
dev.off()

windows()
plot.ts(bk.decom$cycle, ylab = "")
base=l_pib_sa*0
lines(base, col = "red")
legend("topleft", legend = c("BPcycle"), lty = 1, col = c("black"), 
    bty = "n")
dev.off()

# 1.5 Destendenciando con el filtro de Christiano-Fitzgerald
# Es similar al anterior
cf.decom <- cffilter(l_pib_sa, pl = 6, pu = 32, root = TRUE)

windows()
plot.ts(l_pib_sa, ylab = "")
lines(cf.decom$trend, col = "red")
legend("topleft", legend = c("data", "CFtrend"), lty = 1, 
    col = c("black", "red"), bty = "n")
dev.off()

windows()
plot.ts(cf.decom$cycle, ylab = "")
base=l_pib_sa*0
lines(base, col = "red")
legend("topleft", legend = c("CFcycle"), lty = 1, col = c("black"), 
    bty = "n")
dev.off()

comb <- ts.union(lin.cycle, hp.decom$cycle, bk.decom$cycle, 
    cf.decom$cycle)

# par(mfrow = c(1, 1), mar = c(2.2, 2.2, 2, 1), cex = 0.8)
windows()
plot.ts(comb, ylab = "", plot.type = "single", col = c("blue", 
    "red", "darkgrey", "sienna"))
legend("topleft", legend = c("linear", "hp-filter", "bk-filter", 
    "cf-filter"), lty = 1, col = c("blue", 
    "red", "darkgrey", "sienna"), bty = "n")
dev.off()
