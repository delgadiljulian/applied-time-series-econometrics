# Este programa implementa el test de White
# para el caso de un modelo VAR, estimado
# con el paquete "vars"
# Toma el caso sin términos cruzados
# Hecho con IA (ChatGPT)
# El argumento es un Var estimado con el paquete "vars"

# Instala el paquete vars si no está ya instalado
# si ya está , sólo lo actualiza con library()
if (!requireNamespace("vars", quietly = TRUE)) {
  install.packages("vars")
}
library(vars)
#***********************************************
VAR_white_no_cross <- function(v) {
  
  if (!inherits(v, "varest")) {
    stop("El objeto debe venir de vars::VAR().")
  }
  
  if (!is.null(v$restrictions)) {
    stop("La función no está pensada para VAR con restricciones.")
  }
  
  if (!is.null(v$call$exogen)) {
    stop("Por ahora no uso VAR con variables exógenas, para mantener comparabilidad con EViews/het.test.")
  }
  
  if (!is.null(v$call$season)) {
    stop("Por ahora no uso VAR con dummies estacionales, para mantener comparabilidad con EViews/het.test.")
  }
  
  U <- residuals(v)
  U <- as.matrix(U)
  
  T_eff <- nrow(U)
  K <- ncol(U)
  q <- K * (K + 1) / 2
  
  # ------------------------------------------------------------
  # 1. Productos de residuos: vech(u_t u_t')
  # ------------------------------------------------------------
  
  Z <- matrix(NA_real_, nrow = T_eff, ncol = q)
  z_names <- character(q)
  
  counter <- 1
  for (i in 1:K) {
    for (j in i:K) {
      Z[, counter] <- U[, i] * U[, j]
      z_names[counter] <- paste0(colnames(U)[i], "*", colnames(U)[j])
      counter <- counter + 1
    }
  }
  
  colnames(Z) <- z_names
  
  # Centrado, como en la implementación de het.test
  Z <- scale(Z, center = TRUE, scale = FALSE)
  
  # ------------------------------------------------------------
  # 2. Regresores auxiliares: rezagos del VAR y sus cuadrados
  #    No cross terms
  # ------------------------------------------------------------
  
  dat <- as.data.frame(v$datamat)
  
  # En datamat las primeras K columnas son las dependientes;
  # el resto son regresores: rezagos + determinísticos.
  X0 <- dat[, -(1:K), drop = FALSE]
  
  # Sacamos constantes puras, porque la regresión auxiliar ya
  # incluye intercepto.
  is_constant <- vapply(
    X0,
    function(x) length(unique(x[!is.na(x)])) == 1,
    logical(1)
  )
  
  X_base <- X0[, !is_constant, drop = FALSE]
  
  if (ncol(X_base) == 0) {
    stop("No quedaron regresores auxiliares después de quitar constantes.")
  }
  
  X_sq <- as.data.frame(X_base^2)
  names(X_sq) <- paste0("sq(", names(X_base), ")")
  
  X_aux <- as.matrix(cbind(X_base, X_sq))
  X_aux <- cbind(const = 1, X_aux)
  
  m_aux <- ncol(X_aux) - 1
  
  # ------------------------------------------------------------
  # 3. Matriz de covarianza de los productos de residuos
  # ------------------------------------------------------------
  
  Omega_hat <- crossprod(Z) / (T_eff - K)
  
  # ------------------------------------------------------------
  # 4. Regresiones auxiliares
  # ------------------------------------------------------------
  
  qr_aux <- qr(X_aux)
  E_aux <- qr.resid(qr_aux, Z)
  
  Upsilon_hat <- crossprod(E_aux) / (T_eff - K)
  
  # ------------------------------------------------------------
  # 5. Estadístico de White / Doornik
  # ------------------------------------------------------------
  
  trace_term <- sum(diag(solve(Omega_hat, Upsilon_hat)))
  
  statistic <- as.numeric(T_eff * (q - trace_term))
  df <- m_aux * q
  p_value <- pchisq(statistic, df = df, lower.tail = FALSE)
  
  out <- list(
    statistic = statistic,
    parameter = df,
    p.value = p_value,
    method = "White heteroskedasticity test for VAR residuals, no cross terms",
    alternative = "Heteroskedasticity",
    null = "Homoskedasticity",
    T_eff = T_eff,
    K = K,
    q = q,
    m_aux = m_aux,
    residual_products = Z,
    auxiliary_regressors = X_aux,
    Omega_hat = Omega_hat,
    Upsilon_hat = Upsilon_hat,
    call = match.call()
  )
  
  class(out) <- "VAR_white_no_cross"
  return(out)
}

#**********************************************
print.VAR_white_no_cross <- function(x, digits = 4, ...) {
  
  cat("\nVAR Residual Heteroskedasticity Test\n")
  cat("White Test - No Cross Terms\n")
  cat("====================================\n\n")
  
  cat("H0: Homoskedasticity\n")
  cat("H1: Heteroskedasticity\n\n")
  
  cat("Included observations:", x$T_eff, "\n")
  cat("Number of equations:", x$K, "\n")
  cat("Number of residual products:", x$q, "\n")
  cat("Auxiliary regressors, excluding constant:", x$m_aux, "\n\n")
  
  cat("Joint test:\n")
  cat("  Chi-sq statistic:", formatC(x$statistic, format = "f", digits = digits), "\n")
  cat("  Degrees of freedom:", x$parameter, "\n")
  cat("  Prob.:", formatC(x$p.value, format = "f", digits = digits), "\n\n")
  
  invisible(x)
}

#****************************************

# Ejemplo:
# data(Canada)
# v=VAR(Canada, p = 2, type = "const")
# white_test <- VAR_white_no_cross(v)
# white_test
