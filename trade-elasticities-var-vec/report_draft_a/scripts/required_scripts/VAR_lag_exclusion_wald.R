# Función para realizar un test de exclusión
# sobre cada lag del VAR. Ecuación por ecuación
# y significatividad conjunta del lag completo
# Hecho con IA (ChatGPT)
# El argumento es un Var estimado con el paquete "vars"

# Instala el paquete vars si no está ya instalado
# si ya está , sólo lo actualiza con library()
if (!requireNamespace("vars", quietly = TRUE)) {
  install.packages("vars")
}
library(vars)
#************************************************
VAR_lag_exclusion_wald <- function(v, digits = 6) {

  if (!inherits(v, "varest")) {
    stop("El objeto debe venir de vars::VAR().")
  }

  dat <- as.data.frame(v$datamat)
  K <- v$K
  p <- v$p

  y_names <- names(v$varresult)

  Y_dep <- as.matrix(dat[, 1:K, drop = FALSE])
  X <- as.matrix(dat[, -(1:K), drop = FALSE])

  T_eff <- nrow(X)
  m <- ncol(X)

  # Grados de libertad residuales por ecuación.
  # Esto es clave para replicar EViews.
  df_resid <- df.residual(v$varresult[[1]])

  XtX_inv <- solve(crossprod(X))
  B_hat <- XtX_inv %*% crossprod(X, Y_dep)

  U_hat <- Y_dep - X %*% B_hat

  # Covarianza residual con corrección por grados de libertad, estilo EViews
  Sigma_u <- crossprod(U_hat) / df_resid

  stat <- matrix(NA_real_, nrow = p, ncol = K + 1)
  pval <- matrix(NA_real_, nrow = p, ncol = K + 1)

  colnames(stat) <- c(y_names, "Joint")
  colnames(pval) <- c(y_names, "Joint")
  rownames(stat) <- paste("Lag", 1:p)
  rownames(pval) <- paste("Lag", 1:p)

  for (lag_test in 1:p) {

    lag_rows <- grep(paste0("\\.l", lag_test, "$"), colnames(X))

    if (length(lag_rows) == 0) {
      stop(paste("No encontré regresores para el lag", lag_test))
    }

    # ------------------------------------------------------------
    # Tests ecuación por ecuación
    # ------------------------------------------------------------
    for (eq in 1:K) {

      b <- B_hat[lag_rows, eq, drop = FALSE]

      sigma2_eq <- crossprod(U_hat[, eq]) / df_resid
      V_eq <- as.numeric(sigma2_eq) * XtX_inv[lag_rows, lag_rows, drop = FALSE]

      W_eq <- as.numeric(t(b) %*% solve(V_eq) %*% b)
      df_eq <- length(lag_rows)

      stat[lag_test, eq] <- W_eq
      pval[lag_test, eq] <- pchisq(W_eq, df = df_eq, lower.tail = FALSE)
    }

    # ------------------------------------------------------------
    # Test conjunto del sistema: H0: A_lag = 0
    # ------------------------------------------------------------
    V_beta <- kronecker(Sigma_u, XtX_inv)
    beta_vec <- as.vector(B_hat)

    idx <- as.vector(sapply(1:K, function(eq) {
      lag_rows + (eq - 1) * m
    }))

    R <- diag(length(beta_vec))[idx, , drop = FALSE]

    r_beta <- R %*% beta_vec
    RVR <- R %*% V_beta %*% t(R)

    W_joint <- as.numeric(t(r_beta) %*% solve(RVR) %*% r_beta)
    df_joint <- length(idx)

    stat[lag_test, "Joint"] <- W_joint
    pval[lag_test, "Joint"] <- pchisq(W_joint, df = df_joint, lower.tail = FALSE)
  }

  out <- list(
    statistic = stat,
    p.value = pval,
    K = K,
    p = p,
    T_eff = T_eff,
    df_resid = df_resid,
    digits = digits,
    call = v$call
  )

  class(out) <- "VAR_lag_exclusion_wald"
  return(out)
}
#********************************************


print.VAR_lag_exclusion_wald <- function(x, ...) {

  digits <- x$digits
  stat <- x$statistic
  pval <- x$p.value

  cat("\nVAR Lag Exclusion Wald Tests\n")
  cat("Date:", format(Sys.Date(), "%m/%d/%y"),
      " Time:", format(Sys.time(), "%H:%M"), "\n")
  cat("Included observations:", x$T_eff, "after adjustments\n")
  cat("\n")
  cat("Chi-squared test statistics for lag exclusion:\n")
  cat("Numbers in [ ] are p-values\n")
  cat("\n")

  cn <- colnames(stat)

  first_col_width <- 10
  num_width <- 14

  cat(strrep("=", first_col_width + num_width * length(cn)), "\n", sep = "")

  cat(sprintf("%-*s", first_col_width, ""))
  for (j in seq_along(cn)) {
    cat(sprintf("%*s", num_width, cn[j]))
  }
  cat("\n")

  cat(strrep("=", first_col_width + num_width * length(cn)), "\n", sep = "")

  for (i in seq_len(nrow(stat))) {

    cat(sprintf("%-*s", first_col_width, rownames(stat)[i]))

    for (j in seq_len(ncol(stat))) {
      cat(sprintf("%*.6f", num_width, stat[i, j]))
    }
    cat("\n")

    cat(sprintf("%-*s", first_col_width, ""))

    for (j in seq_len(ncol(pval))) {
      cat(sprintf("%*s", num_width, paste0("[ ", formatC(pval[i, j], format = "f", digits = 4), "]")))
    }
    cat("\n\n")
  }

  invisible(x)
}

# Ejemplo
# data(Canada)
# v=VAR(Canada, p = 2, type = "const")
# VAR_lag_exclusion_wald(v)
