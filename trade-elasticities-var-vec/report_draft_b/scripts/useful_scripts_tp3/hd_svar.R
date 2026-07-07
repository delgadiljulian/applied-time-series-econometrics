# ============================================================
# HD_SVAR(): Historical Decomposition para VAR/SVAR con línea base
# ============================================================
# USO:
#   out <- HD_SVAR(endogenas, exogenas, p,
#                  inicio = start(endogenas), frecuencia = frequency(endogenas),
#                  ident = c("BQ","Cholesky","SVAR"),
#                  Amat = NULL, Bmat = NULL, estmethod = c("scoring","direct"),
#                  shock_names = NULL,
#                  verbose = TRUE, ...)
#
# ENTRADAS:
#   endogenas : ts (T x K) con variables endógenas (en diferencias o niveles)
#   exogenas  : ts (T x m) con exógenas determinísticas (const, trend, dummies, etc.)
#   p         : rezagos del VAR
#   inicio, frecuencia : para formatear la salida como ts
#   ident     : "BQ" (Blanchard-Quah via vars::BQ),
#               "Cholesky" (impacto por Cholesky de Sigma_u),
#               "SVAR" (vars::SVAR con Amat/Bmat)
#   Amat,Bmat : matrices de restricciones para vars::SVAR (solo si ident="SVAR")
#   estmethod : "scoring" o "direct" (solo si ident="SVAR")
#   shock_names : nombres para shocks (si NULL, usa colnames(endogenas))
#   verbose   : imprime summaries/validación
#   ...       : argumentos extra para vars::SVAR (p.ej. max.iter, conv.crit, method, hessian)
#
# SALIDA (misma “forma” que venías usando):
#   $v        : VAR estimado (vars::VAR)
#   $B        : matriz de impacto contemporáneo (u_t = B * eps_t)
#   $Eps      : shocks estructurales (T-p x K), Var(eps)=I
#   $y_base   : trayectoria sin shocks (línea base)
#   $y_recon  : reconstrucción usando todos los shocks
#   $y_only_<shock>    : contrafactual usando solo el shock <shock>
#   $contrib_<shock>   : contribución del shock <shock> (y_only - y_base)
#   $y_rec_from_hd     : y_base + suma_k contrib_k  (chequeo)
#
HD_SVAR <- function(endogenas,
                    exogenas,
                    p,
                    inicio = start(endogenas),
                    frecuencia = frequency(endogenas),
                    ident = c("BQ", "Cholesky", "SVAR"),
                    Amat = NULL,
                    Bmat = NULL,
                    estmethod = c("scoring", "direct"),
                    shock_names = NULL,
                    verbose = TRUE,
                    ...) {
  if (!requireNamespace("vars", quietly = TRUE)) {
    stop("Falta el paquete 'vars'. Instalalo con install.packages('vars').")
  }

  ident <- match.arg(ident)
  estmethod <- match.arg(estmethod)

  y <- endogenas
  X <- exogenas

  if (is.null(dim(y)) || ncol(y) < 2) stop("endogenas debe ser un ts/matriz con K>=2 columnas.")
  if (is.null(dim(X))) stop("exogenas debe ser un ts/matriz con al menos 1 columna.")
  if (nrow(X) != nrow(y)) stop("endogenas y exogenas deben tener el mismo número de filas (T).")
  if (p < 1) stop("p debe ser >= 1.")

  # -------------------------
  # 1) VAR reducido
  # -------------------------
  v <- vars::VAR(y, p = p, type = "none", exogen = X)
  if (verbose) print(summary(v))

  U <- as.matrix(resid(v))                 # (T-p) x K
  K <- ncol(U)
  Sigma_u <- cov(U)

  # -------------------------
  # 2) Identificación -> B (impacto): u_t = B eps_t, Var(eps)=I
  # -------------------------
  if (ident == "BQ") {
    bq_obj <- vars::BQ(v)
    if (verbose) print(summary(bq_obj))
    if (!is.null(bq_obj$B)) {
      B <- as.matrix(bq_obj$B)
    } else if (!is.null(bq_obj$Bmat)) {
      B <- as.matrix(bq_obj$Bmat)
    } else {
      stop("No encuentro la matriz de impacto en el objeto BQ (esperaba $B o $Bmat).")
    }
  } else if (ident == "Cholesky") {
    # Sigma_u = L L' con L triangular inferior => u = L eps
    B <- t(chol(Sigma_u))
  } else { # SVAR
    svar_obj <- vars::SVAR(v, estmethod = estmethod, Amat = Amat, Bmat = Bmat, ...)
    if (verbose) print(summary(svar_obj))
    if (is.null(svar_obj$A) || is.null(svar_obj$B)) {
      stop("El objeto SVAR no trae matrices $A y $B como se esperaba.")
    }
    Ahat <- as.matrix(svar_obj$A)
    Bhat <- as.matrix(svar_obj$B)
    B <- solve(Ahat) %*% Bhat
  }

  if (!all(dim(B) == c(K, K))) stop("La matriz B (impacto) no es KxK. Revisar identificación.")

  # Shocks estructurales: eps = B^{-1} u
  Eps <- t(solve(B, t(U)))                 # (T-p) x K
  colnames(Eps) <- colnames(y)

  # -------------------------
  # 3) Coefs de exógenas para baseline
  # -------------------------
  coef_list <- lapply(v$varresult, coef)
  xnames <- colnames(X)

  beta_mat <- sapply(coef_list, function(b) b[xnames])  # m x K
  if (any(is.na(beta_mat))) {
    stop("No pude recuperar coeficientes de exógenas por nombre. Revisá colnames(exogenas) y nombres de coef().")
  }
  beta <- t(beta_mat)  # K x m

  get_exog_term <- function(beta, X_t) beta %*% matrix(X_t, ncol = 1)

  # -------------------------
  # 4) Simulación contrafactual
  # -------------------------
  simulate_counterfactual <- function(v, B, Eps, X, beta,
                                      mode = c("none", "only_one", "all"),
                                      shock_idx = 1) {
    mode <- match.arg(mode)

    A <- vars::Acoef(v)            # lista A1..Ap
    K <- ncol(v$y)
    p <- length(A)

    y_full <- as.matrix(v$y)       # Tfull x K
    Tfull <- nrow(y_full)

    # shocks alineados a longitud Tfull
    eps_full <- matrix(0, nrow = Tfull, ncol = K)
    eps_full[(p + 1):Tfull, ] <- Eps

    if (mode == "none") {
      eps_use <- matrix(0, nrow = Tfull, ncol = K)
    } else if (mode == "all") {
      eps_use <- eps_full
    } else {
      eps_use <- matrix(0, nrow = Tfull, ncol = K)
      eps_use[, shock_idx] <- eps_full[, shock_idx]
    }

    y_sim <- matrix(NA_real_, nrow = Tfull, ncol = K)
    colnames(y_sim) <- colnames(y_full)
    y_sim[1:p, ] <- y_full[1:p, ]

    for (t in (p + 1):Tfull) {
      yhat <- get_exog_term(beta, X[t, ])

      for (i in 1:p) {
        yhat <- yhat + A[[i]] %*% matrix(y_sim[t - i, ], ncol = 1)
      }

      yhat <- yhat + B %*% matrix(eps_use[t, ], ncol = 1)
      y_sim[t, ] <- as.numeric(yhat)
    }

    ts(y_sim, start = start(v$y), frequency = frequency(v$y))
  }

  # Baseline y reconstrucción completa
  y_base  <- simulate_counterfactual(v, B, Eps, X, beta, mode = "none")
  y_recon <- simulate_counterfactual(v, B, Eps, X, beta, mode = "all")

  # -------------------------
  # 5) y_only_k y contrib_k para cualquier K
  # -------------------------
  if (is.null(shock_names)) shock_names <- colnames(endogenas)
  shock_names <- make.names(shock_names, unique = TRUE)
  if (length(shock_names) != K) stop("shock_names debe tener longitud K (ncol(endogenas)).")

  y_only_list <- vector("list", K)
  contrib_list <- vector("list", K)

  for (j in 1:K) {
    y_only_list[[j]] <- simulate_counterfactual(v, B, Eps, X, beta, mode = "only_one", shock_idx = j)
    contrib_list[[j]] <- y_only_list[[j]] - y_base
  }

  y_rec_from_hd <- y_base + Reduce(`+`, contrib_list)

  # -------------------------
  # 6) Validación simple (opcional)
  # -------------------------
  y_obs <- v$y
  err <- y_recon - y_obs
  if (verbose) {
    cat("================================================\n")
    cat("   Errores en la reconstrucción de las series\n")
    cat("================================================\n")
    print(apply(abs(err)[(p + 1):nrow(err), , drop = FALSE], 2, max, na.rm = TRUE))
  }

  # -------------------------
  # 7) Salida "plana" como antes
  # -------------------------
  to_ts <- function(x) ts(x, start = inicio, frequency = frecuencia)
  cols <- colnames(endogenas)
  fix_cols <- function(z) { colnames(z) <- cols; z }

  out <- list(
    v = v,
    B = B,
    Eps = Eps,
    y_base = to_ts(fix_cols(y_base)),
    y_recon = to_ts(fix_cols(y_recon)),
    y_rec_from_hd = to_ts(fix_cols(y_rec_from_hd))
  )

  for (j in 1:K) {
    nm <- shock_names[j]
    out[[paste0("y_only_", nm)]]  <- to_ts(fix_cols(y_only_list[[j]]))
    out[[paste0("contrib_", nm)]] <- to_ts(fix_cols(contrib_list[[j]]))
  }

  class(out) <- "HD_SVAR"
  invisible(out)
}
