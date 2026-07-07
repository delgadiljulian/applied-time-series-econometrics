#********************************************************
#              VAR_LAG_EXCLUSION_WALD
#   Wald tests for excluding each lag from a VAR system
#********************************************************

# This helper recreates the course-style VAR lag exclusion test. For each lag,
# it tests whether all coefficients attached to that lag are jointly zero across
# all VAR equations.
#
# Arguments
# var_reg: output from vars::VAR()
# lags: integer vector of lags to test; defaults to 1:var_reg$p
#
# Returns
# data.frame with Wald chi-square statistic, degrees of freedom and p-value.

# H0: el bloque de coeficientes del rezago evaluado es cero en todo el VAR.
VAR_lag_exclusion_wald <- function(var_reg, lags = NULL) {
  if (is.null(var_reg$varresult) || is.null(var_reg$y)) {
    stop("var_reg must be an object returned by vars::VAR().")
  }

  # La inversa por SVD evita fallos cuando la covarianza del bloque pierde rango.
  generalized_inverse <- function(matrix_x, tolerance = sqrt(.Machine$double.eps)) {
    svd_x <- svd(matrix_x)
    positive_values <- svd_x$d > tolerance * max(svd_x$d)

    if (!any(positive_values)) {
      stop("Covariance matrix has numerical rank zero.")
    }

    inverse_values <- rep(0, length(svd_x$d))
    inverse_values[positive_values] <- 1 / svd_x$d[positive_values]

    list(
      inverse = svd_x$v %*% diag(inverse_values, nrow = length(inverse_values)) %*%
        t(svd_x$u),
      rank = sum(positive_values)
    )
  }

  if (is.null(lags)) {
    lags <- seq_len(var_reg$p)
  }

  equation_names <- names(var_reg$varresult)
  n_equations <- length(var_reg$varresult)

  coefficient_matrix <- do.call(
    cbind,
    lapply(var_reg$varresult, stats::coef)
  )
  rownames(coefficient_matrix) <- names(stats::coef(var_reg$varresult[[1]]))
  colnames(coefficient_matrix) <- equation_names
  coefficient_names <- rownames(coefficient_matrix)

  # Se alinean columnas y coeficientes para testear solo parametros estimados.
  model_matrix_full <- stats::model.matrix(var_reg$varresult[[1]])
  model_matrix <- model_matrix_full[, coefficient_names, drop = FALSE]
  xtx_inverse <- generalized_inverse(crossprod(model_matrix))$inverse

  residual_matrix <- do.call(
    cbind,
    lapply(var_reg$varresult, stats::resid)
  )
  colnames(residual_matrix) <- equation_names
  sigma_u <- crossprod(residual_matrix) / stats::df.residual(var_reg$varresult[[1]])

  beta_vector <- as.vector(coefficient_matrix)
  beta_covariance <- kronecker(sigma_u, xtx_inverse)

  results <- lapply(lags, function(lag_i) {
    lag_pattern <- paste0("\\.l", lag_i, "$")
    lag_rows <- grep(lag_pattern, coefficient_names)

    if (length(lag_rows) == 0) {
      return(data.frame(
        lag = lag_i,
        statistic = NA_real_,
        df = NA_integer_,
        p_value = NA_real_,
        n_restrictions = 0L,
        conclusion_5pct = "lag not found in VAR coefficients",
        stringsAsFactors = FALSE
      ))
    }

    # Se apilan los coeficientes del rezago i para todas las ecuaciones del VAR.
    selected_positions <- unlist(lapply(seq_len(n_equations), function(eq_i) {
      (eq_i - 1L) * nrow(coefficient_matrix) + lag_rows
    }))

    selected_beta <- beta_vector[selected_positions]
    selected_covariance <- beta_covariance[
      selected_positions,
      selected_positions,
      drop = FALSE
    ]

    covariance_inverse <- generalized_inverse(selected_covariance)
    wald_statistic <- as.numeric(
      t(selected_beta) %*% covariance_inverse$inverse %*% selected_beta
    )
    degrees_freedom <- covariance_inverse$rank
    p_value <- stats::pchisq(
      wald_statistic,
      df = degrees_freedom,
      lower.tail = FALSE
    )

    data.frame(
      lag = lag_i,
      statistic = wald_statistic,
      df = degrees_freedom,
      p_value = p_value,
      n_restrictions = length(selected_beta),
      conclusion_5pct = ifelse(
        p_value < 0.05,
        "reject lag exclusion",
        "do not reject lag exclusion"
      ),
      note = ifelse(
        p_value < 0.05,
        "Lag jointly informative; do not exclude mechanically.",
        "Weak evidence against exclusion; compare with BIC and diagnostics."
      ),
      stringsAsFactors = FALSE
    )
  })

  # Se conservan restricciones nominales y df efectivo para auditar el Wald.
  output <- do.call(rbind, results)
  rownames(output) <- NULL
  output
}
