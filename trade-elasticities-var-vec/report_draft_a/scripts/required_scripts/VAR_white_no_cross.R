#********************************************************
#              VAR_WHITE_NO_CROSS
#   White heteroskedasticity test without cross terms
#********************************************************

# This helper recreates a White-type diagnostic for VAR residuals. It avoids
# cross-products by using the VAR regressors and their squares only.
#
# Arguments
# var_reg: output from vars::VAR()
#
# Returns
# data.frame with LM statistic, degrees of freedom and p-value by equation.

VAR_white_no_cross <- function(var_reg) {
  if (is.null(var_reg$varresult) || is.null(var_reg$y)) {
    stop("var_reg must be an object returned by vars::VAR().")
  }

  # La auxiliar usa regresores del VAR y sus cuadrados, sin productos cruzados.
  build_auxiliary_matrix <- function(model_matrix) {
    intercept_cols <- apply(model_matrix, 2, function(x) length(unique(x)) == 1)
    base_matrix <- model_matrix[, !intercept_cols, drop = FALSE]

    squared_matrix <- base_matrix^2
    colnames(squared_matrix) <- paste0(colnames(base_matrix), "_sq")

    auxiliary_matrix <- cbind(base_matrix, squared_matrix)
    auxiliary_matrix <- auxiliary_matrix[, apply(auxiliary_matrix, 2, stats::var) > 0, drop = FALSE]
    auxiliary_matrix <- auxiliary_matrix[, !duplicated(as.data.frame(t(auxiliary_matrix))), drop = FALSE]
    auxiliary_matrix
  }

  # El df efectivo sale del rango de la matriz auxiliar para evitar duplicados.
  run_equation_test <- function(residual_vector, auxiliary_matrix, equation_name) {
    auxiliary_data <- data.frame(
      residual_sq = as.numeric(residual_vector)^2,
      auxiliary_matrix,
      check.names = TRUE
    )

    complete_rows <- stats::complete.cases(auxiliary_data)
    auxiliary_data <- auxiliary_data[complete_rows, , drop = FALSE]

    if (nrow(auxiliary_data) <= 2 || ncol(auxiliary_data) <= 1) {
      return(data.frame(
        equation = equation_name,
        statistic = NA_real_,
        df = NA_integer_,
        p_value = NA_real_,
        n_obs = nrow(auxiliary_data),
        n_auxiliary_terms = 0L,
        conclusion_5pct = "insufficient auxiliary regression",
        note = "Auxiliary regression could not be estimated.",
        stringsAsFactors = FALSE
      ))
    }

    auxiliary_fit <- stats::lm(residual_sq ~ ., data = auxiliary_data)
    r_squared <- summary(auxiliary_fit)$r.squared
    effective_df <- auxiliary_fit$rank - 1L
    statistic <- nrow(auxiliary_data) * r_squared
    p_value <- stats::pchisq(statistic, df = effective_df, lower.tail = FALSE)

    data.frame(
      equation = equation_name,
      statistic = statistic,
      df = effective_df,
      p_value = p_value,
      n_obs = nrow(auxiliary_data),
      n_auxiliary_terms = ncol(auxiliary_data) - 1L,
      conclusion_5pct = ifelse(
        p_value < 0.05,
        "reject homoskedasticity",
        "do not reject homoskedasticity"
      ),
      note = ifelse(
        p_value < 0.05,
        "White no-cross flags heteroskedastic residual variance.",
        "No strong White no-cross evidence of heteroskedasticity."
      ),
      stringsAsFactors = FALSE
    )
  }

  model_matrix <- stats::model.matrix(var_reg$varresult[[1]])
  auxiliary_matrix <- build_auxiliary_matrix(model_matrix)

  equation_results <- do.call(
    rbind,
    lapply(names(var_reg$varresult), function(equation_name) {
      run_equation_test(
        residual_vector = stats::resid(var_reg$varresult[[equation_name]]),
        auxiliary_matrix = auxiliary_matrix,
        equation_name = equation_name
      )
    })
  )

  valid_rows <- stats::complete.cases(equation_results[, c("statistic", "df")])
  joint_statistic <- sum(equation_results$statistic[valid_rows])
  joint_df <- sum(equation_results$df[valid_rows])
  joint_p_value <- stats::pchisq(joint_statistic, df = joint_df, lower.tail = FALSE)

  # La fila joint resume la evidencia de heterocedasticidad del sistema.
  joint_result <- data.frame(
    equation = "joint",
    statistic = joint_statistic,
    df = joint_df,
    p_value = joint_p_value,
    n_obs = unique(equation_results$n_obs)[1],
    n_auxiliary_terms = sum(equation_results$n_auxiliary_terms[valid_rows]),
    conclusion_5pct = ifelse(
      joint_p_value < 0.05,
      "reject homoskedasticity",
      "do not reject homoskedasticity"
    ),
    note = ifelse(
      joint_p_value < 0.05,
      "Joint White no-cross evidence suggests heteroskedasticity.",
      "Joint White no-cross evidence does not reject homoskedasticity."
    ),
    stringsAsFactors = FALSE
  )

  output <- rbind(equation_results, joint_result)
  rownames(output) <- NULL
  output
}
