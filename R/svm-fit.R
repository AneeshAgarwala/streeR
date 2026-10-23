# Internal binary SVM split fitter ---------------------------------------
# Not exported; implementation detail of stree_split().
#
# Fits a single e1071::svm() hyperplane separating a (possibly one-vs-rest)
# binary label vector `y`, after coercing factor/character predictors to
# numeric, dropping zero-variance columns, and optionally scaling. Returns
# the fitted model together with the left/right observation indices implied
# by the sign of the decision function.
stree_fit_binary_svm <- function(X, y, kernel, verbose = FALSE,
                                  use_scaling = TRUE, scaling_params = NULL, ...) {
  empty_result <- list(
    model = NULL, left_idx = NULL, right_idx = NULL,
    scaling_params = NULL, used_features = NULL
  )

  if (nrow(X) == 0 || length(y) == 0) {
    if (verbose) message("Empty data, cannot fit SVM")
    return(empty_result)
  }

  X_processed <- X
  for (col in names(X_processed)) {
    if (is.factor(X_processed[[col]])) {
      X_processed[[col]] <- as.numeric(X_processed[[col]])
    } else if (is.character(X_processed[[col]])) {
      X_processed[[col]] <- as.numeric(as.factor(X_processed[[col]]))
    }
  }

  col_vars <- sapply(X_processed, function(col) {
    if (is.numeric(col)) stats::var(col, na.rm = TRUE) else 1
  })
  non_constant_cols <- col_vars > 1e-10

  if (sum(non_constant_cols) == 0) {
    if (verbose) message("All features are constant, cannot fit SVM")
    return(empty_result)
  }

  X_filtered <- X_processed[, non_constant_cols, drop = FALSE]

  if (use_scaling) {
    if (is.null(scaling_params)) {
      scaled_result <- standard_scaler(X_filtered,
        center = TRUE, scale_var = TRUE, return_params = TRUE
      )
      X_scaled <- scaled_result$data
      scaling_params <- scaled_result$params
    } else {
      X_scaled <- standard_scaler(X_filtered, params = scaling_params)
    }
  } else {
    X_scaled <- X_filtered
    scaling_params <- NULL
  }

  svm_kernel <- switch(kernel,
    "linear" = "linear",
    "polynomial" = "polynomial",
    "radial" = "radial",
    "linear"
  )

  model <- tryCatch(
    {
      e1071::svm(
        x = X_scaled,
        y = y,
        kernel = svm_kernel,
        scale = FALSE,
        decision.values = TRUE,
        tolerance = 0.0001,
        ...
      )
    },
    error = function(e) {
      if (verbose) message("SVM fitting failed: ", e$message)
      NULL
    }
  )

  if (is.null(model)) {
    return(empty_result)
  }

  dec_values <- attr(
    stats::predict(model, X_scaled, decision.values = TRUE),
    "decision.values"
  )

  if (is.matrix(dec_values)) {
    dec_values <- dec_values[, 1]
  } else {
    dec_values <- as.numeric(dec_values)
  }

  left_idx <- which(dec_values > 0)
  right_idx <- which(dec_values <= 0)

  list(
    model = model,
    left_idx = left_idx,
    right_idx = right_idx,
    scaling_params = scaling_params,
    used_features = names(X_filtered)
  )
}
