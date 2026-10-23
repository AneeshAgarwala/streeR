# Internal feature scaler ------------------------------------------------
# Not exported; implementation detail of stree().
#
# Centers and scales numeric columns to unit (population) variance, either
# computing the parameters from `X` (training time) or applying parameters
# computed earlier (prediction time), so that test data is scaled exactly
# the same way as the data used to fit each node's SVM.
standard_scaler <- function(X, center = TRUE, scale_var = TRUE,
                             return_params = FALSE, params = NULL) {
  if (is.matrix(X)) {
    X <- as.data.frame(X)
  }

  numeric_cols <- sapply(X, is.numeric)

  if (sum(numeric_cols) == 0) {
    warning("No numeric columns to scale")
    if (return_params) {
      return(list(data = X, params = list(means = NULL, sds = NULL)))
    } else {
      return(X)
    }
  }

  X_scaled <- X

  if (!is.null(params)) {
    for (col in names(params$means)) {
      if (col %in% names(X_scaled)) {
        if (center) {
          X_scaled[[col]] <- X_scaled[[col]] - params$means[[col]]
        }
        if (scale_var && params$sds[[col]] > 0) {
          X_scaled[[col]] <- X_scaled[[col]] / params$sds[[col]]
        }
      }
    }
    return(X_scaled)
  }

  means <- list()
  sds <- list()

  for (col in names(X_scaled)[numeric_cols]) {
    col_data <- X_scaled[[col]]
    col_data_clean <- col_data[!is.na(col_data)]

    if (length(col_data_clean) == 0) {
      means[[col]] <- 0
      sds[[col]] <- 1
      next
    }

    col_mean <- if (center) mean(col_data_clean, na.rm = TRUE) else 0
    means[[col]] <- col_mean

    if (scale_var) {
      if (length(col_data_clean) > 1) {
        # Population standard deviation (divide by n, not n - 1), to match
        # the scaling convention used by scikit-learn's StandardScaler.
        col_sd <- sqrt(sum((col_data_clean - col_mean)^2) / length(col_data_clean))
      } else {
        col_sd <- 1
      }
      if (col_sd == 0 || is.na(col_sd)) {
        col_sd <- 1
      }
    } else {
      col_sd <- 1
    }
    sds[[col]] <- col_sd

    if (center) {
      X_scaled[[col]] <- X_scaled[[col]] - col_mean
    }
    if (scale_var) {
      X_scaled[[col]] <- X_scaled[[col]] / col_sd
    }
  }

  if (return_params) {
    list(data = X_scaled, params = list(means = means, sds = sds))
  } else {
    X_scaled
  }
}
