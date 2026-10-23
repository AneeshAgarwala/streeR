#' Predict method for `stree` objects
#'
#' Predict class labels or class probabilities for new data from a fitted
#' [stree()] model.
#'
#' @param object A fitted model of class `"stree"`, as returned by
#'   [stree()].
#' @param newdata A data frame of new observations containing (at least) the
#'   predictor columns used to fit `object`.
#' @param type Either `"class"` (default) to return predicted class labels,
#'   or `"prob"` to return a matrix of class membership probabilities.
#' @param ... Currently unused; included for S3 method consistency.
#'
#' @return If `type = "class"`, a factor of predicted class labels with the
#'   same levels as the training response, one per row of `newdata`. If
#'   `type = "prob"`, a numeric matrix with one row per observation in
#'   `newdata` and one column per class, each row summing to 1.
#'
#' @examples
#' fit <- stree(Species ~ ., data = iris, max_depth = 3)
#' predict(fit, iris[1:5, ])
#' predict(fit, iris[1:5, ], type = "prob")
#'
#' @export
predict.stree <- function(object, newdata, type = c("class", "prob"), ...) {
  type <- match.arg(type)

  if (missing(newdata)) {
    stop("`newdata` must be supplied.", call. = FALSE)
  }
  if (!is.data.frame(newdata)) {
    newdata <- as.data.frame(newdata)
  }

  newdata <- apply_xlevels(newdata, object$xlevels)

  result <- stree_predict(object$tree, newdata, return_probs = TRUE)

  n <- nrow(newdata)
  if (n > 1) {
    pred_classes <- result$predictions
    probs <- result$probabilities
  } else {
    pred_classes <- result$prediction
    probs <- matrix(result$probabilities, nrow = 1)
  }

  if (type == "class") {
    factor(pred_classes, levels = object$levels)
  } else {
    colnames(probs) <- object$levels
    probs
  }
}
