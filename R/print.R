#' Print method for `stree` objects
#'
#' Prints the call, class levels, and a depth-first sketch of the fitted
#' tree, showing at each internal node the one-vs-rest class used for the
#' split (in the multi-class case) and its weighted impurity, and at each
#' leaf the predicted class and number of training observations.
#'
#' @param x A fitted model of class `"stree"`.
#' @param show_probs Logical; if `TRUE`, also print each leaf's class
#'   probability vector.
#' @param ... Currently unused; included for S3 method consistency.
#'
#' @return `x`, invisibly. Called for its side effect of printing to the
#'   console.
#'
#' @examples
#' fit <- stree(Species ~ ., data = iris, max_depth = 3)
#' print(fit)
#' print(fit, show_probs = TRUE)
#'
#' @export
print.stree <- function(x, show_probs = FALSE, ...) {
  cat("STree oblique classification tree\n")
  cat("Call:  ", deparse(x$call), "\n", sep = "")
  cat("Classes:", paste(x$levels, collapse = ", "), "\n")
  cat("Kernel: ", x$kernel, "\n\n", sep = "")
  stree_print_node(x$tree, show_probs = show_probs)
  invisible(x)
}

#' Summary method for `stree` objects
#'
#' @param object A fitted model of class `"stree"`.
#' @param ... Currently unused; included for S3 method consistency.
#'
#' @return `object`, invisibly. Called for its side effect of printing a
#'   short summary (number of classes, kernel, tree depth and number of
#'   leaves) to the console.
#'
#' @examples
#' fit <- stree(Species ~ ., data = iris, max_depth = 3)
#' summary(fit)
#'
#' @export
summary.stree <- function(object, ...) {
  stats <- stree_tree_stats(object$tree)

  cat("STree oblique classification tree\n")
  cat("Call:          ", deparse(object$call), "\n", sep = "")
  cat("Response:      ", object$response_name, "\n", sep = "")
  cat("Classes:       ", paste(object$levels, collapse = ", "), "\n", sep = "")
  cat("Kernel:        ", object$kernel, "\n", sep = "")
  cat("Impurity used: ", object$impurity_measure, "\n", sep = "")
  cat("Tree depth:    ", stats$max_depth, " (max_depth = ", object$max_depth, ")\n", sep = "")
  cat("Number of leaves:", stats$leaves, "\n")
  cat("Number of nodes: ", stats$nodes, "\n", sep = "")

  invisible(object)
}
