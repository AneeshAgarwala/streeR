#' Fit an oblique classification tree based on support vector machines
#'
#' `stree()` grows a binary classification tree in which every internal
#' node's split is defined by a support vector machine (SVM) hyperplane
#' rather than a threshold on a single variable. When the response has more
#' than two classes, every class is tried in turn as a one-vs-rest binary
#' problem at each node, and the hyperplane that minimizes the chosen
#' impurity measure is kept as the split, following the 'STree' algorithm of
#' Montanana, Gamez and Puerta (2021).
#'
#' @param formula A [stats::formula] of the form `response ~ predictors`,
#'   e.g. `Species ~ .`. Predictors may be numeric, integer, factor or
#'   character; the response is coerced to a factor.
#' @param data A data frame containing the variables named in `formula`.
#' @param kernel Kernel used to fit every node's SVM: one of `"linear"`
#'   (default), `"polynomial"` or `"radial"`.
#' @param max_depth Maximum depth of the tree (root is depth 1).
#' @param min_samples Minimum number of observations required in a node,
#'   and in each of its children, for a split to be attempted. Nodes with
#'   fewer observations become leaves.
#' @param impurity_measure Impurity measure used to score one-vs-rest splits
#'   at multi-class nodes: `"entropy"` (default) or `"gini"`.
#' @param cost The SVM regularization parameter `C`, passed on to
#'   [e1071::svm()].
#' @param class_order_method How the fixed class ordering used internally is
#'   determined: `"natural"` (sorted, default), `"frequency"` (most frequent
#'   class first) or `"original"` (order of first appearance in `data`).
#' @param tie_break_method How ties between equally good one-vs-rest splits
#'   are broken: `"first"` (default), `"last"` or `"random"`.
#' @param scale Logical; if `TRUE` (default), predictors are centered and
#'   scaled to unit variance before each node's SVM is fit, using statistics
#'   estimated from that node's own training data and re-applied at
#'   prediction time.
#' @param verbose Logical; if `TRUE`, progress information is reported with
#'   [base::message()] while the tree is grown.
#' @param ... Further arguments passed on to [e1071::svm()], for example
#'   `gamma` or `degree` when `kernel = "radial"` or `"polynomial"`.
#'
#' @return An object of class `"stree"`: a list with the fitted tree
#'   structure (`tree`) and the metadata (`call`, `terms`, `xlevels`,
#'   `levels`, `response_name`) needed by [predict.stree()],
#'   [print.stree()] and [summary.stree()].
#'
#' @references
#' Montanana, R., Gamez, J. A., Puerta, J. M. (2021). STree: A Single
#' Multi-class Oblique Decision Tree Based on Support Vector Machines.
#' In *Advances in Artificial Intelligence*, LNAI 12882, Springer.
#' \doi{10.1007/978-3-030-85713-4_6}
#'
#' @examples
#' fit <- stree(Species ~ ., data = iris, max_depth = 3)
#' fit
#' predict(fit, iris[1:5, ])
#' predict(fit, iris[1:5, ], type = "prob")
#'
#' @export
stree <- function(formula, data,
                   kernel = c("linear", "polynomial", "radial"),
                   max_depth = 5, min_samples = 5,
                   impurity_measure = c("entropy", "gini"),
                   cost = 1,
                   class_order_method = c("natural", "frequency", "original"),
                   tie_break_method = c("first", "last", "random"),
                   scale = TRUE, verbose = FALSE, ...) {
  kernel <- match.arg(kernel)
  impurity_measure <- match.arg(impurity_measure)
  class_order_method <- match.arg(class_order_method)
  tie_break_method <- match.arg(tie_break_method)

  if (!inherits(formula, "formula")) {
    stop("`formula` must be a formula, e.g. Species ~ .", call. = FALSE)
  }
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }
  if (!is.numeric(max_depth) || max_depth < 1) {
    stop("`max_depth` must be a positive integer.", call. = FALSE)
  }
  if (!is.numeric(min_samples) || min_samples < 1) {
    stop("`min_samples` must be a positive integer.", call. = FALSE)
  }

  this_call <- match.call()

  mf <- stats::model.frame(formula, data = data, na.action = stats::na.pass)
  terms_obj <- attr(mf, "terms")
  response_name <- names(mf)[1]

  y <- mf[[response_name]]
  if (!is.factor(y)) y <- factor(y)
  y <- droplevels(y)
  if (nlevels(y) < 2) {
    stop("The response must have at least two classes.", call. = FALSE)
  }

  predictors <- mf[, -1, drop = FALSE]
  if (ncol(predictors) == 0) {
    stop("`formula` must include at least one predictor.", call. = FALSE)
  }

  xlevels <- get_xlevels(predictors)

  model_data <- predictors
  model_data[[response_name]] <- y

  all_classes <- get_consistent_class_order(y, class_order_method)

  tree <- stree_split(
    data = model_data, response = response_name, depth = 1,
    max_depth = max_depth, min_samples = min_samples, kernel = kernel,
    impurity_measure = impurity_measure, cost = cost, verbose = verbose,
    all_classes = all_classes, class_order_method = class_order_method,
    tie_break_method = tie_break_method, use_scaling = scale, ...
  )

  structure(
    list(
      tree = tree,
      call = this_call,
      terms = terms_obj,
      xlevels = xlevels,
      levels = all_classes,
      response_name = response_name,
      kernel = kernel,
      max_depth = max_depth,
      min_samples = min_samples,
      impurity_measure = impurity_measure
    ),
    class = "stree"
  )
}
