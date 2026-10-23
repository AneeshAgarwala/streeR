#' streeR: Oblique Classification Trees Based on Support Vector Machines
#'
#' Fits single, multi-class oblique classification trees in which each
#' internal node's split is a support vector machine (SVM) hyperplane rather
#' than a threshold on a single variable, following the 'STree' algorithm of
#' Montanana, Gamez and Puerta (2021). The main entry point is [stree()];
#' fitted models can be inspected with `print()` and `summary()` and used
#' for prediction with `predict()`.
#'
#' @references
#' Montanana, R., Gamez, J. A., Puerta, J. M. (2021). STree: A Single
#' Multi-class Oblique Decision Tree Based on Support Vector Machines.
#' In *Advances in Artificial Intelligence*, LNAI 12882, Springer.
#' \doi{10.1007/978-3-030-85713-4_6}
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
