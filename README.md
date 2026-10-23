<!-- README.md is generated from README.Rmd. Please edit that file, or this
     one directly if you don't need the executable example to stay in sync. -->

# streeR

<!-- badges: start -->
<!-- Add these once you have CI / CRAN set up, e.g.:
[![R-CMD-check](https://github.com/yourusername/streeR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AneeshAgarwala/streeR/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/streeR)](https://CRAN.R-project.org/package=streeR)
-->
<!-- badges: end -->

`streeR` fits **oblique classification trees**: decision trees whose
internal node splits are support vector machine (SVM) hyperplanes rather
than thresholds on a single variable. Multi-class problems are handled
directly at every node with a one-vs-rest strategy, following the **STree**
algorithm of Montanana, Gamez and Puerta (2021).

## Installation

```r
# From CRAN (once published):
install.packages("streeR")

# Development version from GitHub:
# install.packages("remotes")
remotes::install_github("AneeshAgarwala/streeR")
```

## Example

```r
library(streeR)

fit <- stree(Species ~ ., data = iris, max_depth = 3)
fit
#> STree oblique classification tree
#> Call:  stree(formula = Species ~ ., data = iris, max_depth = 3)
#> ...

predict(fit, iris[1:5, ])
predict(fit, iris[1:5, ], type = "prob")
```

See `vignette("streeR-intro")` for a fuller walkthrough.

## Reference

Montanana, R., Gamez, J. A., Puerta, J. M. (2021). STree: A Single
Multi-class Oblique Decision Tree Based on Support Vector Machines. In
*Advances in Artificial Intelligence*, LNAI 12882, Springer.
<https://doi.org/10.1007/978-3-030-85713-4_6>

## License

GPL-3
