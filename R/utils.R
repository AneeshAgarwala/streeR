# Internal utilities -----------------------------------------------------
# Not exported; implementation details of stree().

# Pick the one-vs-rest class with the lowest impurity, with a configurable
# tie-breaking rule.
select_best_ovr_split <- function(impurities_list, tie_break = "first") {
  impurities <- sapply(impurities_list, function(x) {
    if (!is.null(x$impurity) && !is.na(x$impurity)) x$impurity else Inf
  })

  min_impurity <- suppressWarnings(min(impurities[is.finite(impurities)]))

  if (!is.finite(min_impurity)) {
    return(NULL)
  }

  best_classes <- names(impurities)[impurities == min_impurity]

  if (length(best_classes) == 1) {
    return(best_classes[1])
  }

  switch(tie_break,
    "first" = best_classes[1],
    "last" = best_classes[length(best_classes)],
    "random" = sample(best_classes, 1),
    best_classes[1]
  )
}

# Determine the (fixed) order in which classes are considered at every node.
get_consistent_class_order <- function(y, sort_method = "natural") {
  classes <- unique(as.character(y))

  switch(sort_method,
    "natural" = sort(classes),
    "frequency" = {
      freq_table <- table(y)
      names(sort(freq_table, decreasing = TRUE))
    },
    "original" = classes,
    sort(classes)
  )
}

# Build a leaf node: majority-class prediction plus class probabilities.
leaf_node <- function(y, n, all_classes) {
  class_table <- table(factor(y, levels = all_classes))
  class_probs <- prop.table(class_table)

  if (sum(class_probs) == 0) {
    class_probs <- rep(1 / length(all_classes), length(all_classes))
    names(class_probs) <- all_classes
  }

  prediction <- names(which.max(class_probs))

  list(
    is_leaf = TRUE,
    prediction = prediction,
    class_prob = as.numeric(class_probs),
    class_names = all_classes,
    n = n
  )
}

# Record the factor levels of every factor/character predictor, the way
# stats::lm() etc. record $xlevels, so predict.stree() can re-apply them
# consistently to newdata.
get_xlevels <- function(predictors) {
  xlevels <- list()
  for (col in names(predictors)) {
    if (is.factor(predictors[[col]])) {
      xlevels[[col]] <- levels(predictors[[col]])
    } else if (is.character(predictors[[col]])) {
      xlevels[[col]] <- sort(unique(predictors[[col]]))
    }
  }
  xlevels
}

# Re-apply the factor levels recorded at training time to newdata, so that
# factor -> numeric coercion in predict.stree() is consistent with training,
# even if newdata does not contain every level.
apply_xlevels <- function(newdata, xlevels) {
  for (col in names(xlevels)) {
    if (col %in% names(newdata)) {
      newdata[[col]] <- factor(as.character(newdata[[col]]), levels = xlevels[[col]])
    }
  }
  newdata
}
