# Internal recursive tree engine ------------------------------------------
# Not exported. stree() is the user-facing wrapper around stree_split();
# predict.stree() and print.stree() wrap stree_predict() and
# stree_print_node() respectively.

stree_split <- function(data, response, depth = 1, max_depth = 5,
                         min_samples = 5, kernel = "linear",
                         impurity_measure = "entropy",
                         cost = 1, verbose = FALSE,
                         all_classes = NULL,
                         class_order_method = "natural",
                         tie_break_method = "first",
                         use_scaling = TRUE, ...) {
  if (is.null(all_classes)) {
    all_classes <- get_consistent_class_order(data[[response]], class_order_method)
  }

  if (verbose) {
    message("--- STree node at depth ", depth, " (n = ", nrow(data), ") ---")
  }

  if (anyNA(data)) {
    if (verbose) message("NA values detected; stopping and creating a leaf.")
    return(leaf_node(data[[response]], nrow(data), all_classes))
  }

  y <- data[[response]]
  n <- nrow(data)

  if (depth > max_depth || length(unique(y)) == 1 || n < min_samples) {
    if (verbose) message("Stopping condition met; creating a leaf.")
    return(leaf_node(y, n, all_classes))
  }

  present_classes <- intersect(all_classes, unique(as.character(y)))
  k <- length(present_classes)

  if (verbose) message("Number of classes at node: ", k)

  features <- setdiff(names(data), response)
  X <- data[features]

  # --- Binary case --------------------------------------------------
  if (k == 2) {
    if (verbose) message("Binary classification case")

    result <- stree_fit_binary_svm(
      X, factor(y), kernel,
      verbose = verbose, use_scaling = use_scaling, cost = cost, ...
    )

    if (is.null(result$model)) {
      return(leaf_node(y, n, all_classes))
    }

    left_idx <- result$left_idx
    right_idx <- result$right_idx

    if (length(left_idx) == 0 || length(right_idx) == 0 ||
      length(left_idx) < min_samples || length(right_idx) < min_samples) {
      return(leaf_node(y, n, all_classes))
    }

    left_child <- stree_split(
      data[left_idx, , drop = FALSE], response, depth + 1, max_depth, min_samples,
      kernel, impurity_measure, cost, verbose, all_classes,
      class_order_method, tie_break_method, use_scaling, ...
    )
    right_child <- stree_split(
      data[right_idx, , drop = FALSE], response, depth + 1, max_depth, min_samples,
      kernel, impurity_measure, cost, verbose, all_classes,
      class_order_method, tie_break_method, use_scaling, ...
    )

    return(list(
      is_leaf = FALSE,
      model = result$model,
      features = result$used_features,
      scaling_params = result$scaling_params,
      hyperplane_class = NULL,
      partition_column = 1,
      left = left_child,
      right = right_child,
      depth = depth,
      n = n,
      kernel = kernel
    ))
  }

  # --- Multi-class case: one-vs-rest ---------------------------------
  if (verbose) message("Multi-class case: trying ", k, " one-vs-rest splits")

  impurity_func <- if (impurity_measure == "entropy") entropy else gini
  impurities_list <- list()

  for (target_class in present_classes) {
    y_binary <- factor(
      ifelse(y == target_class, "positive", "negative"),
      levels = c("positive", "negative")
    )

    result <- stree_fit_binary_svm(
      X, y_binary, kernel,
      verbose = FALSE, use_scaling = use_scaling, cost = cost, ...
    )

    if (is.null(result$model)) {
      impurities_list[[target_class]] <- list(impurity = NA)
      next
    }

    left_idx <- result$left_idx
    right_idx <- result$right_idx

    if (length(left_idx) == 0 || length(right_idx) == 0) {
      impurities_list[[target_class]] <- list(impurity = NA)
      next
    }

    y_left <- y[left_idx]
    y_right <- y[right_idx]

    impurity_left <- impurity_func(y_left)
    impurity_right <- impurity_func(y_right)

    weighted_impurity <- (length(left_idx) / n) * impurity_left +
      (length(right_idx) / n) * impurity_right

    impurities_list[[target_class]] <- list(
      impurity = weighted_impurity,
      model = result$model,
      left_idx = left_idx,
      right_idx = right_idx,
      scaling_params = result$scaling_params,
      used_features = result$used_features
    )
  }

  best_class <- select_best_ovr_split(impurities_list, tie_break_method)

  if (is.null(best_class)) {
    if (verbose) message("No valid split found; creating a leaf.")
    return(leaf_node(y, n, all_classes))
  }

  best_result <- impurities_list[[best_class]]

  if (length(best_result$left_idx) < min_samples ||
    length(best_result$right_idx) < min_samples) {
    return(leaf_node(y, n, all_classes))
  }

  partition_column <- which(present_classes == best_class)

  left_child <- stree_split(
    data[best_result$left_idx, , drop = FALSE], response, depth + 1, max_depth, min_samples,
    kernel, impurity_measure, cost, verbose, all_classes,
    class_order_method, tie_break_method, use_scaling, ...
  )
  right_child <- stree_split(
    data[best_result$right_idx, , drop = FALSE], response, depth + 1, max_depth, min_samples,
    kernel, impurity_measure, cost, verbose, all_classes,
    class_order_method, tie_break_method, use_scaling, ...
  )

  list(
    is_leaf = FALSE,
    model = best_result$model,
    features = best_result$used_features,
    scaling_params = best_result$scaling_params,
    hyperplane_class = best_class,
    partition_column = partition_column,
    left = left_child,
    right = right_child,
    depth = depth,
    n = n,
    impurity = best_result$impurity,
    kernel = kernel
  )
}

# Recursively predict from a single fitted node down to a leaf, either for
# one row (returns $prediction / $probabilities) or a whole data frame
# (returns $predictions / $probabilities as a matrix).
stree_predict <- function(tree, newdata, return_probs = FALSE) {
  if (is.data.frame(newdata) && nrow(newdata) > 1) {
    preds <- lapply(seq_len(nrow(newdata)), function(i) {
      stree_predict(tree, newdata[i, , drop = FALSE], return_probs)
    })

    if (return_probs) {
      pred_classes <- sapply(preds, function(x) x$prediction)
      prob_matrix <- do.call(rbind, lapply(preds, function(x) x$probabilities))
      return(list(predictions = pred_classes, probabilities = prob_matrix))
    } else {
      return(unlist(preds))
    }
  }

  if (tree$is_leaf) {
    if (return_probs) {
      prob_vector <- tree$class_prob
      names(prob_vector) <- tree$class_names
      return(list(prediction = tree$prediction, probabilities = prob_vector))
    } else {
      return(tree$prediction)
    }
  }

  newdata_subset <- newdata[, tree$features, drop = FALSE]

  for (col in names(newdata_subset)) {
    if (is.factor(newdata_subset[[col]])) {
      newdata_subset[[col]] <- as.numeric(newdata_subset[[col]])
    } else if (is.character(newdata_subset[[col]])) {
      newdata_subset[[col]] <- as.numeric(as.factor(newdata_subset[[col]]))
    }
  }

  if (!is.null(tree$scaling_params)) {
    newdata_subset <- standard_scaler(newdata_subset, params = tree$scaling_params)
  }

  dec_value <- tryCatch(
    {
      pred_result <- stats::predict(tree$model, newdata_subset, decision.values = TRUE)
      attr(pred_result, "decision.values")
    },
    error = function(e) {
      warning("Prediction failed: ", e$message)
      0
    }
  )

  if (is.matrix(dec_value)) {
    dec_value <- dec_value[1, 1]
  } else {
    dec_value <- as.numeric(dec_value)[1]
  }

  if (dec_value > 0) {
    stree_predict(tree$left, newdata, return_probs)
  } else {
    stree_predict(tree$right, newdata, return_probs)
  }
}

# Recursively print a fitted tree node (used by print.stree()).
stree_print_node <- function(tree, indent = "", show_probs = FALSE) {
  if (tree$is_leaf) {
    cat(indent, "[Leaf] predict =", tree$prediction, "| n =", tree$n)
    if (show_probs) {
      cat(" | probs = [", paste(round(tree$class_prob, 3), collapse = ", "), "]")
    }
    cat("\n")
    return(invisible())
  }

  cat(indent, "[Node] depth =", tree$depth, "| n =", tree$n, "| kernel =", tree$kernel)
  if (!is.null(tree$hyperplane_class)) {
    cat(" | split:", tree$hyperplane_class, "vs rest")
  }
  if (!is.null(tree$impurity)) {
    cat(" | impurity =", round(tree$impurity, 4))
  }
  cat("\n")

  cat(indent, "|- Positive branch (distance > 0):\n")
  if (!is.null(tree$left)) {
    stree_print_node(tree$left, paste0(indent, "|  "), show_probs)
  }

  cat(indent, "`- Negative branch (distance <= 0):\n")
  if (!is.null(tree$right)) {
    stree_print_node(tree$right, paste0(indent, "   "), show_probs)
  }

  invisible()
}

# Recursively count leaves and the maximum depth of a fitted tree (used by
# summary.stree()).
stree_tree_stats <- function(tree) {
  if (tree$is_leaf) {
    return(list(leaves = 1L, max_depth = 0L, nodes = 1L))
  }
  left <- stree_tree_stats(tree$left)
  right <- stree_tree_stats(tree$right)
  list(
    leaves = left$leaves + right$leaves,
    max_depth = max(left$max_depth, right$max_depth) + 1L,
    nodes = 1L + left$nodes + right$nodes
  )
}
