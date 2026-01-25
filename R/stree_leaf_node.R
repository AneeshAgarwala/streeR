#' @title Create STree Leaf Node
#' @keywords internal
stree_leaf_node <- function(y, n, all_classes) {

  # Compute class probabilities
  class_table <- table(factor(y, levels = all_classes))
  class_probs <- prop.table(class_table)

  # Handle edge case: empty probabilities
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
