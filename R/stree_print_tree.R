#' @title Print STree Structure
#' @export
print_stree <- function(tree, indent = "", show_probs = FALSE) {

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

  cat(indent, "|- Positive branch (distance >= 0):\n")
  if (!is.null(tree$left)) {
    print_stree(tree$left, paste0(indent, "|  "), show_probs)
  }

  cat(indent, "`- Negative branch (distance < 0):\n")
  if (!is.null(tree$right)) {
    print_stree(tree$right, paste0(indent, "   "), show_probs)
  }

  invisible()
}
