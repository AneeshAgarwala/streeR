# Internal impurity measures -------------------------------------------
#
# These are not exported. They are implementation details of stree_split()
# and are documented here only as developer comments, not as Rd pages.

# Gini impurity of a label vector.
gini <- function(y) {
  if (length(y) == 0) {
    return(0)
  }
  p <- table(y) / length(y)
  1 - sum(p^2)
}

# Shannon entropy (base 2) of a label vector.
entropy <- function(y) {
  if (length(y) == 0) {
    return(0)
  }
  p <- table(y) / length(y)
  p <- p[p > 0] # avoid log2(0)
  -sum(p * log2(p))
}
