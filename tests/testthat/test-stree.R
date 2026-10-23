test_that("stree fits a multi-class tree and predicts sensible classes", {
  fit <- stree(Species ~ ., data = iris, max_depth = 3, min_samples = 5)

  expect_s3_class(fit, "stree")
  expect_true(all(c("tree", "call", "terms", "xlevels", "levels") %in% names(fit)))

  preds <- predict(fit, iris)
  expect_s3_class(preds, "factor")
  expect_equal(length(preds), nrow(iris))
  expect_true(all(levels(preds) == fit$levels))

  # Accuracy should be reasonably high on iris (a well-separated dataset).
  accuracy <- mean(as.character(preds) == as.character(iris$Species))
  expect_gt(accuracy, 0.8)
})

test_that("stree fits a binary tree", {
  binary_data <- droplevels(iris[iris$Species %in% c("setosa", "versicolor"), ])
  fit <- stree(Species ~ ., data = binary_data, max_depth = 2)

  expect_s3_class(fit, "stree")
  expect_length(fit$levels, 2)

  preds <- predict(fit, binary_data)
  expect_equal(length(preds), nrow(binary_data))
})

test_that("predict(type = 'prob') returns a valid probability matrix", {
  fit <- stree(Species ~ ., data = iris, max_depth = 3)
  probs <- predict(fit, iris[1:10, ], type = "prob")

  expect_true(is.matrix(probs))
  expect_equal(nrow(probs), 10)
  expect_equal(ncol(probs), nlevels(iris$Species))
  expect_equal(colnames(probs), fit$levels)
  expect_true(all(abs(rowSums(probs) - 1) < 1e-8))
})

test_that("predict works for a single new observation", {
  fit <- stree(Species ~ ., data = iris, max_depth = 3)
  pred_one <- predict(fit, iris[1, , drop = FALSE])
  expect_length(pred_one, 1)

  prob_one <- predict(fit, iris[1, , drop = FALSE], type = "prob")
  expect_equal(dim(prob_one), c(1, nlevels(iris$Species)))
})

test_that("print.stree and summary.stree run without error", {
  fit <- stree(Species ~ ., data = iris, max_depth = 2)
  expect_output(print(fit), "STree oblique classification tree")
  expect_output(print(fit, show_probs = TRUE), "probs")
  expect_output(summary(fit), "Number of leaves")
})

test_that("different kernels, impurity measures and tie-break rules run", {
  for (k in c("linear", "radial")) {
    fit <- stree(Species ~ ., data = iris, kernel = k, max_depth = 2)
    expect_s3_class(fit, "stree")
  }
  for (im in c("entropy", "gini")) {
    fit <- stree(Species ~ ., data = iris, impurity_measure = im, max_depth = 2)
    expect_s3_class(fit, "stree")
  }
  for (tb in c("first", "last", "random")) {
    fit <- stree(Species ~ ., data = iris, tie_break_method = tb, max_depth = 2)
    expect_s3_class(fit, "stree")
  }
})

test_that("stree handles factor predictors", {
  df <- iris
  df$size <- factor(sample(c("small", "large"), nrow(df), replace = TRUE))
  fit <- stree(Species ~ ., data = df, max_depth = 2)
  preds <- predict(fit, df)
  expect_equal(length(preds), nrow(df))
})

test_that("stree validates its inputs", {
  expect_error(stree("not a formula", data = iris), "formula")
  expect_error(stree(Species ~ ., data = iris, max_depth = 0), "max_depth")
  expect_error(stree(Species ~ ., data = iris, min_samples = 0), "min_samples")
  expect_error(stree(Sepal.Length ~ 1, data = iris), "predictor")

  single_class <- iris[iris$Species == "setosa", ]
  expect_error(stree(Species ~ ., data = single_class), "two classes")
})

test_that("predict.stree requires newdata", {
  fit <- stree(Species ~ ., data = iris, max_depth = 2)
  expect_error(predict(fit), "newdata")
})
