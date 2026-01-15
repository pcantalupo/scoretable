# Create a matrix of scores 'm' (no row nor colnames) - 20 rows, 3 columns
set.seed(15)
m = matrix(rnorm(60), nrow = 20)  # colnames and rownames are NULL


test_that("add_labels_based_on_max - requires data.frame or matrix with row and column names", {

  # Test matrix
  scores = m
  expect_error(add_labels_based_on_max(scores), "Row and column names are required")
  
  colnames(scores) = c("TypeA","TypeB","TypeC")  # Only add colnames
  rownames(scores) = NULL
  expect_error(add_labels_based_on_max(scores), "Row and column names are required")

  rownames(scores) = paste0("c", rep(1:nrow(scores))) # Onlyy add rownames 
  colnames(scores) = NULL
  expect_error(add_labels_based_on_max(scores), "Row and column names are required")
  
  # add both row and col names
  rownames(scores) = paste0("c", rep(1:nrow(scores)))
  colnames(scores) = c("TypeA","TypeB","TypeC")
  expect_no_error(add_labels_based_on_max(scores))
  
  
  # Test data.frame  
  scores = as.data.frame(m)
  colnames(scores) = NULL   # Default colnames V1, V2,...
  rownames(scores) = NULL   # Default 1, 2,... Setting to NULL never removes rownames from a data.frame
  expect_error(add_labels_based_on_max(scores), "Row and column names are required")

  colnames(scores) = c("TypeA","TypeB","TypeC")  # add colnames
  expect_no_error(add_labels_based_on_max(scores))
})


test_that("add_labels_based_on_max - basic functionality", {
  # esmax is 1000 rows x 5 columns 
  pred = add_labels_based_on_max(esmax_scores_small)  
  head(pred, 2)       # 3 pred cols + 5 celltype cols
  
  expect_equal(nrow(pred), 1000)
  expect_equal(ncol(pred), 8)
  
  expect_true(class(pred) == "data.frame")
  expect_equal(colnames(pred)[1:3], c("obs_id", "labels", "score"))
  
  # all labels must be found in the column names of pred
  expect_true(all(pred$labels %in% colnames(esmax_scores_small)))
  
  # obs_id column matches rownames
  expect_equal(pred$obs_id, rownames(esmax_scores_small))
})


test_that("add_labels_based_on_max - unknown and cutoff params work", {
  # test unknown
  pred = add_labels_based_on_max(esmax_scores_small,
                                 unknown = TRUE) # default cutoff 0.25
  expect_equal(sum(pred$labels == "Unknown"), 332)
  
  # test cutoff
  pred = add_labels_based_on_max(esmax_scores_small,
                                 unknown = TRUE, cutoff = 0.5)
  expect_equal(sum(pred$score <= 0.5), 419)         # 419 rows have low score
  expect_equal(sum(pred$labels == "Unknown"), 419)  # 419 unknowns
})


test_that("add_labels_based_on_max - returns correct maximum scores", {
  # Take the first 20 rows of esmax_scores_small for testing
  scores = esmax_scores_small[1:20,]
  head(scores)

  pred = add_labels_based_on_max(scores)

  # check that the pred max scores matches the expected max scores   
  expected_max_scores = apply(scores, 1, max)
  expect_equal(pred$score, as.numeric(expected_max_scores))

  # check that the predicted label is correct based on max score
  for (i in 1:nrow(pred)) {
    max_col = which.max(scores[i, ])
    expect_equal(pred$labels[i], colnames(scores)[max_col])
  }
})
