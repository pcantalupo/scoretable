test_that("expand_group_labels works", {
  # Create a matrix of scores 'm' (no row nor colnames) - 10 rows, 3 columns
  set.seed(1)
  nrow = 10
  m = matrix(rnorm(nrow * 3), nrow = nrow)  # colnames and rownames are NULL
  
  # Create groups vector of same length as 'm' matrix
  set.seed(1)
  groups = sample(0:3, nrow, replace = TRUE)
  
  # Aggregate observations to group level
  scores.group = scores_grouplevel(m, groups)
  colnames(scores.group) = c("Type A", "Type B", "Type C")
  
  # Add labels based on maximum score
  pred = add_labels_based_on_max(scores.group)

  # Expand group labels to observation level
  obs_labels = expand_group_labels(pred, groups)
  
  # Expected result
  expected = c("Type A", "Type C", "Type A", "Type A", "Type B",
               "Type A", "Type A", "Type A", "Type B", "Type B")
  
  expect_equal(obs_labels, expected)
})


