test_that("scores_grouplevel works", {
  
  scores = esmax_scores_small
  groups = esmax_clusters_small
  
  expect_error(scores_grouplevel(scores), 'argument "groups" is missing')
  expect_error(scores_grouplevel(scores, groups[1:2]), "Length of 'groups' must equal number of rows")
  expect_error(scores_grouplevel(scores, groups, fun = "median"), "Only fun='mean' is currently supported")
  
  # Aggregate observations to group level
  (scores.group = scores_grouplevel(scores, groups))
  expected = apply(scores, 2, function(col) {   # calculated expected aggregate scores
    tapply(col, groups, FUN = mean)
  })
  expect_true(all(expected - scores.group < 1e-15))
})

