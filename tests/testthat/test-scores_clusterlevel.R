# Create a matrix of scores 'm' (no row nor colnames) - 20 rows, 3 columns
set.seed(15)
nrow = 20
m = matrix(rnorm(60), nrow = nrow)  # colnames and rownames are NULL

# Create clusters vector of same length as 'm' matrix
set.seed(1)
clusters = sample(0:3, nrow, replace=TRUE)

test_that("scores_clusterlevel works", {
  scores = as.data.frame(m)
  
  expect_error(scores_clusterlevel(scores), 'argument "clusters" is missing')
  expect_error(scores_clusterlevel(scores, clusters[1:2]), "Length of 'clusters' must equal number of rows")
  expect_error(scores_clusterlevel(scores, clusters, fun = "median"), "Only fun='mean' is currently supported")
  
  # Aggregate observations to cluster level
  (scores.cluster = scores_clusterlevel(scores, clusters))
  expected = apply(m, 2, function(col) {   # calculated expected aggregate scores
    tapply(col, clusters, FUN = mean)
  })
  expect_true(all(expected - scores.cluster < 1e-16))
})
