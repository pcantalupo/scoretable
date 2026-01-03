# Create a matrix of scores 'm' (no row nor colnames) - 20 rows, 3 columns
set.seed(15)
nrow = 20
m = matrix(rnorm(60), nrow = nrow)  # colnames and rownames are NULL

# Create groups vector of same length as 'm' matrix
set.seed(1)
groups = sample(0:3, nrow, replace=TRUE)

test_that("scores_grouplevel works", {
  scores = as.data.frame(m)
  
  expect_error(scores_grouplevel(scores), 'argument "groups" is missing')
  expect_error(scores_grouplevel(scores, groups[1:2]), "Length of 'groups' must equal number of rows")
  expect_error(scores_grouplevel(scores, groups, fun = "median"), "Only fun='mean' is currently supported")
  
  # Aggregate observations to group level
  (scores.group = scores_grouplevel(scores, groups))
  expected = apply(m, 2, function(col) {   # calculated expected aggregate scores
    tapply(col, groups, FUN = mean)
  })
  expect_true(all(expected - scores.group < 1e-16))
})
