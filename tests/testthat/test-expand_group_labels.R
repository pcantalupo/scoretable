test_that("expand_group_labels works", {
  
  # Get first 10 rows of example data
  scores = esmax_scores_small[1:10,]
  groups = esmax_clusters_small[1:10]
  
  # Aggregate observations to group level
  scores.group = scores_grouplevel(scores, groups)

  # Add labels based on maximum score
  pred = add_labels_based_on_max(scores.group)

  # Expand group labels to observation level
  obs_labels = expand_group_labels(pred, groups)
  
  expected = c("Retinal Progenitor Cells", "Retinal Progenitor Cells",
               "Retinal Progenitor Cells", "Retinal Ganglion Cells",
               "Neurogenic Cells", "Retinal Progenitor Cells",
               "Neurogenic Cells", "Retinal Progenitor Cells",
               "Retinal Progenitor Cells", "Neurogenic Cells")
  expect_equal(obs_labels, expected)
})


