test_that("plot_signal_vs_noise works", {

  pred = add_labels_based_on_max(esmax_scores_small)
  p = plot_signal_vs_noise(pred)
  
  # check it returns a ggplot object
  expect_s3_class(p, "ggplot")
  
  # Check that predicted labels in plot match those from pred
  plot_labels = levels(as.factor(p$data$labels))
  pred_labels = unique(pred$labels)
  expect_true(all(plot_labels %in% pred_labels))
  
  # Check score values are within expected range
  min_val = min(esmax_scores_small)
  max_val = max(esmax_scores_small)
  expect_true(all(p$data$score >= min_val & p$data$score <= max_val))
})

test_that("plot_signal_vs_noise ncol parameter works", {
  pred = add_labels_based_on_max(esmax_scores_small)  

  p = plot_signal_vs_noise(pred, ncol = 2)
  expect_s3_class(p, "ggplot")
  expect_equal(p$facet$params$ncol, 2)
})

test_that("plot_signal_vs_noise can be customized", {
  pred = add_labels_based_on_max(esmax_scores_small)
  p = plot_signal_vs_noise(pred)
  
  # Test that ggplot2 layers can be added
  p2 = p + ggplot2::geom_jitter(width = 0.1)
  expect_s3_class(p2, "ggplot")
  expect_true(length(p2$layers) > length(p$layers))
})


