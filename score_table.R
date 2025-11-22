# Aggregate a score table from single cell level to the cluster level
# Params
#   scores - data.frame or matrix where rows are cells and columns are celltypes.
#            Rownames or colnames are not required
#   clusters - vector of same length as number of rows in 'scores'.
#              The values specify the group for each row (i.e. c(3, 1, 0, 1, 2, 0, 3, ...)) 
# Return - A matrix with the mean scores. Columns are in the same order.
#          Rows are sorted based on the typeof(clusters) (integer -> integer sorted, character -> character sorted).
#          Rownames are set to the cluster value
scores_clusterlevel = function(scores, clusters) {
  data = apply(scores, 2, function(row) {
    aggregate(row, list(Clusters = clusters), mean)$x   # only need 'x' where 'x' is the mean
  })
  rownames(data) = as.character(sort(unique(clusters)))
  return(data)
}

# Determine the label for each row based on maximum value of the celltype scores in the row.
# scores - rows are cells or clusters and columns are celltypes
#          Rownames and colnames are required.
# unknown - do you want to calculated Unknown labels? (if TRUE, must supply 'clusters' param)
#         - only supported for cluster scores table
# clusters - vector that specifies the cluster for each cell (only needed if calculating unknown labels)
#          - only supported for cluster scores table
# cutoff - if the score is less than this value, the label is 'Unknown' (default is 0.25; based on ScType)
#          - only supported for clusters scores table
# Return - The scores table with 3 preprended columns 'clusters', 'labels', and 'score'
#          'clusters' values are taken from the rownames
#          'labels' the celltype with the maximum score
#          'score' the maximum score
add_labels_based_on_max = function(scores, unknown = FALSE, clusters, cutoff = 0.25) {
  whichmax = apply(scores, 1, which.max)  # for Group 0 the max value is column 3
  labels = colnames(scores)[whichmax]  # extract the label for each Group
  score = sapply(seq_along(whichmax), function(i){  # extract the score for each Group
    maxindex = whichmax[i]
    scores[i,maxindex]
  })
  data = data.frame(clusters = rownames(scores), labels = labels, score = score, scores, check.names = FALSE)
  if(unknown) {
    #numcells = table(clusters)
    data$labels[data$score < cutoff] = "Unknown"
  }
  return(data)
}

# Purpose: You have cluster labels (in your cluster scores table) and you want to expand these labels to the cell level so you can add them to a table of cell metadata
# scores - data.frame of cluster scores containing at least two columns: 'clusters' and 'labels' (rows are clusters)
# clusters - vector that specifies the cluster for each cell 
deconvolute_clusterlabel = function(scores, clusters) {
  return(scores$labels[match(clusters, scores$clusters)]) 
}




#' Boxplot of top scores by predicted label
#'
#' @return ggplot object
#'
#' @export
#' @import ggplot2
plot_scores_by_pred <- function(scores, outlier_shape = 19, alpha_val = 0.7,
                                jitter = FALSE) {
  p = ggplot(scores, aes(x = labels, y = score)) +
    geom_boxplot(outlier.shape = outlier_shape, alpha = alpha_val) +
    coord_flip() +
    labs(
      x = "Predicted label", y = "Score",
      title = "Scores by predicted label"
    )

  if (jitter == TRUE) {
    p = p +
      geom_jitter(aes(color = labels), width = 0.15, size = 0.6, alpha = 0.4)
  }

  return(p)
}



#' Signal vs Noise Boxplot
#'
#' Shows the distribution of scores across all candidate labels for each predicted label.
#'
#' @return ggplot object
#'
#' @export
#' @import ggplot2
#' @import tidyr
#' @import dplyr
plot_signal_vs_noise <- function(scores, outlier_shape = 19, alpha = 0.7,
                                 jitter = FALSE) {

  stopifnot(is.data.frame(scores.labels))
  required_cols <- c("clusters", "labels", "score")
  stopifnot(all(required_cols %in% colnames(scores.labels)))

  # Identify score columns (all columns not in metadata)
  score_cols <- setdiff(colnames(scores.labels), required_cols)

  # Pivot long
  long <- scores.labels %>%
    select(labels, all_of(score_cols)) %>%
    pivot_longer(cols = score_cols, names_to = "all_labels", values_to = "score")

  # Plot
  p = long %>%
    ggplot(aes(x = all_labels, y = score)) +
    geom_boxplot(outlier.shape = outlier_shape, alpha = alpha) +
    coord_flip() +
    facet_wrap(~ labels, scales = "free_y") +
    labs(
      title = "Scores for all labels per predicted label",
      x = "All Labels",
      y = "Score"
    )

  if (jitter == TRUE) {
    p = p +
      geom_jitter(aes(color = labels), width = 0.15, size = 0.6, alpha = 0.4)
  }

  return(p)
}



