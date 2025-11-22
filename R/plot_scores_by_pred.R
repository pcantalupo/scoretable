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

