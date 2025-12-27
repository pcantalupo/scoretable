#' Boxplot of top scores by predicted label
#'
#' Shows the distribution of scores for each predicted label. If you have 3 labels there will be three boxplox drawn in the same plot. Each point is the score of the observations of a label
#'
#' @param scores A data.frame that contains columns "clusters", "labels", and "score" followed by one or more score columns (one for each canditate label)
#' @param outlier_shape Integer value indicating the shape of the outlier points
#' @param alpha_val Numeric value indicating the alpha value for the points in the plot
#' @param jitter Logical value indicating whether to add geom_jitter to the plot
#'
#' @return ggplot object
#'
#' @export
#' @import ggplot2
#'
#' @examples
#' set.seed(15)
#' m = matrix(rnorm(60), nrow = 20)
#' colnames(m) = c("TCell","BCell","Macro")
#' rownames(m) = paste0("c", rep(1:nrow(m)))
#' pred = add_labels_based_on_max(m)
#' plot_scores_by_pred(pred, jitter = TRUE)
#'
plot_scores_by_pred <- function(scores, outlier_shape = 19, alpha_val = 0.7,
                                jitter = FALSE) {

  stopifnot(is.data.frame(scores))
  required_cols <- c("clusters", "labels", "score")
  stopifnot(all(required_cols %in% colnames(scores)))

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

