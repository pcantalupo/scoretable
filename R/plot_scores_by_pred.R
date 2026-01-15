#' Boxplot of top scores by predicted label
#'
#' Shows the distribution of scores for each predicted label. If you have 3 labels 
#' there will be three boxplots drawn in the same plot. Each point is the score of 
#' the observations of a label.
#'
#' @param annotated_scores A data.frame that contains columns "obs_id", "labels", and "score" 
#'   followed by one or more score columns (one for each candidate label). This is 
#'   typically the output from \code{add_labels_based_on_max()}.
#'
#' @return ggplot object that can be further customized with ggplot2 functions
#'
#' @export
#' @import ggplot2
#'
#' @examples
#' set.seed(15)
#' m = matrix(rnorm(60), nrow = 20)
#' colnames(m) = c("TypeA","TypeB","TypeC")
#' rownames(m) = paste0("c", rep(1:nrow(m)))
#' pred = add_labels_based_on_max(m)
#' 
#' # Basic plot
#' plot_scores_by_pred(pred)
#' 
#' # Customize with ggplot2 functions
#' library(ggplot2)
#' p = plot_scores_by_pred(pred)
#' 
#' # Add jitter to see individual points
#' p + geom_jitter(width = 0.15, alpha = 0.5)
#' 
#' # Customize theme and colors
#' p + theme_minimal() + 
#'   theme(plot.title = element_text(hjust = 0.5))
#'
plot_scores_by_pred <- function(annotated_scores) {

  stopifnot(is.data.frame(annotated_scores))
  required_cols <- c("obs_id", "labels", "score")
  stopifnot(all(required_cols %in% colnames(annotated_scores)))

  p = ggplot(annotated_scores, aes(x = labels, y = score)) +
    geom_boxplot(outlier.shape = NA) +
    coord_flip() +
    labs(
      x = "Predicted label", y = "Score",
      title = "Scores by predicted label"
    )

  return(p)
}

