#' Ridge plot of top scores by predicted label
#'
#' Shows the distribution of scores for each predicted label using ridge plots.
#' Creates density ridges for each unique predicted label, where each ridge 
#' represents the distribution of top scores for observations with that label.
#'
#' @param annotated_scores A data.frame that contains columns "obs_id", "labels", and 
#'   "score" followed by one or more score columns (one for each candidate label).
#'   This is typically the output from \code{add_labels_based_on_max()}.
#' @param alpha Numeric value for ridge transparency (0 = transparent, 1 = opaque).
#'   Default is 0.7.
#' @param scale Numeric value controlling ridge height and overlap. A value of 1 
#'   means ridges just touch (maximum point touches baseline above). Values < 1 
#'   create gaps, values > 1 create overlap. Default is 0.9.
#'
#' @return ggplot object
#'
#' @export
#' @import ggplot2
#' @import ggridges
#'
#' @examples
#' set.seed(15)
#' m = matrix(rnorm(60), nrow = 20)
#' colnames(m) = c("TypeA","TypeB","TypeC")
#' rownames(m) = paste0("c", rep(1:nrow(m)))
#' pred = add_labels_based_on_max(m)
#' plot_scores_by_pred_ggridges(pred)
#'
plot_scores_by_pred_ggridges <- function(annotated_scores, alpha = 0.7,
                                         scale = 0.9) {
  
  stopifnot(is.data.frame(annotated_scores))
  required_cols <- c("obs_id", "labels", "score")
  stopifnot(all(required_cols %in% colnames(annotated_scores)))
  
  p = ggplot(annotated_scores, aes(x = score, y = labels)) +
    geom_density_ridges(alpha = alpha, scale = scale) +
    labs(
      y = "Predicted label", 
      x = "Score",
      title = "Scores by predicted label"
    )
  
  return(p)
}

