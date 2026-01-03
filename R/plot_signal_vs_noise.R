#' Signal vs Noise Boxplot
#'
#' Shows the distribution of scores across all candidate labels for each predicted label.
#' 
#' @param scores A data.frame that contains columns "obs_id", "labels", and "score" followed by one or more score columns (one for each candidate label)
#' @param outlier_shape Integer value indicating the shape of the outlier points
#' @param alpha Numeric value indicating the alpha value for the points in the plot
#' @param jitter Logical value indicating whether to add geom_jitter to the plot
#' @param ncol Integer specifying the number of columns in the facet layout.
#'   Default is 3. 
#'
#' @return ggplot object
#'
#' @export
#' @import ggplot2
#' @import tidyr
#' @import dplyr
#'
#' @examples
#' set.seed(15)
#' m = matrix(rnorm(60), nrow = 20)
#' colnames(m) = c("TypeA","TypeB","TypeC")
#' rownames(m) = paste0("c", rep(1:nrow(m)))
#' pred = add_labels_based_on_max(m)
#' plot_signal_vs_noise(pred)
#' plot_signal_vs_noise(pred, jitter = TRUE, ncol = 2)
plot_signal_vs_noise <- function(scores, outlier_shape = 19, alpha = 0.7,
                                 jitter = FALSE, ncol = 3) {
  
  stopifnot(is.data.frame(scores))
  required_cols <- c("obs_id", "labels", "score")
  stopifnot(all(required_cols %in% colnames(scores)))
  
  # Identify score columns (all columns not in metadata)
  score_cols <- setdiff(colnames(scores), required_cols)
  
  # Pivot long
  long <- scores %>%
    select(labels, all_of(score_cols)) %>%
    pivot_longer(cols = all_of(score_cols), names_to = "all_labels",
                 values_to = "score")
  
  # Plot
  p = long %>%
    ggplot(aes(x = all_labels, y = score)) +
    geom_boxplot(outlier.shape = outlier_shape, alpha = alpha) +
    coord_flip() +
    facet_wrap(~ labels, scales = "free_y", ncol = ncol) +
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
