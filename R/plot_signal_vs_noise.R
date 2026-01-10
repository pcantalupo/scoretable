#' Signal vs Noise Boxplot
#'
#' Shows the distribution of scores across all candidate labels for each predicted label.
#' 
#' @param annotated_scores A data.frame that contains columns "obs_id", "labels", and "score" 
#'   followed by one or more score columns (one for each candidate label). This is 
#'   typically the output from \code{add_labels_based_on_max()}.
#' @param ncol Integer specifying the number of columns in the facet layout.
#'   Default is 3. 
#'
#' @return ggplot object that can be further customized with ggplot2 functions
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
#' 
#' # Basic plot with 3 columns
#' plot_signal_vs_noise(pred)
#' 
#' # Change facet layout
#' plot_signal_vs_noise(pred, ncol = 2)
#' 
#' # Customize with ggplot2 functions
#' library(ggplot2)
#' p = plot_signal_vs_noise(pred)
#' 
#' # Add jitter to see individual points
#' p + geom_jitter(width = 0.15, alpha = 0.5)
#' 
#' # Customize theme
#' p + theme_bw() +
#'   theme(strip.text = element_text(face = "bold"))
#'
plot_signal_vs_noise <- function(annotated_scores, ncol = 3) {
  
  stopifnot(is.data.frame(annotated_scores))
  required_cols <- c("obs_id", "labels", "score")
  stopifnot(all(required_cols %in% colnames(annotated_scores)))
  
  # Identify score columns (all columns not in metadata)
  score_cols <- setdiff(colnames(annotated_scores), required_cols)
  
  # Pivot long
  long <- annotated_scores %>%
    select(labels, all_of(score_cols)) %>%
    pivot_longer(cols = all_of(score_cols), names_to = "all_labels",
                 values_to = "score")
  
  # Plot
  p = long %>%
    ggplot(aes(x = all_labels, y = score)) +
    geom_boxplot(outlier.shape = NA) +
    coord_flip() +
    facet_wrap(~ labels, scales = "free_y", ncol = ncol) +
    labs(
      title = "Scores for all labels per predicted label",
      x = "All Labels",
      y = "Score"
    )
  
  return(p)
}
