#' Signal vs Noise Ridge plot
#'
#' Shows the distribution of scores across all candidate labels for each 
#' predicted label. Each facet represents one predicted label, and within 
#' each facet, ridge plots show the score distributions for all possible 
#' candidate labels. This helps visualize the separation between the 
#' "signal" (predicted label's scores) and "noise" (other labels' scores).
#' 
#' @param annotated_scores A data.frame that contains columns "obs_id", "labels", and "score" 
#'   followed by one or more score columns (one for each candidate label). This is 
#'   typically the output from \code{add_labels_based_on_max()}.
#' @param alpha Numeric value for ridge transparency (0 = transparent, 1 = opaque).
#'   Default is 0.7.
#' @param scale Numeric value controlling ridge height and overlap. A value of 1 
#'   means ridges just touch (maximum point touches baseline above). Values < 1 
#'   create gaps, values > 1 create overlap. Default is 0.9.
#' @param ncol Integer specifying the number of columns in the facet layout.
#'   Default is 3.
#'
#' @return ggplot object
#'
#' @export
#' @import ggplot2
#' @import ggridges
#' @import tidyr
#' @import dplyr
#' 
#' @examples
#' set.seed(15)
#' m = matrix(rnorm(60), nrow = 20)
#' colnames(m) = c("TypeA","TypeB","TypeC")
#' rownames(m) = paste0("c", rep(1:nrow(m)))
#' pred = add_labels_based_on_max(m)
#' plot_signal_vs_noise_ggridges(pred)
#' plot_signal_vs_noise_ggridges(pred, scale = 2, ncol = 2)  # More overlap
plot_signal_vs_noise_ggridges <- function(annotated_scores, alpha = 0.7,
                                          scale = 0.9, ncol = 3) {
  
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
    ggplot(aes(x = score, y = all_labels)) +
    geom_density_ridges(alpha = alpha, scale = scale) +
    facet_wrap(~ labels, scales = "free_y", ncol = ncol) +
    labs(
      title = "Scores for all labels per predicted label",
      x = "Score",
      y = "All Labels"
    )

  return(p)
}
