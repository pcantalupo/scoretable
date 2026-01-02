#' Expand Group Labels to Observation Level
#'
#' Purpose: You have a scores table at the group level. However, you have a
#' table of metadata at the observation level with a column specifying the group. You
#' want to add a column of labels to your observation-level metadata. Therefore this function
#' will expand the group-level labels to a vector that you can add directly to your
#' observation-level metadata table.
#'
#' @param scores data.frame containing at least two columns:
#'   'obs_id' and 'labels' (rows represent groups).
#' @param clusters A vector that specifies the group membership for each observation. Must be same length as the number of rows in `scores`.
#'
#' @return A vector of the same length as `clusters` containing the expanded labels.
#'
#' @export
#'
#' @examples
#' # Create group-level annotations
#' scores = data.frame(
#'   obs_id = 0:3,
#'   labels = c("T cells", "B cells", "Monocytes", "NK cells")
#' )
#' # Observation-level group assignments
#' clusters = c(0, 0, 1, 2, 1, 3, 0, 2, 1, 3)
#' # Map labels to observations
#' obs_labels = expand_clusterlabel(scores, clusters)
#' print(obs_labels)
expand_clusterlabel = function(scores, clusters) {
  return(scores$labels[match(clusters, scores$obs_id)])
}
