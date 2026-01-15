#' Expand Group Labels to Observation Level
#'
#' Purpose: You have an annotated scores table at the group level. However, you have a
#' table of metadata at the observation level with a column specifying the group. You
#' want to add a column of labels to your observation-level metadata. Therefore this
#' function will expand the group-level labels to a vector that you can add directly
#' to your observation-level metadata table.
#'
#' @param annotated_scores data.frame containing at least two columns
#'   'obs_id' and 'labels' (rows represent groups) followed optionally by score columns
#' @param groups A vector that specifies the group membership for each observation. Must be same length as the number of rows in `annotated_scores`.
#'
#' @return A vector of the same length as `groups` containing the expanded labels.
#'
#' @export
#'
#' @examples
#' # Create group-level annotations
#' annotated_scores = data.frame(
#'   obs_id = 0:3,
#'   labels = c("Type A", "Type B", "Type C", "Type D")
#' )
#' 
#' # Observation-level group assignments
#' groups = c(0, 0, 1, 2, 1, 3, 0, 2, 1, 3)
#' 
#' # Map labels to observations
#' obs_labels = expand_group_labels(annotated_scores, groups)
#' print(obs_labels)
expand_group_labels = function(annotated_scores, groups) {
  return(annotated_scores$labels[match(groups, annotated_scores$obs_id)])
}
