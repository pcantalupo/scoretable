#' Expand Cluster Labels to Cell Level
#' 
#' Purpose: You have a scores table on the cluster/group level. However, you have a
#' table of metadata on the cell level with a column specifying the cluster. You 
#' want to add a column of labels to your cell level metadata. Therefore this function
#' will expand the cluster/group labels to a vector that you can add directly to your
#' cell level metadata table.
#'
#' @param scores data.frame of cluster scores containing at least two columns:
#'   'clusters' and 'labels' (rows are clusters). DOES IT HAVE TO BE A DF???
#' @param clusters vector that specifies the cluster for each cell. Must be same length
#'   as number of rows in `scores`. 
#' 
#' @return A vector of the same length as `clusters` containing the expanded labels.
#'
#' @export 
#'
#' @examples
#' # Create group-level annotations
#' scores = data.frame(
#'   clusters = 0:3,
#'   labels = c("T cells", "B cells", "Monocytes", "NK cells")
#' )
#' # Observation-level group assignments
#' clusters = c(0, 0, 1, 2, 1, 3, 0, 2, 1, 3)
#' # Map labels to observations
#' obs_labels = expand_clusterlabel(scores, clusters)
#' print(obs_labels)
expand_clusterlabel = function(scores, clusters) {
  return(scores$labels[match(clusters, scores$clusters)]) 
}






