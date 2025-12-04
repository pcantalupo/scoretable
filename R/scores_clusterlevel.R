#' Aggregate Score Table to Group level
#' 
#' Aggregate a score table composed of individual observations to a group level. For scRNAseq data this would aggregrate scores on single cell level to cluster level
#'
#' @param scores A numeric matrix or data frame where rows are observations 
#'   (e.g., cells) and columns are scores (e.g., cell type predictions). 
#'   Rownames nor column names are necessary
#' @param clusters A numeric or character vector of length nrow(scores) 
#'   specifying group membership (e.g., c(3, 1, 0, 1, 2, 0, 3, ...)). 
#'   Must NOT be a factor. Numeric clusters will sort numerically, 
#'   character clusters will sort alphabetically.
#'
#'@details TBD  Can think of it as collapsing the rows to N rows where N is the number of groups (i.e. clusters)
#'
#' @return A matrix with the mean scores. Columns are in the same order as input. 
#'   Rows are sorted by cluster ID (numeric clusters sort numerically, 
#'   character clusters sort alphabetically). Rownames are the cluster IDs.
#' @export
#'
#' @examples
#' # Example scores matrix
#' scores = matrix(rnorm(100), nrow = 20, ncol = 5)
#' #clusters = c(rep(0,3), rep(1,4), rep(2,5), rep(3,8))
#' set.seed(43)
#' clusters = sample(0:3, 20, replace=TRUE)
#' # Aggregate scores to cluster level
#' result = scores_clusterlevel(scores, clusters)
#' print(result)
#' 
scores_clusterlevel = function(scores, clusters, fun = "mean") {
  
  # Validate inputs
  if (nrow(scores) != length(clusters)) {
    stop("Length of 'clusters' must equal number of rows in 'scores'")
  }
  if (is.factor(clusters)) {
    stop("'clusters' must not be a factor. Use as.character(clusters) or as.integer(clusters) to convert.")
  }
  
  # Get unique clusters in sorted order
  unique_clusters = if (is.numeric(clusters)) {
    sort(unique(clusters))
  } else {
    sort(unique(as.character(clusters)))
  }
  
  # Aggregate using rowsum (much faster than apply + aggregate)
  if (fun == "mean") {

    # Use rowsum for efficiency, then divide by cluster sizes
    cluster_sums = rowsum(scores, clusters, reorder = TRUE)
    cluster_sizes = table(clusters)[rownames(cluster_sums)]
    # Note: matrix / vector division recycles the vector across rows
    # (first row divided by first element, second row by second element, etc.)
    cluster_means = cluster_sums / as.numeric(cluster_sizes)
    
    # Reorder to match sorted unique_clusters
    cluster_means = cluster_means[as.character(unique_clusters), , drop = FALSE]
    
    return(cluster_means)
  }
  
  # Fallback for other functions (if you add them later)
  stop("Only fun='mean' is currently supported")
}

