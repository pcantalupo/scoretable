#' Aggregate Score Table to Group level
#' 
#' Aggregate a score table composed of individual observations to a group level. For example, in scRNAseq data this would aggregate scores from single cells to clusters; in other contexts it could aggregate samples to treatment groups or patients to cohorts.
#'
#' @param scores A numeric matrix or data frame where rows are observations 
#'   (e.g., cells) and columns are scores (e.g., cell type predictions). 
#'   Rownames nor column names are necessary
#' @param groups A numeric or character vector of length nrow(scores) 
#'   specifying group membership (e.g., c(3, 1, 0, 1, 2, 0, 3, ...)). 
#'   Must NOT be a factor. Numeric groups will sort numerically, 
#'   character groups will sort alphabetically.
#' @param fun Aggregation function to use. Currently only "mean" is supported. 
#'   Default is "mean".
#'
#' @details
#' This function collapses observation-level scores into group-level summaries by 
#' computing the mean score for each group. The input matrix with M rows (observations) 
#' is reduced to N rows (groups), where N is the number of unique groups.
#' 
#' Common use cases:
#' \itemize{
#'   \item Summarizing prediction scores from observations to group level
#'   \item Creating group-level heatmaps or visualizations
#'   \item Reducing noise by averaging scores across similar observations
#'   \item Comparing score patterns between populations
#' }
#' 
#' The function uses \code{rowsum()} for efficient aggregation, making it suitable 
#' for large datasets. Group IDs are sorted in the output (numerically for numeric 
#' groups, alphabetically for character groups) for consistent ordering.
#'
#' @return A matrix with the mean scores. Columns are in the same order as input. 
#'   Rows are sorted by group ID (numeric groups sort numerically, 
#'   character groups sort alphabetically). Rownames are the group IDs.
#' @export
#'
#' @examples
#' # Example scores matrix
#' scores = matrix(rnorm(100), nrow = 20, ncol = 5)
#' #groups = c(rep(0,3), rep(1,4), rep(2,5), rep(3,8))
#' set.seed(43)
#' groups = sample(0:3, 20, replace=TRUE)
#' # Aggregate scores to group level
#' result = scores_grouplevel(scores, groups)
#' print(result)
#' 
scores_grouplevel = function(scores, groups, fun = "mean") {
  
  # Validate inputs
  if (nrow(scores) != length(groups)) {
    stop("Length of 'groups' must equal number of rows in 'scores'")
  }
  if (is.factor(groups)) {
    stop("'groups' must not be a factor. Use as.character(groups) or as.integer(groups) to convert.")
  }
  
  # Get unique groups in sorted order
  unique_groups = if (is.numeric(groups)) {
    sort(unique(groups))
  } else {
    sort(unique(as.character(groups)))
  }
  
  # Aggregate using rowsum (much faster than apply + aggregate)
  if (fun == "mean") {

    # Use rowsum for efficiency, then divide by group sizes
    group_sums = rowsum(scores, groups, reorder = TRUE)
    group_sizes = table(groups)[rownames(group_sums)]
    # Note: matrix / vector division recycles the vector across rows
    # (first row divided by first element, second row by second element, etc.)
    group_means = group_sums / as.numeric(group_sizes)
    
    # Reorder to match sorted unique_groups
    group_means = group_means[as.character(unique_groups), , drop = FALSE]
    
    return(group_means)
  }
  
  # Fallback for other functions (if you add them later)
  stop("Only fun='mean' is currently supported")
}

