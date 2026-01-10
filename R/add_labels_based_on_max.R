#' Add labels based on maximum score
#' 
#' Determine the label for each row based on maximum value of the scores in the row.
#'
#' @param scores A data frame or matrix of scores with row and column names. Rows represent observations (e.g., cells, samples, or groups) and columns represent label categories (e.g., cell types, classes).
#' @param unknown Logical value indicating whether to assign 'Unknown' labels based on a cutoff score
#' @param cutoff A numeric value between 0 and 1 specifying the cutoff score for assigning 'Unknown' labels (default is 0.25)
#' 
#' @details For each row in the scores table, the function identifies the column with the maximum score and assigns the corresponding column name as the label for that row. If the 'unknown' parameter is set to TRUE, and the maximum score for a row is below the specified cutoff the label for that row is set to 'Unknown'.
#'
#' @return A data frame with 3 preprended columns 'obs_id', 'labels', and 'score', followed by the original scores.
#'    'obs_id' values are taken from the rownames of the input scores table.
#'    'labels' contains the label with the maximum score for each row.
#'    'score' contains the maximum score for each row.
#' @export
#'
#' @examples
#' # Example scores matrix
#' scores = matrix(c(0.1, 0.7, 0.2,
#'                   0.5, 0.5, 0.2,
#'                   0.2, 0.3, 0.4), nrow=3, byrow=TRUE)
#' colnames(scores) = c("TypeA", "TypeB", "TypeC")
#' rownames(scores) = c("Obs1", "Obs2", "Obs3")
#' # Add labels based on maximum score
#' result = add_labels_based_on_max(scores, unknown = TRUE)
#' print(result)
#' # Set cutoff to get Unknown label
#' result = add_labels_based_on_max(scores, unknown = TRUE, cutoff = 0.45)
#' print(result)
#' 
add_labels_based_on_max = function(scores, unknown = FALSE, cutoff = 0.25) {

  stopifnot(is.data.frame(scores) || is.matrix(scores))
  if (is.matrix(scores)) {
    stopifnot(is.numeric(scores))
  } else {
    stopifnot(unique(apply(scores, 2, class)) == "numeric")
  }
  
  if (is.null(colnames(scores)) | is.null(rownames(scores))) {
    stop("Row and column names are required in the scores table")
  }
  
  # Get column index position for each row that has the maximum value 
  max_idx  <- max.col(scores, ties.method = "first")

  # Extract the label for each observation
  labels = colnames(scores)[max_idx]
  
  # Extract the score for each observation
  # Use matrix indexing with (row, column) pairs
  score = scores[cbind(1:nrow(scores), max_idx)]
  
  scores_with_labels = data.frame(obs_id = rownames(scores), labels = labels,
                                  score = score, scores, check.names = FALSE)
  
  if (unknown) {
    scores_with_labels$labels[scores_with_labels$score < cutoff] = "Unknown"
  }
  
  return(scores_with_labels)
}

