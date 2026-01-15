#' Sctype esmax scores
#'
#' A small (1000 rows) esmax scores table taken randomly from the larger 36K original scores table.
#'
#' @format
#' A matrix with 1000 rows and 5 columns representing prediction scores from 
#' single-cell RNA-seq analysis. Rownames are cell barcodes. Column names are 
#' category labels. Values are prediction scores. This example dataset 
#' demonstrates the data structure expected by scoretable functions.
"esmax_scores_small"


#' Sctype esmax clusters
#'
#' A character vector of cluster assignments for the 1000 cells in esmax_scores_small.
#'
#' @format
#' A character vector of cluster assignments for the 1000 cells in esmax_scores_small.
#' They are in the same order as the rows of esmax_scores_small.
"esmax_clusters_small"

