# Aggregate a score table from single cell level to the cluster level
# Params
#   scores - data.frame or matrix where rows are cells and columns are celltypes.
#            Rownames or colnames are not required
#   clusters - vector of same length as number of rows in 'scores'.
#              The values specify the group for each row (i.e. c(3, 1, 0, 1, 2, 0, 3, ...)) 
# Return - A matrix with the mean scores. Columns are in the same order.
#          Rows are sorted based on the typeof(clusters) (integer -> integer sorted, character -> character sorted).
#          Rownames are set to the cluster value
scores_clusterlevel = function(scores, clusters) {
  data = apply(scores, 2, function(row) {
    aggregate(row, list(Clusters = clusters), mean)$x   # only need 'x' where 'x' is the mean
  })
  rownames(data) = as.character(sort(unique(clusters)))
  return(data)
}

