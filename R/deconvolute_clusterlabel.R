# Purpose: You have cluster labels (in your cluster scores table) and you want to expand these labels to the cell level so you can add them to a table of cell metadata
# scores - data.frame of cluster scores containing at least two columns: 'clusters' and 'labels' (rows are clusters)
# clusters - vector that specifies the cluster for each cell 
deconvolute_clusterlabel = function(scores, clusters) {
  return(scores$labels[match(clusters, scores$clusters)]) 
}





