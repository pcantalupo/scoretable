# Determine the label for each row based on maximum value of the celltype scores in the row.
# scores - rows are cells or clusters and columns are celltypes
#          Rownames and colnames are required.
# unknown - do you want to calculated Unknown labels? (if TRUE, must supply 'clusters' param)
#         - only supported for cluster scores table
# clusters - vector that specifies the cluster for each cell (only needed if calculating unknown labels)
#          - only supported for cluster scores table
# cutoff - if the score is less than this value, the label is 'Unknown' (default is 0.25; based on ScType)
#          - only supported for clusters scores table
# Return - The scores table with 3 preprended columns 'clusters', 'labels', and 'score'
#          'clusters' values are taken from the rownames
#          'labels' the celltype with the maximum score
#          'score' the maximum score
add_labels_based_on_max = function(scores, unknown = FALSE, clusters, cutoff = 0.25) {
  whichmax = apply(scores, 1, which.max)  # for Group 0 the max value is column 3
  labels = colnames(scores)[whichmax]  # extract the label for each Group
  score = sapply(seq_along(whichmax), function(i){  # extract the score for each Group
    maxindex = whichmax[i]
    scores[i,maxindex]
  })
  data = data.frame(clusters = rownames(scores), labels = labels, score = score, scores, check.names = FALSE)
  if(unknown) {
    #numcells = table(clusters)
    data$labels[data$score < cutoff] = "Unknown"
  }
  return(data)
}

