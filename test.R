source("score_table.R")

set.seed(15)
m = matrix(rnorm(30), nrow = 10)
colnames(m) = c("foo","bar","baz")
m
clusters = c(rep(0,5),rep(1,5))
clusters
cluster.scores = scores_clusterlevel(m, clusters)
cluster.scores
cluster.scores.labels = add_labels_based_on_max(cluster.scores)
cluster.scores.labels
add_labels_based_on_max(cluster.scores, unknown = TRUE, clusters, cutoff = 0.35)$label
deconvolute_clusterlabel(cluster.scores.labels, clusters)

