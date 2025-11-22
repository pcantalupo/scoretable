pacman::p_load(tidyverse)

# Create scores matrix with row and column names
set.seed(15)
m = matrix(rnorm(30), nrow = 10)
colnames(m) = c("TCell","BCell","Macro")
rownames(m) = paste0("obs", 1:nrow(m))
m


# Clusters level ------------------------#
clusters = c(rep(0,5),rep(1,5)); clusters   # define two clusters 0 and 1

# aggregate single cell scores to cluster level
cluster.scores = scores_clusterlevel(m, clusters)
cluster.scores

# annotate clusters (adds two columns 'clusters' and 'labels')
cluster.scores.labels = add_labels_based_on_max(cluster.scores)
cluster.scores.labels

# annotate clusters with Unknowns based on a cutoff
add_labels_based_on_max(cluster.scores, unknown = TRUE, clusters, cutoff = 0.35)

deconvolute_clusterlabel(cluster.scores.labels, clusters)



# Single cell level ------------------------#
scores.labels = add_labels_based_on_max(m)
scores.labels
table(scores.labels$labels)

plot_scores_by_pred(scores.labels, jitter = TRUE)

plot_signal_vs_noise(scores.labels)
plot_signal_vs_noise(scores.labels, jitter = TRUE)






