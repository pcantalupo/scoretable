pacman::p_load(tidyverse)


# Create a matrix of scores 'm' (no row nor colnames)
set.seed(15)
m = matrix(rnorm(30), nrow = 10)

# Data Frame
scores = as.data.frame(m)
colnames(scores)  # default V1, etc... colnames are added 
rownames(scores)  # default 1, 2, etec... rownamew are added

colnames(scores) = NULL
rownames(scores) = NULL   # rownames are never eliminated with a dataframe
scores

(result = add_labels_based_on_max(scores))

colnames(scores) = c("TCell","BCell","Macro")
(result = add_labels_based_on_max(scores))

rownames(scores) = paste0("c", rep(1:nrow(scores)))
(result = add_labels_based_on_max(scores))


# Matrix
scores = m
colnames(scores)
rownames(scores)

(result = add_labels_based_on_max(scores))

colnames(scores) = c("TCell","BCell","Macro")
(result = add_labels_based_on_max(scores))

rownames(scores) = paste0("c", rep(1:nrow(scores)))
(result = add_labels_based_on_max(scores))


# Unknown test
(result = add_labels_based_on_max(scores, unknown = TRUE)) # default cutoff 0.25
(result = add_labels_based_on_max(scores, unknown = TRUE, cutoff = 0.45))





# Clusters level ------------------------#
clusters = c(rep(0,5),rep(1,5)); clusters   # define two clusters 0 and 1

scores = as.data.frame(m)
colnames(scores) = c("TCell","BCell","Macro")

# aggregate single cell scores to cluster level
cluster.scores = scores_clusterlevel(scores, clusters)
cluster.scores

# annotate clusters (adds two columns 'clusters' and 'labels')
cluster.scores.labels = add_labels_based_on_max(cluster.scores)
cluster.scores.labels

# annotate clusters with Unknowns based on a cutoff
add_labels_based_on_max(cluster.scores, unknown = TRUE, cutoff = 0.35)

deconvolute_clusterlabel(cluster.scores.labels, clusters)



# Single cell level ------------------------#
scores.labels = add_labels_based_on_max(scores)
scores.labels
table(scores.labels$labels)


# Plot scores by Pred
plot_scores_by_pred(scores.labels, jitter = TRUE)

# Plot signal vs noise
plot_signal_vs_noise(scores.labels)
plot_signal_vs_noise(scores.labels, jitter = TRUE)











