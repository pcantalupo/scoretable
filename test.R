



### Plotting
# annotate score table
pred = add_labels_based_on_max(scores)
head(pred)
table(pred$labels)

# Plot scores by Pred
p = plot_scores_by_pred(pred, jitter = TRUE)
stopifnot("ggplot" %in% class(p))

# Plot signal vs noise
p2 = plot_signal_vs_noise(pred)
stopifnot("ggplot" %in% class(p2))

plot_signal_vs_noise(pred, jitter = TRUE)



##########################################
# Clusters level ------------------------#
#m = matrix(rnorm(100), nrow = 20, ncol = 5)
set.seed(1)  # need seed 1 so that when unknowns are determined there is one cluster per label
clusters = sample(0:3, 20, replace=TRUE)
table(clusters)


### DataFrame
scores = as.data.frame(m)
colnames(scores) = c("TCell", "BCell", "Macro")

# Aggregate observations to cluster level
(scores.cluster = scores_clusterlevel(scores, clusters))

# Annotate
(pred = add_labels_based_on_max(scores.cluster))
# Annotate - Unknown test
(pred = add_labels_based_on_max(scores.cluster, unknown = TRUE, cutoff = 0.41))

# Expand cluster labels to observation level
expand_clusterlabel(pred, clusters)





#######################################
# ESMAX data (sampled 1000 rows)
scores = esmax_scores_small
dim(scores)  # cells rows X celltypes columns
head(rownames(scores))
colnames(scores)
min(scores); max(scores)

## Single cell level
pred = add_labels_based_on_max(scores)
head(pred,2)
table(pred$labels)

p1 = plot_scores_by_pred(pred)
p1
p2 = plot_signal_vs_noise(pred)
p2
# Document this ability to set rows rather than adding another parameter to the function
p2 + 
  facet_wrap(~ labels, scales = "free_y", nrow = 3)

pred_wunknown = add_labels_based_on_max(scores, unknown = TRUE)
table(pred_wunknown$labels)
p1 = plot_scores_by_pred(pred_wunknown)
p1
p2 = plot_signal_vs_noise(pred_wunknown)
p2



# Clusters level ------------------------#
# define random clusters for each row of scores
set.seed(1)
clusters = sample(0:9, 1000, replace=TRUE)
table(clusters)

# aggregate scores to cluster level
scores.cluster = scores_clusterlevel(scores, clusters = clusters)
scores.cluster

(pred = add_labels_based_on_max(scores.cluster))
table(pred$labels)

p1 = plot_scores_by_pred(pred)
p1
p2 = plot_signal_vs_noise(pred, jitter = TRUE)
p2
# Document this ability to set rows rather than adding another parameter to the function
p2 + 
  facet_wrap(~ labels, scales = "free_y", nrow = 3)

pred_wunknown = add_labels_based_on_max(scores.cluster, unknown = TRUE, cutoff = 0.1)
table(pred_wunknown$labels)
p1 = plot_scores_by_pred(pred_wunknown)
p1
p2 = plot_signal_vs_noise(pred_wunknown)
p2


# Expand cluster labels to observation level
expand_clusterlabel(pred, clusters)





