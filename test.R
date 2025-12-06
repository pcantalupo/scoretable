pacman::p_load(tidyverse)


##########################################
# Observation level ------------------------#
# Create a matrix of scores 'm' (no row nor colnames) - 20 rows, 3 columns
set.seed(15)
m = matrix(rnorm(60), nrow = 20)
m

### Data Frame
scores = as.data.frame(m)
colnames(scores)  # default V1, etc... colnames are added by default
rownames(scores)  # default 1, 2, etec... rownamew are added by default

colnames(scores) = NULL
rownames(scores) = NULL   # rownames are never eliminated with a dataframe
scores

# Annotate (adds clusters, labels, score)
(pred = add_labels_based_on_max(scores))  # should give error due to colnames missing

colnames(scores) = c("TCell","BCell","Macro")  # add colnames
(pred = add_labels_based_on_max(scores))     # success



### Matrix
scores = m
colnames(scores); rownames(scores)
scores
class(scores)

# Annotate
(pred = add_labels_based_on_max(scores)) # error because both row and col names are missing
class(pred)  # data.frame

colnames(scores) = c("TCell","BCell","Macro")  # add colname but still error b/c rownames missing
(pred = add_labels_based_on_max(scores))

rownames(scores) = paste0("c", rep(1:nrow(scores))) # add row names 
(pred = add_labels_based_on_max(scores))  # success


# Annotate - Unknown test
(pred = add_labels_based_on_max(scores, unknown = TRUE)) # default cutoff 0.25
(pred = add_labels_based_on_max(scores, unknown = TRUE, cutoff = 0.5))


### Plotting
(pred = add_labels_based_on_max(scores))
table(pred$labels)

# Plot scores by Pred
p = plot_scores_by_pred(pred, jitter = TRUE)
class(p)

# Plot signal vs noise
p2 = plot_signal_vs_noise(pred)
class(p2)
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














