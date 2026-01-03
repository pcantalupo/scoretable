# scoretable

A general-purpose R package for annotating and visualizing observation-by-category score matrices.

## Overview

scoretable helps you:
- Automatically assign labels to observations based on highest scores
- Aggregate scores from observation-level to group-level  
- Visualize score distributions across predicted categories
- Explore signal-to-noise ratios in predictions

While originally developed for single-cell RNA-seq cell type annotation, 
scoretable works with any domain where you have observations scored across 
multiple categories: document classification, patient stratification, 
image recognition, survey responses, and more.