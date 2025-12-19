# Create a small esmax_scores object for testing (1000 random rows)

# Load full exmax_scores object (36K rows)
esmax_scores = t(readRDS("data-raw/esmax_scores.rds"))
set.seed(42)
esmax_scores_small = esmax_scores[sample(1:nrow(esmax_scores), 1000), ]

# Save as RDA object (use_data will create the data/ directory if needed)
usethis::use_data(esmax_scores_small, overwrite = TRUE)   # saves the .rda file by same name as the object

# Also save as an RDS object
dir.create("inst/extdata", recursive = TRUE)
saveRDS(esmax_scores_small, "inst/extdata/esmax_scores_small.rds")



