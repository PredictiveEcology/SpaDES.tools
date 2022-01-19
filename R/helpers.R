# NA-aware comparison of two vectors
# Copied from http://www.cookbook-r.com/Manipulating_data/Comparing_vectors_or_factors_with_NA/.
compareNA <- function(v1,v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}
