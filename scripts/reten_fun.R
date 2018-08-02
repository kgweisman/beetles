library(tidyverse)
library(psych)

# make function for implementing criteria
reten_fun <- function(df, rot_type = c("oblimin", "varimax", "none")){
  
  # figure out max number of factors to retain
  n_var <- length(names(df))
  max_k <- max_ok(n_var)
  
  # run efa with max factors, unrotated
  fa_unrot <- fa(df, nfactors = max_k, rotate = "none", 
                 scores = "tenBerge", impute = "median")
  eigen <- fa_unrot$Vaccounted %>%
    data.frame() %>%
    rownames_to_column("param") %>%
    gather(factor, value, -param) %>%
    spread(param, value) %>%
    filter(`SS loadings` > 1, `Proportion Explained` > 0.05)
  retain_k <- nrow(eigen)
  
  fa_rot <- fa(df, nfactors = retain_k, rotate = rot_type,
               scores = "tenBerge", impute = "median")
  
  loadings <- fa_rot$loadings[] %>%
    data.frame() %>%
    rownames_to_column("capacity") %>%
    gather(factor, loading, -capacity) %>%
    group_by(capacity) %>%
    top_n(1, abs(loading)) %>%
    ungroup() %>%
    count(factor)
  retain_k_final <- nrow(loadings)
  
  return(retain_k_final)
}