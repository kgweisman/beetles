library(tidyverse)

s_moments <- function(p) {p*(p+1)/2}
param_est <- function(p, k) {p*k + p - (k*(k-1)/2)}

check_ok <- function(p, k) {
  a <- (p-k)^2
  b <- p+k
  return(ifelse(a>b, TRUE, FALSE))
}

max_ok <- function(p) {
  df_check <- data.frame()
  for(i in 1:p){
    df_check[i,"check"] <- check_ok(p,i)
  }
  max <- df_check %>% filter(check) %>% nrow()
  return(max)
}

max_ok(40)
max_ok(23)
max_ok(23-9)



# df <- function(p,k){
#   s_mom <- s_moments(p)
#   p_est <- param_est(p, k)
#   df <- s_mom - p_est
#   return(df)
# }

# df(6,3)
# df(23,16)
# df(40,31)

# df_p <- 1:40
# df_k <- 1:40
# df_df <- data.frame()
# for(i in 1:length(df_p)) {
#   for(j in 1:length(df_k)) {
#     df_df[i, j] <- df(df_p[i], df_k[j])
#   }
# }
# 
# rownames(df_df) <- df_p
# colnames(df_df) <- df_k
# 
# df_df <- df_df %>%
#   rownames_to_column("p") %>%
#   gather(k, df, -p) %>%
#   mutate_all(vars(as.numeric)) %>%
#   mutate(k = factor(k))
# 
# ggplot(df_df, aes(x = p, y = df, group = k, color = factor(k), label = k)) +
#   geom_line() +
#   geom_hline(yintercept = 0, lty = 1) +
#   # geom_text(color = "black") +
#   theme_minimal()
