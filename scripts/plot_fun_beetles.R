library(tidyverse)
library(psych)

# make function for generating heatmap
heatmap_fun <- function(efa){
  
  # get factor loadings
  loadings <- efa$loadings[] %>%
    data.frame() %>%
    rownames_to_column("capacity") %>%
    gather(factor, loading, -capacity)
  
  # get fa.sort() order
  order <- loadings %>%
    group_by(capacity) %>%
    top_n(1, abs(loading)) %>%
    ungroup() %>%
    arrange(desc(factor), abs(loading)) %>%
    mutate(order = 1:length(levels(factor(loadings$capacity)))) %>%
    select(capacity, order)

  # get percent shared variance explained
  shared_var <- efa$Vaccounted %>%
    data.frame() %>%
    rownames_to_column("stat") %>%
    filter(stat == "Proportion Explained") %>%
    select(-stat) %>%
    gather(factor, var) %>%
    mutate(var = paste0(factor, "\n(", round(var, 2)*100, "%)"))
  
  # make plot
  plot <- ggplot(loadings %>% 
                   left_join(order) %>%
                   left_join(shared_var) %>%
                   mutate(capacity = gsub("_", " ", capacity)),
                 aes(x = var, 
                     y = reorder(capacity, order), 
                     fill = loading, 
                     label = format(round(loading, 2), nsmall = 2))) +
    geom_tile(color = "black") +
    geom_text(size = 3) +
    scale_fill_distiller(limits = c(-1, 1), 
                         palette = "RdYlBu",
                         guide = guide_colorbar(barheight = 20)) +
    theme_minimal() +
    scale_x_discrete(position = "top") +
    labs(x = "", y = "")
  
  return(plot)
  
}

# make function for plotting factor scores by factor, target
scoresplot_fun <- function(efa, target){
  
  # generate list of targets
  if(target == "all"){
    target_list <- c("beetles", "cell phones", "chickens", 
                     "children", "dogs", "flowers", "ghosts", 
                     "God", "mice", "rocks")
  } else if(target == "supernatural"){
    target_list <- c("ghosts", "God")
  } else if(target %in% c("god", "God")){
    target_list <- "God"
  } else {
    target_list <- target
  }
  
  # make usable dataframe
  df <- efa$scores[] %>%
    data.frame() %>%
    rownames_to_column("subid") %>%
    mutate(ResponseId = gsub("_target.*$", "", subid),
           target = gsub("^.*target_", "", subid),
           site = substr(subid, 1, 2),
           age = substr(subid, 4, 5)) %>%
    filter(target %in% target_list) %>%
    select(-subid) %>%
    gather(factor, score, -c(site, age, ResponseId, target)) %>%
    mutate(target = factor(target,
                           levels = c("rocks", "flowers", "cell phones",
                                      "beetles", "chickens", "mice", "dogs",
                                      "children", "ghosts", "God")),
           site = recode_factor(site,
                                "us" = "US", 
                                "gh" = "Ghana", 
                                "th" = "Thailand", 
                                "ch" = "China", 
                                "vt" = "Vanuatu"),
           age = recode_factor(age,
                               "ad" = "adults",
                               "ch" = "children"))
  
  # get bootstrapped means
  df_boot <- df %>%
    group_by(site, age, target, factor) %>%
    multi_boot_standard("score", na.rm = T) %>%
    ungroup() %>%
    mutate(target = factor(target,
                           levels = c("rocks", "flowers", "cell phones",
                                      "beetles", "chickens", "mice", "dogs",
                                      "children", "ghosts", "God")),
           site = recode_factor(site,
                                "us" = "US", 
                                "gh" = "Ghana", 
                                "th" = "Thailand", 
                                "ch" = "China", 
                                "vt" = "Vanuatu"),
           age = recode_factor(age,
                               "ad" = "adults",
                               "ch" = "children"))
  
  # get first items for subtitle
  first_items <- efa$loadings[] %>%
    data.frame() %>%
    rownames_to_column("capacity") %>%
    gather(factor, loading, -capacity) %>%
    group_by(factor) %>%
    top_n(3, abs(loading)) %>%
    mutate(capacity = gsub("_", " ", capacity),
           cap_list = str_c(capacity, collapse = ", "),
           cap_list = paste0(cap_list, "...")) %>%
    ungroup() %>%
    select(-capacity, -loading) %>%
    distinct() %>%
    mutate(funs(as.character))
  
  # get percent shared variance explained for subtitle
  shared_var <- efa$Vaccounted %>%
    data.frame() %>%
    rownames_to_column("stat") %>%
    filter(stat == "Proportion Explained") %>%
    select(-stat) %>%
    gather(factor, var) %>%
    mutate(var = paste0(round(var, 2)*100, "% of shared variance"))
  
  subtitle <- c()
  for(i in 1:nrow(first_items)){
    subtitle <- paste0(subtitle,
                       first_items[i,1], 
                       " (", shared_var[i,2], "): ",
                       first_items[i,2], 
                       "\n")
  }
  subtitle <- gsub("\\n$", "", subtitle)
  
  # make plot
  plot <- ggplot(df,
                 aes(x = target, 
                     y = score, 
                     color = factor)) +
    facet_grid(rows = vars(factor), cols = vars(site, age)) +
    geom_hline(yintercept = 0, lty = 2, color = "darkgray") +
    geom_point(alpha = 0.25, 
               position = position_jitter(width = 0.25, height = 0)) +
    geom_pointrange(data = df_boot,
                    aes(y = mean, ymin = ci_lower, ymax = ci_upper),
                    color = "black", fatten = 0.75) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    labs(x = "target character",
         y = "factor score",
         subtitle = subtitle,
         caption = "Error bars are bootstrapped 95% confidence intervals") +
    guides(color = "none")
  # guides(color = guide_legend(override.aes = list(alpha = 1, size = 1)))
  
  return(plot)
  
}

# make function for plotting individual item means by factor, target
itemsplot_fun <- function(efa, target){
  
  # generate list of targets
  if(target == "all"){
    target_list <- c("beetles", "cell phones", "chickens", 
                     "children", "dogs", "flowers", "ghosts", 
                     "God", "mice", "rocks")
  } else if(target == "supernatural"){
    target_list <- c("ghosts", "God")
  } else if(target %in% c("god", "God")){
    target_list <- "God"
  } else {
    target_list <- target
  }
  
  # make usable dataframe
  df <- d_all %>%
    rownames_to_column("subid") %>%
    mutate(ResponseId = gsub("_target.*$", "", subid),
         target = gsub("^.*target_", "", subid),
         site = substr(subid, 1, 2),
         age = substr(subid, 4, 5)) %>%
    filter(target %in% target_list) %>%
    select(-subid) %>%
    gather(capacity, response, -c(site, age, ResponseId, target)) %>%
    mutate(target = factor(target,
                           levels = c("rocks", "flowers", "cell phones",
                                      "beetles", "chickens", "mice", "dogs",
                                      "children", "ghosts", "God")),
           site = recode_factor(site,
                                "us" = "US", 
                                "gh" = "Ghana", 
                                "th" = "Thailand", 
                                "ch" = "China", 
                                "vt" = "Vanuatu"),
           age = recode_factor(age,
                               "ad" = "adults",
                               "ch" = "children"))
  
  # get factor loadings
  loadings <- efa$loadings[] %>%
    data.frame() %>%
    rownames_to_column("capacity") %>%
    gather(factor, loading, -capacity)
  
  # get fa.sort() order
  order <- efa$loadings[] %>%
    data.frame() %>%
    rownames_to_column("capacity") %>%
    gather(factor, loading, -capacity) %>%
    group_by(capacity) %>%
    top_n(1, abs(loading)) %>%
    ungroup() %>%
    arrange(desc(factor), abs(loading)) %>%
    mutate(order = 1:length(levels(factor(loadings$capacity)))) %>%
    select(factor, capacity, order)
  
  # add order to df
  df <- df %>% left_join(order)
  
  # get bootstrapped means
  df_boot <- df %>%
    group_by(site, age, target, factor, capacity, order) %>%
    multi_boot_standard("response", na.rm = T) %>%
    ungroup() %>%
    mutate(capacity = gsub("_", " ", capacity))
  
  # make plot
  plot <- ggplot(df_boot %>%
                   left_join(order) %>%
                   mutate(capacity = gsub("_", " ", capacity)),
                 aes(x = reorder(capacity, order),
                     y = mean, ymin = ci_lower, ymax = ci_upper,
                     color = target, shape = target)) +
    facet_grid(rows = vars(factor),
               cols = vars(site, age), 
               scales = "free", space = "free") +
    geom_pointrange(position = position_dodge(width = 0.5)) +
    scale_y_continuous(breaks = c(0, 0.5, 1), 
                       labels = c("no", "kinda", "yes")) +
    theme_bw() +
    theme(legend.position = "top",
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    labs(y = "response", x = "",
         color = "target character", shape = "target character",
         caption = "Error bars are bootstrapped 95% confidence intervals") +
    coord_flip()
  
  return(plot)
  
}