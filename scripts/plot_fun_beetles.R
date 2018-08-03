library(tidyverse)
library(psych)

# make function for generating heatmap
heatmap_fun <- function(efa, factor_names = NA){
  
  # get factor names
  if(is.na(factor_names)){
    factor_names <- paste("Factor", 1:efa$factors)
  }
  
  # get factor loadings
  loadings <- efa$loadings[] %>%
    data.frame() %>%
    rownames_to_column("capacity") %>%
    gather(factor, loading, -capacity) %>%
    mutate(factor = as.character(factor(factor, labels = factor_names)))
  
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
    mutate(factor = as.character(factor(factor, labels = factor_names))) %>%
    mutate(var = paste0(factor, "\n(", round(var, 2)*100, "% shared var.)"))
  
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
scoresplot_fun <- function(efa, target, highlight = "none", factor_names = NA){
  
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
  
  # generate list of targets to highlight
  if(highlight == "none"){
    highlight_list <- c()
  } else if(highlight == "supernatural"){
    highlight_list <- c("ghosts", "God")
  } else {
    highlight_list <- highlight
  }
  
  # get factor names
  if(is.na(factor_names)){
    factor_names <- paste("Factor", 1:efa$factors)
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
                               "ch" = "children"),
           highlight = factor(ifelse(target %in% highlight_list,
                                     "highlight", "no_highlight"),
                              levels = c("no_highlight", "highlight")),
           factor = as.character(factor(factor, labels = factor_names)))
  
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
                               "ch" = "children"),
           highlight = factor(ifelse(target %in% highlight_list,
                                     "highlight", "no_highlight"),
                              levels = c("no_highlight", "highlight")))
  
  # get first items for subtitle
  first_items <- efa$loadings[] %>%
    data.frame() %>%
    rownames_to_column("capacity") %>%
    gather(factor, loading, -capacity) %>%
    mutate(factor = as.character(factor(factor, labels = factor_names))) %>%
    group_by(factor) %>%
    top_n(3, abs(loading)) %>%
    mutate(capacity = gsub("_", " ", capacity),
           cap_list = str_c(capacity, collapse = ", "),
           cap_list = paste0(cap_list, "...")) %>%
    ungroup() %>%
    select(-capacity, -loading) %>%
    distinct() %>%
    mutate(funs(as.character))
  
  # get percent shared variance explained
  shared_var <- efa$Vaccounted %>%
    data.frame() %>%
    rownames_to_column("stat") %>%
    filter(stat == "Proportion Explained") %>%
    select(-stat) %>%
    gather(factor, var) %>%
    mutate(factor = as.character(factor(factor, labels = factor_names)),
           var = paste0(round(var, 2)*100, "% shared variance"))
  
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
  plot <- ggplot(df, aes(x = target, y = score, fill = factor)) +
    facet_grid(rows = vars(factor), cols = vars(site, age)) +
    geom_hline(yintercept = 0, lty = 2, color = "darkgray") +
    geom_point(alpha = 0.25, color = "NA", shape = 21,
               position = position_jitter(width = 0.25, height = 0)) +
    geom_errorbar(data = df_boot,
                  aes(y = mean, ymin = ci_lower, ymax = ci_upper, 
                      color = highlight),
                  width = 0) +
    geom_point(data = df_boot,
               aes(y = mean, 
                   color = highlight, size = highlight)) +
    scale_fill_brewer(palette = "Set1") +
    scale_color_manual(values = c("black", "#984ea3")) +
    scale_size_manual(values = c(0.75, 2)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    labs(x = "target entity",
         y = "factor score",
         subtitle = subtitle,
         caption = "Error bars are bootstrapped 95% confidence intervals") +
    guides(fill = "none", color = "none", size = "none")
  # guides(color = guide_legend(override.aes = list(alpha = 1, size = 1)))
  
  return(plot)
  
}

# make function for plotting individual item means by factor, target
itemsplot_fun <- function(efa, target, factor_names = NA){
  
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
  
  # get factor names
  if(is.na(factor_names)){
    factor_names <- paste("Factor", 1:efa$factors)
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
    gather(factor, loading, -capacity) %>%
    mutate(factor = as.character(factor(factor, labels = factor_names)))
  
  # get fa.sort() order
  order <- efa$loadings[] %>%
    data.frame() %>%
    rownames_to_column("capacity") %>%
    gather(factor, loading, -capacity) %>%
    mutate(factor = as.character(factor(factor, labels = factor_names))) %>%
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
    mutate(capacity = gsub("_", " ", capacity),
           factor = as.character(factor(factor, labels = factor_names)))
  
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
         color = "target entity", shape = "target entity",
         caption = "Error bars are bootstrapped 95% confidence intervals") +
    coord_flip()
  
  return(plot)
  
}