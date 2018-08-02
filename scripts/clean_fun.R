library(tidyverse)

clean_fun <- function(df, key, 
                      site = c("us", "gh", "th", "ch", "vt"), 
                      age = c("ad", "ch"),
                      ex_final3 = TRUE, ex_addsub = FALSE,
                      cap_exclude = NULL) {
  
  df_clean <- df %>%
    full_join(key) %>%
    filter(!question %in% cap_exclude,
           !is.na(subnum), !is.na(question)) %>%
    mutate(responseNum = recode(response,
                                "no" = 0,
                                "kind of" = 0.5,
                                "yes" = 1),
           responseNum = as.numeric(responseNum)) %>%
    rename(capacity = question) %>%
    distinct(subnum, character, capacity, responseNum) %>%
    spread(capacity, responseNum) %>%
    select(-subnum) %>%
    remove_rownames() %>%
    mutate(subid = paste(site, age, 
                         10001:(10000+length(levels(factor(df$subnum)))),
                         "target", character,
                         sep = "_")) %>%
    column_to_rownames("subid") %>%
    select(-character)
  
  if(ex_final3) {
    df_clean <- df_clean %>% 
      select(-c(`bleed if they touch something sharp`,
                `have minds`, `have souls`))
  }
  
  if(ex_addsub) {
    df_clean <- df_clean %>% 
      select(-c(`add and subtract numbers`))
  }
  
  return(df_clean)
  
}