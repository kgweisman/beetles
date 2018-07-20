d_all <- d_us_ad_pilot_23 %>%
  rownames_to_column("subid") %>%
  full_join(d_gh_ch_23 %>%
              rownames_to_column("subid") %>%
              mutate(subid = paste("gh_ch", subid, sep = "_"))) %>%
  full_join(d_gh_ch_fante_23 %>%
              remove_rownames() %>%
              rownames_to_column("subid") %>%
              mutate(subid = paste("gh_ch_fante", subid, sep = "_"))) %>%
  full_join(d_th_ad_23 %>%
              rownames_to_column("subid") %>%
              mutate(subid = paste("th_ad", subid, sep = "_"))) %>%
  full_join(d_th_ch_23 %>%
              rownames_to_column("subid") %>%
              mutate(subid = paste("th_ch", subid, sep = "_"))) %>%
  full_join(d_vt_ad_23 %>%
              rownames_to_column("subid") %>%
              mutate(subid = paste("vt_ad", subid, sep = "_"))) %>%
  full_join(d_vt_ch_23 %>%
              rownames_to_column("subid") %>%
              mutate(subid = paste("vt_ch", subid, sep = "_"))) %>%
  left_join(subids) %>%
  mutate(site = ifelse(grepl("R_", subid), "US", toupper(substr(subid, 1, 2))),
         age = case_when(grepl("_ad", subid) ~ "adults",
                         grepl("R_", subid) ~ "adults",
                         grepl("_ch", subid) ~ "children"),
         language = case_when(site == "US" ~ "English",
                              site == "TH" ~ "Thai",
                              site == "VT" ~ "Bislama",
                              site == "GH" & age == "adults" ~ "Fante",
                              site == "GH" & grepl("fante", subid) ~ "Fante",
                              site == "GH" & age == "children" & !grepl("fante", subid) ~ "English")) %>%
  filter(!is.na(character)) %>%
  distinct() %>%
  gather(capacity, response, -c(subid, character, site, age, language))

d_all %>%
  filter(character == "God", !is.na(response)) %>%
  filter(!grepl("ch_fante", subid)) %>%
  ggplot(aes(x = interaction(site, age, language),
             fill = factor(site),
             alpha = factor(response))) +
  facet_wrap(~ capacity, ncol = 6) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Dark2") +
  scale_alpha_discrete(range = c(0.2, 1), labels = c("No", "Kind of", "Yes")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(title = "Attributions of mental capacities to God",
       x = "Sample", y = "Proportion of participants",
       fill = "Site", alpha = "Response")

d_all %>%
  filter(character == "ghosts", !is.na(response)) %>%
  filter(!grepl("ch_fante", subid)) %>%
  ggplot(aes(x = interaction(site, age, language),
             fill = factor(site),
             alpha = factor(response))) +
  facet_wrap(~ capacity, ncol = 6) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Dark2") +
  scale_alpha_discrete(range = c(0.2, 1), labels = c("No", "Kind of", "Yes")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(title = "Attributions of mental capacities to ghosts",
       x = "Sample", y = "Proportion of participants",
       fill = "Site", alpha = "Response")
