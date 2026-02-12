library(tidyverse)
library(igraph)
library(ggraph)
library(betalink)


########## Accuracy of 400 Predictions ##########


# Read predictions

gbif_predictions = read_csv("Plant-Pollinator/Predictions_Sep15.csv")

# Calculate accuracy for predicting interactions

sum(gbif_predictions$interaction_match[gbif_predictions$interaction_match == T]) / nrow(gbif_predictions) # accuracy for interactions
  
# Calculate accuracy for predicting species 2

species_exact_matches = gbif_predictions %>% 
  mutate(species2_input = replace_na(species2_input, "no species"), 
         species2_llm = replace_na(species2_llm, "no species")) %>% 
  mutate(species_exact_matches = if_else(tolower(species2_input) == tolower(species2_llm), 1, 0))

sum(species_exact_matches$species_exact_matches) / nrow(species_exact_matches) # accuracy for exact matches, including no species

species_partial_matches = species_exact_matches %>% 
  filter(species_exact_matches == 0) %>% 
  mutate(species_partial_matches = if_else(tolower(species2_llm) %in% str_split(tolower(species2_input), ", "), 0, 1))

sum(species_partial_matches$species_partial_matches) / nrow(species_exact_matches) # accuracy for partial matches

# Double check if species 2 is indeed not matching

species_others = species_partial_matches %>% 
  filter(species_partial_matches == 0)


########## Compare GBIF to GloBi ##########


# Read GBIF and GloBi data

gbif_all = read_csv("Plant-Pollinator/plant_poll_1000_output.csv")
globi_all = read_csv("Plant-Pollinator/Vanessa_annabella.csv")

# Filter GBIF data for pollination only

gbif_poll = gbif_all %>% 
  filter(interaction == "Pollination" & !is.na(species2)) %>% 
  # separate multiple species 2
  separate_wider_delim(cols = species2_corrected, delim = ", ", names = c("species2_A", "species2_B"), 
                       too_few = "align_start", too_many = "merge") %>% 
  pivot_longer(species2_A:species2_B, names_to = "number", values_to = "species2_corrected") %>% 
  select(-number) %>% 
  filter(!is.na(species2_corrected)) %>% 
  mutate(new_id = 1:nrow(.))

# Find exact match of species between GBIF and GloBi

gbif_spp_match = gbif_poll %>% 
  filter(species2_corrected %in% globi_all$target_taxon_name)

# Examine other GBIF taxa not in GloBi

gbif_others = gbif_poll %>% 
  filter(!new_id %in% gbif_spp_match$new_id)

# In GloBi, manually find species that are in the same genus
# Write csv for easier manual search

write_csv(gbif_others, "Plant-Pollinator/Not_In_GloBi.csv")

# Read csv with genus-level match

gbif_genus_match = read_csv("Plant-Pollinator/Not_In_GloBi_Matched.csv") %>% 
  mutate(Match_Type = if_else(Genus_Match == T, "genus_match", "no_match")) %>% 
  select(-Genus_Match)
  
# Combine with species-level matches

gbif_all_matches = gbif_spp_match %>% 
  mutate(Match_Type = "species_match") %>% 
  bind_rows(gbif_genus_match) %>% 
  select(-new_id) %>% 
  mutate(species1 = "Vanessa annabella") %>% 
  group_by(species2_corrected) %>% 
  mutate(weight = n_distinct(id))

# Visualization

edglst = gbif_all_matches %>% 
  select(species1, species2_corrected, Match_Type, weight) %>% 
  graph_from_data_frame()

pl_all = ggraph(edglst, layout = "treemap") + 
  geom_edge_link0(aes(color = Match_Type, edge_width = weight), show.legend = TRUE) +
  geom_node_point(size = 5, color = "lightblue") +
  ggrepel::geom_text_repel(aes(x = x, y = y, label = name), max.overlaps = 5) + 
  labs(edge_width = "# Observation") + 
  scale_color_discrete(name = "Match type") + 
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white')

pl_all

ggsave("Plant-Pollinator/Vanessa_annabella.png", pl_all, width = 3000, height = 2000, unit = "px")

# Alternative visualization: barplots of counts

pl_bar = gbif_all_matches %>% 
  select(species2_corrected, Match_Type, weight) %>% 
  distinct() %>%
  group_by(Match_Type) %>% 
  arrange(weight) %>% 
  mutate(species2_corrected = factor(species2_corrected, levels = species2_corrected), 
         Match_Type = factor(Match_Type, levels = c("species_match", "genus_match", "no_match"))) %>% 
  ggplot(aes(y = species2_corrected, x = weight)) + 
  geom_col(fill = "#7fa007") + 
  ggh4x::facet_wrap2(.~Match_Type, scales = "free_y", labeller = as_labeller(c("genus_match" = "Genus match", 
                                                                               "no_match" = "No match", 
                                                                               "species_match" = "Species match"))) + 
  # scale_fill_discrete(name = "Match type", labels = c("Species match", "Genus match", "No match")) + 
  xlab("# Observations") + ylab("Plant species") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        axis.title = element_text(size = 15), 
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 12), 
        strip.text = element_text(size = 12), 
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

pl_bar  

ggsave("Plant-Pollinator/Vanessa_annabella_bar.pdf", pl_bar, width = 5000, height = 2000, unit = "px")
