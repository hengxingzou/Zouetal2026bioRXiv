library(tidyverse)
library(igraph)
library(ggraph)
library(betalink)


########## Read and Clean Data ##########


# Read manually labelled interaction data

files = Sys.glob("eBird/Training_Cleaned/*.xlsx")

list_sheets = lapply(files, function(x) readxl::read_xlsx(x))

labeled_int = bind_rows(list_sheets)

# Read original comments

original = Sys.glob("eBird/Original_Comments/*.xlsx")

list_original = lapply(original, function(x) readxl::read_xlsx(x))

original_comments = bind_rows(list_original)

# Combine multispecies interactions into a single row

labeled_int_std = labeled_int %>% 
  group_by(global_unique_identifier) %>% 
  mutate(spp_index = row_number()) %>% 
  ungroup() %>% 
  select(species1_common, species2_common, interaction, global_unique_identifier, spp_index) %>% 
  pivot_wider(names_from = spp_index, values_from = species2_common) %>% 
  unite(col = "species2_common", `1`:`4`, sep = ", ", remove = T, na.rm = T) %>% 
  left_join(original_comments %>% select(global_unique_identifier, species_comments), by = "global_unique_identifier")

# Read input into the LLM, which is a subset of all manually labeled data

input_int = read_csv("eBird/Total_Labeled.csv")

# Match input with the manually labeled data to get standardized common names

labeled_int_std_filtered = labeled_int_std %>% 
  filter(global_unique_identifier %in% input_int$global_unique_identifier) %>% 
  group_by(global_unique_identifier) %>% 
  mutate(num_int = max(row_number())) %>% 
  ungroup() %>% 
  filter(num_int == 1)

input_int_std = input_int %>% 
  select(species1_common, species2_common, interaction, global_unique_identifier) %>% 
  rename(species2_common_orig = species2_common) %>% 
  right_join(labeled_int_std_filtered %>% select(species2_common, global_unique_identifier, species_comments), 
            by = "global_unique_identifier")

# Read and process output to spread multispecies entries

output_int = read_csv("eBird/Output.csv")

output_int_filtered = output_int %>% 
  select(-species1_common) %>% 
  filter(global_unique_identifier %in% input_int_std$global_unique_identifier) %>% 
  rename(species2_predict = species2_common, interaction_predict = interaction)

# Match and compare input and output

compare_input_output = input_int_std %>% 
  rename(species2_common_std = species2_common) %>% 
  full_join(output_int_filtered, by = "global_unique_identifier") %>% 
  select(global_unique_identifier, species1_common, species2_common_orig, species2_predict, 
         interaction, interaction_predict, species2_common_std, species_comments)

# Clean data before calculating accuracy

compare_input_output = compare_input_output %>% 
  mutate(interaction = replace_na(interaction, "no interaction"), 
         interaction_predict = replace_na(interaction_predict, "no interaction")) %>% 
  mutate(species2_common_orig = tolower(species2_common_orig)) %>% 
  mutate(species2_common_orig = replace_na(species2_common_orig, "no species"), 
         species2_predict = replace_na(species2_predict, "no species"))


########## Calculate Accuracy ##########


accuracy_interaction = compare_input_output %>% 
  mutate(interaction_correct = if_else(interaction == interaction_predict, 1, 0)) %>% 
  mutate(species2_correct = if_else(species2_common_orig == species2_predict, 1, 0))

sum(accuracy_interaction$interaction_correct) / nrow(accuracy_interaction)
sum(accuracy_interaction$species2_correct) / nrow(accuracy_interaction)

# 81.44% correct for interaction types
# 80.21% correct for species names, before manual correction

# Export for manual correction of species names

write_csv(accuracy_interaction %>% select(-species2_correct), "eBird/Compare.csv")

# Read manually inspected data

compare_checked = read_csv("eBird/Compare_Checked.csv")

accuracy_both = compare_checked %>% 
  mutate(species_correct = if_else(species2_common_std == species2_predicted_common_std, 1, 0))

sum(accuracy_both$species_correct) / nrow(accuracy_both)

# 96.49% correct for species names, after manual correction


########## Compare Interaction Networks ##########


# Type of interaction

focal_int = "facilitation-feeding"

# Parse multispecies interactions for labeled interactions

labeled_interactions = accuracy_both %>% 
  # filter for species-level observations
  filter(!grepl("sp.", species2_common_std)) %>% 
  select(species1_common, species2_common_std, interaction) %>% 
  separate_wider_delim(cols = "species2_common_std", delim = ", ", names = c("species2_A", "species2_B"), 
                       too_few = "align_start", too_many = "merge") %>% 
  separate_wider_delim(cols = "species2_B", delim = ", ", names = c("species2_B", "species2_C"), 
                       too_few = "align_start", too_many = "merge") %>% 
  separate_wider_delim(cols = "species2_C", delim = ", ", names = c("species2_C", "species2_D"), 
                       too_few = "align_start", too_many = "merge")

# Construct interaction network for labeled interactions and facilitation-mixed flocking only

labeled_edgelist = labeled_interactions %>% 
  pivot_longer(starts_with("species2"), names_to = "Species_Num", values_to = "species2_common") %>% 
  filter(!is.na(species2_common), 
         interaction == focal_int) %>% 
  select(-Species_Num, -interaction) %>% 
  mutate(label = "labeled")
  
labeled_g = graph_from_data_frame(labeled_edgelist)

# Parse multispecies interactions for extracted interactions

extracted_interactions = accuracy_both %>% 
  # filter for species-level observations
  filter(!grepl("sp.", species2_predicted_common_std)) %>% 
  select(species1_common, species2_predicted_common_std, interaction) %>% 
  separate_wider_delim(cols = "species2_predicted_common_std", delim = ", ", names = c("species2_A", "species2_B"), 
                       too_few = "align_start", too_many = "merge") %>% 
  separate_wider_delim(cols = "species2_B", delim = ", ", names = c("species2_B", "species2_C"), 
                       too_few = "align_start", too_many = "merge") %>% 
  separate_wider_delim(cols = "species2_C", delim = ", ", names = c("species2_C", "species2_D"), 
                       too_few = "align_start", too_many = "merge")

# Construct interaction network for labeled interactions and facilitation-mixed flocking only

extracted_edgelist = extracted_interactions %>% 
  pivot_longer(starts_with("species2"), names_to = "Species_Num", values_to = "species2_common") %>% 
  filter(!is.na(species2_common), 
         interaction == focal_int) %>% 
  select(-Species_Num, -interaction) %>% 
  mutate(label = "extracted")

extracted_g = graph_from_data_frame(extracted_edgelist)

# Calculate dissimilarity between the networks

betalink(labeled_g, extracted_g)

# species are all the same; dissimilarities of interactions very low

# Combined network, for visualization

combined_g = rbind(labeled_edgelist, extracted_edgelist)

pl_compare = ggraph(combined_g, layout = "fr") + 
  geom_edge_fan(show.legend = TRUE, strength = 15, color = "steelblue") +
  geom_node_point(size = 5, color = "lightblue") +
  ggrepel::geom_text_repel(aes(x = x, y = y, label = name), max.overlaps = 5) + 
  facet_edges(~ label) + 
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white')

pl_compare

ggsave("eBird/Compare_Networks.png", pl_compare, width = 5000, height = 3000, unit = "px")

# Alternative visualization: interaction matrix heat map

labeled_list = labeled_edgelist %>% 
  group_by(species1_common, species2_common) %>% 
  summarize(weight_labeled = n(), .groups = "drop") %>% 
  distinct()

extracted_list = extracted_edgelist %>% 
  group_by(species1_common, species2_common) %>% 
  summarize(weight_extracted = n(), .groups = "drop") %>% 
  distinct()

complete_comb = expand_grid(extracted_list$species1_common, extracted_list$species2_common) %>% 
  rename(species1_common = `extracted_list$species1_common`, species2_common = `extracted_list$species2_common`)

all_weights = labeled_list %>%
  full_join(extracted_list, by = c("species1_common", "species2_common"),
            relationship = "many-to-many") %>%
  pivot_longer(starts_with("weight"), names_to = "label", values_to = "weight", values_drop_na = F) %>% 
  arrange(species1_common) %>% 
  mutate(x_pos = as.numeric(factor(species1_common)) + 0.5) %>% 
  arrange(species2_common) %>% 
  mutate(y_pos = as.numeric(factor(species2_common)) + 0.5)

species1_common = sort(unique(all_weights$species1_common))
species2_common = sort(unique(all_weights$species2_common))

pl_heatmap = all_weights %>% 
  ggplot(aes(x = x_pos, y = y_pos, fill = weight)) + 
  geom_tile(color = "white", linewidth = 0.5) + 
  scale_fill_gradient(low = "#3e5902", high = "#c9ea3f", name = "# Observations") + 
  ggh4x::facet_wrap2(~label, scales = "free", labeller = as_labeller(c("weight_extracted" = "Extracted", 
                                                                       "weight_labeled" = "Labelled"))) + 
  scale_x_continuous(name = "Species 1", breaks = 1:length(species1_common), 
                     labels = species1_common) + 
  scale_y_continuous(name = "Species 2", breaks = 1:length(species2_common), 
                     labels = species2_common) + 
  ylab("Species 2") + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), 
        axis.title = element_text(size = 15), 
        axis.text.y = element_text(size = 12, vjust = -0.55), 
        axis.text.x = element_text(size = 12, angle = 90, vjust = 1), 
        axis.ticks = element_blank(),
        strip.text = element_text(size = 12), 
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

pl_heatmap

ggsave("eBird/Compare_Heatmap.pdf", pl_heatmap, width = 5000, height = 2500, unit = "px")


########## All Types of Interactions ##########


interaction_types = unique(compare_checked$interaction)

# Remove several categories that are only present in the predicted interactions

# Dissimilarity metrics

all_dissim = tibble()

for (i in 1:length(interaction_types)) {
  
  focal_int = interaction_types[i]
  
  # Parse multispecies interactions for labeled interactions
  
  labeled_interactions = accuracy_both %>% 
    # filter for species-level observations
    filter(!grepl("sp.", species2_common_std)) %>% 
    select(species1_common, species2_common_std, interaction) %>% 
    separate_wider_delim(cols = "species2_common_std", delim = ", ", names = c("species2_A", "species2_B"), 
                         too_few = "align_start", too_many = "merge") %>% 
    separate_wider_delim(cols = "species2_B", delim = ", ", names = c("species2_B", "species2_C"), 
                         too_few = "align_start", too_many = "merge") %>% 
    separate_wider_delim(cols = "species2_C", delim = ", ", names = c("species2_C", "species2_D"), 
                         too_few = "align_start", too_many = "merge")
  
  # Construct interaction network for labeled interactions and facilitation-mixed flocking only
  
  labeled_edgelist = labeled_interactions %>% 
    pivot_longer(starts_with("species2"), names_to = "Species_Num", values_to = "species2_common") %>% 
    filter(!is.na(species2_common), 
           interaction == focal_int) %>% 
    select(-Species_Num, -interaction) %>% 
    mutate(label = "labeled")
  
  n_links_labeled = nrow(labeled_edgelist)
  
  labeled_g = graph_from_data_frame(labeled_edgelist)
  
  # Parse multispecies interactions for extracted interactions
  
  extracted_interactions = accuracy_both %>% 
    # filter for species-level observations
    filter(!grepl("sp.", species2_predicted_common_std)) %>% 
    select(species1_common, species2_predicted_common_std, interaction) %>% 
    separate_wider_delim(cols = "species2_predicted_common_std", delim = ", ", names = c("species2_A", "species2_B"), 
                         too_few = "align_start", too_many = "merge") %>% 
    separate_wider_delim(cols = "species2_B", delim = ", ", names = c("species2_B", "species2_C"), 
                         too_few = "align_start", too_many = "merge") %>% 
    separate_wider_delim(cols = "species2_C", delim = ", ", names = c("species2_C", "species2_D"), 
                         too_few = "align_start", too_many = "merge")
  
  # Construct interaction network for labeled interactions and facilitation-mixed flocking only
  
  extracted_edgelist = extracted_interactions %>% 
    pivot_longer(starts_with("species2"), names_to = "Species_Num", values_to = "species2_common") %>% 
    filter(!is.na(species2_common), 
           interaction == focal_int) %>% 
    select(-Species_Num, -interaction) %>% 
    mutate(label = "extracted")
  
  n_links_extracted = nrow(extracted_edgelist)
  
  extracted_g = graph_from_data_frame(extracted_edgelist)
  
  # Calculate dissimilarity between the networks
  
  dissim = betalink(labeled_g, extracted_g)
  dissim_tibble = tibble(S = dissim$S, OS = dissim$OS, WN = dissim$WN, ST = dissim$ST, interaction = focal_int, 
                         n_links_labeled = n_links_labeled, n_links_extracted = n_links_extracted)
  all_dissim = rbind(all_dissim, dissim_tibble)
  
}

# Remove the no interaction category

all_dissim = all_dissim %>% 
  filter(interaction != "no interaction")

write_csv(all_dissim, "eBird/Network_Dissim.csv")

# Visualization

pl_list = list()

for (i in 1:length(interaction_types)) {
  
  focal_int = interaction_types[i]
  
  labeled_edgelist = labeled_interactions %>% 
    pivot_longer(starts_with("species2"), names_to = "Species_Num", values_to = "species2_common") %>% 
    filter(!is.na(species2_common), 
           interaction == focal_int) %>% 
    select(-Species_Num, -interaction) %>% 
    mutate(label = "labeled")
  
  extracted_edgelist = extracted_interactions %>% 
    pivot_longer(starts_with("species2"), names_to = "Species_Num", values_to = "species2_common") %>% 
    filter(!is.na(species2_common), 
           interaction == focal_int) %>% 
    select(-Species_Num, -interaction) %>% 
    mutate(label = "extracted")

  labeled_list = labeled_edgelist %>% 
    group_by(species1_common, species2_common) %>% 
    summarize(weight_labeled = n(), .groups = "drop") %>% 
    distinct()
  
  extracted_list = extracted_edgelist %>% 
    group_by(species1_common, species2_common) %>% 
    summarize(weight_extracted = n(), .groups = "drop") %>% 
    distinct()
  
  all_weights = labeled_list %>% 
    full_join(extracted_list, by = c("species1_common", "species2_common"), 
              relationship = "many-to-many") %>% 
    pivot_longer(starts_with("weight"), names_to = "label", values_to = "weight", values_drop_na = F)
  
  pl_heatmap = all_weights %>% 
    ggplot(aes(x = species1_common, y = species2_common, fill = weight)) + 
    geom_tile(color = "white") + 
    scale_fill_gradient(low = "blue", high = "red", name = "# Observations") + 
    ggh4x::facet_wrap2(~label, scales = "free", labeller = as_labeller(c("weight_extracted" = "Extracted", 
                                                                         "weight_labeled" = "Labelled"))) + 
    xlab("Focal species") + ylab("Participating species") + 
    ggtitle(focal_int) + 
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          axis.title = element_text(size = 15), 
          axis.text.y = element_text(size = 12), 
          axis.text.x = element_text(size = 12, angle = 90, vjust = 1), 
          strip.text = element_text(size = 12), 
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10))
  
  pl_list[[i]] = pl_heatmap
  
}

names(pl_list) = interaction_types

pl_list

ggsave("eBird/Compare_Heatmap.png", pl_list$`facilitation-feeding`, width = 5000, height = 2500, unit = "px")
