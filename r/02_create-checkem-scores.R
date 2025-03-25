## Creating all 10 CheckEM scores for the open access synthesis data as a test ----
library(tidyverse)
library(CheckEM)

# Read in data ----
metadata <- readRDS("data/raw/metadata.RDS")

count <- readRDS("data/raw/count.RDS") %>%
  dplyr::left_join(metadata %>% select(sample_url, sample, campaignid))

length <- readRDS("data/raw/length.RDS") %>%
  dplyr::left_join(metadata %>% select(sample_url, sample, campaignid)) %>%
  dplyr::select(!scientific_name)

# SCORE 1 ----
# metadata_format
# % of metadata rows that matches the metadata standard
# Scale 0 - 100
# 100% = good

# Columns in metadata
# sample - must not be blank.
# TODO this should actually be opcode/period - but for now Nik still needs Sample
# latitude_dd. Can not be blank. Must be between -90 to 90
# longitude_dd. Can not be blank. Must be between -180 to 180
# date_time. Can not be blank. Must be in YYYY-MM-DDThh:mm:ssTZD format
# site
# location
# status. Can not be blank. Must be Fished, No-take, I, II, III, IV, V, VI
# depth_m. Must be numeric. Can not be blank. 
# successful_count. Must be Yes or No
# successful_length. Must be Yes or No
# observer_count. If successful_count = Yes can not be blank. Must be Yes or No
# observer_length. If successful_length = Yes can not be blank. Must be Yes or No

unique(metadata$status)

# TODO need to add correct format for date
# TODO add distinct lat and longtiude within campaign

metadata_format <- metadata %>%
  
  # This removes the dummy data that I created to get these campaigns into GA
  dplyr::mutate(depth_m = na_if(depth_m, 0)) %>%
  dplyr::mutate(observer_count = na_if(observer_count, "Unknown")) %>%
  dplyr::mutate(observer_length = na_if(observer_length, "Unknown")) %>%
  
  dplyr::mutate(longitude_dd = as.numeric(longitude_dd)) %>% 
  dplyr::mutate(latitude_dd = as.numeric(latitude_dd)) %>% 
  dplyr::mutate(depth_m = as.numeric(depth_m)) %>% 
  dplyr::mutate(metadata_score = case_when(
    (
      is.na(longitude_dd) | 
        longitude_dd > 180 |
        longitude_dd < -180 |
        latitude_dd > 90 |
        latitude_dd < -90 |
        is.na(latitude_dd) | 
        is.na(date_time) | 
        is.na(depth_m) | 
        !status %in% c("Fished", "No-Take", "I", "II", "III", "IV", "V", "VI") | 
        !successful_count %in% c("TRUE", "FALSE") | 
        !successful_length %in% c("TRUE", "FALSE") | 
        successful_count %in% c("TRUE") & is.na(observer_count) |
        successful_length %in% c("TRUE") & is.na(observer_length) |
        is.na(sample)) ~ "failed",
    .default = "passed")) %>%
  dplyr::group_by(campaignid, metadata_score) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(names_from = metadata_score, values_from = n) %>%
  tidyr::replace_na(list(failed = 0, passed = 0)) %>%
  dplyr::mutate(total = failed + passed) %>%
  dplyr::mutate(metadata_format = ((total-failed)/total)*100) %>%
  dplyr::select(campaignid, metadata_format)


# Score 2 ----
# metadata_matches_count
# % of samples in the count data that have a match in the metadata
# Scale 0-100
# Direction 100 = good

unique_count_samples <- count %>%
  distinct(campaignid, sample)

total_count_samples <- unique_count_samples %>%
  dplyr::group_by(campaignid) %>%
  dplyr::summarise(number_of_samples = n())

metadata_matches_count <- anti_join(unique_count_samples, metadata) %>%
  dplyr::group_by(campaignid) %>%
  dplyr::summarise(number_of_samples_without_match = n()) %>%
  dplyr::ungroup() %>%
  dplyr::full_join(total_count_samples) %>%
  tidyr::replace_na(list(number_of_samples_without_match = 0)) %>%
  dplyr::mutate(metadata_matches_count = ((number_of_samples-number_of_samples_without_match)/number_of_samples)*100) %>%
  dplyr::select(campaignid, metadata_matches_count)


# Score 3 ----
# metadata_matches_length
# % of samples in the length data that have a match in the metadata
# Scale 0-100
# Direction 100 = good

unique_length_samples <- length %>%
  distinct(campaignid, sample)

total_length_samples <- unique_length_samples %>%
  dplyr::group_by(campaignid) %>%
  dplyr::summarise(number_of_samples = n())

metadata_matches_length <- anti_join(unique_length_samples, metadata) %>%
  dplyr::group_by(campaignid) %>%
  dplyr::summarise(number_of_samples_without_match = n()) %>%
  dplyr::ungroup() %>%
  dplyr::full_join(total_length_samples) %>%
  tidyr::replace_na(list(number_of_samples_without_match = 0)) %>%
  dplyr::mutate(metadata_matches_length = ((number_of_samples-number_of_samples_without_match)/number_of_samples)*100) %>%
  dplyr::select(campaignid, metadata_matches_length)

# Score 4 ----
# count_matches_schema
# % of species that match the schema
# Scale 0-100
# Direction 100 = good

unique_species <- count %>%
  dplyr::distinct(campaignid, family, genus, species)

total_species <- unique_species %>%
  dplyr::group_by(campaignid) %>%
  dplyr::summarise(number_of_species = n())

count_matches_schema <- anti_join(unique_species, CheckEM::australia_life_history) %>%
  dplyr::group_by(campaignid) %>%
  dplyr::summarise(number_of_species_dont_match = n()) %>%
  dplyr::ungroup() %>%
  dplyr::full_join(total_species) %>%
  tidyr::replace_na(list(number_of_species_dont_match = 0)) %>%
  dplyr::mutate(count_matches_schema = ((number_of_species-number_of_species_dont_match)/number_of_species)*100) %>%
  dplyr::select(campaignid, count_matches_schema)

# Score 5 ----
# count_vs_length
# "% of MaxNs that are equal to the number of length measurements + number of 3D point measurements"
# Scale 0-100
# Direction 100 = good

number_of_counts <- count %>%
  dplyr::group_by(campaignid) %>%
  dplyr::summarise(total_number_of_rows = n())

campaigns_with_lengths <- metadata %>%
  dplyr::group_by(campaignid) %>%
  dplyr::summarise(length_not_uploaded = all(successful_length %in% "No"))

count_vs_length <- length %>%
  dplyr::group_by(campaignid, sample, family, genus, species) %>%
  dplyr::summarise(stereo_maxn = sum(number)) %>%
  dplyr::ungroup() %>%
  dplyr::full_join(count) %>%
  dplyr::full_join(metadata) %>%
  dplyr::filter(successful_length %in% "TRUE") %>% # only use samples that had lengths
  dplyr::filter(!count %in% stereo_maxn) %>%
  dplyr::group_by(campaignid) %>%
  dplyr::summarise(number_of_errors = n()) %>%
  dplyr::ungroup() %>%
  dplyr::full_join(number_of_counts) %>%
  tidyr::replace_na(list(number_of_errors = 0)) %>%
  dplyr::mutate(count_vs_length = ((total_number_of_rows-number_of_errors)/total_number_of_rows)*100) %>%
  dplyr::left_join(campaigns_with_lengths) %>%
  dplyr::mutate(count_vs_length = case_when(
    length_not_uploaded %in% TRUE ~ NA,
    .default = count_vs_length
  )) %>%
  dplyr::select(campaignid, count_vs_length)

# Score 6 ----
# count_less_length
# "% of lengths more than expected from MaxN (no 3D points)"
# Scale 0-infinity
# Direction 0 = good

number_in_counts <- count %>%
  dplyr::group_by(campaignid) %>%
  dplyr::summarise(total_count = sum(count))

count_less_length <- length %>%
  dplyr::filter(!is.na(length_mm)) %>%
  dplyr::group_by(campaignid, sample, family, genus, species) %>%
  dplyr::summarise(number_length = sum(number)) %>%
  ungroup() %>%
  dplyr::left_join(count) %>%
  dplyr::left_join(metadata) %>%
  dplyr::filter(successful_count %in% "TRUE") %>%
  dplyr::filter(successful_length %in% "TRUE") %>%
  dplyr::filter(number_length > count) %>%
  dplyr::mutate(difference = number_length - count) %>%
  dplyr::group_by(campaignid) %>%
  dplyr::summarise(extra_lengths = sum(difference)) %>%
  full_join(number_in_counts) %>%
  tidyr::replace_na(list(extra_lengths = 0)) %>%
  dplyr::mutate(count_less_length = extra_lengths/total_count*100) %>%
  dplyr::left_join(campaigns_with_lengths) %>%
  dplyr::mutate(count_less_length = case_when(
    length_not_uploaded %in% TRUE ~ NA,
    .default = count_less_length
  )) %>%
  dplyr::select(campaignid, count_less_length)

# Score 7 ----
# count_greater_length
# "% of lengths missing from MaxN (no 3D points)"
# Scale 0-100
# Direction 0 = good

number_in_counts <- count %>%
  dplyr::group_by(campaignid) %>%
  dplyr::summarise(total_count = sum(count))

count_greater_length <- length %>%
  dplyr::filter(!is.na(length_mm)) %>%
  dplyr::group_by(campaignid, sample, family, genus, species) %>%
  dplyr::summarise(number_length = sum(number)) %>%
  ungroup() %>%
  dplyr::left_join(count) %>%
  dplyr::left_join(metadata) %>%
  dplyr::filter(successful_count %in% "TRUE") %>%
  dplyr::filter(successful_length %in% "TRUE") %>%
  dplyr::filter(number_length < count) %>%
  dplyr::mutate(difference = count - number_length) %>%
  dplyr::group_by(campaignid) %>%
  dplyr::summarise(extra_count = sum(difference)) %>%
  full_join(number_in_counts) %>%
  tidyr::replace_na(list(extra_count = 0)) %>%
  dplyr::mutate(count_greater_length = extra_count/total_count*100) %>%
  dplyr::left_join(campaigns_with_lengths) %>%
  dplyr::mutate(count_greater_length = case_when(
    length_not_uploaded %in% TRUE ~ NA,
    .default = count_greater_length
  )) %>%
  dplyr::select(campaignid, count_greater_length)

# Score 7 ----
# count_less_length_large_bodied_carnivores
# "% of lengths more than expected from MaxN (no 3D points)"
# Scale 0-infinity
# Direction 0 = good

# TODO add large bodied carnivores
large_bodied_carnivores <- CheckEM::australia_life_history %>%
  dplyr::filter(fb_trophic_level > 2.8) %>%
  dplyr::filter(length_max_cm > 40) %>%
  dplyr::filter(class %in% "Actinopterygii") %>%
  dplyr::filter(!order %in% c("Anguilliformes", "Ophidiiformes", "Notacanthiformes","Tetraodontiformes","Syngnathiformes", 
                              "Synbranchiformes", "Stomiiformes", "Siluriformes", "Saccopharyngiformes", "Osmeriformes", 
                              "Osteoglossiformes", "Lophiiformes", "Lampriformes", "Beloniformes", "Zeiformes")) %>%
  dplyr::filter(!is.na(fb_length_at_maturity_cm)) %>%
  dplyr::select(-caab_code)

number_in_counts_large_bodied_carnivores <- count %>%
  semi_join(large_bodied_carnivores) %>%
  dplyr::group_by(campaignid) %>%
  dplyr::summarise(total_count = sum(count))

count_less_length_large_bodied_carnivores <- length %>%
  semi_join(large_bodied_carnivores) %>%
  dplyr::filter(!is.na(length_mm)) %>%
  dplyr::group_by(campaignid, sample, family, genus, species) %>%
  dplyr::summarise(number_length = sum(number)) %>%
  ungroup() %>%
  dplyr::left_join(count) %>%
  dplyr::left_join(metadata) %>%
  dplyr::filter(successful_count %in% "TRUE") %>%
  dplyr::filter(successful_length %in% "TRUE") %>%
  dplyr::filter(number_length > count) %>%
  dplyr::mutate(difference = number_length - count) %>%
  dplyr::group_by(campaignid) %>%
  dplyr::summarise(extra_lengths = sum(difference)) %>%
  full_join(number_in_counts_large_bodied_carnivores) %>%
  tidyr::replace_na(list(extra_lengths = 0)) %>%
  dplyr::mutate(count_less_length_large_bodied_carnivores = extra_lengths/total_count*100) %>%
  dplyr::left_join(campaigns_with_lengths) %>%
  dplyr::mutate(count_less_length_large_bodied_carnivores = case_when(
    length_not_uploaded %in% TRUE ~ NA,
    .default = count_less_length_large_bodied_carnivores
  )) %>%
  dplyr::select(campaignid, count_less_length_large_bodied_carnivores)

# Score 7 ----
# count_greater_length_large_bodied_carnivores
# "% of lengths missing from MaxN (no 3D points)"
# Scale 0-100
# Direction 0 = good

number_in_counts_large_bodied_carnivores <- count %>%
  semi_join(large_bodied_carnivores) %>%
  dplyr::group_by(campaignid) %>%
  dplyr::summarise(total_count = sum(count))

count_greater_length_large_bodied_carnivores <- length %>%
  semi_join(large_bodied_carnivores) %>%
  dplyr::filter(!is.na(length_mm)) %>%
  dplyr::group_by(campaignid, sample, family, genus, species) %>%
  dplyr::summarise(number_length = sum(number)) %>%
  ungroup() %>%
  dplyr::left_join(count) %>%
  dplyr::left_join(metadata) %>%
  dplyr::filter(successful_count %in% "TRUE") %>%
  dplyr::filter(successful_length %in% "TRUE") %>%
  dplyr::filter(number_length < count) %>%
  dplyr::mutate(difference = count - number_length) %>%
  dplyr::group_by(campaignid) %>%
  dplyr::summarise(extra_count = sum(difference)) %>%
  full_join(number_in_counts_large_bodied_carnivores) %>%
  tidyr::replace_na(list(extra_count = 0)) %>%
  dplyr::mutate(count_greater_length_large_bodied_carnivores = extra_count/total_count*100) %>%
  dplyr::left_join(campaigns_with_lengths) %>%
  dplyr::mutate(count_greater_length_large_bodied_carnivores = case_when(
    length_not_uploaded %in% TRUE ~ NA,
    .default = count_greater_length_large_bodied_carnivores
  )) %>%
  dplyr::select(campaignid, count_greater_length_large_bodied_carnivores)

# Score 10 ----
# length_greater_maximum_size
# "% of lengths that are greater than the maximum size limit"
# Scale 0-100
# Direction 0 = good

total_measurements <- length %>%
  dplyr::group_by(campaignid) %>%
  dplyr::summarise(number_of_lengths = sum(number))

length_greater_maximum_size <- length %>%
  left_join(CheckEM::australia_life_history %>% 
              dplyr::mutate(caab_code = as.integer(caab_code))) %>%
  dplyr::mutate(length_max_mm = length_max_cm * 10) %>%
  dplyr::select(campaignid, sample, family, genus, species, number, length_mm, length_max_mm) %>%
  dplyr::filter(length_mm > length_max_mm) %>%
  dplyr::group_by(campaignid) %>%
  dplyr::summarise(number_of_lengths_over = sum(number)) %>%
  dplyr::full_join(total_measurements) %>%
  tidyr::replace_na(list(number_of_lengths_over = 0)) %>%
  dplyr::mutate(length_greater_maximum_size = (number_of_lengths_over/number_of_lengths)*100) %>%
  dplyr::select(campaignid, length_greater_maximum_size)



# Combine scores together ----

checkem_scores <- metadata_format %>%
  left_join(metadata_matches_count) %>%
  left_join(metadata_matches_length) %>%
  left_join(count_matches_schema) %>%
  left_join(count_vs_length) %>%
  left_join(count_less_length) %>%
  left_join(count_greater_length) %>%
  left_join(count_less_length_large_bodied_carnivores) %>%
  left_join(count_greater_length_large_bodied_carnivores) %>%
  left_join(length_greater_maximum_size)

saveRDS(checkem_scores, "data/scores.RDS")

