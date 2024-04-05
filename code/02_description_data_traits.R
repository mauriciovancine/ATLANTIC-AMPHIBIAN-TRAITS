#' ----
#' title: atlantic amphibian traits - description traits data
#' author: mauricio vancine
#' date: 2023-11-18
#' ----

# prepare r -------------------------------------------------------------

# packages
library(tidyverse)
library(ggridges)
library(GGally)

# individual-level traits ----------------------------------------------------

# import data
atlantic_amphibian_traits_individual <- readr::read_csv("02_results/ATLANTIC_AMPHIBIAN_TRAITS_individual.csv")
atlantic_amphibian_traits_individual

atlantic_amphibian_traits_individual1 <- atlantic_amphibian_traits_individual[1, ]
atlantic_amphibian_traits_individual1

length(table(atlantic_amphibian_traits_individual$id))
length(table(atlantic_amphibian_traits_individual$voucher))
length(table(atlantic_amphibian_traits_individual$species))
length(table(atlantic_amphibian_traits_individual$species_frost2023))
length(table(atlantic_amphibian_traits_individual$order))
length(table(atlantic_amphibian_traits_individual$superfamily))
length(table(atlantic_amphibian_traits_individual$family))
length(table(atlantic_amphibian_traits_individual$subfamily))
length(table(atlantic_amphibian_traits_individual$genus))
length(table(atlantic_amphibian_traits_individual$collection))
range(atlantic_amphibian_traits_individual$body_size)
range(atlantic_amphibian_traits_individual$head_width)
range(atlantic_amphibian_traits_individual$head_length)
range(atlantic_amphibian_traits_individual$humerus)
range(atlantic_amphibian_traits_individual$radioulna)
range(atlantic_amphibian_traits_individual$hand, na.rm = TRUE)
range(atlantic_amphibian_traits_individual$femur)
range(atlantic_amphibian_traits_individual$tibiofibula)
range(atlantic_amphibian_traits_individual$tarsus)
range(atlantic_amphibian_traits_individual$foot, na.rm = TRUE)
range(atlantic_amphibian_traits_individual$forelimb)
range(atlantic_amphibian_traits_individual$hindlimb)
length(table(atlantic_amphibian_traits_individual$species_cfbh))
length(table(atlantic_amphibian_traits_individual$country))
length(table(atlantic_amphibian_traits_individual$state))
length(table(atlantic_amphibian_traits_individual$municipality))
length(table(atlantic_amphibian_traits_individual$locality))
length(table(atlantic_amphibian_traits_individual$longitude))
length(table(atlantic_amphibian_traits_individual$latitude))
length(table(atlantic_amphibian_traits_individual$longitude_approximate))
length(table(atlantic_amphibian_traits_individual$latitude_approximate))
length(table(atlantic_amphibian_traits_individual$collection_date))
length(table(atlantic_amphibian_traits_individual$collector_name))

# species-level traits ----------------------------------------------------

# import data
atlantic_amphibian_traits_species <- readr::read_csv("02_results/ATLANTIC_AMPHIBIAN_TRAITS_species.csv")
atlantic_amphibian_traits_species

# description
length(table(atlantic_amphibian_traits_species$species_frost2023))
nrow(atlantic_amphibian_traits_species)

atlantic_amphibian_traits_species1 <- atlantic_amphibian_traits_species[1, ]
atlantic_amphibian_traits_species1

length(table(atlantic_amphibian_traits_species$id))
length(table(atlantic_amphibian_traits_species$species_frost2023))
length(table(atlantic_amphibian_traits_species$order))
length(table(atlantic_amphibian_traits_species$superfamily))
length(table(atlantic_amphibian_traits_species$family))
length(table(atlantic_amphibian_traits_species$subfamily))
length(table(atlantic_amphibian_traits_species$genus))
range(atlantic_amphibian_traits_species$male_body_size_svl, na.rm = TRUE)
range(atlantic_amphibian_traits_species$female_body_size_svl, na.rm = TRUE)
table(atlantic_amphibian_traits_species$male_body_size_cat)
table(atlantic_amphibian_traits_species$female_body_size_cat)
table(atlantic_amphibian_traits_species$toe_disc)
names(table(atlantic_amphibian_traits_species$locomotion_mode))
names(table(atlantic_amphibian_traits_species$activity))
names(table(atlantic_amphibian_traits_species$adult_habitat))
names(table(atlantic_amphibian_traits_species$adult_habit))
names(table(atlantic_amphibian_traits_species$tadpole_habit))
length(table(atlantic_amphibian_traits_species$reproductive_mode))
table(atlantic_amphibian_traits_species$reproductive_mode)
names(table(atlantic_amphibian_traits_species$calling_site))
names(table(atlantic_amphibian_traits_species$oviposition))
names(table(atlantic_amphibian_traits_species$egg_type))
names(table(atlantic_amphibian_traits_species$tadpole_nutrition))
names(table(atlantic_amphibian_traits_species$abundance))
names(table(atlantic_amphibian_traits_species$toxicity))
names(table(atlantic_amphibian_traits_species$endemic_of_atlantic_forest))

# tables ----------------------------------------------------------------

## table 2 ----
atlantic_amphibian_traits_individual_family_species <- atlantic_amphibian_traits_individual %>%
    dplyr::distinct(family, species_frost2023) %>% 
    dplyr::group_by(family) %>% 
    dplyr::summarise(species_ind = n())
atlantic_amphibian_traits_individual_family_species

atlantic_amphibian_traits_individual_family_species_ind <- atlantic_amphibian_traits_individual %>% 
    dplyr::group_by(family) %>% 
    dplyr::summarise(individual = n())
atlantic_amphibian_traits_individual_family_species_ind

atlantic_amphibian_traits_individual_family_species_local <- atlantic_amphibian_traits_individual %>% 
    dplyr::mutate(lon = ifelse(is.na(longitude), longitude_approximate, longitude), .after = longitude_approximate) %>% 
    dplyr::mutate(lat = ifelse(is.na(latitude), latitude_approximate, latitude), .after = latitude_approximate) %>% 
    dplyr::distinct(family, species, lon, lat) %>% 
    tidyr::drop_na(lon, lat) %>% 
    dplyr::group_by(family) %>% 
    dplyr::summarise(locality = n())
atlantic_amphibian_traits_individual_family_species_local

atlantic_amphibian_traits_individual_family_species_local_comb <- atlantic_amphibian_traits_individual_family_species %>% 
    dplyr::left_join(atlantic_amphibian_traits_individual_family_species_ind) %>% 
    dplyr::left_join(atlantic_amphibian_traits_individual_family_species_local) %>% 
    dplyr::mutate(across(everything(), ~replace_na(.x, 0))) %>% 
    janitor::adorn_totals()
atlantic_amphibian_traits_individual_family_species_local_comb

atlantic_amphibian_traits_individual_family_species_local_comb %>% 
    dplyr::arrange(-species_ind, -individual, -locality)

readr::write_csv(atlantic_amphibian_traits_individual_family_species_local_comb, "02_results/tables/table02.csv")

## table 3 ----
figueiredo_etal_2021 <- readr::read_csv("01_data/figueiredo_etal_2021/amphibian_list_figueiredo_etal_2021.csv") %>% 
    dplyr::select(species)
figueiredo_etal_2021

# taxonomy
asw_synonyms <- read.csv("01_data/asw_synonyms.csv")
asw_synonyms

sync <- AmphiNom::aswSync(query = figueiredo_etal_2021$species,
                          asw = asw_synonyms,
                          interactive = TRUE,
                          return.no.matches = TRUE) %>% 
    tibble::as_tibble() %>% 
    dplyr::distinct()
sync

synonym_report <- AmphiNom::synonymReport(query_info = sync)
synonym_report

synonym_report_verbose <- AmphiNom::synonymReport(query_info = sync, verbose = TRUE)
synonym_report_verbose

synonym_report_verbose$names_not_found
synonym_report_verbose$ambiguities
synonym_report_verbose$duplicated

figueiredo_etal_2021_taxonomy <- figueiredo_etal_2021 %>%
    dplyr::left_join(sync, by = c("species" = "query")) %>%
    dplyr::mutate(ASW_names = stringr::str_replace_all(ASW_names, "Cycloramphus carvalhoi2", "Cycloramphus carvalhoi")) %>%
    dplyr::mutate(species_frost2023 = ASW_names) %>%
    dplyr::select(-c(ASW_names, stripped, status, warnings)) %>% 
    dplyr::mutate(species_frost2023 = ifelse(species_frost2023 == "Lithobates catesbeianus", "Aquarana catesbeiana", species_frost2023))
figueiredo_etal_2021_taxonomy

# complete taxonomy
asw_taxonomy <- read.csv("01_data/asw_taxonomy.csv") %>%
    dplyr::select(-url)
asw_taxonomy

figueiredo_etal_2021_taxonomy_complete <- figueiredo_etal_2021_taxonomy %>%
    dplyr::left_join(asw_taxonomy, by = c("species_frost2023" = "species")) %>%
    dplyr::relocate(order,  superfamily, family, subfamily, genus, .after = 3) %>%
    dplyr::mutate(order = ifelse(is.na(order), "Anura", order),
                  family = ifelse(is.na(family), "Hylidae", family),
                  subfamily = ifelse(is.na(subfamily), "Hylinae", subfamily),
                  genus = ifelse(is.na(genus), "Scinax", genus)) %>% 
    tibble::rowid_to_column(var = "id")
figueiredo_etal_2021_taxonomy_complete

# group
figueiredo_etal_2021_taxonomy_complete_family_species <- figueiredo_etal_2021_taxonomy_complete %>% 
    dplyr::group_by(family) %>% 
    dplyr::summarise(species_af = n())
figueiredo_etal_2021_taxonomy_complete_family_species

# group haddad et al 2013 family
atlantic_amphibian_traits_species_family_spe <- atlantic_amphibian_traits_species %>% 
    dplyr::group_by(family) %>% 
    dplyr::summarise(species_spe = n())
atlantic_amphibian_traits_species_family_spe

# table
figueiredo_etal_2021_taxonomy_complete_family_species_ind <- figueiredo_etal_2021_taxonomy_complete_family_species %>% 
    dplyr::left_join(atlantic_amphibian_traits_species_family_spe) %>% 
    dplyr::left_join(atlantic_amphibian_traits_individual_family_species_local_comb) %>% 
    dplyr::mutate(across(everything(), ~replace_na(.x, 0))) %>% 
    dplyr::select(-locality) %>% 
    janitor::adorn_totals()
figueiredo_etal_2021_taxonomy_complete_family_species_ind

# export
readr::write_csv(figueiredo_etal_2021_taxonomy_complete_family_species_ind, "02_results/tables/table03.csv")

# figures -----------------------------------------------------------------

## figure 4 ----
figure4 <- ggplot(atlantic_amphibian_traits_individual,
                  aes(x = body_size, y = forcats::fct_rev(family))) +
    geom_density_ridges(fill = "#1c78b2") +
    labs(x = "Body size (SVL - mm)", y = "Family") +
    theme_bw(base_size = 20)
figure4
ggsave("02_results/figures/fig04.png", figure4, width = 30, height = 20, units = "cm", dpi = 300)

## figure 5 ----
figure5a <- ggplot(atlantic_amphibian_traits_individual,
                   aes(x = head_width, y = forcats::fct_rev(family))) +
    geom_density_ridges(fill = "#ff7f24") +
    labs(x = "Head width (mm)", y = "Family") +
    theme_bw(base_size = 20)
figure5a
ggsave("02_results/figures/fig05a.png", figure5a, width = 30, height = 20, units = "cm", dpi = 300)

figure5b <- ggplot(atlantic_amphibian_traits_individual,
                   aes(x = head_length, y = forcats::fct_rev(family))) +
    geom_density_ridges(fill = "#ff7f24") +
    labs(x = "Head length (mm)", y = "Family") +
    theme_bw(base_size = 20)
figure5b
ggsave("02_results/figures/fig05b.png", figure5b, width = 30, height = 20, units = "cm", dpi = 300)

figure5c <- atlantic_amphibian_traits_individual %>% 
    dplyr::select(body_size, head_width, head_length) %>% 
    GGally::ggpairs(lower = list(continuous = wrap("smooth", color = "#ff7f24")),
                    diag = list(continuous = wrap("densityDiag", fill = "#ff7f24")),
                    upper = list(continuous = wrap("cor", size = 5))) +
    theme_bw(base_size = 15)
figure5c
ggsave("02_results/figures/fig05c.png", figure5e, width = 30, height = 20, units = "cm", dpi = 300)

## figure 6 ----
figure6a <- ggplot(atlantic_amphibian_traits_individual,
                   aes(x = forelimb, y = forcats::fct_rev(family))) +
    geom_density_ridges(fill = "#d6272b") +
    labs(x = "Forelimb (mm)", y = "Family") +
    theme_bw(base_size = 20)
figure6a
ggsave("02_results/figures/fig06a.png", figure6a, width = 30, height = 20, units = "cm", dpi = 300)

figure6b <- ggplot(atlantic_amphibian_traits_individual,
                   aes(x = humerus, y = forcats::fct_rev(family))) +
    geom_density_ridges(fill = "#d6272b") +
    labs(x = "Humerus (mm)", y = "Family") +
    theme_bw(base_size = 20)
figure6b
ggsave("02_results/figures/fig06b.png", figure6b, width = 30, height = 20, units = "cm", dpi = 300)

figure6c <- ggplot(atlantic_amphibian_traits_individual,
                   aes(x = radioulna, y = forcats::fct_rev(family))) +
    geom_density_ridges(fill = "#d6272b") +
    labs(x = "Radioulna (mm)", y = "Family") +
    theme_bw(base_size = 20)
figure6c
ggsave("02_results/figures/fig06c.png", figure6c, width = 30, height = 20, units = "cm", dpi = 300)

figure6d <- ggplot(atlantic_amphibian_traits_individual,
                   aes(x = hand, y = forcats::fct_rev(family))) +
    geom_density_ridges(fill = "#d6272b") +
    labs(x = "Hand (mm)", y = "Family") +
    theme_bw(base_size = 20)
figure6d
ggsave("02_results/figures/fig06d.png", figure6d, width = 30, height = 20, units = "cm", dpi = 300)

figure6e <- atlantic_amphibian_traits_individual %>% 
    dplyr::select(body_size, humerus, radioulna, hand) %>% 
    GGally::ggpairs(lower = list(continuous = wrap("smooth", color = "#d6272b")),
                    diag = list(continuous = wrap("densityDiag", fill = "#d6272b")),
                    upper = list(continuous = wrap("cor", size = 5))) +
    theme_bw(base_size = 15)
figure6e
ggsave("02_results/figures/fig06e.png", figure6e, width = 30, height = 20, units = "cm", dpi = 300)

## figure 7 ----
figure7a <- ggplot(atlantic_amphibian_traits_individual,
                   aes(x = hindlimb, y = forcats::fct_rev(family))) +
    geom_density_ridges(fill = "#2d9f38") +
    labs(x = "Hindlimb (mm)", y = "Family") +
    theme_bw(base_size = 20)
figure7a
ggsave("02_results/figures/fig07a.png", figure7a, width = 30, height = 20, units = "cm", dpi = 300)

figure7b <- ggplot(atlantic_amphibian_traits_individual,
                   aes(x = femur, y = forcats::fct_rev(family))) +
    geom_density_ridges(fill = "#2d9f38") +
    # xlim(0, max(atlantic_amphibian_traits_individual$mouth_width)) +
    labs(x = "Femur (mm)", y = "Family") +
    theme_bw(base_size = 20)
figure7b
ggsave("02_results/figures/fig07b.png", figure7b, width = 30, height = 20, units = "cm", dpi = 300)

figure7c <- ggplot(atlantic_amphibian_traits_individual,
                   aes(x = tibiofibula, y = forcats::fct_rev(family))) +
    geom_density_ridges(fill = "#2d9f38") +
    labs(x = "Tibiofibula (mm)", y = "Family") +
    theme_bw(base_size = 20)
figure7c
ggsave("02_results/figures/fig07c.png", figure7c, width = 30, height = 20, units = "cm", dpi = 300)

figure7d <- ggplot(atlantic_amphibian_traits_individual,
                   aes(x = tarsus, y = forcats::fct_rev(family))) +
    geom_density_ridges(fill = "#2d9f38") +
    labs(x = "Tarsus (mm)", y = "Family") +
    theme_bw(base_size = 20)
figure7d
ggsave("02_results/figures/fig07d.png", figure7d, width = 30, height = 20, units = "cm", dpi = 300)

figure7e <- ggplot(atlantic_amphibian_traits_individual,
                   aes(x = foot, y = forcats::fct_rev(family))) +
    geom_density_ridges(fill = "#2d9f38") +
    labs(x = "Foot (mm)", y = "Family") +
    theme_bw(base_size = 20)
figure7e
ggsave("02_results/figures/fig07e.png", figure7e, width = 30, height = 20, units = "cm", dpi = 300)

figure7f <- atlantic_amphibian_traits_individual %>% 
    dplyr::select(body_size, femur, tibiofibula, tarsus, foot) %>% 
    GGally::ggpairs(lower = list(continuous = wrap("smooth", color = "#2d9f38")),
                    diag = list(continuous = wrap("densityDiag", fill = "#2d9f38")),
                    upper = list(continuous = wrap("cor", size = 5))) +
    theme_bw(base_size = 15)
figure7f
ggsave("02_results/figures/fig07f.png", figure7f, width = 30, height = 20, units = "cm", dpi = 300)

## figure 8 ----
atlantic_amphibian_traits_species_nspecies <- atlantic_amphibian_traits_species %>% 
    dplyr::select(male_body_size_svl:toxicity) %>% 
    summarise_all(~sum(!is.na(.))) %>% 
    tidyr::pivot_longer(cols = everything()) %>% 
    dplyr::arrange(-value) %>% 
    dplyr::mutate(name = forcats::as_factor(name),
                  per = round(value/533*100, 2))
atlantic_amphibian_traits_species_nspecies

figure08 <- ggplot(atlantic_amphibian_traits_species_nspecies, 
                   aes(x = forcats::fct_rev(name), y = value)) +
    geom_bar(stat = "identity", fill = "#06996d") +
    geom_text(aes(x = name, y = value, label = paste0(per, "%")), size = 6, hjust  = 1) +
    coord_flip() +
    labs(x = "Species-level traits", y = "Number of species") +
    theme_bw(base_size = 20)
figure08
ggsave("02_results/figures/fig08.png", figure08, width = 30, height = 20, units = "cm", dpi = 300)

## figure 9 ----
atlantic_amphibian_traits_species_toe_per <- atlantic_amphibian_traits_species %>% 
    dplyr::count(toe_disc) %>% 
    dplyr::mutate(toe_disc = as.factor(toe_disc),
                  per = round(n/sum(n)*100, 1)) %>% 
    tidyr::drop_na()
atlantic_amphibian_traits_species_toe_per

figure9a <- ggplot(atlantic_amphibian_traits_species_toe_per, 
                  aes(x = toe_disc, y = n)) +
    geom_bar(stat = "identity", fill = c("#d6272b", "#1c78b2")) +
    geom_text(aes(x = toe_disc, y = n, label = paste0(per, "%")), size = 8, vjust = 1.6) +
    labs(x = "Presence of toe disc", y = "Number of species") +
    theme_bw(base_size = 20)
figure9a
ggsave("02_results/figures/fig09a.png", figure9a, width = 30, height = 20, units = "cm", dpi = 300)

atlantic_amphibian_traits_species_loc_per <- atlantic_amphibian_traits_species %>% 
    dplyr::mutate(locomotion_mode = stringr::str_replace_all(locomotion_mode, "hopper-burrower", "hopper-\nburrower")) %>%
    dplyr::mutate(locomotion_mode = stringr::str_replace_all(locomotion_mode, "walker-jumper", "walker-\njumper")) %>%
    dplyr::mutate(locomotion_mode = stringr::str_replace_all(locomotion_mode, "walker-hopper", "walker-\nhopper")) %>%
    dplyr::count(locomotion_mode) %>% 
    dplyr::mutate(locomotion_mode = as.factor(locomotion_mode),
                  per = round(n/sum(n)*100, 1)) %>% 
    tidyr::drop_na()
atlantic_amphibian_traits_species_loc_per

figure9b <- ggplot(atlantic_amphibian_traits_species_loc_per, 
                  aes(x = locomotion_mode, y = n)) +
    geom_bar(stat = "identity", fill = c("#d6272b", "#ff7f24", "#bcbc38", "#1c78b2", "#9468ba", "#8c564c", "#2d9f38")) +
    geom_text(aes(x = locomotion_mode, y = n, label = paste0(per, "%")), size = 6, vjust = c(1.6, 1.6, 1.6, -.5, 1.6, 1.6, 1.6)) +
    labs(x = "Locomotion mode", y = "Number of species") +
    theme_bw(base_size = 20)
figure9b
ggsave("02_results/figures/fig09b.png", figure9b, width = 30, height = 20, units = "cm", dpi = 300)

atlantic_amphibian_traits_species_male_body_size <- atlantic_amphibian_traits_species %>% 
    dplyr::count(male_body_size_cat) %>% 
    dplyr::mutate(size_male = as.factor(male_body_size_cat),
                  per = round(n/sum(n)*100, 1)) %>% 
    tidyr::drop_na()
atlantic_amphibian_traits_species_male_body_size

figure9c <- ggplot(atlantic_amphibian_traits_species_male_body_size, 
                   aes(x = forcats::fct_rev(size_male), y = n)) +
    geom_bar(stat = "identity", fill = c("#d6272b", "#1c78b2", "#2d9f38")) +
    geom_text(aes(x = size_male, y = n, label = paste0(per, "%")), size = 8, vjust = 1.6) +
    labs(x = "Size male", y = "Number of species") +
    theme_bw(base_size = 20)
figure9c
ggsave("02_results/figures/fig09c.png", figure9c, width = 30, height = 20, units = "cm", dpi = 300)

## figure 10 ----
figure10a <- atlantic_amphibian_traits_species %>% 
    dplyr::filter(family %in% unique(atlantic_amphibian_traits_individual$family)) %>% 
    ggplot(aes(x = male_body_size_svl, y = forcats::fct_rev(family))) +
    geom_density_ridges(fill = "#12bece") +
    labs(x = "Body size male (SVL - mm)", y = "Family") +
    theme_bw(base_size = 20)
figure10a
ggsave("02_results/figures/fig10a.png", figure10a, width = 30, height = 20, units = "cm", dpi = 300)

figure10b <- atlantic_amphibian_traits_species %>% 
    dplyr::filter(family %in% unique(atlantic_amphibian_traits_individual$family)) %>% 
    ggplot(aes(x = female_body_size_svl, y = forcats::fct_rev(family))) +
    geom_density_ridges(fill = "#e378c0") +
    labs(x = "Body size female (SVL - mm)", y = "Family") +
    theme_bw(base_size = 20)
figure10b
ggsave("02_results/figures/fig10b.png", figure10b, width = 30, height = 20, units = "cm", dpi = 300)

# end ---------------------------------------------------------------------