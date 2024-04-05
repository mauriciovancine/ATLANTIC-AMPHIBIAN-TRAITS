#' ----
#' title: atlantic amphibian traits - prepare traits data
#' author: mauricio vancine
#' date: 2023-11-18
#' ----

# prepare r -------------------------------------------------------------

# packages
library(tidyverse)
library(janitor)
library(sf)
library(geobr)
library(tidygeocoder)
library(tmap)

# individual-level traits ----------------------------------------------------------------

## cfbh ----

# atlantic amphibian traits
atlantic_amphibian_traits <- readxl::read_excel("01_data/vancine_etal_2024_atlantic_amphibian_traits/medidas_caracteristicas_anfibios_2023_11_d28.xlsx") %>%
    tibble::rowid_to_column(var = "id") %>%
    dplyr::filter(obs == 0) %>%
    dplyr::relocate(font, voucher, .after = obs)
atlantic_amphibian_traits

atlantic_amphibian_traits_count <- atlantic_amphibian_traits %>%
    dplyr::count(species, sort = TRUE)
atlantic_amphibian_traits_count

# cfbh collection
cfbh_collection <- readr::read_csv("01_data/vancine_etal_2024_atlantic_amphibian_traits/cfbh_collection/cfbh_collection.csv") %>%
    janitor::clean_names()
cfbh_collection

cfbh_collection_filtered <- cfbh_collection %>%
    dplyr::filter(numero %in% atlantic_amphibian_traits$voucher)
cfbh_collection_filtered

# readr::write_csv(cfbh_collection_filtered, "01_data/vancine_etal_2024_atlantic_amphibian_traits/cfbh_collection/cfbh_filtered.csv")

# after edit
cfbh_collection_filtered_edited <- readr::read_csv("01_data/vancine_etal_2024_atlantic_amphibian_traits/cfbh_collection/cfbh_collection_filtered_edited.csv")
cfbh_collection_filtered_edited

# coordinates # 22, 23 and 24
cfbh_collection_filtered_edited_24 <- cfbh_collection_filtered_edited %>%
    filter(str_detect(zona_utm_datum, "24")) %>%
    mutate(xutm = x_utm, yutm = y_utm) %>%
    sf::st_as_sf(coords = c("xutm", "yutm"), crs = 32724) %>%
    sf::st_transform(4326) %>%
    dplyr::mutate(lon = sf::st_coordinates(.)[, 1]) %>%
    dplyr::mutate(lat = sf::st_coordinates(.)[, 2])
cfbh_collection_filtered_edited_24
as.data.frame(sf::st_drop_geometry(cfbh_collection_filtered_edited_24))[, c("numero", "lat", "lon")]

br <- geobr::read_country() %>%
    sf::st_transform(crs = st_crs(cfbh_collection_filtered_edited_24))

cfbh_collection_filtered_edited_24c <- sf::st_drop_geometry(cfbh_collection_filtered_edited_24[br,])[, c("numero", "lat", "lon")]
as.data.frame(cfbh_collection_filtered_edited_24c)

tm_shape(br, bbox = cfbh_collection_filtered_edited_24) +
    tm_borders() +
    tm_shape(cfbh_collection_filtered_edited_24) +
    tm_bubbles(size = .3) +
    tm_graticules()

# after edit coordinates
cfbh_collection_filtered_edited <- readr::read_csv("01_data/vancine_etal_2024_atlantic_amphibian_traits/cfbh_collection/cfbh_collection_filtered_edited.csv") %>%
    dplyr::mutate(voucher = as.character(numero),
                  family = familia,
                  genus = genero,
                  epithet = especie,
                  country = stringr::str_replace(pais, "Brasil", "Brazil"),
                  state = estado,
                  municipality = municipio,
                  locality = localidade,
                  longitude = long_graus_decimais,
                  latitude = lat_graus_decimais,
                  collection_date = lubridate::dmy(data_inicial),
                  collector_name = coletor) %>%
    dplyr::select(voucher, family, genus, epithet, country, state, municipality,
                  locality, longitude, latitude, collection_date, collector_name)
cfbh_collection_filtered_edited

# cfbh_collection_filtered_edited_coords <- cfbh_collection_filtered_edited %>%
#   dplyr::distinct(municipality, state, country) %>%
#   dplyr::mutate(tidygeocoder::geo(city = municipality, state = state, country = country, method = "osm", timeout = 1e3)) %>%
#   dplyr::rename(longitude_approximate = long,
#                 latitude_approximate = lat) %>%
#   dplyr::select(state, city, longitude_approximate, latitude_approximate)
#
# readr::write_csv(cfbh_collection_filtered_edited_coords,
#                  "01_data/vancine_etal_2024_atlantic_amphibian_traits/cfbh_collection/cfbh_collection_filtered_edited_coords.csv")

cfbh_collection_filtered_edited_coords <- readr::read_csv("01_data/vancine_etal_2024_atlantic_amphibian_traits/cfbh_collection/cfbh_collection_filtered_edited_coords.csv") %>%
    dplyr::rename(longitude_approximate = longitude_regional, latitude_approximate = latitude_regional)
cfbh_collection_filtered_edited_coords

cfbh_collection_filtered_edited_approximate_coords <- cfbh_collection_filtered_edited %>%
    dplyr::left_join(cfbh_collection_filtered_edited_coords, by = c("state", "municipality" = "city")) %>%
    dplyr::relocate(longitude_approximate, latitude_approximate, .after = latitude)
cfbh_collection_filtered_edited_approximate_coords

cfbh_collection_filtered_edited_approximate_coords_v <- cfbh_collection_filtered_edited_approximate_coords %>%
    tidyr::drop_na(longitude_approximate, latitude_approximate) %>%
    sf::st_as_sf(coords = c("longitude_approximate", "latitude_approximate"), crs = 4326)
cfbh_collection_filtered_edited_approximate_coords_v

tm_shape(br) +
    tm_polygons() +
    tm_shape(cfbh_collection_filtered_edited_approximate_coords_v) +
    tm_bubbles(size = .3) +
    tm_graticules(lines = FALSE)

# spatial filter
cfbh_collection_filtered_edited_approximate_coords_states <- cfbh_collection_filtered_edited_approximate_coords %>%
    dplyr::filter(!state %in% c("AC", "AM", "GO", "MA", "MT", "MS", "PA", "PI", "RO", "TO")) %>%
    dplyr::filter(!voucher %in% c(29990, 37299, 10196, 20567, 20549, 20528, 23449,
                                  20418, 16116, 16150, 25889, 24528, 34817, 34818,
                                  31686, 31687, 34703, 35670, 429, 39788))

# export
readr::write_csv(cfbh_collection_filtered_edited_approximate_coords_states,
                 "01_data/vancine_etal_2024_atlantic_amphibian_traits/cfbh_collection/cfbh_collection_filtered_edited_approximate_coords_states.csv")

## import ----
traits_measured <- readr::read_csv("01_data/vancine_etal_2024_atlantic_amphibian_traits/ATLANTIC_AMPHIBIAN_TRAITS_measurements.csv") %>%
    dplyr::filter(obs == 0,
                  voucher %in% cfbh_collection_filtered_edited_approximate_coords_states$voucher) %>%
    dplyr::rename(head_width = mouth_width,
                  head_length = mouth_length) %>% 
    dplyr::select(-c(obs)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(forelimb = sum(humerus, radio_ulna, hand, na.rm = TRUE),
                  hindlimb = sum(femur, tibio_fibula, tarsus, feet, na.rm = TRUE)) %>%
    dplyr::relocate(voucher, .before = 1) %>% 
    dplyr::rename(radioulna = radio_ulna, 
                  tibiofibula = tibio_fibula,
                  foot = feet,
                  body_size = svl,
                  collection = font)
traits_measured

names(traits_measured)
nrow(traits_measured)
length(unique(traits_measured$species))

## update taxonomy ----

# taxonomy
asw_taxonomy <- read.csv("01_data/asw_taxonomy.csv") %>%
    dplyr::select(-url)
asw_taxonomy

asw_synonyms <- read.csv("01_data/asw_synonyms.csv")
asw_synonyms

sync <- AmphiNom::aswSync(query = sort(unique(traits_measured$species)),
                          asw = asw_synonyms,
                          interactive = TRUE,
                          return.no.matches = TRUE)

# synonym report
synonym_report <- AmphiNom::synonymReport(query_info = sync)
synonym_report

synonym_report_verbose <- AmphiNom::synonymReport(query_info = sync, verbose = TRUE)
synonym_report_verbose

synonym_report_verbose$names_not_found
synonym_report_verbose$ambiguities
synonym_report_verbose$duplicated

# join taxonomic data
traits_measured_taxonomic <- traits_measured %>%
    dplyr::left_join(sync, by = c("species" = "query")) %>%
    dplyr::mutate(ASW_names = stringr::str_replace_all(ASW_names, "Cycloramphus carvalhoi2", "Cycloramphus carvalhoi")) %>%
    dplyr::mutate(species_frost2023 = ASW_names) %>%
    dplyr::relocate(species_frost2023, .after = species) %>%
    dplyr::select(-c(ASW_names, stripped, status, warnings)) %>%
    dplyr::mutate(species_frost2023 = ifelse(species_frost2023 == "Lithobates catesbeianus", "Aquarana catesbeiana", species_frost2023)) %>% 
    dplyr::left_join(asw_taxonomy, by = c("species_frost2023" = "species")) %>%
    dplyr::relocate(order,  superfamily, family, subfamily, genus, .after = 3) %>%
    dplyr::mutate(order = ifelse(is.na(order), "Anura", order),
                  family = ifelse(is.na(family), "Hylidae", family),
                  subfamily = ifelse(is.na(subfamily), "Hylinae", subfamily),
                  genus = ifelse(is.na(genus), "Scinax", genus))
traits_measured_taxonomic

traits_measured_taxonomic %>%
    dplyr::count(species, sort = TRUE)

traits_measured_taxonomic %>%
    dplyr::count(species_frost2023, sort = TRUE)

## join cfbh data ----
cfbh <- readr::read_csv("01_data/vancine_etal_2024_atlantic_amphibian_traits/cfbh_collection/cfbh_collection_filtered_edited_approximate_coords_states.csv") %>%
    dplyr::mutate(voucher = as.character(voucher)) %>%
    dplyr::mutate(species_cfbh = paste(genus, epithet), .before = 1) %>%
    dplyr::mutate(collection = "cfbh") %>%
    dplyr::relocate(voucher, .before = 1) %>%
    dplyr::select(-c(family, genus, epithet))
cfbh

traits_measured_taxonomic_cfbh <- traits_measured_taxonomic %>%
    dplyr::left_join(cfbh, by = c("voucher", "collection")) %>% 
    tibble::rowid_to_column(var = "id")
traits_measured_taxonomic_cfbh

traits_measured_taxonomic_cfbh_species <- traits_measured_taxonomic_cfbh %>%
    dplyr::count(species_frost2023, sort = TRUE)
traits_measured_taxonomic_cfbh_species

## map ----
traits_measured_taxonomic_cfbh_precise_coords <- traits_measured_taxonomic_cfbh %>% 
    tidyr::drop_na(longitude, latitude)
traits_measured_taxonomic_cfbh_precise_coords

traits_measured_taxonomic_cfbh_approximate_coords <- traits_measured_taxonomic_cfbh %>% 
    dplyr::filter(!id %in% traits_measured_taxonomic_cfbh_precise_coords$id)
traits_measured_taxonomic_cfbh_approximate_coords

traits_measured_taxonomic_cfbh_precise_coords_v <- traits_measured_taxonomic_cfbh_precise_coords %>%
    tidyr::drop_na(longitude, latitude) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

traits_measured_taxonomic_cfbh_approximate_coords_v <- traits_measured_taxonomic_cfbh_approximate_coords %>%
    tidyr::drop_na(longitude_approximate, latitude_approximate) %>%
    sf::st_as_sf(coords = c("longitude_approximate", "latitude_approximate"), crs = 4326)

br <- geobr::read_country(year = 2020)
af <- sf::st_read("01_data/geodata/000_atlantic_spatial_delimitation.gpkg")

map <- tm_shape(af) +
    tm_polygons(col = NA, fill = "gray") +
    tm_shape(br) +
    tm_borders() +
    tm_shape(traits_measured_taxonomic_cfbh_approximate_coords_v) +
    tm_bubbles(fill = "gray30", size = .7) +
    tm_shape(traits_measured_taxonomic_cfbh_precise_coords_v) +
    tm_bubbles(fill = "blue", size = .7) +
    tm_graticules(lines = FALSE, labels.size = 1.2)
map

# export
readr::write_csv(traits_measured_taxonomic_cfbh, "02_results/ATLANTIC_AMPHIBIAN_TRAITS_individual.csv")
readr::write_csv(traits_measured_taxonomic_cfbh_precise_coords, "02_results/ATLANTIC_AMPHIBIAN_TRAITS_individual_precise_coords.csv")
readr::write_csv(traits_measured_taxonomic_cfbh_approximate_coords, "02_results/ATLANTIC_AMPHIBIAN_TRAITS_individual_approximate_coords.csv")

# species-level traits -------------------------------------------------------------

## import ----
traits_book <- readr::read_csv("01_data/vancine_etal_2024_atlantic_amphibian_traits/ATLANTIC_AMPHIBIAN_TRAITS_book.csv") %>%
    dplyr::filter(obs == 0) %>%
    dplyr::rename(id = n, species_haddad2013 = specie) %>%
    dplyr::mutate(species_haddad2013 = ifelse(species_haddad2013 == "Rhinella scheneideri", "Rhinella schneideri", species_haddad2013)) %>%
    dplyr::mutate(species_haddad2013 = ifelse(species_haddad2013 == "Phyllodytes brevirostis", "Phyllodytes brevirostris", species_haddad2013)) %>%
    dplyr::mutate(species_haddad2013 = ifelse(species_haddad2013 == "Xenohyla eugenoi", "Xenohyla eugenioi", species_haddad2013)) %>%
    dplyr::mutate(species_haddad2013 = ifelse(species_haddad2013 == "Physalaemus irrotatus", "Physalaemus irroratus", species_haddad2013)) %>%
    dplyr::mutate(species_haddad2013 = ifelse(species_haddad2013 == "Leptodactylus marambaie", "Leptodactylus marambaiae", species_haddad2013)) %>%
    dplyr::mutate(species_haddad2013 = ifelse(species_haddad2013 == "Leptodactylus spixii", "Leptodactylus spixi", species_haddad2013)) %>%
    dplyr::mutate(species_haddad2013 = ifelse(species_haddad2013 == "Leptodactylus macorosternum", "Leptodactylus macrosternum", species_haddad2013)) %>%
    dplyr::select(id, species_haddad2013, svl_male_mm:threat_level, endemic_of_atlantic_forest) %>% 
    dplyr::rename(oviposition = ovoposition,
                  male_body_size_svl = svl_male_mm,
                  female_body_size_svl = svl_female_mm,
                  male_body_size_cat = size_male,
                  female_body_size_cat = size_female,
                  adult_habitat = habitat,
                  adult_habit = habit,
                  egg_type = eggs,
                  toxicity = poisonous) %>% 
    dplyr::mutate(calling_site = stringr::str_replace_all(calling_site, "vegetation", "low_vegetation")) %>% 
    dplyr::mutate(calling_site = stringr::str_replace_all(calling_site, "low_low_vegetation", "low_vegetation")) %>% 
    dplyr::mutate(oviposition = stringr::str_replace_all(oviposition, "basings", "basins")) %>% 
    dplyr::mutate(oviposition = stringr::str_replace_all(oviposition, "foan_nest", "foam_nest")) %>% 
    dplyr::mutate(oviposition = stringr::str_replace_all(oviposition, "basins", "basin")) %>% 
    dplyr::mutate(oviposition = stringr::str_replace_all(oviposition, "chambers", "chamber")) %>% 
    dplyr::mutate(oviposition = stringr::str_replace_all(oviposition, "nests", "subterranean_nest")) %>% 
    dplyr::mutate(oviposition = stringr::str_replace_all(oviposition, "directly_terrestrial\n", "directly_terrestrial")) %>% 
    dplyr::mutate(oviposition = stringr::str_replace_all(oviposition, "aerial_plants", "aerial_plant")) %>% 
    dplyr::mutate(oviposition = stringr::str_replace_all(oviposition, "aerial_plants", "aerial_plant")) %>% 
    dplyr::mutate(reproductive_mode = ifelse(reproductive_mode == "Vivípara", "viviparous", reproductive_mode)) %>% 
    dplyr::mutate(reproductive_mode = ifelse(reproductive_mode == "Ovípara, desenvol direto", "oviparous,direct_development", reproductive_mode)) %>% 
    dplyr::mutate(tadpoles = stringr::str_replace_all(tadpoles, "still_water,running_water", "still_water;running_water")) %>%  
    dplyr::mutate(tadpoles = stringr::str_replace_all(tadpoles, "still_water,terrestrial", "still_water;terrestrial")) %>% 
    dplyr::mutate(tadpoles = stringr::str_replace_all(tadpoles, "running_water,terrestrial\n", "running_water;terrestrial")) %>% 
    tidyr::separate(tadpoles, c("tadpole_nutrition", "tadpole_habit"), sep = ",") %>% 
    dplyr::mutate(tadpole_habit = stringr::str_replace_all(tadpole_habit, "still_water;running_water", "still_water,running_water")) %>%  
    dplyr::mutate(tadpole_habit = stringr::str_replace_all(tadpole_habit, "still_water;terrestrial", "still_water,terrestrial")) %>% 
    dplyr::mutate(tadpole_habit = stringr::str_replace_all(tadpole_habit, "running_water;terrestrial", "running_water,terrestrial")) %>% 
    dplyr::mutate(adult_habit = stringr::str_replace_all(adult_habit, "fossorial, aquatic", "fossorial,aquatic")) %>% 
    dplyr::mutate(adult_habit = stringr::str_replace_all(adult_habit, "semi-aquatic", "semiaquatic")) %>% 
    dplyr::mutate(tadpole_nutrition = stringr::str_replace_all(tadpole_nutrition, "direc_develop", "direc_development")) %>% 
    dplyr::mutate(calling_site = stringr::str_replace_all(calling_site, " ", "_")) %>% 
    dplyr::mutate(calling_site = stringr::str_replace_all(calling_site, "caves_burrows", "cave_burrow")) %>% 
    dplyr::relocate(adult_habitat, .after = activity) %>% 
    dplyr::relocate(adult_habit, .after = adult_habitat) %>%
    dplyr::relocate(tadpole_habit, .after = adult_habit) %>%
    dplyr::relocate(tadpole_habit, .after = adult_habit) %>% 
    dplyr::relocate(abundance, .before = toxicity) %>% 
    dplyr::relocate(calling_site, .after = reproductive_mode) %>% 
    dplyr::mutate(male_body_size_svl = round(male_body_size_svl, 2),
                  female_body_size_svl = round(female_body_size_svl, 2)) %>% 
    dplyr::select(-threat_level)
traits_book

## update taxonomy ----
# taxonomy
asw_synonyms <- read.csv("01_data/asw_synonyms.csv")
asw_synonyms

sync <- AmphiNom::aswSync(query = traits_book$species_haddad2013,
                          asw = asw_synonyms,
                          interactive = TRUE,
                          return.no.matches = TRUE)

synonym_report <- AmphiNom::synonymReport(query_info = sync)
synonym_report

synonym_report_verbose <- AmphiNom::synonymReport(query_info = sync, verbose = TRUE)
synonym_report_verbose

synonym_report_verbose$names_not_found
synonym_report_verbose$ambiguities
synonym_report_verbose$duplicated

traits_book_taxonomy <- traits_book %>%
    dplyr::left_join(sync, by = c("species_haddad2013" = "query")) %>%
    dplyr::mutate(ASW_names = stringr::str_replace_all(ASW_names, "Cycloramphus carvalhoi2", "Cycloramphus carvalhoi")) %>%
    dplyr::mutate(species_frost2023 = ASW_names, .after = 2) %>%
    dplyr::select(-c(ASW_names, stripped, status, warnings)) %>% 
    dplyr::mutate(species_frost2023 = ifelse(species_frost2023 == "Lithobates catesbeianus", "Aquarana catesbeiana", species_frost2023))
traits_book_taxonomy

# complete taxonomy
asw_taxonomy <- read.csv("01_data/asw_taxonomy.csv") %>%
    dplyr::select(-url)
asw_taxonomy

# species traits toe disc and locomotion mode
locomotion_mode <- readr::read_csv("01_data/vancine_etal_2024_atlantic_amphibian_traits/ATLANTIC_AMPHIBIAN_TRAITS_locomotion_mode_genus.csv") %>% 
    dplyr::mutate(locomotion_mode = stringr::str_replace_all(locomotion_mode, "-", "_"))
locomotion_mode

traits_book_taxonomy_complete <- traits_book_taxonomy %>%
    dplyr::left_join(asw_taxonomy, by = c("species_frost2023" = "species")) %>%
    dplyr::relocate(order,  superfamily, family, subfamily, genus, .after = 3) %>%
    dplyr::mutate(order = ifelse(is.na(order), "Anura", order),
                  family = ifelse(is.na(family), "Hylidae", family),
                  subfamily = ifelse(is.na(subfamily), "Hylinae", subfamily),
                  genus = ifelse(is.na(genus), "Scinax", genus)) %>% 
    dplyr::mutate(toe_disc = ifelse(family == "Centrolenidae" | family == "Hylidae" & !genus %in% c("Lysapsyus", "Pseudis"), 1, 0), .after = female_body_size_cat) %>% 
    dplyr::mutate(toe_disc = ifelse(order == "Gymnophiona", NA, toe_disc), .after = female_body_size_cat) %>% 
    dplyr::left_join(locomotion_mode) %>% 
    dplyr::relocate(locomotion_mode, .after = toe_disc) %>% 
    dplyr::select(-id) %>% 
    tibble::rowid_to_column(var = "id")
traits_book_taxonomy_complete

# export
readr::write_csv(traits_book_taxonomy_complete, "02_results/ATLANTIC_AMPHIBIAN_TRAITS_species.csv")

# end ---------------------------------------------------------------------