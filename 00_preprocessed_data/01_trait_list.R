# Mon Apr 29 09:56:19 2024 ------------------------------
library(tidyverse)
#library(visdat)

# joins list and traits into a dataframe traits_list for classification
# of the new traits. It also checks if the three columns have
# the same number of species. If false, they have different counts.
combine_and_count <- function(list_trait_non_classified) {
  for (i in 1:nrow(list_trait_non_classified)) {
    # inner_join to combine with all names in both dataset
    list_trait_non_classified$trait_list[[i]] <- inner_join(list_trait_non_classified$list[[i]],
                                                            list_trait_non_classified$traits[[i]],
                                                            by = "Morfospecies_name")
    list_trait_non_classified$nrow_list[[i]] <- nrow(list_trait_non_classified$list[[i]])
    list_trait_non_classified$nrow_traits[[i]] <- nrow(list_trait_non_classified$traits[[i]])
    list_trait_non_classified$nrow_trait_list[[i]] <- nrow(list_trait_non_classified$trait_list[[i]])
    
    list_trait_non_classified$validation[[i]] <- (
      list_trait_non_classified$nrow_list[[i]] == list_trait_non_classified$nrow_traits[[i]] && 
        list_trait_non_classified$nrow_list[[i]] == list_trait_non_classified$nrow_trait_list[[i]])
  }
  list_trait_non_classified <- list_trait_non_classified %>% relocate(
    c(nrow_list,nrow_traits,nrow_trait_list, validation), .after = ID) %>%
    
    return(list_trait_non_classified)
}

# Herein, we generate new nested lists as the database experiment progresses
# This part is no longer necessary to be executed. 
# Here, we combined all species from the ‘list’ and ‘traits’ sheets 
# into a single list to receive new life history trait classifications.
# This process occurred over time, so each case required an alternative solution.
# Fri Jul 26 18:14:52 2024 ------------------------------


# aqui vamos preparar uma lista de especies com os traits para revisao
# provavelmente algumas spp irao sair pois sao terrestres e 
# nao devem contar para o experimento de microcosmos. 
load("dados_microcosmos/nested_df.RData")
#View(data)
# para facilitar a manipulacao dos dados, retiramos as colunas 
# desnecessarias e tambem os experimentos que nao tinha invertebrados
# md24, md25, md34 nao tem invertebrados, por isso foram removidos
list_traits <- data %>% 
  select(-"roof_treatment", -"abundance", -"measures", -"obs") %>%
  filter(ID != "MD24" & ID != "MD25" & ID != "MD34")

# Loop for para dar join entre list e traits
# depois percorrer o dataframe aninhado 
# e contar o numero de linhas em cada dataframe para conferencia
for (i in 1:nrow(list_traits)) {
  # inner_join to combine with all names in both dataset
  list_traits$trait_list[[i]] <- inner_join(list_traits$list[[i]],
                                 list_traits$traits[[i]],
                                by = "Morfospecies_name")
  list_traits$nrow_list[[i]] <- nrow(list_traits$list[[i]])
  list_traits$nrow_traits[[i]] <- nrow(list_traits$traits[[i]])
  list_traits$nrow_trait_list[[i]] <- nrow(list_traits$trait_list[[i]])
  }

# code created to make adjustments and confer type of variables
# this procedure is necessary to create list_traits_unnest
# since the syntax to create unnest dataframe is necessary to
# change the type of columns from the same (numeric). 
#for (i in 1:nrow(list_traits)) {
#  print(list_traits$ID[[i]])
#  glimpse(list_traits$traits[[i]])
#}
# needs adjusments traits
# MD8 rename average_length to total_length
# MD13 rename total_length(mm) to total_length
# MD20 rename `total_length (mm)` to total_length
# MD26 change type of variable and remove 'mm'
# MD31 rename  `total_length(mm)` to total_length]
# MD32 rename  `total_length(mm)` to total_length
# MD36 change type of variable and remove mm
# MD37 change type of variable and remove mm
# MD43 change type of variable and remove mm
# MD44 change type of variable and remove mm
# MD45 rename  `total_length(mm)` to total_length
# MD53 change type of total_length variable 

# unnested dataframe
list_traits_unnest <- list_traits %>% 
       select("ID","researcher","locality","trait_list") %>%
       unnest(cols="trait_list")

# Fri May 10 14:34:49 2024 ------------------------------
# unificando algumas colunas de anotacoes de diferentes autores em menos
# colunas, apenas para facilitar e diminuir colunas desnecessarias e NAs
names(list_traits_unnest)
glimpse(list_traits_unnest)
vis_miss(list_traits_unnest)

#View(list_traits_unnest)

# tratamento para mudar o que nao esta como na, e na vdd eh na
# por exemplo, undefined = na
#unique(list_traits_unnest$Family)

list_traits_unnest <- list_traits_unnest %>%
  mutate_all(~ifelse(. == "NA", NA, .)) %>%
  mutate_all(~ifelse(. == "unidentified", NA, .))

# Combinar as colunas com dados ausentes
list_traits_unnest$notes_combined <- coalesce(
  list_traits_unnest$...1,
  as.character(list_traits_unnest$...1.x)) 

list_traits_unnest$notes_combined <- coalesce(
  list_traits_unnest$notes_combined,
  list_traits_unnest$"Liam Notes")

list_traits_unnest$notes_combined <- coalesce(
  list_traits_unnest$notes_combined,
  list_traits_unnest$"remarks")

list_traits_unnest$notes_combined <- coalesce(
  list_traits_unnest$notes_combined,
  list_traits_unnest$"OBS.x")

list_traits_unnest$notes_combined <- coalesce(
  list_traits_unnest$notes_combined,
  list_traits_unnest$"...1.y")

#---
# add this code after inclusion claas and anikka 
list_traits_unnest$notes_combined <- coalesce(
  list_traits_unnest$notes_combined,
  list_traits_unnest$"notes...7")
# add this code after inclusion claas and anikka 
list_traits_unnest$OBS2 <- coalesce(
  list_traits_unnest$OBS2,
  list_traits_unnest$"notes...8")
###---

# coalesce life_stage and development stage
list_traits_unnest$life_stage <- coalesce(
  list_traits_unnest$life_stage,
  list_traits_unnest$"Development stage")

# column "Information" has the same information as the "life_stage"
# for this reason, we use the coalesce function
list_traits_unnest$life_stage <- coalesce(
  list_traits_unnest$life_stage,
  list_traits_unnest$Information)

#View(list_traits_unnest)

list_traits_unnest <- list_traits_unnest %>%
  select(-"...1", -"...1.x", -"...1.y", 
         -"Liam Notes", -"remarks",
         -"Information", -"Development stage", -"OBS.x",
         -"Subfamily", "notes...7", "notes...8")

View(list_traits_unnest)

# coalesce unnecessary columns
list_traits_unnest$notes <- coalesce(
  list_traits_unnest$OBS2,
  list_traits_unnest$notes)

list_traits_unnest <- list_traits_unnest %>%
  select(-"OBS2")

vis_miss(list_traits_unnest) # 18% missing data "Family"
#View(list_traits_unnest)

# agora precisamos adicionar novas colunas para que seja possivel 
# preencher fuzzy traits. Os traits escolhidos sao similares aos de
# Cereghino et al., 2018 - Functional Ecology
# trait - type
# mean body size - continuous
# aquatic stage - ordinal (categories)
# reproduction - fuzzy
# resistance form - fuzzy
# locomotion - fuzzy
# food - fuzzy
# feeding group - fuzzy
# body form - fuzzy
# cohort production interval - fuzzy
# morphological defence - fuzzy
# dispersal mode - binary
list_traits_review <- list_traits_unnest %>%
       rename("aquatic_stage" = "life_stage") %>%
  mutate(egg = NA,
         larva = NA,
         nymph = NA,
         adult = NA) %>%
  mutate(uncertain_trait = NA) %>%
  mutate(ovoviparity = NA,
         isolated_eggs_free = NA,
         isolated_eggs_cemented = NA,
         clutches_cemented = NA,
         clutches_free = NA,
         clutches_in_vegetation = NA,
         clutches_in_terrestrial = NA,
         clutches_terrestrial = NA,
         assexual_reproduction = NA) %>%
  mutate(disp_passive = NA,
         disp_active = NA) %>%
  mutate(eggs_statoblasts = NA,
         cocoons = NA,
         diapause_or_dormancy = NA,
         none_resistence = NA) %>%
  mutate(integument = NA,
         gill	= NA,
         plastron	= NA,
         "Siphon/spiracle" = NA,
         hydrostatic_vesicle = NA) %>%
  mutate(flier = NA,
         surface_swimmer = NA,
         full_water_swimmer	= NA,
         crawler = NA,
         burrower	= NA,
         interstitial	= NA,
         tube_builder = NA) %>%
  mutate(microorganisms = NA,
         "detritus_(<1 mm)" = NA,
         "dead_plant_(litter)" = NA,
         living_microphytes	= NA,
         living_leaf_tissue	= NA, 
         "dead_animals_(>1 mm)" =	NA,
         living_microinvertebrates = NA,
         living_macroinvertebrates = NA) %>%
  mutate(deposit_feeder = NA,
         shredder = NA,
         scraper = NA,
         filter_feeder = NA,
         piercer = NA, 
         predator = NA) %>%
  mutate(terrestrial = NA,
         pelagic = NA, 
         benthic = NA,
         water_surface = NA) %>%
  mutate("<21days" = NA, 
         "21-60days" = NA,
         ">60days" = NA) %>%
  mutate(none_defense = NA,
         elongate_tubercle = NA,
         hairs = NA,
         sclerotized_spines	= NA,
         dorsal_plates = NA,
         sclerotized_exoskeleton = NA,
         shell	= NA,
         case_or_tube = NA) %>%
  mutate(flat_elongate = NA,
         flat_ovoid = NA,
         cylindrical_elongate = NA,
         cylindrical_ovoid = NA) %>%
  filter(habitat != "terrestrial" &
           habitat != "Terrestrial") %>% # remove td terrestre 
  mutate(siphon_absent = NA,
         siphon_short = NA,
         siphon_long = NA)


# Mon Jul 29 16:47:41 2024 ------------------------------
load(here::here("00_preprocessed_data",
                "nested_df_original.RData"))
# The traits have already been classified and will return to the nested_df
# here, we just need col name order
#traits_new <- readxl::read_xlsx(
#  file.path(local_directory,
#            "list_traits_microcosms.xlsx"))
#names_columns <- names(traits_new %>% select(-notes))

# filter experiments without traits_revised
traits_new <- data_number %>%
  filter(ID == "MD57" | 
           ID == "MD58" |
           ID == "MD59" |
           ID == "MD60" |
           ID == "MD61")

trais_need_revision <- combine_and_count(traits_new )

trais_list_revision <- trais_need_revision %>% 
  select("ID","researcher","locality","trait_list") %>%
  unnest(cols="trait_list") %>% 
  mutate(uncertain_trait = NA) %>%
  select(
    "ID","researcher","locality","Class", "Order", "Family", "Genus", 
    "Morfospecies_name", "(morpho)Species", "uncertain_trait", "life_cycle",
    "total_length")

getwd()
write.csv2(trais_list_revision, "need_revision.csv")
