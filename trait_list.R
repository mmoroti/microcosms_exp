# Mon Apr 29 09:56:19 2024 ------------------------------
library(tidyverse)
library(visdat)

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

# TODO Needs adjustments
#View(list_traits_unnest)  
#View(list_traits_review)
#ncol(list_traits_review) # 90 colunas

# especificando a ordem desejada das colunas
#ordem_desejada <- c("ID", "researcher", "locality", 
#                    "Class","Order", "Family", 
#                    "Genus","Morfospecies_name", "(morpho)Species", 
#                    "uncertain_trait", "life_cycle",
#                    "total_length","flat_elongate","flat_ovoid",
#                    "cylindrical_elongate","cylindrical_ovoid","aquatic_stage", 
#                    "feeding_guild", "deposit_feeder","shredder",
#                    "scraper","filter_feeder","piercer","predator",
#                    "microorganisms","detritus_(<1 mm)","dead_plant_(litter)",
#                    "living_microphytes","living_leaf_tissue",
#                    "dead_animals_(>1 mm)","living_microinvertebrates",
#                    "living_macroinvertebrates", "defense", "elongate_tubercle",
#                    "hairs","sclerotized_spines","dorsal_plates",
#                    "sclerotized_exoskeleton","shell" ,"case_or_tube", 
#                    "none_defense", "eggs_statoblasts", "cocoons",             
#                    "diapause_or_dormancy","none_resistence","<21days",
#                    "21-60days",">60days", "ovoviparity",
#                    "isolated_eggs_free","isolated_eggs_cemented",
#                    "clutches_cemented", "clutches_free", 
#                    "clutches_in_vegetation", "clutches_in_terrestrial",
#                    "clutches_terrestrial",
#                    "assexual_reproduction", "habitat", "terrestrial",
#                    "pelagic", "benthic", "water_surface", "integument",
#                    "gill", "plastron" , "Siphon/spiracle","hydrostatic_vesicle",
#                    "flier","surface_swimmer","full_water_swimmer","crawler",
#                    "burrower","interstitial","tube_builder", "dispersal_mode",
#                    "notes","OBS.y", "notes_combined",
#                    "possible classification","reference")

# reordenar as colunas de acordo com a ordem desejada
#list_traits_review <- list_traits_review %>%
#  select(all_of(ordem_desejada))
#
#ncol(list_traits_review) # 77 colunas
#View(list_traits_review)
#
## needs confer
#unique(list_traits_review$life_cycle)
#unique(list_traits_review$habitat)
#
## save list
#write.csv2(list_traits_review, "list_traits_microcosms.csv")

#write.table(list_traits_review, "list_traits_microcosms.txt", 
#            sep=",", row.names = FALSE)

# Fri May 17 20:30:44 2024 ------------------------------
# ATENCAO
# aqui precisamos fazer um ajuste manualmente (gambiarra)
# Gustavo Romero ja havia iniciado a preencher a planilha
# enquanto chegaram mais datasets, por isso, precisamos
# adequar a planilha para que fosse possivel a uniao delas sem
# causar problemas de sincronizacao dos dados em cada coluna
# aqui vamos carregar a planilha que ele esta trabalhando
# para conferir o nome e a ordem, para que seja possivel
# copiar e colar manualmente no excel os dados novos sem retirar
# da ordem os dados. por isso eh fundamental preservar os ID dos experimentos
# na ordem descrita no script anterior (nested_df)
# para mais infos, consultar Matheus Moroti ou Gustavo Romero
#planilha_comparativa <- read_xlsx("list_traits_microcosms_comparative.xlsx")
#teste <- names(planilha_comparativa)
# experimentos que estavam faltando na planilha de traits
#teste2 <- list_traits_review %>%
#  select(all_of(teste[-1])) %>%
#  filter(ID == "MD19" |
#           ID == "MD54" |
#           ID == "MD52" |
#           ID == "MD53")

#View(teste2)

#write.csv2(teste2, "teste.csv", row.names = F, dec = ".")
#write.csv2(dec = ".")
#?write.csv2

# Mon May 20 20:13:47 2024 ------------------------------
# A planilha anterior "Izadora_Nardi_Daiane_Montoia - Nucleo_Santa_Virginia_wrong" 
# foi adicionada a pasta backup foi duas morfoespecies precisaram passar 
# por novas classificações. Como a planilha de traits desaninhada 
# já está com o gustavo, vamos apenas substituir as linhas na planilha dele, 
# e atualizar no fluxo do script essas novas classificações para que a aba
# "abundance" seja compátivel com a planilha de traits nova que está sendo
# classificada pelo gustavo, tenha as mesmas espécies.
# qualquer duvida, consultar MATHEUS MOROTI ou GUSTAVO ROMERO

# Especificamente,
# 'diptera_19' passou para 'oligochaeta'
# 'diptera_16' antes era classificado como pelecorhynchidae_sp1 agr tabanidae_sp2
#stavirginia_replace <- list_traits_review %>%
#  select(all_of(teste[-1])) %>%
#  filter(ID == "MD47" &
#           Morfospecies_name == "oligochaeta" |
#           Morfospecies_name == "diptera_sp16") 

#View(stavirginia_replace)
#write.csv2(stavirginia_replace, "stavirginia_replace.csv", 
#           row.names = F, dec = ".")


###----------------------------------------------------------------------------
# Sat Jul  6 10:36:40 2024 ------------------------------
# Os traits ja foram classificados e irao retornar para o dataframe aninhado
load("dados_microcosmos/nested_df.RData")

traits_new <- read_xlsx(
  here(
    "dados_microcosmos",
    "list_traits_microcosms.xlsx"))
#View(traits_new)

# catch all columns to transform
transform_columns <- c(names(traits_new[,14:ncol(traits_new)]))

traits_new_nested <- traits_new %>% 
  mutate_at(vars(transform_columns), as.integer) %>%
  mutate(total_length = as.double(total_length)) %>%
  select(
  -"researcher", -"locality",
  -"notes", -"OBS.y", -"notes_combined", 
  -"possible classification", -"reference", -"Column1",
  -"life_cycle", -"feeding_guild", -"defense", -"habitat") %>%
  nest(.by = "ID", .key = "traits_revised")

head(traits_new_nested)

# nested dataframe
nested_traits_join <- left_join(
  data, 
  traits_new_nested,
  by = "ID") %>%
  relocate(traits_revised, .before = measures)

nested_traits <- nested_traits_join %>%
  filter(ID != "MD20" & ID != "MD48" & ID != "MD49"
         & ID != "MD36" & ID != "MD24" & ID != "MD25"
         & ID != "MD34")

for (i in 1:nrow(nested_traits)) {
  # inner_join to combine with all names in both dataset
  nested_traits$nrow_list[[i]] <- nrow(nested_traits$list[[i]])
  nested_traits$nrow_traits[[i]] <- nrow(nested_traits$traits[[i]])
  nested_traits$nrow_trait_revised[[i]] <- nrow(nested_traits$traits_revised[[i]])
  
}

View(nested_traits_join)
View(nested_traits %>%
       relocate(c(nrow_list,nrow_traits,nrow_trait_revised), 
                .after = ID))
View(traits_new)

# Sat Jul  6 14:06:17 2024 ------------------------------
# TODO
# alguns nao batem os numeros pq passaram por tratamento na elaboracao da lista
# invertebrados Terrestres foram filtrados na linha 220 e 221, mas nao no dataset
# original. 

# Precisa checar quais nao fora identificados e quais deveriam ser excluidos
# por serem terrestres. Experimentos com numeros diferentes
# MD4  MD15  MD16  MD28  MD29  MD30  MD32  MD33  MD41  MD46  MD47

# Esses experimentos nao foram classificados pois tinham na coluna 'habitat' 
# NAs, NAs nessa coluna foram filtrados provavelmente na linha 220 e 221.
# MD20, MD48, MD49 
# Ja acrescentar dados do Gossner para classificar os traits faltantes e checar
# pendencias

# checar com Joice e Gustavo
# Pettermann, Austria tem um valor de tamanho ausente.
# MD52 tem uma especie sem valor no "uncertain_trait"