# Mon Jul 29 16:53:39 2024 
# Author: Matheus Moroti
library(visdat)
library(tidyverse)
library(gridExtra)
# This script is to add the revised traits to the nested dataframe. 
# To do this, we will load the spreadsheet previously generated in 
# the 01_trait_list script and filled in Excel by Gustavo Romero.

# LOAD DATA ----
# load nested dataframe generate by 00_preprocessing_data
load(here::here("00_preprocessed_data",
                "nested_df_original.RData"))

# The traits have already been classified and will return to the nested_df
# WITHOUT SPECIES_NEW AND SPECIES_OLD NAMES
#traits_new <- readxl::read_xlsx(
#  file.path(local_directory,
#            "list_traits_microcosms.xlsx"))
# WITH SPECIES_NEW AND SPECIES_OLD NAMES
traits_new <- readxl::read_xlsx(
  file.path(local_directory,
            "list_traits_microcosms_UPDATEJ.xlsx"))

# UNIFYING DATAFRAME TRAITS_NEW IN NESTED_DF_ORIGINAL ----
# catch all columns to transform
transform_columns <- c("uncertain_trait","life_cycle",
                       names(traits_new[,17:ncol(traits_new)]))

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
nrow(traits_new_nested)
# nested dataframe
nested_traits_join <- left_join(
  data_number, 
  traits_new_nested,
  by = "ID") %>%
  relocate(traits_revised, .before = measures)

# Conferring join traits and traits_revised
# MD24, MD25 and MD34 had no invertebrates
# MD36 is an incorrect experiment (Validated information by Gustavo Romero)
# another filter is the experiments without traits_revised
nested_traits <- nested_traits_join %>%
  filter(ID != "MD24" & ID != "MD25" & ID != "MD34" & 
           ID != "MD36", ID != "MD53" & ID != "MD69" & ID != "MD56" ) #%>%
  #filter(ID != "MD57" & ID != "MD58" & ID != "MD59" & ID != "MD60" & ID != "MD61")

for (i in 1:nrow(nested_traits)) {
  #nested_traits$anti_join[[i]] <- anti_join(nested_traits$list[[i]],
  #                                          nested_traits$traits_revised[[i]],
  #                                          by = "Morfospecies_name")
  # inner_join to combine with all names in both dataset
  nested_traits$nrow_list[[i]] <- nrow(nested_traits$list[[i]])
  nested_traits$nrow_traits[[i]] <- nrow(nested_traits$traits[[i]])
  nested_traits$nrow_trait_revised[[i]] <- nrow(nested_traits$traits_revised[[i]])
  # Criação da coluna de validação
  nested_traits$validation[[i]] <- (
    nested_traits$nrow_list[[i]] == nested_traits$nrow_traits[[i]] && 
      nested_traits$nrow_list[[i]] == nested_traits$nrow_trait_revised[[i]])
  
}

View(nested_traits %>%
       relocate(c(nrow_list,nrow_traits,nrow_trait_revised, validation), 
                .after = ID))

# If everything is correct between the crossing of the dataframes, that is,
# validation = TRUE, you can remove the list and traits columns 
# as this information is together in traits_revised
# and exclude experiment MD36 (confirmed by Gustavo Romero and Joice Souza)
# and exclude experiment MD64 (confirmed by Joice Souza & Gustavo Romero)
# and exclude experiment MD56 (confirmed by Joice Souza) #only natural forest
# and exclude experiment MD48 (confirmed by Joice Souza) #only natural forest
nested_database <- nested_traits_join %>% 
  filter(ID != 'MD36' & ID != "MD48" & ID != "MD64" & ID != "MD56") %>%
  select(-list, -traits)

# CLEANING ----
# REMOVING MORPHOSPECIES WITH ''UNCERTAIN_TRAIT == 1'' IN TRAITS AND ABUNDANCE
# WE NEED TO RENAME NAMES IN ABUNDANCE ACCORDING TRAITS_REVISED TOO 
data_teste <- nested_database %>%
  filter(ID != "MD24" & ID != "MD25" & ID != "MD34",
         ID != "MD53" & ID != "MD69") # sem dados de invertebrados

unir_novamente <- nested_database %>%
  filter(ID %in% c("MD24", "MD25", "MD34", "MD53","MD69")) 
  
remove_cols <- c("Class", "Order", "Family", "Genus", 
                 "uncertain_trait","Morfospecies_name", "(morpho)Species", "OTU")

for (i in 1:nrow(data_teste)) {
  print(data_teste[[i, "ID"]][[1]])
  # Extrair os dataframes abundance e traits_revised
  df_abundance <- data_teste[[i, "abundance"]][[1]]
  df_traits <- data_teste[[i, "traits_revised"]][[1]]
  
  # Criar o dicionário de nomes com base em species_NEW e species_OLD
  # primeiro tirar os uncertain_trait = 1, depois renomear
  # Criar a nova coluna species_NEW concatenando o valor original com o valor de OTU
  df_traits_filter <- df_traits %>% 
    mutate(species_NEW = paste0(species_NEW, "_", OTU)) %>%
    filter(uncertain_trait == 0) %>%
    select(-c(remove_cols))
  
  # morphospecies (colunas em abundance) que precisam ser renomeadas). Para isso
  # geramos um dicionario dos nomes novos e antigos das especies que entram no dataset
  vtr_dict_names <- df_traits_filter %>%
    #filter(uncertain_trait == 0) %>%
    select(species_NEW, species_OLD) %>%
    deframe()
  
  # morphospecies que devem ser removidas
  vector_remove <- df_traits %>% 
    filter(uncertain_trait == 1) %>%
    pull(species_OLD) # remover colunas de abundancia
  
  # Renomear as colunas de df_abundance usando o dicionário
  df_abundance_renamed <- df_abundance %>%
    select(-c(vector_remove)) %>%
    rename(all_of(vtr_dict_names))
  
  # Atualizar o dataframe dentro do nested_database com o dataframe renomeado
  data_teste[[i, "abundance"]][[1]] <- df_abundance_renamed
  data_teste[[i, "traits_revised"]][[1]] <- df_traits_filter %>%
    select(-species_OLD)
}

# VALIDATIONS ----
# conferir n abundancia 
# conferir n traits
for (i in 1:nrow(data_teste)) {
  
  print(data_teste[[i, "ID"]][[1]])
  
  df_abundance <- data_teste[[i, "abundance"]][[1]]
  df_traits <- data_teste[[i, "traits_revised"]][[1]]

  data_teste$nrow_abundance[[i]] <- ncol(df_abundance %>% 
                                              select(-Treatment, -Replicate))
  
  data_teste$nrow_traits[[i]] <- nrow(df_traits)
  
  # Criação da coluna de validação
  data_teste$validation[[i]] <- (
    data_teste$nrow_abundance[[i]] == data_teste$nrow_traits[[i]])# && 
    #  nested_traits$nrow_list[[i]] == nested_traits$nrow_trait_revised[[i]]) 

}
View(data_teste %>%
       relocate(c(nrow_traits,nrow_abundance, validation), 
                .after = ID)) 

# MAPEAR NA's NAS TRES PLANILHAS
# Caminho para salvar o arquivo PDF
pdf("missing_traitdata.pdf")
# Loop por cada linha do dataframe aninhado
for (i in seq_len(nrow(data_teste))) {
  
  # Extrai o ID e o dataframe aninhado
  current_id <- data_teste$ID[i]
  current_df <- data_teste$traits_revised[[i]]
  
  # Título com a chave 'ID'
  title <- paste("ID:", current_id)
  
  # Cria a visualização de dados ausentes com vis_miss
  vis_plot <- vis_miss(current_df)
  
  # Plota a visualização e o título
  grid.arrange(vis_plot, top = title)
}
dev.off()

# measures
pdf("missing_measuresdata.pdf")
# Loop por cada linha do dataframe aninhado
for (i in seq_len(nrow(data_teste))) {
  
  # Extrai o ID e o dataframe aninhado
  current_id <- data_teste$ID[i]
  current_df <- data_teste$measures[[i]]
  
  # Título com a chave 'ID'
  title <- paste("ID:", current_id)
  
  # Cria a visualização de dados ausentes com vis_miss
  vis_plot <- vis_miss(current_df)
  
  # Plota a visualização e o título
  grid.arrange(vis_plot, top = title)
}
dev.off()

# CONFERIR NOME DAS COLUNAS
# Fazer conferencia de cada dataframe se cottomstrip_after > cottomstrip_before

# SAVE DATA AFTER VALIDATIONS  ----
nested_database_cleaned <- data_teste %>%
  select(-c(nrow_traits,nrow_abundance, validation)) %>%
  bind_rows(unir_novamente)

# salva no drive do projeto
save(nested_database_cleaned,
     file = file.path(local_directory,
                      "nested_df.RData"))
# salva no github
save(nested_database_cleaned,
     file = here::here("nested_df.RData"))
