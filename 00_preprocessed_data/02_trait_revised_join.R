# Mon Jul 29 16:53:39 2024 ------------------------------
# Author: Matheus Moroti

# This script is to add the revised traits to the nested dataframe. 
# To do this, we will load the spreadsheet previously generated in 
# the 01_trait_list script and filled in Excel by Gustavo Romero.

# load nested dataframe generate by 00_preprocessing_data
load(here::here("00_preprocessed_data",
                "nested_df_original.RData"))

# The traits have already been classified and will return to the nested_df
traits_new <- readxl::read_xlsx(
  file.path(local_directory,
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
  filter(ID != "MD24" & ID != "MD25" & ID != "MD34" & ID != "MD36", ID != "MD53") #%>%
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
nested_database <- nested_traits_join %>% 
  filter(ID != 'MD36') %>%
  select(-list, -traits)

# Rename names in abundance according traits_revised
# a gente precisa criar uma funcao que olhe para os nomes presentes na abundancia,
# ou seja, renomear nomes das colunas nas matrizes de abundancia, e tenha o nome
# correspondente nos traits. Para isso, é possível usar o nome equivalente que o
# autor deu na abundancia na coluna "Morfospecies_name" que tambem esta presente
# em traits revised. A partir dela conseguimos renomear 
df_abundance <- nested_database[[9, "abundance"]][[1]]
df_traits <- nested_database[[9, "traits_revised"]][[1]]

head(df_abundance)

vtr_dict_names <- df_traits %>%
  select("(morpho)Species", "Morfospecies_name") %>%
  deframe()

df_abundance %>%
  rename(all_of(vtr_dict_names))

#View(nested_database)
# salva no drive do projeto
save(nested_database,
     file = file.path(local_directory,
                      "nested_df.RData"))
# salva no github
save(nested_database,
     file = here::here("nested_df.RData"))
