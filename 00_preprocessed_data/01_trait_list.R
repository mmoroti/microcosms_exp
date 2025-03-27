library(tidyverse)

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
load(here::here("00_preprocessed_data",
                "nested_df_original.RData"))

# para facilitar a manipulacao dos dados, retiramos as colunas 
# desnecessarias e tambem os experimentos que nao possuem nenhum taxa
list_traits <- data_number %>% 
  select(-"roof_treatment", -"abundance", -"measures", -"obs") %>%
  filter(ID != "MD24" & ID != "MD25" & ID != "MD34" & ID != "MD53" &
           ID != "MD69")

# Loop for para dar join entre list e traits
# depois percorrer o dataframe aninhado 
# e contar o numero de linhas em cada dataframe para conferencia
# aqui, os numeros tem que ser IGUAIS, se não for, tem algum erro no processamento
# anterior. Conferir no respectivo MD no script 00_preprocessing_data.R
for (i in 1:nrow(list_traits)) {
  # inner_join to combine with all names in both dataset
  list_traits$trait_list[[i]] <- inner_join(list_traits$list[[i]],
                                 list_traits$traits[[i]],
                                by = "Morfospecies_name")
  list_traits$nrow_list[[i]] <- nrow(list_traits$list[[i]])
  list_traits$nrow_traits[[i]] <- nrow(list_traits$traits[[i]])
  list_traits$nrow_trait_list[[i]] <- nrow(list_traits$trait_list[[i]])
  }

View(list_traits)

# aqui geramos a lista para preenchimento dos traits no excel. Para isso, eh so
# definir no vetor 'mds_to_fill' quais MDs precisam preencher os traits
mds_to_fill <- c("MD72", "MD73", 
                 "MD74", "MD75", "MD76",
                 "MD77", "MD78", "MD79",
                 "MD80", "MD81", "MD82")

list_traits_unnest <- list_traits %>% 
       filter(ID %in% mds_to_fill) %>%
       select("ID","researcher","locality","trait_list") %>%
       unnest(cols="trait_list")

# Salva no repositorio local para preencher manualmente no excel
write.csv2(list_traits_unnest, "need_revision.csv")
