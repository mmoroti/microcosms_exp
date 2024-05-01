# Mon Apr 29 09:56:19 2024 ------------------------------
library(tidyverse)
# aqui vamos preparar uma lista de especies com os traits para revisao
# provavelmente algumas spp irao sair pois sao terrestres e 
# nao devem contar para o experimento de microcosmos. 
load("dados_microcosmos/nested_df.RData")
View(data)
#View(data)
# para facilitar a manipulacao dos dados, retiramos as colunas 
# desnecessarias e tambem os experimentos que nao tinha invertebrados
# md19 removido pois ainda nao foram adicionados os dados
list_traits <- data %>% 
  select(-"roof_treatment", -"abundance", -"measures", -"obs") %>%
  filter(ID != "MD24" & ID != "MD25" & ID != "MD34" & ID != "MD19")

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

View(list_traits %>% 
       select("ID","researcher","locality","trait_list") %>%
       unnest(cols="trait_list"))
names(list_traits)

for (i in 1:nrow(list_traits)) {
  print(list_traits$ID[[i]])
  glimpse(list_traits$traits[[i]])
}
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
