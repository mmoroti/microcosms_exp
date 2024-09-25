# Mon Apr 15 18:45:16 2024 ------------------------------
# packages
library(tidyverse) # data science handling
library(readxl) # read .xlsx
library(renv) # versioning R and packages
library(glue) # to create acronyms

# functions
# create a key column to join between datasets
column_id <- function(data, acronym){

  for (i in 1:nrow(data)) {
    # cria o acronimo desejado  
    data$ID[i] <- glue({acronym}, i)
  }
  # coloca como primeira coluna
  data <- data %>%
    relocate(ID, .before = 1)
  
  return(data)
}

# Set directory 
local_directory <- "G:/Meu Drive/Microcosmos/dados_microcosmos"

# Data dictiorary ----
dict_data <- read_xlsx(
  file.path(local_directory,
       "data_dictionary.xlsx"),
  "measures_decomposition_geograph")#[-29,]

dict_names <- dict_data %>%
  select(new_name, old_name) %>%
  deframe()

# transform variables
var_char <- dict_data %>%
  filter(type == "character") %>%
  pull(new_name)

var_numeric <- dict_data %>%
  filter(type == "numeric") %>%
  pull(new_name)

# cotton-strip columns vector
cols_to_convert_g_to_mg <- pull(dict_data[3:8,1])

# As pastas nesse diretorio correspondem aos autores e as localidades
# onde foram executadas os microcosmos. # Existem 4 planilhas dentro de 
# cada .xlsx. Alguns autores possuem dados temporais de loggers,
# e alguns também fizeram dois tratamentos a mais
# com telhado e sem telhado. Por isso, uma coluna "with_roof" foi criada
# para designar o tratamento aplicado. 
# 1 = present or with_roof
# 0 = ausent or without_roof
# NA = non treatment apply

# another treatment is heigth_treatment, where
# 1 = 1,5 m (low)
# 2 = 15 m (mid)
# 3 = >20 (high)
# NA = non treatment apply

#--- MD1 & MD2 & MD3 --- Boukal_Czech ----
# (with roof) 
boukal_roofs_fa <- read_xlsx(
  file.path(
    local_directory,
    "Boukal_Czech",
    "Boukal_Site.2 Hluboka_roofs.xlsx"),
  "fauna_abundance")

boukal_roofs_list <- read_xlsx(
  file.path(
    local_directory,
    "Boukal_Czech",
    "Boukal_Site.2 Hluboka_roofs.xlsx"),
  "Fauna_morphospecies_list")

boukal_roofs_traits <- read_xlsx(
  file.path(
    local_directory,
    "Boukal_Czech",
    "Boukal_Site.2 Hluboka_roofs.xlsx"),
  "Fauna_traits")

boukal_roofs_measures <- read_xlsx(
  file.path(
    local_directory,
    "Boukal_Czech",
    "Boukal_Site.2 Hluboka_roofs.xlsx"),
  "measures_decomposition_geograph")

# abundace with NA (missing data) needs to be fill with 0
boukal_roofs_fa[is.na(boukal_roofs_fa)] <- 0
glimpse(boukal_roofs_fa)

# information in column removed 
# most or all individuals belong to subfamily Tanypodinae
boukal_roofs_list <- boukal_roofs_list %>%
  select(-"...7")
glimpse(boukal_roofs_list)

glimpse(boukal_roofs_traits)

# rename variables with data dictionary
boukal_roofs_measures <- boukal_roofs_measures %>%
  mutate(Remaining_water_volume = NA) %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA")))

glimpse(boukal_roofs_measures)

# boukal_czech_roof
boukal_czech_roof <- tibble(
  researcher = "Boukal",
  locality = "Hluboka_Czech", 
  roof_treatment = 1,
  abundance = list(tibble(boukal_roofs_fa)),
  list = list(tibble(boukal_roofs_list)),
  traits=list(tibble(boukal_roofs_traits)),
  measures=list(tibble(boukal_roofs_measures))
  )

#View(boukal_czech_roof)

#--- Boukal_Czech (without roof)
boukal_nonroofs_fa <- read_xlsx(
  file.path(
    local_directory,
    "Boukal_Czech",
    "Boukal_Site.2 Hluboka_without-roofs.xlsx"),
  "fauna_abundance")

boukal_nonroofs_list <- read_xlsx(
  file.path(
    local_directory,
    "Boukal_Czech",
    "Boukal_Site.2 Hluboka_without-roofs.xlsx"),
  "Fauna_morphospecies_list")

boukal_nonroofs_traits <- read_xlsx(
  file.path(
    local_directory,
    "Boukal_Czech",
    "Boukal_Site.2 Hluboka_without-roofs.xlsx"),
  "Fauna_traits")

boukal_nonroofs_measures <- read_xlsx(
  file.path(
    local_directory,
    "Boukal_Czech",
    "Boukal_Site.2 Hluboka_without-roofs.xlsx"),
  "measures_decomposition_geograph")

# abundace with NA (missing data) needs to be fill with 0
boukal_nonroofs_fa[is.na(boukal_nonroofs_fa)] <- 0
glimpse(boukal_nonroofs_fa)

# information in column removed 
# most or all individuals belong to subfamily Tanypodinae
boukal_nonroofs_list <- boukal_nonroofs_list %>%
  select(-"...7")
glimpse(boukal_nonroofs_list)

glimpse(boukal_nonroofs_traits)

# rename variables with data dictionary
boukal_nonroofs_measures <- boukal_nonroofs_measures %>%
  mutate(Remaining_water_volume = NA) %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA")))

glimpse(boukal_roofs_measures)

# NOTE: se eu colocar sem arg 'list' ele replica o dataset aninhado
boukal_czech_nonroof <- tibble(
  researcher = "Boukal",
  locality = "Hluboka_Czech", 
  roof_treatment = 0,
  abundance = list(tibble(boukal_nonroofs_fa)),
  list = list(tibble(boukal_nonroofs_list)),
  traits=list(tibble(boukal_nonroofs_traits)),
  measures=list(tibble(boukal_nonroofs_measures)))

###---- Boukal_Czech (TODO: third treatment?)
boukal_plesnelake_fa <- read_xlsx(
  file.path(
    local_directory,
    "Boukal_Czech",
    "Boukal_Site.2 PlesneLake.xlsx"),
  "fauna_abundance")

boukal_plesnelake_list <- read_xlsx(
  file.path(
    local_directory,
    "Boukal_Czech",
    "Boukal_Site.2 PlesneLake.xlsx"),
  "Fauna_morphospecies_list")

boukal_plesnelake_traits <- read_xlsx(
  file.path(
    local_directory,
    "Boukal_Czech",
    "Boukal_Site.2 PlesneLake.xlsx"),
  "Fauna_traits")

boukal_plesnelake_measures <- read_xlsx(
  file.path(
    local_directory,
    "Boukal_Czech",
    "Boukal_Site.2 PlesneLake.xlsx"),
  "measures_decomposition_geograph")

# abundance
# only two species
boukal_plesnelake_fa <- boukal_plesnelake_fa %>% 
    select(-Morphospecies.3, -Morphospecies.4)

boukal_plesnelake_fa[is.na(boukal_plesnelake_fa)] <- 0

# list
# only two species
boukal_plesnelake_list <- boukal_plesnelake_list[1:2,]

boukal_plesnelake_traits <- boukal_plesnelake_traits %>%
  filter(Morfospecies_name != "Morphospecies.3" &
         Morfospecies_name != "Morphospecies.4")

# rename variables with data dictionary
# gambiarra para renomear as colunas
boukal_plesnelake_measures <- 
  boukal_plesnelake_measures %>%
  mutate(Remaining_water_volume = NA) %>%
  rename("dissolved_O2" = "dissolved_O2 (%)") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA")))

glimpse(boukal_plesnelake_measures)

boukal_czech_plesnelake <- tibble(
  researcher = "Boukal",
  locality = "PlesneLake_Czech", 
  roof_treatment = NA,
  abundance = list(tibble(boukal_plesnelake_fa)),
  list = list(tibble(boukal_plesnelake_list)),
  traits=list(tibble(boukal_plesnelake_traits)),
  measures=list(tibble(boukal_plesnelake_measures)))


# save .RData from Boukal
save(boukal_czech_roof,
        boukal_czech_nonroof,
        boukal_czech_plesnelake,
        file =   file.path(local_directory,
                    "Boukal_Czech",
                    "Boukal_Czech.RData")) 

#load(file.path("dados_microcosmos",
#          "Boukal_Czech",
#          "Boukal_Czech.RData"))


#--- MD4 & MD62 --- Caliman_Natal_BR ----
caliman_fa <- read_xlsx(
  file.path(local_directory,
    "Caliman_Natal_BR",
    "Adriano.Caliman_Natal,Restinga,Atlantic Forest_VOLUME.xlsx"),
  "fauna_abundance")

caliman_list <- read_xlsx(
  file.path(local_directory,
    "Caliman_Natal_BR",
    "Adriano.Caliman_Natal,Restinga,Atlantic Forest_VOLUME.xlsx"),
  "Fauna_morphospecies_list")

caliman_traits <- read_xlsx(
  file.path(local_directory,
    "Caliman_Natal_BR",
    "Adriano.Caliman_Natal,Restinga,Atlantic Forest_VOLUME.xlsx"),
  "Fauna_traits")

caliman_measures <- read_xlsx(
  file.path(local_directory,
    "Caliman_Natal_BR",
    "Adriano.Caliman_Natal,Restinga,Atlantic Forest_VOLUME.xlsx"),
  "measures_decomposition_geograph")

# abundance
# filter rows with na's
# separing roof and nonroof
# exclude spp ausences in dataframes
caliman_nonroof_fa <- caliman_fa %>% 
  filter(Treatment == "Natural forest" | Treatment == "Managed forest") %>%
  filter(Replicate != "pot.9" | Treatment != "Natural forest") %>%
  mutate(across(c(
    "Aedes sp.", "Xenelmis sp.", "M. fuscatus", "Lymnaeidae family"),
    as.numeric)) %>%
  select(-"Lymnaeidae family") 

caliman_roof_fa <- caliman_fa %>% 
  filter(Treatment != "Natural forest" & Treatment != "Managed forest") %>%
  filter(Replicate != "pot.1" & Replicate != "pot.3" & Replicate != "pot.6" &
           Replicate != "pot.8" | 
           Treatment != "Managed forest (allochthonous detritus)") %>%
  mutate(across(c(
    "Aedes sp.", "Xenelmis sp.", "M. fuscatus", "Lymnaeidae family"),
    as.numeric)) %>%
  select(-"M. fuscatus") %>%
  mutate(Treatment = str_remove(Treatment, "\\s*\\(allochthonous detritus\\)"))


#colSums(caliman_roof_fa[,-c(1:2)])
#colSums(caliman_nonroof_fa[,-c(1:2)])

# list
caliman_nonroof_list <- caliman_list %>%
  filter(Morfospecies_name != "Lymnaeidae family")

caliman_roof_list <- caliman_list %>%
  filter(Morfospecies_name != "M. fuscatus")

# traits
caliman_nonroof_traits <- caliman_traits %>%
  filter(Morfospecies_name != "Lymnaeidae family")

caliman_roof_traits <- caliman_traits %>%
  filter(Morfospecies_name != "M. fuscatus")

# measures
# rename variables with data dictionary
# gambiarra para renomear as colunas
caliman_nonroof_measures <- caliman_measures %>%
  filter(Treatment == "Natural forest" | Treatment == "Managed forest") %>%
  filter(Replicate != "pot.9" | Treatment != "Natural forest") %>%
  rename("Elevation (m a.s.l.)" = "Elevation (m.s.l.)") %>%
  rename("Remaining_water_volume" = "final water volume (ml)") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA"))) %>%
  mutate(across(all_of(cols_to_convert_g_to_mg), ~ .x * 1000))

caliman_roof_measures <- caliman_measures %>%
  filter(Treatment != "Natural forest" & Treatment != "Managed forest") %>%
  filter(Replicate != "pot.1" & Replicate != "pot.3" & Replicate != "pot.6" &
           Replicate != "pot.8" | 
           Treatment != "Managed forest (allochthonous detritus)") %>%
  rename("Elevation (m a.s.l.)" = "Elevation (m.s.l.)") %>%
  rename("Remaining_water_volume" = "final water volume (ml)") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA"))) %>%
  mutate(across(all_of(cols_to_convert_g_to_mg), ~ .x * 1000))

caliman_roof_natal_br <- tibble(
  researcher = "Caliman",
  locality = "Natal, Brazil", 
  roof_treatment = 1,
  abundance = list(tibble(caliman_roof_fa)),
  list = list(tibble(caliman_roof_list)),
  traits=list(tibble(caliman_roof_traits)),
  measures=list(tibble(caliman_roof_measures)),
  obs= NA)

caliman_nonroof_natal_br <- tibble(
  researcher = "Caliman",
  locality = "Natal, Brazil", 
  roof_treatment = 0,
  abundance = list(tibble(caliman_nonroof_fa)),
  list = list(tibble(caliman_nonroof_list)),
  traits=list(tibble(caliman_nonroof_traits)),
  measures=list(tibble(caliman_nonroof_measures)),
  obs= NA)

# save .RData from Boukal
save(caliman_roof_natal_br,
     caliman_nonroof_natal_br,
     file = file.path(local_directory,
                 "Caliman_Natal_BR",
                 "Caliman_Natal_BR.RData")) 

#--- MD47 & MD50 --- Campos_do_Jordao_e_Sta_Virginia ----
# Sta virginia
# dados de sta virginia passaram por reclassificacao no dia 20/05, 
# duvidas consultar data_log dos dados ou Matheus Moroti/Gustavo Romero
romero_br <- file.path(local_directory,
                   "Campos_do_Jordao_e_Sta_Virginia")

romero_stavirginia_fa <- read_xlsx(
  file.path(
    romero_br,
    "Izadora_Nardi_Daiane_Montoia - Núcleo_Santa_Virginia.xlsx"),
  "fauna_abundance")

romero_stavirginia_list <- read_xlsx(
  file.path(
    romero_br,
    "Izadora_Nardi_Daiane_Montoia - Núcleo_Santa_Virginia.xlsx"),
  "Fauna_morphospecies_list")

romero_stavirginia_traits <- read_xlsx(
  file.path(
    romero_br,
    "Izadora_Nardi_Daiane_Montoia - Núcleo_Santa_Virginia.xlsx"),
  "Fauna_traits")

romero_stavirginia_measures <- read_xlsx(
  file.path(
    romero_br,
    "Izadora_Nardi_Daiane_Montoia - Núcleo_Santa_Virginia.xlsx"),
  "measures_decomposition_geograph")

head(romero_stavirginia_fa)

head(romero_stavirginia_list)

head(romero_stavirginia_traits)

romero_stavirginia_measures <- 
  romero_stavirginia_measures %>%
  mutate(Remaining_water_volume = "Final water volume (ml)") %>%
  rename(all_of(dict_names)) %>%
  mutate(tree_dbh = tree_dbh / pi) %>% 
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA"))) %>%
  mutate(across(all_of(cols_to_convert_g_to_mg), ~ .x * 1000))

romero_stavirginia_data <- tibble(
  researcher = "Romero",
  locality = "StaVirginia_Brazil", 
  roof_treatment = NA,
  abundance = list(tibble(romero_stavirginia_fa)),
  list = list(tibble(romero_stavirginia_list)),
  traits=list(tibble(romero_stavirginia_traits)),
  measures=list(tibble(romero_stavirginia_measures)))

# save .RData from romero
save(romero_stavirginia_data,
     file = file.path(romero_br,
                 "romero_stavirginia.RData")) 

# Campos do Jordao
romero_campos_fa <- read_xlsx(
  file.path(
    romero_br,
    "Felipe_Rezende_Daiane_Montoia - Campos_do_Jordao_community_dataset.xlsx"),
  "fauna_abundance")

romero_campos_list <- read_xlsx(
  file.path(
    romero_br,
    "Felipe_Rezende_Daiane_Montoia - Campos_do_Jordao_community_dataset.xlsx"),
  "Fauna_morphospecies_list")

romero_campos_traits <- read_xlsx(
  file.path(
    romero_br,
    "Felipe_Rezende_Daiane_Montoia - Campos_do_Jordao_community_dataset.xlsx"),
  "Fauna_traits")

romero_campos_measures <- read_xlsx(
  file.path(
    romero_br,
    "Felipe_Rezende_Daiane_Montoia - Campos_do_Jordao_community_dataset.xlsx"),
  "measures_decomposition_geograph")

head(romero_campos_fa)

# removacao validada com o Felipe e a Izadora
# erros de classificacao taxonomica e ausentes na abundancia
romero_campos_list <- romero_campos_list %>%
  filter(Morfospecies_name != "Diptera_sp4" &
        Morfospecies_name != "Diptera_sp13" &
        Morfospecies_name != "Chironomidae_sp5")
nrow(romero_campos_list)
nrow(romero_campos_traits)

# rename variables with data dictionary
romero_campos_measures <- 
  romero_campos_measures %>%
  rename("canopy openness" = "Canopy openness") %>%
  rename("Remaining_water_volume" = "Final water volume (ml)") %>%
  rename(all_of(dict_names)) %>%
  mutate(tree_dbh = tree_dbh / pi) %>% 
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA"))) %>%
  mutate(across(all_of(cols_to_convert_g_to_mg), ~ .x * 1000))

names(romero_campos_measures)

romero_campos_data <- tibble(
  researcher = "Romero",
  locality = "CamposDoJordao_Brazil", 
  roof_treatment = NA,
  abundance = list(tibble(romero_campos_fa)),
  list = list(tibble(romero_campos_list)),
  traits=list(tibble(romero_campos_traits)),
  measures=list(tibble(romero_campos_measures)))

# save .RData from romero
save(romero_campos_data,
     file = file.path(romero_br,
                 "romero_camposdojordao.RData"))

#--- MD5 --- Cardinale_USA ----
cardinale_usa <- file.path(local_directory,
                      "Cardinale_USA")

cardinale_fa <- read_xlsx(
  file.path(
    cardinale_usa,
    "Cardinale_modified_microcosm_data.xlsx"),
  "fauna_abundance")

cardinale_list <- read_xlsx(
  file.path(
    cardinale_usa,
    "Cardinale_modified_microcosm_data.xlsx"),
  "Fauna_morphospecies_list")

cardinale_traits <- read_xlsx(
  file.path(
    cardinale_usa,
    "Cardinale_modified_microcosm_data.xlsx"),
  "Fauna_traits")

cardinale_measures <- read_xlsx(
  file.path(
    cardinale_usa,
    "Cardinale_modified_microcosm_data.xlsx"),
  "measures_decomposition_geograph")

# abundance
cardinale_fa <- cardinale_fa %>%
  mutate(Treatment = str_replace(Treatment,"Forest patch","Managed forest")) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0))) %>%
  filter(Replicate != "pot.4" | Treatment != "Natural forest") %>%
  filter(Replicate != "pot.7" | Treatment != "Natural forest") %>%
  filter(Replicate != "pot.1" | Treatment != "Managed forest") %>%
  filter(Replicate != "pot.2" | Treatment != "Managed forest") %>%
  filter(Replicate != "pot.5" | Treatment != "Managed forest") %>%
  filter(Replicate != "pot.7" | Treatment != "Managed forest") 

# list
#cardinale_list

# traits
#cardinale_traits

# measures
#cardinale_measures
# pattern change in forest patch to managed forest
# filter replicates excluded
# rename variables with data dictionary
cardinale_measures <- 
  cardinale_measures %>%
  mutate(Treatment = str_replace(Treatment,"Forest Patch","Managed forest")) %>%
  mutate("Remaining_water_volume" = "Final water volume (ml)") %>%
  filter(Replicate != "pot.4" | Treatment != "Natural forest") %>%
  filter(Replicate != "pot.7" | Treatment != "Natural forest") %>%
  filter(Replicate != "pot.1" | Treatment != "Managed forest") %>%
  filter(Replicate != "pot.2" | Treatment != "Managed forest") %>%
  filter(Replicate != "pot.5" | Treatment != "Managed forest") %>%
  filter(Replicate != "pot.7" | Treatment != "Managed forest") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA")))

cardinale_usa_data <- tibble(
  researcher = "Cardinale",
  locality = "Pennsylvania_USA", 
  roof_treatment = NA,
  abundance = list(tibble(cardinale_fa)),
  list = list(tibble(cardinale_list)),
  traits=list(tibble(cardinale_traits)),
  measures=list(tibble(cardinale_measures)))

# save .RData from Boukal
save(cardinale_usa_data,
     file = file.path(cardinale_usa,
                 "Cardinale_USA.RData")) 

#--- MD51 --- Cardoso_Romero ----
romero_cardoso <- file.path(local_directory,
                  "Cardoso_Romero")

romero_cardoso_fa <- read_xlsx(
  file.path(
    romero_cardoso,
    "Daiane_Montoia_Felipe_Rezende_Cardoso_Island-Cananeia_community_dataset.xlsx"),
  "fauna_abundance")

romero_cardoso_list <- read_xlsx(
  file.path(
    romero_cardoso,
    "Daiane_Montoia_Felipe_Rezende_Cardoso_Island-Cananeia_community_dataset.xlsx"),
  "Fauna_morphospecies_list")

romero_cardoso_traits <- read_xlsx(
  file.path(
    romero_cardoso,
    "Daiane_Montoia_Felipe_Rezende_Cardoso_Island-Cananeia_community_dataset.xlsx"),
  "Fauna_traits")[1:11,] # retirando a leitura de um ponto aleatorio na planilha

romero_cardoso_measures <- read_xlsx(
  file.path(
    romero_cardoso,
    "Daiane_Montoia_Felipe_Rezende_Cardoso_Island-Cananeia_community_dataset.xlsx"),
  "measures_decomposition_geograph")

#head(romero_cardoso_fa)

#View(romero_cardoso_list)

#View(romero_cardoso_traits)

# rename variables with data dictionary
romero_cardoso_measures <- 
  romero_cardoso_measures %>%
  rename("canopy openness" = "canopy openness_Daiane") %>%
  mutate("Remaining_water_volume" = "Final water volume (ml)") %>%
  rename("microcosm position (N, S, E, W)" = "microcosm position (N. S. E. W)") %>%
  rename(all_of(dict_names)) %>%
  mutate(tree_dbh = tree_dbh / pi) %>% 
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA"))) %>%
  mutate(across(all_of(cols_to_convert_g_to_mg), ~ .x * 1000))

romero_cardoso_data <- tibble(
  researcher = "Romero",
  locality = "IlhaDoCardoso_Brazil", 
  roof_treatment = NA,
  abundance = list(tibble(romero_cardoso_fa)),
  list = list(tibble(romero_cardoso_list)),
  traits=list(tibble(romero_cardoso_traits)),
  measures=list(tibble(romero_cardoso_measures)))

#--- MD6 --- Collyer_Japan ----
collyer_japan <- file.path(local_directory,
                      "Collyer_Japan")

collyer_fa <- read_xlsx(
  file.path(
    collyer_japan,
    "Giovanna Collyer & Takehito Yoshida_Japan.xlsx"),
  "fauna_abundance")

collyer_list <- read_xlsx(
  file.path(
    collyer_japan,
    "Giovanna Collyer & Takehito Yoshida_Japan.xlsx"),
  "Fauna_morphospecies_list")

collyer_traits <- read_xlsx(
  file.path(
    collyer_japan,
    "Giovanna Collyer & Takehito Yoshida_Japan.xlsx"),
  "Fauna_traits")

collyer_measures <- read_xlsx(
  file.path(
    collyer_japan,
    "Giovanna Collyer & Takehito Yoshida_Japan.xlsx"),
  "measures_decomposition_geograph")

# abundance
#collyer_fa

# list
#collyer_list

# traits
#collyer_traits

# measures
#collyer_measures
# rename variables with data dictionary
collyer_measures <- 
  collyer_measures %>%
  rename("Remaining_water_volume" = "Final water volume (ml)") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA"))) 

collyer_japan_data <- tibble(
  researcher = "Collyer",
  locality = "Tokyo_Japan", 
  roof_treatment = NA,
  abundance = list(tibble(collyer_fa)),
  list = list(tibble(collyer_list)),
  traits=list(tibble(collyer_traits)),
  measures=list(tibble(collyer_measures)))

save(collyer_japan_data,
     #boukal_czech_nonroof,
     file = file.path(collyer_japan,
                 "Collyer_Japan.RData")) 

#--- MD7 & MD8 --- Cornelissen_BR ----
cornelissen_br <- file.path(local_directory,
                      "Cornelissen_BR",
                      "dados_definitivos")

#--- Cornelissen_BR with roof
cornelissen_roof_fa <- read_xlsx(
  file.path(
    cornelissen_br,
    "Sao Bartolomeu_Site.2_MICROCOSMS (with_roof).xlsx"),
  "fauna_abundance")

cornelissen_roof_list <- read_xlsx(
  file.path(
    cornelissen_br,
    "Sao Bartolomeu_Site.2_MICROCOSMS (with_roof).xlsx"),
  "Fauna_morphospecies_list")

cornelissen_roof_traits <- read_xlsx(
  file.path(
    cornelissen_br,
    "Sao Bartolomeu_Site.2_MICROCOSMS (with_roof).xlsx"),
  "Fauna_traits")

cornelissen_roof_measures <- read_xlsx(
  file.path(
    cornelissen_br,
    "Sao Bartolomeu_Site.2_MICROCOSMS (with_roof).xlsx"),
  "measures_decomposition_geograph")

#abundance 
cornelissen_roof_fa <- cornelissen_roof_fa %>% 
  mutate(Treatment = str_replace_all(Treatment,
                                     c(
                                       "Eucalyptus_forest" = "Managed forest",
                                       "Natural_forest" = "Natural forest"))
         )

#list
# substituir o Undetermined por NA
cornelissen_roof_list <- cornelissen_roof_list %>%
  mutate_all(~ifelse(. == "Undetermined", NA, .))

# traits
# substituir o undetermined por NA
# remover colunas desnecessarias (informacao no data_log.txt)
cornelissen_roof_traits <- cornelissen_roof_traits %>% 
  mutate_all(~ifelse(.=="undetermined", NA, .)) %>%
  select(-"...7", -"...8", -"...9", 
         -"...10", -"...11", -"...12")

# measures
# substituir o undetermined por NA
# o df nao tinha as colunas natural tree hole
# adicionar para renomear e manter o padrao
cornelissen_roof_measures <- cornelissen_roof_measures %>% 
  mutate_all(~ifelse(.=="undetermined", NA, .)) %>%
  mutate("Natural tree hole.1" = NA,
         "Natural tree hole.2" = NA) %>%
  rename("Remaining_water_volume" = "Final Volum (mL)") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA"))) %>%
  mutate(treatment = str_replace_all(
    treatment,c(
      "Eucalyptus_forest" = "Managed forest",
      "Natural_forest" = "Natural forest")))

# once cottomstrip is losing, multiplicate the values by two
# duplicate values in these cells (check data_log explanation)
cornelissen_roof_measures[9,"outside_after_g"] <- 96.4

cornelissen_roof_BR_data <- tibble(
  researcher = "Cornelissen",
  locality = "MinasGerais_BR", 
  roof_treatment = 1,
  abundance = list(tibble(cornelissen_roof_fa)),
  list = list(tibble(cornelissen_roof_list)),
  traits=list(tibble(cornelissen_roof_traits)),
  measures=list(tibble(cornelissen_roof_measures)))

#--- Cornelissen_BR without roof
cornelissen_nonroof_fa <- read_xlsx(
  file.path(
    cornelissen_br,
    "Sao Bartolomeu_Site.2_MICROCOSMS (Without_roof)2.0.xlsx"),
  "fauna_abundance")

cornelissen_nonroof_list <- read_xlsx(
  file.path(
    cornelissen_br,
    "Sao Bartolomeu_Site.2_MICROCOSMS (Without_roof)2.0.xlsx"),
  "Fauna_morphospecies_list")

cornelissen_nonroof_traits <- read_xlsx(
  file.path(
    cornelissen_br,
    "Sao Bartolomeu_Site.2_MICROCOSMS (Without_roof)2.0.xlsx"),
  "Fauna_traits")

cornelissen_nonroof_measures <- read_xlsx(
  file.path(
    cornelissen_br,
    "Sao Bartolomeu_Site.2_MICROCOSMS (Without_roof)2.0.xlsx"),
  "measures_decomposition_geograph")

#abundance 
cornelissen_nonroof_fa <- cornelissen_nonroof_fa %>% 
  mutate(Treatment = str_replace_all(Treatment,
                                     c(
                                       "Eucalyptus_forest" = "Managed forest",
                                       "Natural_forest" = "Natural forest"))
  )

#list
# substituir o Not_identified por NA
cornelissen_nonroof_list <- cornelissen_nonroof_list %>%
  mutate_all(~ifelse(. == "Not_identified", NA, .))

# traits
# substituir o Undetermined por NA
# remover colunas desnecessarias (informacao no data_log.txt)
cornelissen_nonroof_traits <- cornelissen_nonroof_traits %>% 
  mutate_all(~ifelse(.=="Undetermined", NA, .)) %>%
  select(-"...7", -"...8", -"...9", 
         -"...10", -"...11", -"...12") %>%
  rename("total_length" = "average_length")

# measures
# substituir o undetermined por NA
# o df nao tinha as colunas natural tree hole
# adicionar para renomear e manter o padrao
cornelissen_nonroof_measures <- cornelissen_nonroof_measures %>% 
  mutate_all(~ifelse(.=="undetermined", NA, .)) %>%
  mutate("Natural tree hole.1" = NA,
         "Natural tree hole.2" = NA) %>%
  rename("Remaining_water_volume" = "Final Volum (mL)") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA"))) %>%
  mutate(treatment = str_replace_all(
    treatment,c(
      "Eucalyptus_forest" = "Managed forest",
      "Natural_forest" = "Natural forest")))

# duplicate values in these cells (check data_log explanation)
# once cottomstrip is losing, multiplicate the values by two
cornelissen_nonroof_measures[9,"coarse_after_g"] <- 164.2
cornelissen_nonroof_measures[9,"outside_after_g"] <- 90.4

cornelissen_nonroof_BR_data <- tibble(
  researcher = "Cornelissen",
  locality = "MinasGerais_BR", 
  roof_treatment = 0,
  abundance = list(tibble(cornelissen_nonroof_fa)),
  list = list(tibble(cornelissen_nonroof_list)),
  traits=list(tibble(cornelissen_nonroof_traits)),
  measures=list(tibble(cornelissen_nonroof_measures)))

save(cornelissen_roof_BR_data,
     cornelissen_nonroof_BR_data,
     file = file.path(local_directory,
                 "Cornelissen_BR",
                 "Cornelissen_BR.RData"))

#load(file.path("dados_microcosmos",
#          "Cornelissen_BR",
#          "Cornelissen_BR.RData"))

#--- MD9 & MD63 & MD64 & MD68 --- Cotriguacu_Romero ----
cotriguacu_br <- file.path(local_directory,
                       "Cotriguacu_Romero")

cotriguacu_fa <- read_xlsx(
  file.path(
    cotriguacu_br,
    "Romero.Amazon.xlsx"),
  "fauna_abundance")

cotriguacu_list <- read_xlsx(
  file.path(
    cotriguacu_br,
    "Romero.Amazon.xlsx"),
  "Fauna_morphospecies_list")

cotriguacu_traits <- read_xlsx(
  file.path(
    cotriguacu_br,
    "Romero.Amazon.xlsx"),
  "Fauna_traits")

cotriguacu_measures <- read_xlsx(
  file.path(
    cotriguacu_br,
    "Romero.Amazon.xlsx"),
  "measures_decomposition_geograph")

# filter species per heigth_treatment
# remove species sums 0 occurences in each treatment
# colSums(cotriguacu_fa_low[,-c(1:3)])
# MD9 tem que separar em com roof e sem roof;
# réplicas com roof são: 2, 5, 8, 11, 15,16, 19, 22, 25, 27, 28, 31
with_roof <-  c("pot.2", "pot.5", "pot.8", "pot.11", "pot.15",
                "pot.16", "pot.19", "pot.22", "pot.25", "pot.27",
                "pot.28", "pot.31")

filter_low_roof <- c(
  "Morphospecies.5", "Morphospecies.7", "Morphospecies.9",
  "Morphospecies.10", "Morphospecies.11", "Morphospecies.12",
  "Morphospecies.13", "Morphospecies.14", "Morphospecies.15",
  "Morphospecies.16", "Morphospecies.20", "Morphospecies.21",
  "Morphospecies.22", "Morphospecies.27", "Morphospecies.28",
  "Morphospecies.29", "Morphospecies.30", "Morphospecies.32",
  "Morphospecies.34","Morphospecies.35", "Morphospecies.36", 
  "Morphospecies.37", "Morphospecies.38", "Morphospecies.39", 
  "Morphospecies.40", "Morphospecies.41", "Morphospecies.42"
)

filter_low_nonroof <- c(
  "Morphospecies.3", "Morphospecies.18", "Morphospecies.24",
  "Morphospecies.25", "Morphospecies.31", "Morphospecies.33",
  "Morphospecies.35", "Morphospecies.36",
  "Morphospecies.37", "Morphospecies.38", "Morphospecies.39", 
  "Morphospecies.40", "Morphospecies.41", "Morphospecies.42"
)

# colSums(cotriguacu_fa_mid[,-c(1:3)])
filter_mid <- c("Morphospecies.3" ,"Morphospecies.5",
                "Morphospecies.9" ,"Morphospecies.10",
                "Morphospecies.11","Morphospecies.13",
                "Morphospecies.14","Morphospecies.15",
                "Morphospecies.19","Morphospecies.20",
                "Morphospecies.23","Morphospecies.24",
                "Morphospecies.27","Morphospecies.28",
                "Morphospecies.29","Morphospecies.30",
                "Morphospecies.31","Morphospecies.32", 
                "Morphospecies.33","Morphospecies.34",
                "Morphospecies.38","Morphospecies.39")
# colSums(cotriguacu_fa_high[,-c(1:3)])
filter_high <- c("Morphospecies.3",  "Morphospecies.6",
                 "Morphospecies.7",  "Morphospecies.9",
                 "Morphospecies.10", "Morphospecies.11",
                 "Morphospecies.13", "Morphospecies.14",
                 "Morphospecies.18", "Morphospecies.19",
                 "Morphospecies.24", "Morphospecies.26",
                 "Morphospecies.27", "Morphospecies.30",
                 "Morphospecies.31", "Morphospecies.32",
                 "Morphospecies.33", "Morphospecies.34", 
                 "Morphospecies.35", "Morphospecies.37",
                 "Morphospecies.40", "Morphospecies.41",
                 "Morphospecies.42")

# abundance
# serao separados por tres classes de altura, 1.5, 15, e acima de >20
#head(cotriguacu_fa)
cotriguacu_fa_low_roof <- cotriguacu_fa %>%
  filter(height == "1.5") %>%
  filter(Replicate %in% with_roof) %>%
  select(!all_of(filter_low_roof)) %>%
  select(-height)

cotriguacu_fa_low_nonroof <- cotriguacu_fa %>%
  filter(height == "1.5") %>%
  filter(!(Replicate %in% with_roof)) %>%
  select(!all_of(filter_low_nonroof)) %>%
  select(-height)

cotriguacu_fa_mid <- cotriguacu_fa %>%
  filter(height == "15") %>%
  select(!all_of(filter_mid)) %>%
  select(-height)

cotriguacu_fa_high <- cotriguacu_fa %>%
  filter(height != "15" & height != "1.5") %>%
  select(!all_of(filter_high)) %>%
  select(-height)

# list
cotriguacu_list_low_roof <- cotriguacu_list %>% 
  mutate_all(~na_if(., "-")) %>%
  filter(!Morfospecies_name %in% filter_low_roof)

cotriguacu_list_low_nonroof <- cotriguacu_list %>% 
  mutate_all(~na_if(., "-")) %>%
  filter(!Morfospecies_name %in% filter_low_nonroof)

cotriguacu_list_mid <- cotriguacu_list %>% 
  mutate_all(~na_if(., "-")) %>%
  filter(!Morfospecies_name %in% filter_mid)

cotriguacu_list_high <- cotriguacu_list %>% 
  mutate_all(~na_if(., "-")) %>%
  filter(!Morfospecies_name %in% filter_high)

# traits
cotriguacu_traits_low_roof <- cotriguacu_traits %>%
  filter(!Morfospecies_name %in% filter_low_roof)

cotriguacu_traits_low_nonroof <- cotriguacu_traits %>%
  filter(!Morfospecies_name %in% filter_low_nonroof) 

cotriguacu_traits_mid <- cotriguacu_traits %>%
  filter(!Morfospecies_name %in% filter_mid)

cotriguacu_traits_high <- cotriguacu_traits %>%
  filter(!Morfospecies_name %in% filter_high)

# measures
cotriguacu_measures_low_roof <- cotriguacu_measures %>%
  filter(height == "1.5") %>%
  filter(Replicate %in% with_roof) %>%
  rename("Remaining_water_volume" = "Final water volume (ml)") %>%
  mutate(Remaining_water_volume = str_replace_all(
    Remaining_water_volume,c(
      "Full" = "800"))) %>%
  rename(all_of(dict_names)) %>%
  mutate(tree_dbh = tree_dbh / pi) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA"))) %>%
  mutate(across(all_of(cols_to_convert_g_to_mg), ~ .x * 1000))

cotriguacu_measures_low_nonroof <- cotriguacu_measures %>%
  filter(height == "1.5") %>%
  filter(!(Replicate %in% with_roof)) %>%
  rename("Remaining_water_volume" = "Final water volume (ml)") %>%
  mutate(Remaining_water_volume = str_replace_all(
    Remaining_water_volume,c(
      "Full" = "800"))) %>%
  rename(all_of(dict_names)) %>%
  mutate(tree_dbh = tree_dbh / pi) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA"))) %>%
  mutate(across(all_of(cols_to_convert_g_to_mg), ~ .x * 1000))

cotriguacu_measures_mid <- cotriguacu_measures %>%
  filter(height == "15") %>%
  rename("Remaining_water_volume" = "Final water volume (ml)") %>%
  mutate(Remaining_water_volume = str_replace_all(
    Remaining_water_volume,c(
      "Full" = "800"))) %>%
  rename(all_of(dict_names)) %>%
  mutate(tree_dbh = tree_dbh / pi) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA"))) %>%
  mutate(across(all_of(cols_to_convert_g_to_mg), ~ .x * 1000))

cotriguacu_measures_high <- cotriguacu_measures %>%
  filter(height != "15" & height != "1.5") %>%
  rename("Remaining_water_volume" = "Final water volume (ml)") %>%
  mutate(Remaining_water_volume = str_replace_all(
    Remaining_water_volume,c(
      "Full" = "800"))) %>%
  rename(all_of(dict_names)) %>%
  mutate(tree_dbh = tree_dbh / pi) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA"))) %>%
  mutate(across(all_of(cols_to_convert_g_to_mg), ~ .x * 1000))

# data 
cotriguacu_romero_data_low_roof <- tibble(
  researcher = "Romero",
  locality = "Cotriguacu_Brazil", 
  roof_treatment = 1,
  heigth_treatment = 1,
  abundance = list(tibble(cotriguacu_fa_low_roof)),
  list = list(tibble(cotriguacu_list_low_roof)),
  traits=list(tibble(cotriguacu_traits_low_roof)),
  measures=list(tibble(cotriguacu_measures_low_roof)),
  obs= "")

cotriguacu_romero_data_low_nonroof <- tibble(
  researcher = "Romero",
  locality = "Cotriguacu_Brazil", 
  roof_treatment = 0,
  heigth_treatment = 1,
  abundance = list(tibble(cotriguacu_fa_low_nonroof)),
  list = list(tibble(cotriguacu_list_low_nonroof)),
  traits=list(tibble(cotriguacu_traits_low_nonroof)),
  measures=list(tibble(cotriguacu_measures_low_nonroof)),
  obs= "")

cotriguacu_romero_data_mid <- tibble(
  researcher = "Romero",
  locality = "Cotriguacu_Brazil", 
  roof_treatment = NA,
  heigth_treatment = 2,
  abundance = list(tibble(cotriguacu_fa_mid)),
  list = list(tibble(cotriguacu_list_mid)),
  traits=list(tibble(cotriguacu_traits_mid)),
  measures=list(tibble(cotriguacu_measures_mid)),
  obs= "")

cotriguacu_romero_data_high <- tibble(
  researcher = "Romero",
  locality = "Cotriguacu_Brazil", 
  roof_treatment = NA,
  heigth_treatment = 3,
  abundance = list(tibble(cotriguacu_fa_high)),
  list = list(tibble(cotriguacu_list_high)),
  traits=list(tibble(cotriguacu_traits_high)),
  measures=list(tibble(cotriguacu_measures_high)),
  obs= "")

save(cotriguacu_romero_data_low_roof,
     cotriguacu_romero_data_low_nonroof,
     cotriguacu_romero_data_mid,
     cotriguacu_romero_data_high,
     file = file.path(cotriguacu_br,
                      "Cotriguacu_romero.RData"))

#--- MD10 & MD11 --- Fabiola_Colombia ----
# os dados dos tratamentos com telhado e sem telhado estao na mesma
# planilha, por isso irei separar em duas linhas distintas no df aninhado
# para ficar comparavel com o que esta sendo feito

# acrônimos usados para indicar os tratamentos sao:
# BC, BR, PC, and PR were a personal ID that I used. 
# B=forest; P=plantation; C=without roof; R=roof
fabiola_colombia <- file.path(local_directory,
                      "Fabiola_Colombia",
                      "Fabiola_Site.Colombia.xlsx")

fabiola_fa <- read_xlsx(
  file.path(
    fabiola_colombia),
  "fauna_abundance")

fabiola_list <- read_xlsx(
  file.path(
    fabiola_colombia),
  "Fauna_morphospecies_list")

fabiola_traits <- read_xlsx(
  file.path(
    fabiola_colombia),
  "Fauna_traits")

fabiola_measures <- read_xlsx(
  file.path(
    fabiola_colombia),
  "measures_decomposition_geograph")

# abundance
# as spp daphinia.sp.1 e Wyeomyia.sp.1 nao foram amostradas 
# no tratamento com telhado, por isso foram retiradas da abundance
# isopoda e terrestre, por isso foi retirado
fabiola_roof_fa <- fabiola_fa %>%
  filter(str_detect(fabiola_fa$`ID. Own`,
                    "^BR|^PR")) %>%
  select(-"daphnia.sp.1", -"Wyeomyia.sp.1", 
         -"Isopoda.sp.1", -"Replicate...3") %>%
  rename(Replicate = "Replicate...2") %>%
  select(-"ID. Own")

#colSums(fabiola_roof_fa[,5:15])
#rowSums(fabiola_roof_fa[,5:15])

# as especies Eristalis.sp.1 Forcipomyia.sp.1 nao foram amostradas
# no tratamento sem telhado, por isso foram retiradas da list
# da traits 
# isopoda e terrestre, por isso foi retirado
fabiola_nonroof_fa <- fabiola_fa %>%
  filter(str_detect(fabiola_fa$`ID. Own`,
                    "^BC|^PC")) %>%
  select(-"Eristalis.sp.1", -"Forcipomyia.sp.1", 
         -"Isopoda.sp.1", -"Replicate...3") %>%
  rename(Replicate = "Replicate...2") %>%
  select(-"ID. Own")

#colSums(fabiola_nonroof_fa[,5:15])
#rowSums(fabiola_nonroof_fa[,5:15]) # alguns potes com zero

# list
fabiola_roof_list <- fabiola_list %>%
  filter(Morfospecies_name != "daphnia.sp.1" &
         Morfospecies_name != "Wyeomyia.sp.1" &
           Morfospecies_name != "Isopoda.sp.1") # spp terrestre)

fabiola_nonroof_list <- fabiola_list %>%
  filter(Morfospecies_name != "Eristalis.sp.1" &
           Morfospecies_name != "Forcipomyia.sp.1" &
           Morfospecies_name != "Isopoda.sp.1") # spp terrestre

# traits
# tem mais especies com traits do que tem em abundance e list
# tambem ha indicativos de spp terrestres, que nao serao consideradas
# no experimento de microcosmos 

# coluna habit retirada para ficar semelhante aos outros
# Gasteropoda retirada pois nao esta presente na lista e nem na abundace
fabiola_traits <- fabiola_traits %>%
  rename("habit" = "...7") %>%
  filter(habit != "terrestrial" & Morfospecies_name != "Gasteropoda") %>%
  mutate(Morfospecies_name = case_when(
    Morfospecies_name == "daphnia" ~ "daphnia.sp.1",
    Morfospecies_name == "Culex" ~ "Culex.sp.1",
    Morfospecies_name == "Ephydridae" ~ "Ephydridae.sp.1",
    Morfospecies_name == "Eristalis" ~ "Eristalis.sp.1",
    Morfospecies_name == "Forcipomyia" ~ "Forcipomyia.sp.1",
    Morfospecies_name == "Haemagogus" ~ "Haemagogus.sp.1",
    Morfospecies_name == "Orthocladiinae" ~ "Orthocladiinae.sp.1",
    Morfospecies_name == "Orthopodomyia" ~ "Orthopodomyia.sp.1",
    Morfospecies_name == "Pericoma" ~ "Pericoma.sp.1",
    Morfospecies_name == "Wyeomyia" ~ "Wyeomyia.sp.1",
    TRUE ~ Morfospecies_name)) %>%
  select(-habit)

fabiola_roof_traits <- fabiola_traits %>%
  filter(Morfospecies_name != "Wyeomyia.sp.1" & 
           Morfospecies_name != "daphnia.sp.1")

fabiola_nonroof_traits <- fabiola_traits %>%
  filter(Morfospecies_name != "Eristalis.sp.1" & 
           Morfospecies_name != "Forcipomyia.sp.1")

# conferindo
#list(fabiola_roof_list$Morfospecies_name) # ok
#names(fabiola_roof_fa) # ok
#list(fabiola_roof_traits$Morfospecies_name)# ok

# conferindo
#list(fabiola_nonroof_list$Morfospecies_name) # ok
#names(fabiola_nonroof_fa) # ok
#list(fabiola_nonroof_traits$Morfospecies_name)# ok

# measures
fabiola_roof_measures <- fabiola_measures %>%
  filter(str_detect(fabiola_fa$`ID. Own`, "^BR|^PR")) %>%
  rename("Remaining_water_volume" = "water.amount (ml)") %>%
  rename(all_of(dict_names)) %>%
  select(-`ID. Own`) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA")))

fabiola_nonroof_measures <- fabiola_measures %>%
  filter(str_detect(fabiola_measures$`ID. Own`, "^BC|^PC")) %>%
  rename("Remaining_water_volume" = "water.amount (ml)") %>%
  rename(all_of(dict_names)) %>%
  select(-`ID. Own`) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA")))

# data 
fabiola_roof_colombia_data <- tibble(
  researcher = "Fabiola",
  locality = "MontaneForest_Colombia", 
  roof_treatment = 1,
  abundance = list(tibble(fabiola_roof_fa)),
  list = list(tibble(fabiola_roof_list)),
  traits=list(tibble(fabiola_roof_traits)),
  measures=list(tibble(fabiola_roof_measures)))

fabiola_nonroof_colombia_data <- tibble(
  researcher = "Fabiola",
  locality = "MontaneForest_Colombia", 
  roof_treatment = 0,
  abundance = list(tibble(fabiola_nonroof_fa)),
  list = list(tibble(fabiola_nonroof_list)),
  traits=list(tibble(fabiola_nonroof_traits)),
  measures=list(tibble(fabiola_nonroof_measures)))

save(fabiola_roof_colombia_data,
     fabiola_nonroof_colombia_data,
     file = file.path(local_directory,
                 "Fabiola_Colombia",
                 "Fabiola_Colombia.RData"))
#--- MD12 & MD13 --- French_Guyana_Celine ----
# linhas 11-15 precisam ser deletadas, deletei direto no .xlsx
# Canopy data
celine_guyana <- file.path(local_directory,
                         "French_Guyana_Celine")

celine_canopy_fa <- read_xlsx(
  file.path(
    celine_guyana, "Celine Leroy_French Guiana_canopy.xlsx"),
  "fauna_abundance")

celine_canopy_list <- read_xlsx(
  file.path(
    celine_guyana, "Celine Leroy_French Guiana_canopy.xlsx"),
  "Fauna_morphospecies_list")

celine_canopy_traits <- read_xlsx(
  file.path(
    celine_guyana, "Celine Leroy_French Guiana_canopy.xlsx"),
  "Fauna_traits")

celine_canopy_measures <- read_xlsx(
  file.path(
    celine_guyana, "Celine Leroy_French Guiana_canopy.xlsx"),
  "measures_decomposition_geograph")

# abundance
celine_canopy_fa <- celine_canopy_fa %>%
  mutate(across(all_of(names(celine_canopy_fa[,3:5])), as.numeric))

# list
head(celine_canopy_list)

# traits
head(celine_canopy_traits)

# measures
celine_canopy_measures <- celine_canopy_measures %>%
  mutate("Remaining_water_volume" = NA) %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA"))) %>%
  mutate(across(all_of(cols_to_convert_g_to_mg), ~ .x * 1000))

# data 
celine_canopy_frenchguyana_data <- tibble(
  researcher = "Celine",
  locality = "FrenchGuyana", 
  roof_treatment = NA,
  heigth_treatment = 2,
  abundance = list(tibble(celine_canopy_fa)),
  list = list(tibble(celine_canopy_list)),
  traits=list(tibble(celine_canopy_traits)),
  measures=list(tibble(celine_canopy_measures)),
  obs = "")

# general data
celine_general_fa <- read_xlsx(
  file.path(
    celine_guyana, "Celine Leroy_French Guiana_general.xlsx"),
  "fauna_abundance")

celine_general_list <- read_xlsx(
  file.path(
    celine_guyana, "Celine Leroy_French Guiana_general.xlsx"),
  "Fauna_morphospecies_list")

celine_general_traits <- read_xlsx(
  file.path(
    celine_guyana, "Celine Leroy_French Guiana_general.xlsx"),
  "Fauna_traits")

celine_general_measures <- read_xlsx(
  file.path(
    celine_guyana, "Celine Leroy_French Guiana_general.xlsx"),
  "measures_decomposition_geograph")

# abundance
head(celine_general_fa)

# list
head(celine_general_list)

# traits
celine_general_traits <- celine_general_traits %>%
  rename("total_length" = "total_length (mm)")

# measures
celine_general_measures <- celine_general_measures %>%
  mutate("Remaining_water_volume" = NA) %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA"))) %>%
  mutate(across(all_of(cols_to_convert_g_to_mg), ~ .x * 1000))

# data 
celine_general_frenchguyana_data <- tibble(
  researcher = "Celine",
  locality = "FrenchGuyana", 
  roof_treatment = NA,
  heigth_treatment = 1,
  abundance = list(tibble(celine_general_fa)),
  list = list(tibble(celine_general_list)),
  traits=list(tibble(celine_general_traits)),
  measures=list(tibble(celine_general_measures)),
  obs = "")

save(celine_canopy_frenchguyana_data,
     celine_general_frenchguyana_data,
     file = file.path(local_directory,
                 "French_Guyana_Celine",
                 "Celine_FrenchGuyana.RData"))
#--- MD14 & MD15 & MD16 & MD65 & MD66 & MD67 --- Gonzalez_USA ----
gonzales_usa <- file.path(local_directory,
  "Gonzalez_USA",
  "González.NJ_Site_Microcosm_Updated_2.xlsx")

gonzales_fa <- read_xlsx(
  file.path(
    gonzales_usa),
  "fauna_abundance")

gonzales_list <- read_xlsx(
  file.path(
    gonzales_usa),
  "Fauna_morphospecies_list")

gonzales_traits <- read_xlsx(
  file.path(
    gonzales_usa),
  "Fauna_traits")

gonzales_measures <- read_xlsx(
  file.path(
    gonzales_usa),
  "measures_decomposition_geograph")

# dentro do mesmo site, tem tratamento de telhado e sem telhado
# mas a coluna que informa se tem telhado ou nao, esta no measures
# aqui vou criar um df para separar os experimentos com e sem telhado
# em cada um dos sites, isso ira gerar novos codigos ID 
site1_vector <- gonzales_measures %>% 
  filter(Site == "Site 1") %>%
  select(Site, Treatment, Replicate, Roof) %>%
  mutate(Treatment = str_replace_all(
    Treatment,c(
      "managed_forest" = "Managed forest",
      "natural_forest" = "Natural forest")))

site2_vector <- gonzales_measures %>% 
  filter(Site == "Site 2") %>%
  select(Site, Treatment, Replicate, Roof) %>%
  mutate(Treatment = str_replace_all(
    Treatment,c(
      "managed_forest" = "Managed forest",
      "natural_forest" = "Natural forest")))

site3_vector <- gonzales_measures %>% 
  filter(Site == "Site 3") %>%
  select(Site, Treatment, Replicate, Roof) %>%
  mutate(Treatment = str_replace_all(
    Treatment,c(
      "managed_forest" = "Managed forest",
      "natural_forest" = "Natural forest")))

# abundance
gonzales_site1_roof_fa <- gonzales_fa %>% 
  filter(Site == "Site 1") %>%
  select(-Morphospecies.4, -Morphospecies.5, -Morphospecies.7,
         -Morphospecies.8, -Morphospecies.9, -Morphospecies.10,
         -Morphospecies.6) %>%
  left_join(site1_vector, by = c("Site", "Treatment", "Replicate")) %>%
  filter(Roof == "yes") %>%
  select(-Roof, -Site) %>%
  filter(!(#Treatment == "Managed forest" & Replicate == "pot.5" |
             Treatment == "Managed forest" & Replicate == "pot.6" |
             #Treatment == "Managed forest" & Replicate == "pot.12" |
             Treatment == "Managed forest" & Replicate == "pot.13" |
             Treatment == "Managed forest" & Replicate == "pot.16" |
             Treatment == "Natural forest" & Replicate == "pot.7" |
             Treatment == "Natural forest" & Replicate == "pot.9" |
             Treatment == "Natural forest" & Replicate == "pot.11" |
             Treatment == "Natural forest" & Replicate == "pot.20" ))

gonzales_site1_nonroof_fa <- gonzales_fa %>% 
  filter(Site == "Site 1") %>%
  select(-Morphospecies.4, -Morphospecies.5, -Morphospecies.7,
         -Morphospecies.8, -Morphospecies.9, -Morphospecies.10) %>%
  left_join(site1_vector, by = c("Site", "Treatment", "Replicate")) %>%
  filter(Roof == "no") %>%
  select(-Roof, -Site) %>%
  filter(!(#Treatment == "Managed forest" & Replicate == "pot.5" |
             Treatment == "Managed forest" & Replicate == "pot.6" |
             #Treatment == "Managed forest" & Replicate == "pot.12" |
             Treatment == "Managed forest" & Replicate == "pot.13" |
             Treatment == "Managed forest" & Replicate == "pot.16" |
             Treatment == "Natural forest" & Replicate == "pot.7" |
             Treatment == "Natural forest" & Replicate == "pot.9" |
             Treatment == "Natural forest" & Replicate == "pot.11" |
             Treatment == "Natural forest" & Replicate == "pot.20" ))

gonzales_site2_roof_fa <- gonzales_fa %>% 
  filter(Site == "Site 2") %>%
  select(-Morphospecies.4, -Morphospecies.6, -Morphospecies.9,
         -Morphospecies.8) %>%
  left_join(site2_vector, by = c("Site", "Treatment", "Replicate")) %>%
  filter(Roof == "yes") %>%
  select(-Roof, -Site) #%>%
  #filter(!(Treatment == "Managed forest" & Replicate == "pot.3" |
  #           Treatment == "Natural forest" & Replicate == "pot.3" |
  #           Treatment == "Natural forest" & Replicate == "pot.11" ))

gonzales_site2_nonroof_fa <- gonzales_fa %>% 
  filter(Site == "Site 2") %>%
  select(-Morphospecies.4, -Morphospecies.6, -Morphospecies.9,
         -Morphospecies.2, -Morphospecies.7) %>%
  left_join(site2_vector, by = c("Site", "Treatment", "Replicate")) %>%
  filter(Roof == "no") %>%
  select(-Roof, -Site) #%>%
  #filter(!(Treatment == "Managed forest" & Replicate == "pot.3" |
   #          Treatment == "Natural forest" & Replicate == "pot.3" |
    #         Treatment == "Natural forest" & Replicate == "pot.11" ))

gonzales_site3_roof_fa <- gonzales_fa %>% 
  filter(Site == "Site 3") %>%
  select(-Morphospecies.5, -Morphospecies.6, -Morphospecies.7, 
         -Morphospecies.8, -Morphospecies.4, -Morphospecies.10) %>%
  left_join(site3_vector, by = c("Site", "Treatment", "Replicate")) %>%
  filter(Roof == "yes") %>%
  select(-Roof, -Site) %>%
  filter(!(Treatment == "Managed forest" & Replicate == "pot.3" |
             Treatment == "Managed forest" & Replicate == "pot.14" ))

gonzales_site3_nonroof_fa <- gonzales_fa %>% 
  filter(Site == "Site 3") %>%
  select(-Morphospecies.5, -Morphospecies.6, -Morphospecies.7, 
         -Morphospecies.8, -Morphospecies.9) %>%
  left_join(site3_vector, by = c("Site", "Treatment", "Replicate")) %>%
  filter(Roof == "no") %>%
  select(-Roof, -Site) %>%
  filter(!(Treatment == "Managed forest" & Replicate == "pot.3" |
             Treatment == "Managed forest" & Replicate == "pot.14" ))

# MD14, MD15 e MD16
colSums(gonzales_site1_roof_fa[,-c(1:2)]) # Morphospecies.6
colSums(gonzales_site2_roof_fa[,-c(1:2)]) # Morphospecies.8
colSums(gonzales_site3_roof_fa[,-c(1:2)]) # Morphospecies.4 Morphospecies.10
# New MDs
colSums(gonzales_site1_nonroof_fa[,-c(1:2)]) # ok
colSums(gonzales_site2_nonroof_fa[,-c(1:2)]) # Morphospecies.2 Morphospecies.7 
colSums(gonzales_site3_nonroof_fa[,-c(1:2)]) # Morphospecies.9

# list
gonzales_list <- mutate_all(
  gonzales_list, ~(replace(., .=="?", NA)))

gonzales_site1_roof_list <- gonzales_list %>%
  filter(Morfospecies_name %in% names(gonzales_site1_roof_fa[,-c(1:2)]))

gonzales_site1_nonroof_list <- gonzales_list %>%
  filter(Morfospecies_name %in% names(gonzales_site1_nonroof_fa[,-c(1:2)]))

gonzales_site2_roof_list <- gonzales_list %>%
  filter(Morfospecies_name %in% names(gonzales_site2_roof_fa[,-c(1:2)]))

gonzales_site2_nonroof_list <- gonzales_list %>%
  filter(Morfospecies_name %in% names(gonzales_site2_nonroof_fa[,-c(1:2)]))

gonzales_site3_roof_list <- gonzales_list %>%
  filter(Morfospecies_name %in% names(gonzales_site3_roof_fa[,-c(1:2)]))

gonzales_site3_nonroof_list <- gonzales_list %>%
  filter(Morfospecies_name %in% names(gonzales_site3_nonroof_fa[,-c(1:2)]))

# traits
gonzales_traits <- mutate_all(
  gonzales_traits, ~(replace(., .=="?", NA)))

gonzales_site1_roof_traits <- gonzales_traits %>%
  filter(Morfospecies_name %in% names(gonzales_site1_roof_fa[,-c(1:2)]))

gonzales_site1_nonroof_traits <- gonzales_traits %>%
  filter(Morfospecies_name %in% names(gonzales_site1_nonroof_fa[,-c(1:2)]))

gonzales_site2_roof_traits <- gonzales_traits %>%
  filter(Morfospecies_name %in% names(gonzales_site2_roof_fa[,-c(1:2)]))

gonzales_site2_nonroof_traits <- gonzales_traits %>%
  filter(Morfospecies_name %in% names(gonzales_site2_nonroof_fa[,-c(1:2)]))

gonzales_site3_roof_traits <- gonzales_traits %>%
  filter(Morfospecies_name %in% names(gonzales_site3_roof_fa[,-c(1:2)]))

gonzales_site3_nonroof_traits <- gonzales_traits %>%
  filter(Morfospecies_name %in% names(gonzales_site3_nonroof_fa[,-c(1:2)]))

# measures
# experiments with single cottomstrip are necessary to duplicate
gonzales_measures[5,8] <- "227.18"
gonzales_measures[12,8] <- "185.82"
gonzales_measures[43,8] <- "209.02"
gonzales_measures[63,8] <- "154.68"
gonzales_measures[71,8] <- "164.14"

gonzales_site1_roof_measures <- gonzales_measures %>% 
  mutate(Remaining_water_volume = NA) %>%
  filter(Site == "Site 1" & Roof == "yes") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA"))) %>%
  mutate(treatment = str_replace_all(
    treatment,c(
      "managed_forest" = "Managed forest",
      "natural_forest" = "Natural forest"))) %>%
  filter(!(#treatment == "Managed forest" & replicate == "pot.5" |
           treatment == "Managed forest" & replicate == "pot.6" |
           #treatment == "Managed forest" & replicate == "pot.12" |
           treatment == "Managed forest" & replicate == "pot.13" |
           treatment == "Managed forest" & replicate == "pot.16" |
           treatment == "Natural forest" & replicate == "pot.7" |
           treatment == "Natural forest" & replicate == "pot.9" |
           treatment == "Natural forest" & replicate == "pot.11" |
           treatment == "Natural forest" & replicate == "pot.20" ))

gonzales_site1_nonroof_measures <- gonzales_measures %>% 
  mutate(Remaining_water_volume = NA) %>%
  filter(Site == "Site 1" & Roof == "no") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA"))) %>%
  mutate(treatment = str_replace_all(
    treatment,c(
      "managed_forest" = "Managed forest",
      "natural_forest" = "Natural forest"))) %>%
  filter(!(#treatment == "Managed forest" & replicate == "pot.5" |
           treatment == "Managed forest" & replicate == "pot.6" |
           #treatment == "Managed forest" & replicate == "pot.12" |
           treatment == "Managed forest" & replicate == "pot.13" |
           treatment == "Managed forest" & replicate == "pot.16" |
           treatment == "Natural forest" & replicate == "pot.7" |
           treatment == "Natural forest" & replicate == "pot.9" |
           treatment == "Natural forest" & replicate == "pot.11" |
           treatment == "Natural forest" & replicate == "pot.20" ))

gonzales_site2_roof_measures <- gonzales_measures %>% 
  mutate(Remaining_water_volume = NA) %>%
  filter(Site == "Site 2" & Roof == "yes") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA"))) %>%
  mutate(treatment = str_replace_all(
    treatment,c(
      "managed_forest" = "Managed forest",
      "natural_forest" = "Natural forest"))) #%>%
  #filter(!(treatment == "Managed forest" & replicate == "pot.3" |
             #treatment == "Natural forest" & replicate == "pot.3" |
             #treatment == "Natural forest" & replicate == "pot.11" ))

gonzales_site2_nonroof_measures <- gonzales_measures %>% 
  mutate(Remaining_water_volume = NA) %>%
  filter(Site == "Site 2" & Roof == "no") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA"))) %>%
  mutate(treatment = str_replace_all(
    treatment,c(
      "managed_forest" = "Managed forest",
      "natural_forest" = "Natural forest"))) #%>%
  #filter(!(treatment == "Managed forest" & replicate == "pot.3" |
            # treatment == "Natural forest" & replicate == "pot.3" |
             #treatment == "Natural forest" & replicate == "pot.11" ))

gonzales_site3_roof_measures <- gonzales_measures %>% 
  mutate(Remaining_water_volume = NA) %>%
  filter(Site == "Site 3" & Roof == "yes") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA"))) %>%
  mutate(treatment = str_replace_all(
    treatment,c(
      "managed_forest" = "Managed forest",
      "natural_forest" = "Natural forest"))) %>%
  filter(!(treatment == "Managed forest" & replicate == "pot.3" |
             treatment == "Managed forest" & replicate == "pot.14" ))


gonzales_site3_nonroof_measures <- gonzales_measures %>% 
  mutate(Remaining_water_volume = NA) %>%
  filter(Site == "Site 3" & Roof == "no") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA"))) %>%
  mutate(treatment = str_replace_all(
    treatment,c(
      "managed_forest" = "Managed forest",
      "natural_forest" = "Natural forest"))) %>%
  filter(!(treatment == "Managed forest" & replicate == "pot.3" |
             treatment == "Managed forest" & replicate == "pot.14" ))

# data 
gonzales_site1_roof_data <- tibble(
  researcher = "Gonzales",
  locality = "Philadelphia_USA", 
  roof_treatment = 1,
  abundance = list(tibble(gonzales_site1_roof_fa)),
  list = list(tibble(gonzales_site1_roof_list)),
  traits=list(tibble(gonzales_site1_roof_traits)),
  measures=list(tibble(gonzales_site1_roof_measures)),
  obs = "")

gonzales_site2_roof_data <- tibble(
  researcher = "Gonzales",
  locality = "Philadelphia_USA", 
  roof_treatment = 1,
  abundance = list(tibble(gonzales_site2_roof_fa)),
  list = list(tibble(gonzales_site2_roof_list)),
  traits=list(tibble(gonzales_site2_roof_traits)),
  measures=list(tibble(gonzales_site2_roof_measures)),
  obs = "")

gonzales_site3_roof_data <- tibble(
  researcher = "Gonzales",
  locality = "Philadelphia_USA", 
  roof_treatment = 1,
  abundance = list(tibble(gonzales_site3_roof_fa)),
  list = list(tibble(gonzales_site3_roof_list)),
  traits=list(tibble(gonzales_site3_roof_traits)),
  measures=list(tibble(gonzales_site3_roof_measures)),
  obs = "")

gonzales_site1_nonroof_data <- tibble(
  researcher = "Gonzales",
  locality = "Philadelphia_USA", 
  roof_treatment = 0,
  abundance = list(tibble(gonzales_site1_nonroof_fa)),
  list = list(tibble(gonzales_site1_nonroof_list)),
  traits=list(tibble(gonzales_site1_nonroof_traits)),
  measures=list(tibble(gonzales_site1_nonroof_measures)),
  obs = "")

gonzales_site2_nonroof_data <- tibble(
  researcher = "Gonzales",
  locality = "Philadelphia_USA", 
  roof_treatment = 0,
  abundance = list(tibble(gonzales_site2_nonroof_fa)),
  list = list(tibble(gonzales_site2_nonroof_list)),
  traits=list(tibble(gonzales_site2_nonroof_traits)),
  measures=list(tibble(gonzales_site2_nonroof_measures)),
  obs = "")

gonzales_site3_nonroof_data <- tibble(
  researcher = "Gonzales",
  locality = "Philadelphia_USA", 
  roof_treatment = 0,
  abundance = list(tibble(gonzales_site3_nonroof_fa)),
  list = list(tibble(gonzales_site3_nonroof_list)),
  traits=list(tibble(gonzales_site3_nonroof_traits)),
  measures=list(tibble(gonzales_site3_nonroof_measures)),
  obs = "")

save(gonzales_site1_roof_data,
     gonzales_site2_roof_data,
     gonzales_site3_roof_data,
     gonzales_site1_nonroof_data,
     gonzales_site2_nonroof_data,
     gonzales_site3_nonroof_data,
     file = file.path(local_directory,
                 "Gonzalez_USA",
                 "Gonzalez_USA.RData"))

#--- MD17 --- Horvath_HU ----
# dados do logger estão na mesma planilha
horvath_hungria <- file.path(local_directory,
                     "Horvath_HU",
                     "Microcosm_HU_Horvath.xlsx")

horvath_fa <- read_xlsx(
  file.path(
    horvath_hungria),
  "fauna_abundance")

horvath_list <- read_xlsx(
  file.path(
    horvath_hungria),
  "Fauna_morphospecies_list")

horvath_traits <- read_xlsx(
  file.path(
    horvath_hungria),
  "Fauna_traits")

horvath_measures <- read_xlsx(
  file.path(
    horvath_hungria),
  "measures_decomposition_geograph")

# abundance
head(horvath_fa)

# list
head(horvath_list)

# traits
head(horvath_traits)

# measures
horvath_measures <- horvath_measures %>%
  rename("Remaining_water_volume" = "Final volume of water") %>%
  mutate("Natural tree hole.1" = NA,
         "Natural tree hole.2" = NA) %>%
  mutate(across("ammonium_concentration", ~ str_replace(., "<0.1", "0"))) %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA"))) %>%
  mutate(across(all_of(cols_to_convert_g_to_mg), ~ .x * 1000))

horvath_data <- tibble(
  researcher = "Horvath",
  locality = "PilisMountain_Hungria", 
  roof_treatment = NA,
  abundance = list(tibble(horvath_fa)),
  list = list(tibble(horvath_list)),
  traits=list(tibble(horvath_traits)),
  measures=list(tibble(horvath_measures)),
  obs = "informações sobre tree hole estão na aba metadados.")

save(horvath_data,
     file = file.path(local_directory,
                 "Horvath_HU",
                 "horvath_hungria.RData"))



#--- MD18 --- Izzo_Chapada_BR ----
# Fauna_morphospecies_list: substituído '-' por 'NA'
# (fiz direto na planilha a remoção do hífen)
izzo_br <- file.path(local_directory,
                        "Izzo_Chapada_BR",
                        "TJIZZO.Chapada.xlsx")

izzo_fa <- read_xlsx(
  file.path(
    izzo_br),
  "fauna_abundance")

izzo_list <- read_xlsx(
  file.path(
    izzo_br),
  "Fauna_morphospecies_list")

izzo_traits <- read_xlsx(
  file.path(
    izzo_br),
  "Fauna_traits")

izzo_measures <- read_xlsx(
  file.path(
    izzo_br),
  "measures_decomposition_geograph")

izzo_fa <- izzo_fa %>%
  filter(Treatment != "Natural forest" | Replicate != "pot.5")

# list
head(izzo_list)

# traits
head(izzo_traits)

# measures
izzo_measures <- izzo_measures %>%
  rename("Remaining_water_volume" = "Final water volume (ml)") %>%
  filter(Treatment != "Natural forest" | Replicate != "pot.5") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA"))) %>%
  mutate(across(all_of(cols_to_convert_g_to_mg), ~ .x * 1000))

izzo_data <- tibble(
  researcher = "Izzo",
  locality = "Chapada_Brazil", 
  roof_treatment = NA,
  abundance = list(tibble(izzo_fa)),
  list = list(tibble(izzo_list)),
  traits=list(tibble(izzo_traits)),
  measures=list(tibble(izzo_measures)),
  obs = "")

save(izzo_data,
     file = file.path(local_directory,
                 "Izzo_Chapada_BR",
                 "izzo_brazil.RData"))
#--- MD19 & MD54 --- GRomero_Japi ----
# coordenadas convertidas direto no xlsx
romero_japi <- file.path(local_directory,
                "Japi_romero",
                "Romero.Japi_updated_May_2024.xlsx")

romero_japi_fa <- read_xlsx(
  file.path(
    romero_japi),
  "Japi-fauna_abundance")

romero_japi_list <- read_xlsx(
  file.path(
    romero_japi),
  "Japi-Fauna_morphospecies_list")

romero_japi_traits <- read_xlsx(
  file.path(
    romero_japi),
  "Fauna_traits")

romero_japi_measures <- read_xlsx(
  file.path(
    romero_japi),
  "measures_decomposition_geograph")

# Mon May 20 18:09:26 2024 
# aqui temos 40 potes, sendo que 20 sao com telhado, e 20 sem telhado
# como os dados ja foram adicionar uma identificacao-chave (Coluna 'ID'), esse
# experimento foi inicialmente identificado apenas com o ID19. No entanto,
# esses dados terao que receber duas identificacoes, pois cada tratamento
# sera uma lista distinta no dataframe aninhado, pois receberao 1 e 0 na coluna
# roof_treatment, permitindo analisar esses dados de maneira separada
# por essa razao, ID19 sera o experimento SEM telhado (roof_treatment = 0)
# e o ID54 COM telhado (roof_treatment = 1). Qualquer duvida, consultar
# MATHEUS MOROTI ou GUSTAVO ROMERO

# abundance
# essa aba nao foi classificada com telhado e sem telhado, por isso vou usar
# a aba measures para identificar quais potes correspondem aos tratamentos
japi_treatment <- romero_japi_measures %>% 
  select(Roof_treatment, Replicate)

# como nem todas as sp estao presentes nos tratamentos, precisamos retirar
# e deixar os dados harmonizados, ou seja, com apenas as especies presentes
# e que estejam na lista e nos traits
# roof
#colSums(romero_roof_japi_fa[,-c(1:2)])
filter_roof <- c("Morphospecies.12", "Morphospecies.13", "Morphospecies.14",
                 "Morphospecies.15", "Morphospecies.18", "Morphospecies.19",
                 "Morphospecies.21", "Morphospecies.24", "Morphospecies.25",
                 "Morphospecies.26", "Morphospecies.30", "Morphospecies.34",
                 "Morphospecies.35", "Morphospecies.36", "Morphospecies.37",
                 "Morphospecies.38", "Morphospecies.39", "Morphospecies.41", 
                 "Morphospecies.43") 
# roof
#colSums(romero_nonroof_japi_fa[,-c(1:2)])
filter_nonroof <- c("Morphospecies.1", "Morphospecies.2", "Morphospecies.9",
                    "Morphospecies.10", "Morphospecies.16", "Morphospecies.17",
                    "Morphospecies.20", "Morphospecies.22", "Morphospecies.28",
                    "Morphospecies.29", "Morphospecies.31", "Morphospecies.42",
                    "Morphospecies.44") 

# abundance
romero_roof_japi_fa <- romero_japi_fa %>%
  left_join(japi_treatment, by="Replicate") %>%
  filter(Roof_treatment == "roof") %>%
  select(-Roof_treatment) %>%
  select(!(filter_roof)) %>%
  mutate(Treatment = str_replace_all(
    Treatment,c(
      "Degraded" = "Managed forest",
      "Natural" = "Natural forest"))) 

romero_nonroof_japi_fa <- romero_japi_fa %>%
  left_join(japi_treatment, by="Replicate") %>%
  filter(Roof_treatment == "open") %>%
  select(-Roof_treatment) %>%
  select(!(filter_nonroof)) %>%
  mutate(Treatment = str_replace_all(
    Treatment,c(
      "Degraded" = "Managed forest",
      "Natural" = "Natural forest"))) 

# list
romero_roof_japi_list <- romero_japi_list %>% 
  filter(!(Morfospecies_name %in% filter_roof)) %>%
  mutate(across(everything(), ~ na_if(., "-")))

romero_nonroof_japi_list <- romero_japi_list %>% 
  filter(!(Morfospecies_name %in% filter_nonroof)) %>%
  mutate(across(everything(), ~ na_if(., "-")))

# traits
# mesma coisa nos traits
romero_roof_japi_traits <- romero_japi_traits %>%
  filter(!(Morfospecies_name %in% filter_roof))

romero_nonroof_japi_traits <- romero_japi_traits %>%
  filter(!(Morfospecies_name %in% filter_nonroof))

# measures
romero_roof_japi_measures <- romero_japi_measures %>%
  rename("Remaining_water_volume" = "Final water volume (ml)") %>%
  filter(Roof_treatment == "roof") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA"))) %>%
  mutate(across(all_of(cols_to_convert_g_to_mg), ~ .x * 1000)) %>%
  mutate(tree_dbh = tree_dbh / pi) %>%
  mutate(treatment = str_replace_all(
    treatment,c(
      "Degraded" = "Managed forest",
      "Natural" = "Natural forest"))) %>%
  select(-Roof_treatment)

romero_nonroof_japi_measures <- romero_japi_measures %>%
  rename("Remaining_water_volume" = "Final water volume (ml)") %>%
  filter(Roof_treatment == "open") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA"))) %>%
  mutate(across(all_of(cols_to_convert_g_to_mg), ~ .x * 1000)) %>%
  mutate(tree_dbh = tree_dbh / pi) %>%
  mutate(treatment = str_replace_all(
    treatment,c(
      "Degraded" = "Managed forest",
      "Natural" = "Natural forest"))) %>%
  select(-Roof_treatment)

# save data
romero_roof_japi_data <- tibble(
  researcher = "Romero",
  locality = "Japi_Brazil", 
  roof_treatment = 1,
  abundance = list(tibble(romero_roof_japi_fa)),
  list = list(tibble(romero_roof_japi_list)),
  traits=list(tibble(romero_roof_japi_traits)),
  measures=list(tibble(romero_roof_japi_measures)),
  obs = NA)

romero_nonroof_japi_data <- tibble(
  researcher = "Romero",
  locality = "Japi_Brazil", 
  roof_treatment = 0,
  abundance = list(tibble(romero_nonroof_japi_fa)),
  list = list(tibble(romero_nonroof_japi_list)),
  traits=list(tibble(romero_nonroof_japi_traits)),
  measures=list(tibble(romero_nonroof_japi_measures)),
  obs = NA)

save(romero_roof_japi_data,
     romero_nonroof_japi_data,
     file =file.path(local_directory,
                 "Japi_romero",
                 "romero_japi_brazil.RData"))

#--- MD20 --- Jari Finland ----
jari_finland <- file.path(local_directory,
                    "Jari_Finland",
                    "JariKouki-Finland-draft-data.xlsx")

jari_fa <- read_xlsx(
  file.path(
    jari_finland),
  "fauna_abundance")

jari_list <- read_xlsx(
  file.path(
    jari_finland),
  "Fauna_morphospecies_list")

jari_traits <- read_xlsx(
  file.path(
    jari_finland),
  "Fauna_traits")

jari_measures <- read_xlsx(
  file.path(
    jari_finland),
  "measures_decomposition_geograph")

# abundance
jari_fa <- jari_fa %>%
  select(-"...3") %>%
  mutate(across(-c(1, 2), ~ replace_na(., 0)))

names(jari_fa)
# list
jari_list <- jari_list %>%
       bind_rows(tibble(Morfospecies_name = "Morphospecies.38")) # present in
# traits, but ausent in list

# traits
jari_traits <- jari_traits[-c(39:46),] # retirando as linhas a mais

jari_traits <- jari_traits %>% 
  rename("total_length" = "total_length (mm)")

jari_traits$total_length <- as.double(jari_traits$total_length)

# measures
jari_measures <- jari_measures %>%
  mutate(
    `detritus dry mass (fine)` = `detritus dry mass (filterpaper <0,2 mm)` +
      `detritus dry mass mg (fine 0,5 mm-0,2 mm)`
  ) %>% 
  rename("Remaining_water_volume" = "volume of the water in sample") %>%
  rename(all_of(dict_names)) %>%
  mutate(across("water_volume", ~ str_replace(., "0/completely dry", "0"))) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  select(-"detritus dry mass (filterpaper <0,2 mm)",
         -"detritus dry mass mg (fine 0,5 mm-0,2 mm)")

jari_data <- tibble(
  researcher = "Jari",
  locality = "Finland", 
  roof_treatment = NA,
  abundance = list(tibble(jari_fa)),
  list = list(tibble(jari_list)),
  traits=list(tibble(jari_traits)),
  measures=list(tibble(jari_measures)),
  obs = "")

save(jari_data,
     file = file.path(local_directory,
                 "Jari_Finland",
                 "jari_finland.RData"))

#--- MD21 & MD22 & MD23 --- Juen_Belem_BR ----
# os dados dos diferentes tratamentos estao todos juntos
# precisamos separar em linhas distintas e limpar as abas correspondentes
# por ex, no tratamento com telhado, alguns taxons nao estao presentes, assim
# como nos outros experimentos. Pela estrutura dos dados, tem 3 experimentos 
# aqui, sendo um deles a comparacao entre UFPA vs. Utinga
juen_belem <- file.path(local_directory,
                     "Juen_Belem_BR")

juen_fa <- read_xlsx(
  file.path(
    juen_belem,
    "Juen_Belem_Amazon.xlsx"),
  "fauna_abundance")

juen_list <- read_xlsx(
  file.path(
    juen_belem,
    "Juen_Belem_Amazon.xlsx"),
  "Fauna_morphospecies_list")

juen_traits <- read_xlsx(
  file.path(
    juen_belem,
    "Juen_Belem_Amazon.xlsx"),
  "Fauna_traits")

juen_measures <- read_xlsx(
  file.path(
    juen_belem,
    "Juen_Belem_Amazon.xlsx"),
  "measures_decomposition_geograph")

# abundance
juen_roof_fa <- juen_fa %>%
  filter(Roof == "com roof") %>% # 20 amostras
  select(-"Morphospecies.1", -"Morphospecies.2", -"Morphospecies.6",
         -"Morphospecies.8", -"Morphospecies.10", -"Morphospecies.11",
         -"Morphospecies.16", -"Morphospecies.18", -"Morphospecies.20") %>%
  mutate(Morphospecies.5 = Morphospecies.4 + Morphospecies.5) %>%
  mutate(Morphospecies.7 = Morphospecies.7 + Morphospecies.9) %>%
  select(-"Morphospecies.9", -"Morphospecies.4", -"Local", -"Roof")

#dim(juen_roof_fa)
#colSums(juen_roof_fa[,5:16])

juen_nonroof_fa <- juen_fa %>%
  filter(Roof == "Sem roof") %>% # 40 amostras
  filter(Local != "Utinga" & Local !="UFPA" ) %>% # agora 20 amostras
  select(-"Morphospecies.1", -"Morphospecies.2", -"Morphospecies.6",
         -"Morphospecies.9", -"Morphospecies.10", -"Morphospecies.13",
         -"Morphospecies.21") %>%
  mutate(Morphospecies.5 = Morphospecies.4 + Morphospecies.5) %>%
  select(-"Morphospecies.4", -"Local", -"Roof")

#dim(juen_nonroof_fa)
#colSums(juen_nonroof_fa[,5:18])

juen_na_fa <- juen_fa %>%
  filter(Local == "Utinga" | Local == "UFPA") %>%
  select(-"Morphospecies.13", -"Morphospecies.14", -"Morphospecies.15",
         -"Morphospecies.16", -"Morphospecies.17", -"Morphospecies.18",
         -"Morphospecies.19", -"Morphospecies.20", -"Morphospecies.21") %>%
  mutate(Morphospecies.5 = Morphospecies.4 + Morphospecies.5) %>%
  mutate(Morphospecies.7 = Morphospecies.7 + Morphospecies.9) %>%
  select(-"Morphospecies.9", -"Morphospecies.4", )

#dim(juen_na_fa)
#colSums(juen_na_fa[,5:16])

# list
juen_roof_list <- juen_list %>%
  filter(Morfospecies_name != "Morphospecies.1" & 
         Morfospecies_name != "Morphospecies.2" &
         Morfospecies_name != "Morphospecies.6" &
         Morfospecies_name != "Morphospecies.8" &
         Morfospecies_name != "Morphospecies.10" &
         Morfospecies_name != "Morphospecies.11" &
         Morfospecies_name != "Morphospecies.16" &
         Morfospecies_name != "Morphospecies.18" &
         Morfospecies_name != "Morphospecies.20" & 
           Morfospecies_name != "Morphospecies.9" &
           Morfospecies_name != "Morphospecies.4")

juen_nonroof_list <- juen_list %>%
  filter(Morfospecies_name != "Morphospecies.1" & 
           Morfospecies_name != "Morphospecies.2" &
           Morfospecies_name != "Morphospecies.6" &
           Morfospecies_name != "Morphospecies.9" &
           Morfospecies_name != "Morphospecies.10" &
           Morfospecies_name != "Morphospecies.13" &
           Morfospecies_name != "Morphospecies.21"&
           Morfospecies_name != "Morphospecies.4")

juen_na_list <- juen_list %>%
  filter(Morfospecies_name != "Morphospecies.13" & 
           Morfospecies_name != "Morphospecies.14" &
           Morfospecies_name != "Morphospecies.15" &
           Morfospecies_name != "Morphospecies.16" &
           Morfospecies_name != "Morphospecies.17" &
           Morfospecies_name != "Morphospecies.18" &
           Morfospecies_name != "Morphospecies.19" &
           Morfospecies_name != "Morphospecies.20" &
           Morfospecies_name != "Morphospecies.21" & 
           Morfospecies_name != "Morphospecies.9" &
           Morfospecies_name != "Morphospecies.4")

# traits
juen_roof_traits <- juen_traits %>%
  filter(Morfospecies_name != "Morphospecies.1" & 
           Morfospecies_name != "Morphospecies.2" &
           Morfospecies_name != "Morphospecies.6" &
           Morfospecies_name != "Morphospecies.8" &
           Morfospecies_name != "Morphospecies.10" &
           Morfospecies_name != "Morphospecies.11" &
           Morfospecies_name != "Morphospecies.16" &
           Morfospecies_name != "Morphospecies.18" &
           Morfospecies_name != "Morphospecies.20" & 
           Morfospecies_name != "Morphospecies.9" &
           Morfospecies_name != "Morphospecies.4")

juen_nonroof_traits <- juen_traits %>%
  filter(Morfospecies_name != "Morphospecies.1" & 
           Morfospecies_name != "Morphospecies.2" &
           Morfospecies_name != "Morphospecies.6" &
           Morfospecies_name != "Morphospecies.9" &
           Morfospecies_name != "Morphospecies.10" &
           Morfospecies_name != "Morphospecies.13" &
           Morfospecies_name != "Morphospecies.21" &
           Morfospecies_name != "Morphospecies.4")

juen_na_traits <- juen_traits %>%
  filter(Morfospecies_name != "Morphospecies.13" & 
           Morfospecies_name != "Morphospecies.14" &
           Morfospecies_name != "Morphospecies.15" &
           Morfospecies_name != "Morphospecies.16" &
           Morfospecies_name != "Morphospecies.17" &
           Morfospecies_name != "Morphospecies.18" &
           Morfospecies_name != "Morphospecies.19" &
           Morfospecies_name != "Morphospecies.20" &
           Morfospecies_name != "Morphospecies.21" & 
           Morfospecies_name != "Morphospecies.9" &
           Morfospecies_name != "Morphospecies.4")

# measures
juen_roof_measures <- juen_measures %>%
  filter(Roof == "com roof") %>%
  rename("dissolved_O2" = "dissolved_O2 (%)") %>%
  rename("Remaining_water_volume" = "Final water volume (ml)") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) 

juen_nonroof_measures <- juen_measures %>%
  filter(Roof == "Sem roof") %>%
  filter(Local != "Utinga" & Local !="UFPA") %>%
  rename("dissolved_O2" = "dissolved_O2 (%)") %>%
  rename("Remaining_water_volume" = "Final water volume (ml)") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) 

juen_na_measures <- juen_measures %>%
  filter(Roof == "Sem roof") %>%
  filter(Local == "Utinga" | Local == "UFPA") %>%
  rename("dissolved_O2" = "dissolved_O2 (%)") %>%
  rename("Remaining_water_volume" = "Final water volume (ml)") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) 

# data
juen_roof_data <- tibble(
  researcher = "Juen",
  locality = "Belem_Brazil", 
  roof_treatment = 1,
  abundance = list(tibble(juen_roof_fa)),
  list = list(tibble(juen_roof_list)),
  traits=list(tibble(juen_roof_traits)),
  measures=list(tibble(juen_roof_measures)),
  obs = "Sem dados dos algodoes")

juen_nonroof_data <- tibble(
  researcher = "Juen",
  locality = "Belem_Brazil", 
  roof_treatment = 0,
  abundance = list(tibble(juen_nonroof_fa)),
  list = list(tibble(juen_nonroof_list)),
  traits=list(tibble(juen_nonroof_traits)),
  measures=list(tibble(juen_nonroof_measures)),
  obs = "Sem dados dos algodoes")

juen_na_data <- tibble(
  researcher = "Juen",
  locality = "Belem_Brazil", 
  roof_treatment = NA,
  abundance = list(tibble(juen_na_fa)),
  list = list(tibble(juen_na_list)),
  traits=list(tibble(juen_na_traits)),
  measures=list(tibble(juen_na_measures)),
  obs = "Sem dados dos algodoes")

save(juen_na_data,
     juen_nonroof_data,
     juen_roof_data,
     file = file.path(local_directory,
                 "Juen_Belem_BR",
                 "juen_belem_br.RData"))


#--- MD24 & MD25 & MD26 --- Knapp_Czech ----
knapp_czech <- file.path(local_directory,
                   "Knapp_Czech")

knapp_fa <- read_xlsx(
  file.path(
    knapp_czech,
    "Knapp_Michal_Krivoklatsko.xlsx"),
  "fauna_abundance")[1,1]

knapp_list <- read_xlsx(
  file.path(
    knapp_czech,
    "Knapp_Michal_Krivoklatsko.xlsx"),
  "Fauna_morphospecies_list")[1,1]

knapp_traits <- read_xlsx(
  file.path(
    knapp_czech,
    "Knapp_Michal_Krivoklatsko.xlsx"),
  "Fauna_traits")[1,1]

knapp_measures <- read_xlsx(
  file.path(
    knapp_czech,
    "Knapp_Michal_Krivoklatsko.xlsx"),
  "measures_decomposition_geograph")[1:40, ]

# measures
knapp_roof_measures <- knapp_measures %>%
  filter(Experiment == "roof") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  select(-Experiment)

knapp_nonroof_measures <- knapp_measures %>%
  filter(Experiment == "standard") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  select(-Experiment) %>%
  filter(!(replicate %in% c("L02", "L22", "L21", "L27"))) 

knapp_roof_data <- tibble(
  researcher = "Knapp",
  locality = "Křivoklátsko_Czech",
  roof_treatment = 1,
  abundance = NA, #list(tibble(knapp_fa)),
  list = NA, #list(tibble(knapp_list)),
  traits= NA, #list(tibble(knapp_traits)),
  measures=list(tibble(knapp_roof_measures)),
  obs = "Sem invertebrados presentes na coleta")

knapp_nonroof_data <- tibble(
  researcher = "Knapp",
  locality = "Křivoklátsko_Czech", 
  roof_treatment = 0,
  abundance = NA, #list(tibble(knapp_fa)),
  list = NA, #list(tibble(knapp_list)),
  traits= NA , #list(tibble(knapp_traits)),
  measures=list(tibble(knapp_nonroof_measures)),
  obs = "Sem invertebrados presentes na coleta")

# hory 
knapp_hory_fa <- read_xlsx(
  file.path(
    knapp_czech,
    "Knapp Michal_Krusne_hory.xlsx"),
  "fauna_abundance")

knapp_hory_list <- read_xlsx(
  file.path(
    knapp_czech,
    "Knapp Michal_Krusne_hory.xlsx"),
  "Fauna_morphospecies_list")

knapp_hory_traits <- read_xlsx(
  file.path(
    knapp_czech,
    "Knapp Michal_Krusne_hory.xlsx"),
  "Fauna_traits")

knapp_hory_measures <- read_xlsx(
  file.path(
    knapp_czech,
    "Knapp Michal_Krusne_hory.xlsx"),
  "measures_decomposition_geograph")[1:20, ]

# measures
knapp_hory_fa
knapp_hory_list
knapp_hory_traits
knapp_hory_measures

# abundance
knapp_hory_fa[is.na(knapp_hory_fa)] <- 0

# list
knapp_hory_list

# traits
knapp_hory_traits <- knapp_hory_traits %>%
  mutate(total_length = case_when(
    total_length == "5 mm" ~ 5,
    TRUE ~ as.double(total_length)
  ))

# measures
knapp_hory_measures_filter <- knapp_hory_measures %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  select(-Experiment) %>%
  rename(outside_after_mg = outside_before_mg,
         outside_before_mg = outside_after_mg) %>%
  filter(!(replicate %in% c("H44", "H48", "H52")))

knapp_data <- tibble(
  researcher = "Knapp",
  locality = "OreMountains_Czech",
  roof_treatment = NA,
  abundance = list(tibble(knapp_hory_fa)),
  list = list(tibble(knapp_hory_list)),
  traits= list(tibble(knapp_hory_traits)),
  measures=list(tibble(knapp_hory_measures_filter)),
  obs = "")

save(knapp_roof_data,
     knapp_nonroof_data,
     knapp_data,
     file = file.path(local_directory,
                 "Knapp_Czech",
                 "knapp_czech.RData"))

#--- MD27 --- Luciano_argentina ----
luciano_argentina <- file.path(local_directory,
                    "Luciano_Argentina")

luciano_fa <- read_xlsx(
  file.path(
    luciano_argentina,
    "Microcosm_data_Cordoba.xlsx"),
  "Fauna_abundance")

luciano_list <- read_xlsx(
  file.path(
    luciano_argentina,
    "Microcosm_data_Cordoba.xlsx"),
  "Fauna_morphospecies_list")

luciano_traits <- read_xlsx(
  file.path(
    luciano_argentina,
    "Microcosm_data_Cordoba.xlsx"),
  "Fauna_traits")

luciano_measures <- read_xlsx(
  file.path(
    luciano_argentina,
    "Microcosm_data_Cordoba.xlsx"),
  "measures_decomposition_geograph")

# abundance
luciano_fa

# list
luciano_list

# traits
luciano_traits

# measures
luciano_measures <- luciano_measures %>%
  rename("Remaining_water_volume" = "Final water volume (ml)") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric))

luciano_data <- tibble(
  researcher = "Luciano",
  locality = "Cordoba_Argentina",
  roof_treatment = NA,
  abundance = list(tibble(luciano_fa)),
  list = list(tibble(luciano_list)),
  traits= list(tibble(luciano_traits)),
  measures=list(tibble(luciano_measures)),
  obs = "")

save(luciano_data,
     file = file.path(local_directory,
                 "Luciano_Argentina",
                 "luciano_argentina.RData"))

#--- MD28 & MD29 & MD30 --- Martins_Hamada_Amazon ----
# aqui existem alguns tratamentos juntos, com telhado, sem telhado
# e a diferenca de estratificacao com os microcosmos colocados a 15m de altura
# depois precisamos remover de cada experimento as faunas que nao estiveram
# presentes no respectivo tratamento (validado com Gustavo Romero)
martins_amazon <- file.path(local_directory,
                      "Martins_Hamada_Amazon")

martins_fa <- read_xlsx(
  file.path(
    martins_amazon,
    "Martins&Hamada_Manaus_01_12_23.xlsx"),
  "fauna_abundance")

martins_list <- read_xlsx(
  file.path(
    martins_amazon,
    "Martins&Hamada_Manaus_01_12_23.xlsx"),
  "Fauna_morphospecies_list")

martins_traits <- read_xlsx(
  file.path(
    martins_amazon,
    "Martins&Hamada_Manaus_01_12_23.xlsx"),
  "Fauna_traits")

martins_measures <- read_xlsx(
  file.path(
    martins_amazon,
    "Martins&Hamada_Manaus_01_12_23.xlsx"),
  "measures_decomposition_geograph")

# abundance
# selecionando cada tratamento e
# removendo a abundancia 0 em cada tratamento
martins_roof_fa <- martins_fa %>%
  filter(Treatment == "Managed forest - roof" |
         Treatment == "Natural forest - roof") %>%
  select(-Dytiscidae.sp2, -Curculionidae,
         -Megapodagrionidae.Heteropodagrion,
         -Formicidae.morpho2, -Cicadidae,
         -Colembola, -Pompilidae, -Chilopoda) %>%
  mutate(Treatment = str_replace_all(
    Treatment,c(
      "Managed forest - roof" = "Managed forest",
      "Natural forest - roof" = "Natural forest"))) 

martins_nonroof_fa <- martins_fa %>%
  filter(Treatment == "Managed forest - standard experiment" |
         Treatment == "Natural forest - standard experiment") %>%
  select(-Culicidae.Haemagogus, -Psychodidae, -Dytiscidae.sp2,
         -Curculionidae, -Termitidae, -Blaberidae, -Formicidae.morpho2,
         -Cicadidae, -Colembola, -"Anuro(girino)") %>%
  mutate(Treatment = str_replace_all(
    Treatment,c(
      "Managed forest - standard experiment" = "Managed forest",
      "Natural forest - standard experiment" = "Natural forest"))) 
  
martins_mid_fa <- martins_fa %>%
  filter(Treatment == "Managed forest - 15m" |
         Treatment == "Natural forest - 15m") %>%
  select(-Culicidae.Toxorhynchites, -Ceratopogonidae, -Psychodidae,
         -Scirtidae, -Stratiomidae, -Megapodagrionidae.Heteropodagrion,
         -Termitidae, -Blaberidae, -Pompilidae, -Chilopoda, -"Anuro(girino)") %>%
  mutate(Treatment = str_replace_all(
    Treatment,c(
      "Managed forest - 15m" = "Managed forest",
      "Natural forest - 15m" = "Natural forest"))) 
  
martins_roof_fa[is.na(martins_roof_fa)] <- 0
martins_nonroof_fa[is.na(martins_nonroof_fa)] <- 0
martins_mid_fa[is.na(martins_mid_fa)] <- 0

# list
martins_roof_list <- martins_list %>% 
  filter(Morfospecies_name != "Dytiscidae.sp2" &
           Morfospecies_name != "Curculionidae" &
           Morfospecies_name != "Megapodagrionidae.Heteropodagrion" &
           Morfospecies_name != "Formicidae.morpho2" &
           Morfospecies_name != "Cicadidae" &
           Morfospecies_name != "Colembola" &
           Morfospecies_name != "Pompilidae" &
           Morfospecies_name != "Chilopoda")

martins_nonroof_list <- martins_list %>% 
  filter(Morfospecies_name != "Culicidae.Haemagogus" &
          Morfospecies_name != "Psychodidae" &
          Morfospecies_name != "Dytiscidae.sp2" &
          Morfospecies_name != "Curculionidae" &
          Morfospecies_name != "Termitidae" &
          Morfospecies_name != "Blaberidae" &
          Morfospecies_name != "Formicidae.morpho2" &
          Morfospecies_name != "Cicadidae" &
          Morfospecies_name != "Colembola" &
          Morfospecies_name != "Anuro(girino)")

martins_mid_list <- martins_list %>% 
  filter(Morfospecies_name != "Culicidae.Toxorhynchites" &
           Morfospecies_name != "Ceratopogonidae" &
           Morfospecies_name != "Psychodidae" &
           Morfospecies_name != "Scirtidae" &
           Morfospecies_name != "Stratiomidae" &
           Morfospecies_name != "Megapodagrionidae.Heteropodagrion" &
           Morfospecies_name != "Termitidae" &
           Morfospecies_name != "Blaberidae" &
           Morfospecies_name != "Pompilidae" &
           Morfospecies_name != "Chilopoda" &
           Morfospecies_name != "Anuro(girino)")

# traits
martins_roof_traits <- martins_traits %>%
  filter(Morfospecies_name != "Dytiscidae.sp2" &
           Morfospecies_name != "Curculionidae" &
           Morfospecies_name != "Megapodagrionidae.Heteropodagrion" &
           Morfospecies_name != "Formicidae.morpho2" &
           Morfospecies_name != "Cicadidae" &
           Morfospecies_name != "Colembola" &
           Morfospecies_name != "Pompilidae" &
           Morfospecies_name != "Chilopoda")
  
martins_nonroof_traits <- martins_traits %>%
  filter(Morfospecies_name != "Culicidae.Haemagogus" &
           Morfospecies_name != "Psychodidae" &
           Morfospecies_name != "Dytiscidae.sp2" &
           Morfospecies_name != "Curculionidae" &
           Morfospecies_name != "Termitidae" &
           Morfospecies_name != "Blaberidae" &
           Morfospecies_name != "Formicidae.morpho2" &
           Morfospecies_name != "Cicadidae" &
           Morfospecies_name != "Colembola" &
           Morfospecies_name != "Anuro(girino)")

martins_mid_traits <- martins_traits %>%
  filter(Morfospecies_name != "Culicidae.Toxorhynchites" &
           Morfospecies_name != "Ceratopogonidae" &
           Morfospecies_name != "Psychodidae" &
           Morfospecies_name != "Scirtidae" &
           Morfospecies_name != "Stratiomidae" &
           Morfospecies_name != "Megapodagrionidae.Heteropodagrion" &
           Morfospecies_name != "Termitidae" &
           Morfospecies_name != "Blaberidae" &
           Morfospecies_name != "Pompilidae" &
           Morfospecies_name != "Chilopoda" &
           Morfospecies_name != "Anuro(girino)")

# measures
# rename wrong columns and filter out monkey-bitten replicas
experiments_damaged <- c("pot.33", "pot.47", "pot.50", "pot.51", "pot.53", "pot.54", "pot.58")

martins_measures_adjusts <- martins_measures %>%
  rename(
    "biomass_cotton_stripes_outside_bag_after (mg)" = "biomass_cotton_stripes_outside_bag_before (mg)",
    "biomass_cotton_stripes_outside_bag_before (mg)" = "biomass_cotton_stripes_outside_bag_after (mg)"
  ) %>%
  filter(
    !(Replicate %in% experiments_damaged
  ))

martins_measures_adjusts[53,8] <- NA # 0 in outside_before_g pot.60.
martins_measures_adjusts$'Final water volume (ml)' <- gsub(
  "Without water", 0, martins_measures_adjusts$'Final water volume (ml)')

martins_roof_measures <- martins_measures_adjusts %>%
  filter(Treatment == "Managed forest - roof" |
         Treatment == "Natural forest - roof") %>%
  rename("Remaining_water_volume" = "Final water volume (ml)") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric))

martins_nonroof_measures <- martins_measures_adjusts %>%
  filter(Treatment == "Managed forest - standard experiment" |
         Treatment == "Natural forest - standard experiment") %>%
  rename("Remaining_water_volume" = "Final water volume (ml)") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric))

martins_mid_measures <- martins_measures_adjusts %>%
  filter(Treatment == "Managed forest - 15m" |
        Treatment == "Natural forest - 15m") %>%
  rename("Remaining_water_volume" = "Final water volume (ml)") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric))

# data
martins_roof_data <- tibble(
  researcher = "Martins",
  locality = "Amazon_Brazil", 
  roof_treatment = 1,
  abundance = list(tibble(martins_roof_fa)),
  list = list(tibble(martins_roof_list)),
  traits=list(tibble(martins_roof_traits)),
  measures=list(tibble(martins_roof_measures)),
  obs = NA)

martins_nonroof_data <- tibble(
  researcher = "Martins",
  locality = "Amazon_Brazil", 
  roof_treatment = 0,
  abundance = list(tibble(martins_nonroof_fa)),
  list = list(tibble(martins_nonroof_list)),
  traits=list(tibble(martins_nonroof_traits)),
  measures=list(tibble(martins_nonroof_measures)),
  obs = NA)

martins_na_data <- tibble(
  researcher = "Martins",
  locality = "Amazon_Brazil", 
  roof_treatment = NA,
  heigth_treatment = 2,
  abundance = list(tibble(martins_mid_fa)),
  list = list(tibble(martins_mid_list)),
  traits=list(tibble(martins_mid_traits)),
  measures=list(tibble(martins_mid_measures)),
  obs = "Experiments damaged removed")

save(martins_roof_data,
     martins_nonroof_data,
     martins_na_data,
     file = file.path(local_directory,
                 "Martins_Hamada_Amazon",
                 "martins_amazon_br.RData"))

#--- MD31 --- Mexico_Wesley ----
wesley_mexico <- file.path(local_directory,
                       "Mexico_Wesley")

wesley_fa <- read_xlsx(
  file.path(
    wesley_mexico,
    "PI.WesleyDattilo_Mexico_FINAL.xlsx"),
  "fauna_abundance")

wesley_list <- read_xlsx(
  file.path(
    wesley_mexico,
    "PI.WesleyDattilo_Mexico_FINAL.xlsx"),
  "Fauna_morphospecies_list")

wesley_traits <- read_xlsx(
  file.path(
    wesley_mexico,
    "PI.WesleyDattilo_Mexico_FINAL.xlsx"),
  "Fauna_traits")

wesley_measures <- read_xlsx(
  file.path(
    wesley_mexico,
    "PI.WesleyDattilo_Mexico_FINAL.xlsx"),
  "measures_decomposition_geograph")

# abundance
head(wesley_fa)

# list
head(wesley_list)

# traits
wesley_traits <- wesley_traits %>%
  rename("total_length" = "total_length(mm)")

# measures
head(wesley_measures)

wesley_measures_adjust <- wesley_measures %>%
  mutate(Remaining_water_volume = 800) %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(all_of(cols_to_convert_g_to_mg), ~ .x * 1000))

wesley_data <- tibble(
  researcher = "WesleyDattilo",
  locality = "Mexico", 
  roof_treatment = NA,
  abundance = list(tibble(wesley_fa)),
  list = list(tibble(wesley_list)),
  traits=list(tibble(wesley_traits)),
  measures=list(tibble(wesley_measures_adjust)),
  obs = "")

save(wesley_data,
     file = file.path(local_directory,
                 "Mexico_Wesley",
                 "wesley_mexico.RData"))


#--- MD32 & MD33 --- MMoretti Lab_BR ----
moretti_br <- file.path(local_directory,
                      "MMoretti Lab_BR")

moretti_site1_fa <- read_xlsx(
  file.path(
    moretti_br,
    "PI.Marcelo.Moretti.Site.1.xlsx"),
  "Fauna_abundance")

moretti_site1_list <- read_xlsx(
  file.path(
    moretti_br,
    "PI.Marcelo.Moretti.Site.1.xlsx"),
  "Fauna_morphospecies_list")

moretti_site1_traits <- read_xlsx(
  file.path(
    moretti_br,
    "PI.Marcelo.Moretti.Site.1.xlsx"),
  "Fauna_traits")

moretti_site1_measures <- read_xlsx(
  file.path(
    moretti_br,
    "PI.Marcelo.Moretti.Site.1.xlsx"),
  "Measures_decomposition_geograph")

# abundance
head(moretti_site1_fa)
moretti_site1_fa[is.na(moretti_site1_fa)] <- 0

# list
moretti_site1_list

# traits
# change names to join with list 
moretti_site1_traits <- moretti_site1_traits %>% 
  bind_rows(tibble(Morfospecies_name = "Culicidae.pupa")) %>% # ausente nos traits
  mutate(Morfospecies_name = case_when(
    Morfospecies_name == "Forcipomyia sp" ~ "Forcipomyia.sp",
    Morfospecies_name == "Wyeomyia sp1" ~ "Wyeomyia.sp1",
    Morfospecies_name == "Wyeomyia sp2" ~ "Wyeomyia.sp2",
    Morfospecies_name == "Pupa.Psychodidae" ~ "Psychodidae.pupa",
    TRUE ~ Morfospecies_name  # caso padrao, mantem o valor
  )) %>%
  rename("total_length" = "total_length (mm)")

# measures
moretti_site1_measures_adjust <- moretti_site1_measures %>%
  rename("dissolved_O2" = "dissolved_O2 (mg/L)",
         "turbidity" = "turbidity (NTU)",
         "detritus dry mass (fine)" = "detritus dry mass (fine) (mg)",
         "detritus dry mass (coarse)" = "detritus dry mass (coarse) (g)",
         "Tree dbh" = "Tree dbh (cm)",
         "Remaining_water_volume" = "Final water volume (ml)") %>%
  rename(
    "biomass_cotton_stripes_outside_bag_after (mg)" = "biomass_cotton_stripes_outside_bag_before (mg)",
    "biomass_cotton_stripes_outside_bag_before (mg)" = "biomass_cotton_stripes_outside_bag_after (mg)"
  ) %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(all_of(cols_to_convert_g_to_mg), ~ .x * 1000))

# site 2
moretti_site2_fa <- read_xlsx(
  file.path(
    moretti_br,
    "PI.Marcelo.Moretti.Site.2.xlsx"),
  "Fauna_abundance")

moretti_site2_list <- read_xlsx(
  file.path(
    moretti_br,
    "PI.Marcelo.Moretti.Site.2.xlsx"),
  "Fauna_morphospecies_list")

moretti_site2_traits <- read_xlsx(
  file.path(
    moretti_br,
    "PI.Marcelo.Moretti.Site.2.xlsx"),
  "Fauna_traits")

moretti_site2_measures <- read_xlsx(
  file.path(
    moretti_br,
    "PI.Marcelo.Moretti.Site.2.xlsx"),
  "Measures_decomposition_geograph")

# abundance
head(moretti_site2_fa)
moretti_site2_fa[is.na(moretti_site2_fa)] <- 0

# list
moretti_site2_list

# traits
glimpse(moretti_site1_traits)

# measures
moretti_site2_measures_adjust <- moretti_site2_measures %>%
  rename("dissolved_O2" = "dissolved_O2 (mg/L)",
         "turbidity" = "turbidity (NTU)",
         "detritus dry mass (fine)" = "detritus dry mass (fine) (mg)",
         "detritus dry mass (coarse)" = "detritus dry mass (coarse) (g)",
         "Tree dbh" = "Tree dbh (cm)",
         "Remaining_water_volume" = "Final water volume (ml)") %>%
  rename(
    "biomass_cotton_stripes_outside_bag_after (mg)" = "biomass_cotton_stripes_outside_bag_before (mg)",
    "biomass_cotton_stripes_outside_bag_before (mg)" = "biomass_cotton_stripes_outside_bag_after (mg)"
  ) %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(all_of(cols_to_convert_g_to_mg), ~ .x * 1000))

#names(moretti_site2_measures)

moretti_site1_data <- tibble(
  researcher = "Moretti",
  locality = "SantaTereza_Brazil", 
  roof_treatment = NA,
  abundance = list(tibble(moretti_site1_fa)),
  list = list(tibble(moretti_site1_list)),
  traits=list(tibble(moretti_site1_traits)),
  measures=list(tibble(moretti_site1_measures_adjust)),
  obs = "")

moretti_site2_data <- tibble(
  researcher = "Moretti",
  locality = "MarechalFloriano_Brazil", 
  roof_treatment = NA,
  abundance = list(tibble(moretti_site2_fa)),
  list = list(tibble(moretti_site2_list)),
  traits=list(tibble(moretti_site2_traits)),
  measures=list(tibble(moretti_site2_measures_adjust)),
  obs = "")

save(moretti_site1_data,
     moretti_site2_data,
     file = file.path(local_directory,
                 "MMoretti Lab_BR",
                 "moretti_br.RData"))

#--- MD48 & MD49 --- Nakamura_China ---- 
nakamura <- file.path(local_directory,
                  "Nakamura_China")

nakamura_site1_fa <- read_xlsx(
  file.path(
    nakamura,
    "Nakamura_AilaoMountainYunnanChina_MICROcosm.xlsx"),
  "fauna_abundance")

nakamura_site1_list <- read_xlsx(
  file.path(
    nakamura,
    "Nakamura_AilaoMountainYunnanChina_MICROcosm.xlsx"),
  "Fauna_morphospecies_list")

nakamura_site1_traits <- read_xlsx(
  file.path(
    nakamura,
    "Nakamura_AilaoMountainYunnanChina_MICROcosm.xlsx"),
  "Fauna_traits")[1:11,] # retirando linhas de anotacao

nakamura_site1_measures <- read_xlsx(
  file.path(
    nakamura,
    "Nakamura_AilaoMountainYunnanChina_MICROcosm.xlsx"),
  "measures_decomposition_geograph")

# abundance
nakamura_site1_fa[is.na(nakamura_site1_fa)] <- 0

nakamura_site1_fa <- nakamura_site1_fa %>%
  mutate(Treatment = str_replace_all(
    Treatment,c(
      "Natural forest Understory" = "Natural forest",
      "Natural forest Midstory" = "Natural forest",
      "Natural forest Canopy" = "Natural forest"))) 

# list
head(nakamura_site1_list)

# traits
head(nakamura_site1_traits)

# measures
# rename variables with data dictionary
nakamura_site1_measures <- 
  nakamura_site1_measures %>%
  rename(Remaining_water_volume = "Final water volume (ml)") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(all_of(cols_to_convert_g_to_mg), ~ .x * 1000))

# join works!
#View(left_join(nakamura_site1_list,
#               nakamura_site1_traits,
#          by = "Morfospecies_name"))

nakamura_site1_data <- tibble(
  researcher = "Nakamura",
  locality = "AilaoMountain_China", 
  roof_treatment = NA,
  abundance = list(tibble(nakamura_site1_fa)),
  list = list(tibble(nakamura_site1_list)),
  traits=list(tibble(nakamura_site1_traits)),
  measures=list(tibble(nakamura_site1_measures)),
  obs = "Excluir MD")

# site 2
nakamura_site2_fa <- read_xlsx(
  file.path(
    nakamura,
    "Nakamura_BubengYunnanChina_MICROcosm.xlsx"),
  "fauna_abundance")

nakamura_site2_list <- read_xlsx(
  file.path(
    nakamura,
    "Nakamura_BubengYunnanChina_MICROcosm.xlsx"),
  "Fauna_morphospecies_list")

nakamura_site2_traits <- read_xlsx(
  file.path(
    nakamura,
    "Nakamura_BubengYunnanChina_MICROcosm.xlsx"),
  "Fauna_traits")[1:16,]

nakamura_site2_measures <- read_xlsx(
  file.path(
    nakamura,
    "Nakamura_BubengYunnanChina_MICROcosm.xlsx"),
  "measures_decomposition_geograph")

# abundance
nakamura_site2_fa[is.na(nakamura_site2_fa)] <- 0

nakamura_site2_fa_adjust <- nakamura_site2_fa %>%
  filter(!(Treatment %in% c("Natural forest Canopy", "Natural forest Midstory"))) %>%
  mutate(Treatment = str_replace_all(
    Treatment,c(
      "Natural forest Understory" = "Natural forest",
      "Managed forest Rubber plantation" = "Managed forest"))) 

# colSums(nakamura_site2_fa_adjust[,-c(1:2)])  

# list
head(nakamura_site2_list)

# traits
View(nakamura_site2_traits)

# measures
# rename variables with data dictionary
nakamura_site2_measures_adjust <- 
  nakamura_site2_measures %>%
  filter(!(Treatment %in% c("Natural forest Canopy", "Natural forest Midstory"))) %>%
  mutate("biomass_cotton_stripes_outside_bag_after (mg)" = NA,
         "biomass_cotton_stripes_outside_bag_before (mg)" = NA,
         "Natural tree hole.1" = NA,
         "Natural tree hole.2" = NA) %>%
  rename(Remaining_water_volume = "Final water volume (ml)") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(all_of(cols_to_convert_g_to_mg), ~ .x * 1000)) %>%
  mutate(treatment = str_replace_all(
    treatment,c(
      "Natural forest Understory" = "Natural forest",
      "Managed forest Rubber plantation" = "Managed forest"))) 

# join works!
#View(left_join(nakamura_site2_list,
#               nakamura_site2_traits,
#          by = "Morfospecies_name"))

nakamura_site2_data <- tibble(
  researcher = "Nakamura",
  locality = "Bubeng_China", 
  roof_treatment = NA,
  abundance = list(tibble(nakamura_site2_fa_adjust)),
  list = list(tibble(nakamura_site2_list)),
  traits=list(tibble(nakamura_site2_traits)),
  measures=list(tibble(nakamura_site2_measures_adjust)),
  obs = "Falta tamanho do corpo")

# save .RData from romero
save(nakamura_site1_data,
     nakamura_site2_data,
     file = file.path(nakamura,
                 "nakamura_china.RData"))

#--- MD34 & MD69 --- Nock ----
nock_canada <- file.path(local_directory,
                    "Nock")

#nock_fa <- read_xlsx(
#  file.path(
#    nock_canada,
#    "CA_NOCK_EMEND.2.xlsx"),
#  "fauna_abundance")
#
#nock_list <- read_xlsx(
#  file.path(
#    nock_canada,
#    "CA_NOCK_EMEND.2.xlsx"),
#  "Fauna_morphospecies_list")
#
#nock_traits <- read_xlsx(
#  file.path(
#    nock_canada,
#    "CA_NOCK_EMEND.2.xlsx"),
#  "Fauna_traits")

nock_measures <- read_xlsx(
  file.path(
    nock_canada,
    "CA_NOCK_EMEND.2_NEW.xlsx"),
  "measures_decomposition_geograph")

#names(nock_Detritus)
# measures
nock_measures_adjust <- nock_measures %>%
  rename("Remaining_water_volume" = "water_volume") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric))

# esse autor rodou o experimento em duas areas manejadas, mas apenas uma area
# controle (area natural), aqui vamos comparar com um teste-t se ha diferenca
# no peso dos cottomstrip no final do experimento, se nao houver, vamos selecionar
# 10 amostras aleatorias. Se houver, serao separados em dois MD's
#nock_measures_adjust_mf1 <- nock_measures_adjust %>%
#  filter(treatment == "Managed forest") %>%
#  select("replicate", "coarse_after_mg","fine_after_mg","outside_after_mg")
#
#nock_measures_adjust_mf2 <- nock_measures_adjust %>%
#  filter(treatment == "Managed forest 2") %>%
#  select("replicate", "coarse_after_mg","fine_after_mg","outside_after_mg")
#
#df_comparation_nock <- left_join(nock_measures_adjust_mf1,
#          nock_measures_adjust_mf2,
#          by = "replicate") %>%
#  filter(replicate != "pot.1")
#
#t.test(df_comparation_nock$coarse_after_mg.x,
#       df_comparation_nock$coarse_after_mg.y)
#
#t.test(df_comparation_nock$fine_after_mg.x,
#       df_comparation_nock$fine_after_mg.y)
#
#t.test(df_comparation_nock$outside_after_mg.x,
#       df_comparation_nock$outside_after_mg.y)

# visto que ha diferenca significativa entre no peso dos cottomstrip depois
# do experimento, iremos duplicar esse dataset para que tenhamos duas areas de 
# referencia para comparar o experimento manejado. 
nock_measures_adjust_mf1 <- nock_measures_adjust %>%
  filter(treatment == "Managed forest" | treatment == "Natural forest")

nock_measures_adjust_mf2 <- nock_measures_adjust %>%
  filter(treatment == "Managed forest 2" | treatment == "Natural forest")

nock_data_mf1 <- tibble(
  researcher = "Nock",
  locality = "Alberta_Canada",
  roof_treatment = NA,
  abundance = NA, #list(tibble(knapp_fa)),
  list = NA, #list(tibble(knapp_list)),
  traits= NA, #list(tibble(knapp_traits)),
  measures=list(tibble(nock_measures_adjust_mf1)),
  obs = "O experimento 'Natural forest' é o mesmo do MD69")

nock_data_mf2 <- tibble(
  researcher = "Nock",
  locality = "Alberta_Canada",
  roof_treatment = NA,
  abundance = NA, #list(tibble(knapp_fa)),
  list = NA, #list(tibble(knapp_list)),
  traits= NA, #list(tibble(knapp_traits)),
  measures=list(tibble(nock_measures_adjust_mf2)),
  obs = "O experimento 'Natural forest' é o mesmo do MD34")

save(nock_data_mf1,
     nock_data_mf2,
     file = file.path(nock_canada,
                 "nock_canada.RData"))

#--- MD35 & MD36 --- Pavel Kratina_UK ----
pavel_uk <- file.path(local_directory,
                   "Pavel Kratina_UK")

pavel_site1_fa <- read_xlsx(
  file.path(
    pavel_uk,
    "Microcosm_UK_Kratina.xlsx"),
  "fauna_abundance")

pavel_site1_list <- read_xlsx(
  file.path(
    pavel_uk,
    "Microcosm_UK_Kratina.xlsx"),
  "Fauna_morphospecies_list")

pavel_site1_traits <- read_xlsx(
  file.path(
    pavel_uk,
    "Microcosm_UK_Kratina.xlsx"),
  "Fauna_traits")

pavel_site1_measures <- read_xlsx(
  file.path(
    pavel_uk,
    "Microcosm_UK_Kratina.xlsx"),
  "measures_decomposition_geograph")

# abundance
pavel_site1_fa <- pavel_site1_fa %>%
  mutate(across(-c(Treatment, Replicate), as.numeric)) %>%
  mutate(across(-c(Treatment, Replicate), ~ replace_na(., 0))) %>%
  filter(Treatment != "Natural forest" | Replicate != "pot.1") %>%
  filter(Treatment != "Natural forest" | Replicate != "pot.2") %>%
  filter(Treatment != "Natural forest" | Replicate != "pot.3")

# list
pavel_site1_list

# traits
pavel_site1_traits

# measures
pavel_site1_measures <- pavel_site1_measures %>%
  mutate(across(everything(), ~ gsub("<", "", .))) %>%
  filter(Treatment != "Natural forest" | Replicate != "pot.1") %>%
  filter(Treatment != "Natural forest" | Replicate != "pot.2") %>%
  filter(Treatment != "Natural forest" | Replicate != "pot.3") %>%
  select(-"...27", -"DOC" , -"TP") %>%
  rename("Remaining_water_volume" = "Final volume of water") %>%
  mutate("Natural tree hole.1" = NA,
         "Natural tree hole.2" = NA) %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) 

# site 2
pavel_site2_fa <- read_xlsx(
  file.path(
    pavel_uk,
    "PavelKratina_Site_UK.xlsx"),
  "fauna_abundance")

pavel_site2_list <- read_xlsx(
  file.path(
    pavel_uk,
    "PavelKratina_Site_UK.xlsx"),
  "Fauna_morphospecies_list")

pavel_site2_traits <- read_xlsx(
  file.path(
    pavel_uk,
    "PavelKratina_Site_UK.xlsx"),
  "Fauna_traits")

pavel_site2_measures <- read_xlsx(
  file.path(
    pavel_uk,
    "PavelKratina_Site_UK.xlsx"),
  "measures_decomposition_geograph")

# abundance
#table(pavel_site2_fa$Treatment) # managed forest aparece 20x e natural forest 15
#
#pavel_site2_fa <- pavel_site2_fa %>%
#  mutate(across(-c(Treatment, Replicate), as.numeric)) %>%
#  mutate(across(-c(Treatment, Replicate), ~ replace_na(., 0)))
#
## list
#pavel_site2_list
#
## traits
#pavel_site2_traits$total_length <- as.double(pavel_site2_traits$total_length)
#
## measures
#pavel_site2_measures <- pavel_site2_measures %>%
#  rename("Remaining_water_volume" = "final volume (ml)") %>%
#  rename(all_of(dict_names)) %>%
#  mutate(across(all_of(var_char), as.character)) %>%
#  mutate(across(all_of(var_numeric), as.numeric)) 

pavel_site1_data <- tibble(
  researcher = "PavelKratina",
  locality = "UnitedKingdom",
  roof_treatment = NA,
  abundance = list(tibble(pavel_site1_fa)),
  list = list(tibble(pavel_site1_list)),
  traits= list(tibble(pavel_site1_traits)),
  measures=list(tibble(pavel_site1_measures)),
  obs = "Sem dados dos algodoes inteiros")

pavel_site2_data <- tibble(
  researcher = "PavelKratina",
  locality = "UnitedKingdom",
  roof_treatment = NA,
  abundance = list(tibble(pavel_site2_fa)),
  list = list(tibble(pavel_site2_list)),
  traits= list(tibble(pavel_site2_traits)),
  measures=list(tibble(pavel_site2_measures)),
  obs = "Excluir MD")

#View(pavel_site2_data)
save(pavel_site1_data,
     pavel_site2_data,
     file = file.path(pavel_uk,
                 "pavel_uk.RData"))


#--- MD37 --- Petterman_Austria ----
petermann_austria <- file.path(local_directory,
                        "Petermann_Austria"
                 )

pettermann_fa <- read_xlsx(
  file.path(
    petermann_austria,
    "Petermann.Austria Wienerwald_Global experiment_v2.xlsx"),
  "fauna_abundance")

pettermann_list <- read_xlsx(
  file.path(
    petermann_austria,
    "Petermann.Austria Wienerwald_Global experiment_v2.xlsx"),
  "Fauna_morphospecies_list")

pettermann_traits <- read_xlsx(
  file.path(
    petermann_austria,
    "Petermann.Austria Wienerwald_Global experiment_v2.xlsx"),
  "Fauna_traits")

pettermann_measures <- read_xlsx(
  file.path(
    petermann_austria,
    "Petermann.Austria Wienerwald_Global experiment_v2.xlsx"),
  "measures_decomposition")

# abundance
pettermann_fa <- pettermann_fa %>%
  mutate(Treatment = str_replace_all(
    Treatment,c(
      "managed" = "Managed forest",
      "natural" = "Natural forest"))) 

# list
glimpse(pettermann_list)

# traits
pettermann_traits$total_length <-as.double(pettermann_traits$total_length)

# measures
glimpse(pettermann_measures)

pettermann_measures_adjust <- pettermann_measures %>%
  rename(Remaining_water_volume = "Final water volume (ml)") %>% 
  rename(all_of(dict_names)) %>%
  mutate(
    outside_after_mg = if_else(treatment == "Natural forest" & replicate == "pot.5",
                      "0.097",
                      outside_after_mg)) %>%
  mutate(
    outside_after_mg = if_else(treatment == "Managed forest" & replicate == "pot.10",
                               "0.0904",
                               outside_after_mg)) %>%
  mutate(
    dissolved_O2 = if_else(treatment == "Managed forest" & replicate == "pot.2",
                               0.93,
                           dissolved_O2)) %>%
  mutate(
    dissolved_O2 = if_else(treatment == "Managed forest" & replicate == "pot.5",
                               6.0,
                               dissolved_O2)) %>%
  mutate(across(everything(), ~ gsub(",", ".", .))) %>%
  mutate(across(everything(), ~ gsub("/out", "", .))) %>%
  mutate(across(everything(), ~ gsub("/1missing", "", .))) %>%
  mutate(across(everything(), ~ gsub("/1 missing", "", .))) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(
  outside_after_mg = if_else(treatment == "Natural forest" & replicate == "pot.1",
                             0.1982,
                             outside_after_mg)) %>%
  mutate(
    outside_after_mg = if_else(treatment == "Natural forest" & replicate == "pot.5",
                               0.0904*2,
                               outside_after_mg)) %>%
  mutate(
    outside_after_mg = if_else(treatment == "Natural forest" & replicate == "pot.10",
                               0.0335*2,
                               outside_after_mg)) %>%
  mutate(
    outside_after_mg = if_else(treatment == "Managed forest" & replicate == "pot.3",
                               0.0943*2,
                               outside_after_mg)) %>%
  mutate(
    outside_after_mg = if_else(treatment == "Managed forest" & replicate == "pot.10",
                               0.0904*2,
                               outside_after_mg)) %>%
  mutate(across(all_of(c("outside_after_mg", "outside_before_mg")), ~ .x * 1000)) %>%
  mutate(natural_tree_1 = NA) %>%
  mutate(natural_tree_2 = NA)

View(pettermann_measures_adjust)

pettermann_data <- tibble(
  researcher = "Petermann",
  locality = "Austria",
  roof_treatment = NA,
  abundance = list(tibble(pettermann_fa)),
  list = list(tibble(pettermann_list)),
  traits= list(tibble(pettermann_traits)),
  measures=list(tibble(pettermann_measures_adjust)),
  obs = "")

save(pettermann_data,
     file = file.path(petermann_austria,
                 "petermann_austria.RData"))

#--- MD38 & MD39 & MD40 --- Renan_Chapeco ----
renan_br <- file.path(local_directory,
                "Renan_Chapeco")

renan_alloch_fa <- read_xlsx(
  file.path(
    renan_br,
    "Allochthonous_Chapeco_BR.xlsx"),
  "fauna_abundance_allochthonous")

renan_alloch_list <- read_xlsx(
  file.path(
    renan_br,
    "Allochthonous_Chapeco_BR.xlsx"),
  "Fauna_morphospecies_list")

renan_alloch_traits <- read_xlsx(
  file.path(
    renan_br,
    "Allochthonous_Chapeco_BR.xlsx"),
  "Fauna_traits")

renan_alloch_measures <- read_xlsx(
  file.path(
    renan_br,
    "Allochthonous_Chapeco_BR.xlsx"),
  "measures_decomposition_allochth")

#abundance
renan_alloch_fa

#list
# na lista esta ChirAnomidae e precisa ser ChirOnomidae
renan_alloch_list <- mutate_all(renan_alloch_list, ~(replace(., .=="*", NA))) %>%
  mutate(Morfospecies_name = case_when(
    Morfospecies_name == "Chiranomidae_1" ~ "Chironomidae_1",
    TRUE ~ Morfospecies_name  # Caso padrão, mantém o valor original
  ))

#traits
renan_alloch_traits

#measures
renan_alloch_measures_adjust <- renan_alloch_measures %>% 
  mutate("dissolved_CO2" = NA) %>%
  rename("dissolved_O2" = "dissolved_O2 mg/L",
         "turbidity" = "turbidity NTU",
         "Tree dbh" = "Tree dbh (cm)",
         "canopy openness" = "canopy openness %") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  rename("outside_after_mg" = "outside_before_mg",
         "outside_before_mg" = "outside_after_mg") %>%
  mutate(across(all_of(cols_to_convert_g_to_mg), ~ .x * 1000))

#vertical_chapeco
renan_vertical_fa <- read_xlsx(
  file.path(
    renan_br,
    "Vertical_Chapeco_BR.xlsx"),
  "fauna_abundance_vertical")

renan_vertical_list <- read_xlsx(
  file.path(
    renan_br,
    "vertical_Chapeco_BR.xlsx"),
  "Fauna_morphospecies_list")

renan_vertical_traits <- read_xlsx(
  file.path(
    renan_br,
    "vertical_Chapeco_BR.xlsx"),
  "Fauna_traits")

renan_vertical_measures <- read_xlsx(
  file.path(
    renan_br,
    "vertical_Chapeco_BR.xlsx"),
  "measures_decomposition_vertical")

#abundance
colSums(renan_vertical_fa[,-c(1:2)])

#list
renan_vertical_list <- mutate_all(
  renan_vertical_list, ~(replace(., .=="*", NA))) %>%
  mutate(Morfospecies_name = case_when(
    Morfospecies_name == "Chiranomidae_1" ~ "Chironomidae_1",
    TRUE ~ Morfospecies_name  # Caso padrão, mantém o valor original
  ))

#traits
renan_vertical_traits

#measures
renan_vertical_measures_adjust <- renan_vertical_measures %>% 
  mutate("dissolved_CO2" = NA) %>%
  rename("Tree dbh" = "Tree dbh (cm)",
        "canopy openness" = "canopy openness %") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  rename("outside_after_mg" = "outside_before_mg",
         "outside_before_mg" = "outside_after_mg") %>%
  mutate(across(all_of(cols_to_convert_g_to_mg), ~ .x * 1000)) 

#names(renan_vertical_measures)

#basic_chapeco
renan_basic_fa <- read_xlsx(
  file.path(
    renan_br,
    "Basic_Chapeco_BR.xlsx"),
  "fauna_abundance_basic")

renan_basic_list <- read_xlsx(
  file.path(
    renan_br,
    "Basic_Chapeco_BR.xlsx"),
  "Fauna_morphospecies_list")

renan_basic_traits <- read_xlsx(
  file.path(
    renan_br,
    "Basic_Chapeco_BR.xlsx"),
  "Fauna_traits")

renan_basic_measures <- read_xlsx(
  file.path(
    renan_br,
    "Basic_Chapeco_BR.xlsx"),
  "measures_decomposition_basic")

#abundance
colSums(renan_basic_fa[,-c(1:2)])

renan_basic_fa_adjust <- renan_basic_fa %>%
  mutate(Corculionidae_adulto_1 = Curculionidae_1 + Corculionidae_adulto_1) %>%
  mutate(Elmidae_adulto_1 = Elmidae_1 + Elmidae_adulto_1) 

colSums(renan_basic_fa_adjust[,-c(1:2)])

#list
renan_basic_list <- mutate_all(renan_basic_list, ~(replace(., .=="*", NA))) %>%
  bind_rows(tibble(Morfospecies_name = "Ceratopogonidae_3")) %>%
  bind_rows(tibble(Morfospecies_name = "Psychoda_2")) %>%
  mutate(Morfospecies_name = case_when(
    Morfospecies_name == "Chiranomidae_1" ~ "Chironomidae_1",
    Morfospecies_name == "Planorbiidae_1" ~ "Planorbidae_1",
    TRUE ~ Morfospecies_name  
  )) %>%
  filter(Morfospecies_name != "Collembola_3")

#traits
renan_basic_traits <- renan_basic_traits %>%
  mutate(Morfospecies_name = case_when(
    Morfospecies_name == "Corculionidae_adulto_1" ~ "Curculionidae_adulto_1",
    TRUE ~ Morfospecies_name  
  ))

#measures
renan_basic_measures_adjust <- renan_basic_measures %>% 
  mutate("dissolved_CO2" = NA) %>%
  rename("Tree dbh" = "Tree dbh (cm)",
         "canopy openness" = "canopy openness %") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  rename("outside_after_mg" = "outside_before_mg",
         "outside_before_mg" = "outside_after_mg") %>%
  mutate(across(all_of(cols_to_convert_g_to_mg), ~ .x * 1000)) 

#data save
renan_alloch_data <- tibble(
  researcher = "Renan_BR",
  locality = "Chapeco_Brazil", 
  roof_treatment = 1,
  abundance = list(tibble(renan_alloch_fa)),
  list = list(tibble(renan_alloch_list)),
  traits=list(tibble(renan_alloch_traits)),
  measures=list(tibble(renan_alloch_measures_adjust)),
  obs = NA)

renan_vertical_data <- tibble(
  researcher = "Renan_BR",
  locality = "Chapeco_Brazil", 
  roof_treatment = 0,
  heigth_treatment = 2, 
  abundance = list(tibble(renan_vertical_fa)),
  list = list(tibble(renan_vertical_list)),
  traits=list(tibble(renan_vertical_traits)),
  measures=list(tibble(renan_vertical_measures_adjust)),
  obs = NA)

renan_basic_data <- tibble(
  researcher = "Renan_BR",
  locality = "Chapeco_Brazil", 
  roof_treatment = 0,
  heigth_treatment = 1,
  abundance = list(tibble(renan_basic_fa_adjust)),
  list = list(tibble(renan_basic_list)),
  traits=list(tibble(renan_basic_traits)),
  measures=list(tibble(renan_basic_measures_adjust)),
  obs = NA)

save(renan_alloch_data,
     renan_vertical_data,
     renan_basic_data,
     file = file.path(renan_br,
                 "renan_br.RData"))

#--- MD41 --- Rodrigo_Argentina ----
rodrigo_argentina <- file.path(local_directory,
                 "Rodrigo_Argentina")

rodrigo_fa <- read_xlsx(
  file.path(
    rodrigo_argentina,
    "FreireRodrigo_Site.1.xlsx"),
  "fauna_abundance")

rodrigo_list <- read_xlsx(
  file.path(
    rodrigo_argentina,
    "FreireRodrigo_Site.1.xlsx"),
  "Fauna_morphospecies_list")

rodrigo_traits <- read_xlsx(
  file.path(
    rodrigo_argentina,
    "FreireRodrigo_Site.1.xlsx"),
  "Fauna_traits")

rodrigo_measures <- read_xlsx(
  file.path(
    rodrigo_argentina,
    "FreireRodrigo_Site.1.xlsx"),
  "measures_decomposition_geograph")

# abundance
rodrigo_fa_adjust <- rodrigo_fa %>%
  filter(Replicate != "pot.3" | Treatment != "Natural forest") %>%
  mutate(across(-c(Treatment, Replicate), as.numeric)) %>%
  mutate(across(-c(Treatment, Replicate), ~ replace_na(., 0)))

# list
head(rodrigo_list)

# traits
rodrigo_traits <- rodrigo_traits %>%
  mutate(total_length = case_when(
    total_length == "3,755/8,975" ~ 3.755,
    TRUE ~ as.double(total_length)
  ))

# measures
#View(rodrigo_measures)
rodrigo_measures_adjust <- rodrigo_measures %>%
  filter(Replicate != "pot.3" | Treatment != "Natural forest") %>%
  rename("Remaining_water_volume" = "Water.volume.final") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) 

rodrigo_data <- tibble(
  researcher = "RodrigoFreire",
  locality = "ParanaDelta_Argentina",
  roof_treatment = NA,
  abundance = list(tibble(rodrigo_fa)),
  list = list(tibble(rodrigo_list)),
  traits= list(tibble(rodrigo_traits)),
  measures=list(tibble(rodrigo_measures_adjust)),
  obs = "")

save(rodrigo_data,
     file = file.path(rodrigo_argentina,
                 "rodrigo_data.RData"))

#--- MD42 --- Sedney_Francis_Filipinas ----
sedney_filipinas <- file.path(local_directory,
                          "Sedney_Francis_Filipinas")

sedney_fa <- read_xlsx(
  file.path(
    sedney_filipinas,
    "Magbanua_Philippines_MICROcosms revised data.xlsx"),
  "fauna_abundance")

sedney_list <- read_xlsx(
  file.path(
    sedney_filipinas,
    "Magbanua_Philippines_MICROcosms revised data.xlsx"),
  "Fauna_morphospecies_list")

sedney_traits <- read_xlsx(
  file.path(
    sedney_filipinas,
    "Magbanua_Philippines_MICROcosms revised data.xlsx"),
  "Fauna_traits")

sedney_measures <- read_xlsx(
  file.path(
    sedney_filipinas,
    "Magbanua_Philippines_MICROcosms revised data.xlsx"),
  "measures_decomposition_geograph")

# abundance
#View(sedney_fa)

# list
head(sedney_list)

# traits
head(sedney_traits)

# measures
sedney_measures_adjust <- sedney_measures %>% 
  rename(Remaining_water_volume = "Final water volume (ml)") %>%
  #mutate_all(~(replace(., .=="ND", NA))) %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(all_of(cols_to_convert_g_to_mg), ~ .x * 1000))

sedney_data <- tibble(
  researcher = "Sedney",
  locality = "Mt.MakilingForest_Filipinas",
  roof_treatment = NA,
  abundance = list(tibble(sedney_fa)),
  list = list(tibble(sedney_list)),
  traits= list(tibble(sedney_traits)),
  measures=list(tibble(sedney_measures_adjust)),
  obs = NA)

save(sedney_data,
     file = file.path(sedney_filipinas,
                 "sedney_filipinas.RData"))

#--- MD43 & MD44 --- Srivastava_Canada ----
srivastava_canada <- file.path(local_directory,
                         "Srivastava_Canada")

srivastava_site1_fa <- read_xlsx(
  file.path(
    srivastava_canada,
    "Srivastava_site1.xlsx"),
  "fauna_abundance")

srivastava_site1_list <- read_xlsx(
  file.path(
    srivastava_canada,
    "Srivastava_site1.xlsx"),
  "Fauna_morphospecies_list")

srivastava_site1_traits <- read_xlsx(
  file.path(
    srivastava_canada,
    "Srivastava_site1.xlsx"),
  "Fauna_traits")

srivastava_site1_measures <- read_xlsx(
  file.path(
    srivastava_canada,
    "Srivastava_site1.xlsx"),
  "measures_decomposition_geograph")

# abundance
srivastava_site1_fa <- srivastava_site1_fa %>%
  filter(Treatment != "Natural forest" | Replicate != "pot.10") %>%
  select(-treehole_number)

# list
head(srivastava_site1_list)

# traits
srivastava_site1_traits <- srivastava_site1_traits %>%
  mutate(total_length = case_when(
    total_length == "3 - 3.5 mm" ~ 3.25,
    total_length == "3,5 mm" ~ 3.5,
    TRUE ~ as.double(total_length)
  ))

# measures
srivastava_site1_measures_adjust <- srivastava_site1_measures %>% 
  rename(Remaining_water_volume = "water_volume_mL") %>%
  rename(all_of(dict_names)) %>%
  filter(treatment != "Natural forest" | replicate != "pot.10") %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(7, ~ case_when(
    row_number() %in% c(10, 12, 18) ~ . * 2,# Multiplica por 2 nas linhas 10, 12 e 18
    TRUE ~ .                                # Mantem os outros valores inalterados
  )))

# site 2
srivastava_site2_fa <- read_xlsx(
  file.path(
    srivastava_canada,
    "Srivastava_site2.xlsx"),
  "fauna_abundance")

srivastava_site2_list <- read_xlsx(
  file.path(
    srivastava_canada,
    "Srivastava_site2.xlsx"),
  "Fauna_morphospecies_list")

srivastava_site2_traits <- read_xlsx(
  file.path(
    srivastava_canada,
    "Srivastava_site2.xlsx"),
  "Fauna_traits")

srivastava_site2_measures <- read_xlsx(
  file.path(
    srivastava_canada,
    "Srivastava_site2.xlsx"),
  "measures_decomposition_geograph")

# abundance
head(srivastava_site2_fa)

# list
head(srivastava_site2_list)

# traits
srivastava_site2_traits <- srivastava_site2_traits %>%
  mutate(total_length = case_when(
    total_length == "3 mm" ~ 3,
    total_length == "4 mm" ~ 4,
    TRUE ~ as.double(total_length)
  ))

# measures
srivastava_site2_measures_adjust <- srivastava_site2_measures %>% 
  rename(Remaining_water_volume = "water_volume_mL") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(7, ~ if_else(row_number() == 20, . * 2, .)))

srivastava_site1_data <- tibble(
  researcher = "Srivastava",
  locality = "Vancouver_Canada",
  roof_treatment = NA,
  abundance = list(tibble(srivastava_site1_fa)),
  list = list(tibble(srivastava_site1_list)),
  traits= list(tibble(srivastava_site1_traits)),
  measures=list(tibble(srivastava_site1_measures_adjust)),
  obs = NA)

srivastava_site2_data <- tibble(
  researcher = "Srivastava",
  locality = "Vancouver_Canada",
  roof_treatment = NA,
  abundance = list(tibble(srivastava_site2_fa)),
  list = list(tibble(srivastava_site2_list)),
  traits= list(tibble(srivastava_site2_traits)),
  measures=list(tibble(srivastava_site2_measures_adjust)),
  obs = "Valor errado pot.10 managed forest")

save(srivastava_site1_data,
     srivastava_site2_data,
     file = file.path(srivastava_canada,
                 "srivastava_canada.RData"))

#--- MD45 --- Sweet_UK ----
sweet_uk <- file.path(local_directory,
                  "Sweet_UK")

sweet_fa <- read_xlsx(
  file.path(
    sweet_uk,
    "Sweet Site UK_NEW.xlsx"),
  "fauna_abundance")

sweet_list <- read_xlsx(
  file.path(
    sweet_uk,
    "Sweet Site UK_NEW.xlsx"),
  "Fauna_morphospecies_list")

sweet_traits <- read_xlsx(
  file.path(
    sweet_uk,
    "Sweet Site UK_NEW.xlsx"),
  "Fauna_traits")

sweet_measures <- read_xlsx(
  file.path(
    sweet_uk,
    "Sweet Site UK_NEW.xlsx"),
  "measures_decomposition_geograph")

# abundance
head(sweet_fa)

# list
names(sweet_list)

# traits
sweet_traits <- sweet_traits %>%
  rename("Morfospecies_name" = "Morphospecies_name") %>%
  rename("total_length" = "total_length (mm)")

# measures
sweet_measures_adjust <- sweet_measures %>% 
  select(-"Water volume before (ml)") %>%
  rename(Remaining_water_volume = "Water volume after (ml)") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) 

sweet_data <- tibble(
  researcher = "Sweet",
  locality = "Midlands_UnitedKingdom",
  roof_treatment = NA,
  abundance = list(tibble(sweet_fa)),
  list = list(tibble(sweet_list)),
  traits= list(tibble(sweet_traits)),
  measures=list(tibble(sweet_measures_adjust)),
  obs = NA)

save(sweet_data,
     file = file.path(sweet_uk,
                 "sweet_uk.RData"))

#--- MD46 --- Thomas_Alemanha ----
thomas_alemanha <- file.path(local_directory,
                        "Thomas_Alemanha")

thomas_fa <- read_xlsx(
  file.path(
    thomas_alemanha,
    "Scheuerl_Site.2.xlsx"),
  "fauna_abundance")

thomas_list <- read_xlsx(
  file.path(
    thomas_alemanha,
    "Scheuerl_Site.2.xlsx"),
  "Fauna_morphospecies_list")

thomas_traits <- read_xlsx(
  file.path(
    thomas_alemanha,
    "Scheuerl_Site.2.xlsx"),
  "Fauna_traits_Scheuerl")

thomas_measures <- read_xlsx(
  file.path(
    thomas_alemanha,
    "Scheuerl_Site.2.xlsx"),
  "measures_decomposition_geograph")

# abundance
# Morphospecies 6 e 7 sao pupas e serao removidas no pós processamento
thomas_fa_adjust <- thomas_fa %>%
  mutate(Morphospecies.1 = Morphospecies.1 + Morphospecies.6) %>%
  mutate(Morphospecies.3 = Morphospecies.3 + Morphospecies.7) %>% 
  select(-"Morphospecies.15") # TODO precisa ajustar

# list
thomas_list 

# traits
thomas_traits$total_length <- as.double(thomas_traits$total_length)

# measures
thomas_measures_adjust <- thomas_measures %>% 
  mutate("Natural tree hole.1" = NA,
         "Natural tree hole.2" = NA) %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric))

thomas_data <- tibble(
  researcher = "Thomas",
  locality = "Bavaria_Alemanha",
  roof_treatment = NA,
  abundance = list(tibble(thomas_fa_adjust)),
  list = list(tibble(thomas_list)),
  traits= list(tibble(thomas_traits)),
  measures=list(tibble(thomas_measures_adjust)),
  obs = "Confirmar dados no data_log.txt")

save(thomas_data,
     file = file.path(thomas_alemanha,
                 "thomas_alemanha.RData"))

#--- MD52 --- Anikka Germany ----
anikka_alemanha <- file.path(local_directory,
                        "Annika_Germany")

anikka_fa <- read_xlsx(
  file.path(
    anikka_alemanha,
    "AnnikaBusse_BavarianForst_Germany.xlsx"),
  "fauna_abundance")

anikka_list <- read_xlsx(
  file.path(
    anikka_alemanha,
    "AnnikaBusse_BavarianForst_Germany.xlsx"),
  "Fauna_morphospecies_list")

anikka_traits <- read_xlsx(
  file.path(
    anikka_alemanha,
    "AnnikaBusse_BavarianForst_Germany.xlsx"),
  "Fauna_traits")

anikka_measures <- read_xlsx(
  file.path(
    anikka_alemanha,
    "AnnikaBusse_BavarianForst_Germany.xlsx"),
  "measures_decomposition_geograph")

# abundance
head(anikka_fa)

# list
head(anikka_list)

# traits
head(anikka_traits)

# measures
# TODO renomear colunas, mas pra isso, checar unidades de medida
anikka_measures_adjust <- anikka_measures %>%
  rename(dissolved_O2 = "dissolved_O2_mg_L") %>%
  rename(ammonium_concentration = "ammonium_concentration_mg_L") %>%
  rename(nitrate_concentration = "nitrate_concentration_mg_L") %>%
  rename("detritus dry mass (fine)" = "detritus dry mass (fine)_g") %>%
  rename("detritus dry mass (coarse)" = "detritus dry mass (coarse)_g") %>%
  rename("Tree dbh" = "Tree dbh_cm") %>%
  rename("Remaining_water_volume" = "remaining_water_volume_mL") %>%
  rename("Natural tree hole.1" = "Natural tree hole.1 (yes/no)") %>%
  rename("Natural tree hole.2" = "Natural tree hole.2 (number per hectare)") %>%
  rename("canopy openness" = "canopy openness end (%)") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) 

anikka_data <- tibble(
  researcher = "Anikka",
  locality = "Glashutte_Alemanha",
  roof_treatment = NA,
  abundance = list(tibble(anikka_fa)),
  list = list(tibble(anikka_list)),
  traits= list(tibble(anikka_traits)),
  measures=list(tibble(anikka_measures_adjust)),
  obs = NA)
#View(anikka_data)
save(anikka_data,
     file = file.path(anikka_alemanha,
                 "anikka_alemanha.RData"))


#--- MD53 --- Claas_New Zealand ----
claas_newzealand <- file.path(local_directory,
                        "Claas_NewZealand")

#claas_fa <- read_xlsx(
#  file.path(
#    claas_newzealand,
#    "ClaasDamken_Dunedin_NewZealand.xlsx"),
#  "fauna_abundance")
#
#claas_list <- read_xlsx(
#  file.path(
#    claas_newzealand,
#    "ClaasDamken_Dunedin_NewZealand.xlsx"),
#  "Fauna_morphospecies_list")
#
#claas_traits <- read_xlsx(
#  file.path(
#    claas_newzealand,
#    "ClaasDamken_Dunedin_NewZealand.xlsx"),
#  "Fauna_traits")[1:2,]

claas_measures <- read_xlsx(
  file.path(
    claas_newzealand,
    "ClaasDamken_Dunedin_NewZealand.xlsx"),
  "measures_decomposition_geograph")

# abundance
#claas_fa %>%
#  select(-ID_metal_tag, -notes1) %>%
#  mutate(across(-c(Treatment, Replicate), as.numeric))
  
# list
#head(claas_list)

# traits
#head(claas_traits)
#claas_traits$total_length <- as.double(claas_traits$total_length)

# measures
claas_measures_adjust <- claas_measures %>%
  mutate_all(~ifelse(. == "na", NA, .)) %>%
  rename("dissolved_O2" = "dissolved_O2_%",
         "Natural tree hole.1" = "Natural tree hole.1 (yes/no)",
         "Natural tree hole.2" = "Natural tree hole.2 (number per hectare)",
         "Remaining_water_volume" = "remaining_water_volume_sampling_mL") %>%
  mutate_all(~ifelse(. == "na", NA, .)) %>%
  rename(all_of(dict_names)) %>%
  mutate(tree_dbh = tree_dbh / pi) %>% 
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(
    coarse_after_mg = if_else(coarse_before_mg > coarse_after_mg, coarse_after_mg, NA_real_),
    fine_after_mg = if_else(fine_before_mg > fine_after_mg, fine_after_mg, NA_real_),
    outside_after_mg = if_else(outside_before_mg > outside_after_mg, outside_after_mg, NA_real_)
  ) 

#claas_measures_adjust %>% mutate(
#  coarse_comparison = coarse_before_mg > coarse_after_mg,
#  fine_comparison = fine_before_mg > fine_after_mg,
#  outside_comparison = outside_before_mg > outside_after_mg
#) %>%
#  select(treatment, replicate, coarse_comparison,
#         fine_comparison, outside_comparison)

claas_data <- tibble(
  researcher = "Claas",
  locality = "Dunedin_NewZealand",
  roof_treatment = NA,
  abundance = NA,
  list = NA,
  traits= NA,
  measures=list(tibble(claas_measures_adjust)),
  obs = NA)
#View(anikka_data)
save(claas_data,
     file = file.path(claas_newzealand,
                 "claas_newzealand.RData"))

#--- MD55 & MD56 --- Martin Gossner ----
martin_suica <- file.path(local_directory,
                         "MartinGossner_Holsetein")

martin_fa <- read_xlsx(
  file.path(
    martin_suica,
    "Hölstein_AB_22.11.2023_MartinGossner.xlsx"),
  "fauna_abundance")

martin_list <- read_xlsx(
  file.path(
    martin_suica,
    "Hölstein_AB_22.11.2023_MartinGossner.xlsx"),
  "Fauna_morphospecies_list")

martin_traits <- read_xlsx(
  file.path(
    martin_suica,
    "Hölstein_AB_22.11.2023_MartinGossner.xlsx"),
  "Fauna_traits")

martin_measures <- read_xlsx(
  file.path(
    martin_suica,
    "Hölstein_AB_22.11.2023_MartinGossner.xlsx"),
  "measures_decomposition_geograph")

# abundance
# martin_fa_2022 low = md do 2021
# martin_fa_2022 middle = md do 2022

#martin_fa_2021 <- martin_fa %>% filter(Year == "2021") %>%
#  relocate(c(SampleID, Stratum, Year), .after = Replicate) %>%
#  select(-Morphospecies.1, -Morphospecies.4,
#         -Morphospecies.5, -Morphospecies.11)

#colSums(martin_fa_2021[,-c(1:5)]) 
# 0 Morphospecies.1, Morphospecies.4,
# 0 Morphospecies.5, Morphospecies.11

martin_fa_2022 <- martin_fa %>% filter(Year == "2022") %>%
  relocate(c(SampleID, Stratum, Year), .after = Replicate) %>%
  select(-Morphospecies.12, -Morphospecies.7) # ausentes
#colSums(martin_fa_2022[,-c(1:5)]) 
# 0 Morphospecies.7 and Morphospecies.12

martin_fa_low <- martin_fa_2022 %>% 
  filter(Stratum == "low") %>%
  select(-Stratum, -Year,-SampleID, -Morphospecies.10)
#colSums(martin_fa_low[,-c(1:3)]) 
# 0 Morphospecies.10

martin_fa_middle <- martin_fa_2022 %>%
  filter(Stratum == "middle") %>%
  select(-Stratum, -Year,-SampleID, -Morphospecies.4,
         -Morphospecies.5,  -Morphospecies.6, -Morphospecies.11)
#colSums(martin_fa_middle[,-c(1:3)]) 
# 0 Morphospecies.4, Morphospecies.5, Morphospecies.6, Morphospecies.11

# list
filter_species_low <- c("Morphospecies.7","Morphospecies.10",
                        "Morphospecies.12")

filter_species_middle <- c("Morphospecies.7","Morphospecies.4",
                           "Morphospecies.5","Morphospecies.6",
                           "Morphospecies.11", "Morphospecies.12")

#martin_list_2021 <- martin_list %>% filter(
#  Morfospecies_name != "Morphospecies.1" &
#  Morfospecies_name != "Morphospecies.4" &
#  Morfospecies_name != "Morphospecies.5" &
#  Morfospecies_name != "Morphospecies.11"
#  )

martin_list_low <- martin_list %>% 
  filter(!(Morfospecies_name %in% filter_species_low)) 
  
martin_list_middle <- martin_list %>% 
  filter(!(Morfospecies_name %in% filter_species_middle)) 

# traits
martin_traits_low <- martin_traits %>% 
  filter(!(Morfospecies_name %in% filter_species_low)) 

martin_traits_middle <- martin_traits %>% 
  filter(!(Morfospecies_name %in% filter_species_middle)) 

# measures
#martin_measures_2021 <- martin_measures %>%
#  filter(year == 2021) %>%
#  rename(dissolved_O2 = "dissolved_O2_mg_L") %>%
#  rename(CDOM = "CDOM_µg_L") %>%
#  rename(turbidity = "turbidity_NTU") %>%
#  rename(ammonium_concentration = "ammonium_concentration_mg_L") %>%
#  rename(nitrate_concentration = "nitrate_concentration_mg_L") %>%
#  rename("chlorophyll-a" = "chlorophyll-a_µg_L") %>%
#  rename("canopy openness" = "canopy openness beginning (%)") %>%
#  rename("Tree dbh" = "Tree dbh_cm") %>%
#  rename("detritus dry mass (fine)" = "detritus_dry_mass_(fine)_0.25-0.5mm_g") %>%
#  rename("detritus dry mass (coarse)" = "detritus_dry_mass_(coarse)_>0.5mm_g") %>%
#  rename("Remaining_water_volume" = "remaining_water_volume_mL") %>%
#  rename("Natural tree hole.1" = "Natural tree hole.1 (yes/no)") %>%
#  rename("Natural tree hole.2" = "Natural tree hole.2 (number per hectare)") %>%
#  rename(all_of(dict_names)) %>%
#  mutate(across(all_of(var_char), as.character)) %>%
#  mutate(across(all_of(var_numeric), as.numeric)) 

martin_measures_low <- martin_measures %>%
  filter(year == 2022) %>% # apenas os de 2022
  mutate(SampleID = sub(".*_", "", ID)) %>% 
  filter(grepl("_L", ID)) %>% # os low que sobram sao equivalentes, pq os outros sao Upper e Middle
  relocate(SampleID, .after = Replicate) %>%
  rename(dissolved_O2 = "dissolved_O2_mg_L") %>%
  rename(CDOM = "CDOM_µg_L") %>%
  rename(turbidity = "turbidity_NTU") %>%
  rename(ammonium_concentration = "ammonium_concentration_mg_L") %>%
  rename(nitrate_concentration = "nitrate_concentration_mg_L") %>%
  rename("chlorophyll-a" = "chlorophyll-a_µg_L") %>%
  rename("canopy openness" = "canopy openness beginning (%)") %>%
  rename("Tree dbh" = "Tree dbh_cm") %>%
  rename("detritus dry mass (fine)" = "detritus_dry_mass_(fine)_0.25-0.5mm_g") %>%
  rename("detritus dry mass (coarse)" = "detritus_dry_mass_(coarse)_>0.5mm_g") %>%
  rename("Remaining_water_volume" = "remaining_water_volume_mL") %>%
  rename("Natural tree hole.1" = "Natural tree hole.1 (yes/no)") %>%
  rename("Natural tree hole.2" = "Natural tree hole.2 (number per hectare)") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) 

martin_measures_middle <- martin_measures %>%
  filter(year == 2022) %>% # apenas os de 2022
  mutate(SampleID = sub(".*_", "", ID)) %>% # criando sampleid que tem na abundance
  filter(grepl("_M", ID)) %>% # Middle
  relocate(SampleID, .after = Replicate) %>%
  rename(dissolved_O2 = "dissolved_O2_mg_L") %>%
  rename(CDOM = "CDOM_µg_L") %>%
  rename(turbidity = "turbidity_NTU") %>%
  rename(ammonium_concentration = "ammonium_concentration_mg_L") %>%
  rename(nitrate_concentration = "nitrate_concentration_mg_L") %>%
  rename("chlorophyll-a" = "chlorophyll-a_µg_L") %>%
  rename("canopy openness" = "canopy openness beginning (%)") %>%
  rename("Tree dbh" = "Tree dbh_cm") %>%
  rename("detritus dry mass (fine)" = "detritus_dry_mass_(fine)_0.25-0.5mm_g") %>%
  rename("detritus dry mass (coarse)" = "detritus_dry_mass_(coarse)_>0.5mm_g") %>%
  rename("Remaining_water_volume" = "remaining_water_volume_mL") %>%
  rename("Natural tree hole.1" = "Natural tree hole.1 (yes/no)") %>%
  rename("Natural tree hole.2" = "Natural tree hole.2 (number per hectare)") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric))

martin_data_low <- tibble(
  #ID = "MD55", 
  researcher = "MartinGossner",
  locality = "Holstein_Switzerland",
  roof_treatment = NA,
  heigth_treatment = 1,
  abundance = list(tibble(martin_fa_low)),
  list = list(tibble(martin_list_low)),
  traits= list(tibble(martin_traits_low)),
  measures=list(tibble(martin_measures_low)),
  obs = "Unidades de medidas estao nas colunas originais")

martin_data_middle <- tibble(
  #ID = "MD56", 
  researcher = "MartinGossner",
  locality = "Holstein_Switzerland",
  roof_treatment = NA,
  heigth_treatment = 2,
  abundance = list(tibble(martin_fa_middle)),
  list = list(tibble(martin_list_middle)),
  traits= list(tibble(martin_traits_middle)),
  measures=list(tibble(martin_measures_middle)),
  obs = "Apenas natural forest. Diferenca nos replicates de 'abundance' e 'measures'")

#View(anikka_data)
save(martin_data_low, # martin_data_2021,
     martin_data_middle, # martin_data_2022, 
     file = file.path(martin_suica,
                 "martin_suica.RData"))

#--- MD57 --- Sam_Czech ----
sam_czech <- file.path(local_directory,
                          "Sam_Czech")
sam_fa <- read_xlsx(
  file.path(
    sam_czech,
    "Sam.Czechia_Lipi.2.xlsx"),
  "fauna_abundance")

sam_list <- read_xlsx(
  file.path(
    sam_czech,
    "Sam.Czechia_Lipi.2.xlsx"),
  "Fauna_morphospecies_list")

sam_traits <- read_xlsx(
  file.path(
    sam_czech,
    "Sam.Czechia_Lipi.2.xlsx"),
  "Fauna_traits")

sam_measures <- read_xlsx(
  file.path(
    sam_czech,
    "Sam.Czechia_Lipi.2.xlsx"),
  "measures_decomposition_geograph")

head(sam_fa)
head(sam_list)
head(sam_traits)

sam_measures$'Latitude (in decimals)' <- gsub("[^0-9.]", "", sam_measures$'Latitude (in decimals)')
sam_measures$'Longitude (in decimals)' <- gsub("[^0-9.]", "", sam_measures$'Longitude (in decimals)')

sam_measures_adjust <- sam_measures %>%
  filter(!(Replicate %in% c("pot.11", "pot.12", "pot.13", "pot.14", "pot.15"))) %>%
  rename("Remaining_water_volume" = "Final water volume (ml)") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric))

sam_data <- tibble(
  researcher = "Sam",
  locality = "Czechia",
  roof_treatment = NA,
  abundance = list(tibble(sam_fa)),
  list = list(tibble(sam_list)),
  traits= list(tibble(sam_traits)),
  measures=list(tibble(sam_measures_adjust)),
  obs = NA)

save(sam_data, 
     file = file.path(sam_czech,
                      "sam_czechia.RData"))

#--- MD58 & MD59 --- Larrieu_France ----
larrieu_france <- file.path(local_directory,
                       "Larrieu_Bouget_Burat_Fontainebleu")

# Burat
larrieu_burat_fa <- read_xlsx(
  file.path(
    larrieu_france,
    "Burat France_Final.xlsx"),
  "fauna_abundance")

larrieu_burat_list <- read_xlsx(
  file.path(
    larrieu_france,
    "Burat France_Final.xlsx"),
  "Fauna_morphospecies_list")

larrieu_burat_traits <- read_xlsx(
  file.path(
    larrieu_france,
    "Burat France_Final.xlsx"),
  "Fauna_traits")

larrieu_burat_measures <- read_xlsx(
  file.path(
    larrieu_france,
    "Burat France_Final.xlsx"),
  "measures_decomposition_geograph")

head(larrieu_burat_fa)
head(larrieu_burat_list)
head(larrieu_burat_traits)

larrieu_burat_measures_adjust <- larrieu_burat_measures %>%
  rename("Remaining_water_volume" = "remaining_water_volume_sampling_mL") %>%
  rename("Natural tree hole.1" = "Natural tree hole.1 (yes/no)") %>%
  rename("Natural tree hole.2" = "Natural tree hole.2 (number per hectare)") %>%
  rename("canopy openness" = "canopy cover") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric))

larrieu_burat_data <- tibble(
  researcher = "Larrieu",
  locality = "Burat_France",
  roof_treatment = NA,
  abundance = list(tibble(larrieu_burat_fa)),
  list = list(tibble(larrieu_burat_list)),
  traits= list(tibble(larrieu_burat_traits)),
  measures=list(tibble(larrieu_burat_measures_adjust)),
  obs = NA)

# Fontainebleau
larrieu_france <- file.path(local_directory,
                            "Larrieu_Bouget_Burat_Fontainebleu")
larrieu_fontainebleau_fa <- read_xlsx(
  file.path(
    larrieu_france,
    "Fontainebleau France_FINAL.xlsx"),
  "fauna_abundance")

larrieu_fontainebleau_list <- read_xlsx(
  file.path(
    larrieu_france,
    "Fontainebleau France_FINAL.xlsx"),
  "Fauna_morphospecies_list")

larrieu_fontainebleau_traits <- read_xlsx(
  file.path(
    larrieu_france,
    "Fontainebleau France_FINAL.xlsx"),
  "Fauna_traits")

larrieu_fontainebleau_measures <- read_xlsx(
  file.path(
    larrieu_france,
    "Fontainebleau France_FINAL.xlsx"),
  "measures_decomposition_geograph")

head(larrieu_fontainebleau_fa)
head(larrieu_fontainebleau_list)
head(larrieu_fontainebleau_traits)

larrieu_fontainebleau_measures_adjust <- larrieu_fontainebleau_measures %>%
  rename("Remaining_water_volume" = "remaining_water_volume_sampling_mL") %>%
  rename("Tree dbh" = "Tree dbh_cm") %>%
  rename("canopy openness" = "canopy cover (%)") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric))

larrieu_fontainebleau_data <- tibble(
  researcher = "Larrieu",
  locality = "Fontainebleau_France",
  roof_treatment = NA,
  abundance = list(tibble(larrieu_fontainebleau_fa)),
  list = list(tibble(larrieu_fontainebleau_list)),
  traits= list(tibble(larrieu_fontainebleau_traits)),
  measures=list(tibble(larrieu_fontainebleau_measures_adjust)),
  obs = NA)

#View(anikka_data)
save(larrieu_burat_data, 
     larrieu_fontainebleau_data,
     file = file.path(larrieu_france,
                      "larrieu_france.RData"))

#--- MD60 --- Yoshida_Kusaki ----
yoshida_kusaki <- file.path(local_directory,
                            "Yoshida_Kusaki")
yoshida_kusaki_fa <- read_xlsx(
  file.path(
    yoshida_kusaki,
    "Japan_Kusaki_Updated_01.08.2024.xlsx"),
  "fauna_abundance")

yoshida_kusaki_list <- read_xlsx(
  file.path(
    yoshida_kusaki,
    "Japan_Kusaki_Updated_01.08.2024.xlsx"),
  "Fauna_morphospecies_list")

yoshida_kusaki_traits <- read_xlsx(
  file.path(
    yoshida_kusaki,
    "Japan_Kusaki_Updated_01.08.2024.xlsx"),
  "Fauna_traits")

yoshida_kusaki_measures <- read_xlsx(
  file.path(
    yoshida_kusaki,
    "Japan_Kusaki_Updated_01.08.2024.xlsx"),
  "measures_decomposition_geograph")

yoshida_kusaki_fa <- yoshida_kusaki_fa %>%
  mutate(across(-c(Treatment, Replicate), as.numeric)) %>%
  mutate(across(-c(Treatment, Replicate), ~ replace_na(., 0)))

head(yoshida_kusaki_list)
head(yoshida_kusaki_traits)

yoshida_kusaki_measures_adjust <- yoshida_kusaki_measures %>%
  rename(Remaining_water_volume = "remaining_water_volume_mL") %>%
  rename("dissolved_O2" = "dissolved_O2_mg_L") %>%
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(all_of(cols_to_convert_g_to_mg), ~ .x * 1000))

# no data_log, esta que as medidas de before estao menores que after
# parece que isso ja foi resolvido!
#yoshida_kusaki_measures_adjust %>% mutate(
#  coarse_comparison = coarse_before_mg > coarse_after_mg,
#  fine_comparison = fine_before_mg > fine_after_mg,
#  outside_comparison = outside_before_mg > outside_after_mg) %>%
#  select(treatment, replicate, coarse_comparison,
#         fine_comparison, outside_comparison)

yoshida_kusaki_data <- tibble(
  researcher = "Yoshida",
  locality = "Kusaki_Japan",
  roof_treatment = NA,
  abundance = list(tibble(yoshida_kusaki_fa)),
  list = list(tibble(yoshida_kusaki_list)),
  traits= list(tibble(yoshida_kusaki_traits)),
  measures=list(tibble(yoshida_kusaki_measures_adjust)),
  obs = NA)

save(yoshida_kusaki_data, 
     file = file.path(yoshida_kusaki,
                      "yoshida_kusaki.RData"))

#--- MD61 --- Yoshida_Karasawayama ---- 
yoshida_karasawayama <- file.path(local_directory,
                                  "Yoshida_Karasawayama")
yoshida_karasawayama_fa <- read_xlsx(
  file.path(
    yoshida_karasawayama,
    "Japan_Karasawayama_Updated_01.08.2024.xlsx"),
  "fauna_abundance")

yoshida_karasawayama_list <- read_xlsx(
  file.path(
    yoshida_karasawayama,
    "Japan_Karasawayama_Updated_01.08.2024.xlsx"),
  "Fauna_morphospecies_list")

yoshida_karasawayama_traits <- read_xlsx(
  file.path(
    yoshida_karasawayama,
    "Japan_Karasawayama_Updated_01.08.2024.xlsx"),
  "Fauna_traits")

yoshida_karasawayama_measures <- read_xlsx(
  file.path(
    yoshida_karasawayama,
    "Japan_Karasawayama_Updated_01.08.2024.xlsx"),
  "measures_decomposition_geograph")

head(yoshida_karasawayama_fa)
head(yoshida_karasawayama_list)
head(yoshida_karasawayama_traits)

yoshida_karasawayama_fa_adjust <- yoshida_karasawayama_fa %>%
  select(-ID)

yoshida_karasawayama_measures_adjust <- yoshida_karasawayama_measures %>%
  rename("dissolved_O2" = "dissolved_O2_mg_L") %>%
  rename(Remaining_water_volume = "remaining_water_volume_mL") %>% 
  rename(all_of(dict_names)) %>%
  mutate(across(all_of(var_char), as.character)) %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(all_of(cols_to_convert_g_to_mg), ~ .x * 1000))

yoshida_karasawayama_data <- tibble(
  researcher = "Yoshida",
  locality = "karasawayama_Japan",
  roof_treatment = NA,
  abundance = list(tibble(yoshida_karasawayama_fa_adjust)),
  list = list(tibble(yoshida_karasawayama_list)),
  traits= list(tibble(yoshida_karasawayama_traits)),
  measures=list(tibble(yoshida_karasawayama_measures_adjust)),
  obs = NA)

save(yoshida_karasawayama_data, 
     file = file.path(yoshida_karasawayama,
                      "yoshida_karasawayama.RData"))

#--- Nested dataframe ----
### ATENCAO ###
# FUNDAMENTAL NAO ALTERAR A ORDEM DOS DATAFRAMES INSERIDOS AQUI
# POIS ELES RECEBERAM CODIGOS DE ID CONFORME A ORDEM. ESSES CODIGOS
# FORAM CRIADOS PARA POSTERIOR UNIAO (JOIN) DOS DATAFRAMES ANINHADOS
# COM AS RESPECTIVAS CHAVES ID APOS A CONFERENCIA DOS ATRIBUTOS FUNCIONAIS
# QUE FORAM FEITAS MANUALMENTE. QUALQUER DUVIDA, 
# CONSULTAR Joice Souza, Matheus Moroti ou Gustavo Romero
nested_df <- bind_rows(boukal_czech_roof,
                       boukal_czech_nonroof,
                       boukal_czech_plesnelake,
                       caliman_nonroof_natal_br,
                       cardinale_usa_data,
                       collyer_japan_data,
                       cornelissen_roof_BR_data,
                       cornelissen_nonroof_BR_data,
                       cotriguacu_romero_data_low_roof, #cotriguacu_romero_data,
                       fabiola_roof_colombia_data,
                       fabiola_nonroof_colombia_data,
                       celine_canopy_frenchguyana_data,
                       celine_general_frenchguyana_data,
                       gonzales_site1_roof_data,
                       gonzales_site2_roof_data,
                       gonzales_site3_roof_data,
                       horvath_data,
                       izzo_data,
                       romero_nonroof_japi_data,
                       jari_data,
                       juen_roof_data,
                       juen_nonroof_data,
                       juen_na_data,
                       knapp_roof_data,
                       knapp_nonroof_data,
                       knapp_data,
                       luciano_data,
                       martins_roof_data,
                       martins_nonroof_data,
                       martins_na_data,
                       wesley_data, 
                       moretti_site1_data,
                       moretti_site2_data,
                       nock_data_mf1,
                       pavel_site1_data,
                       pavel_site2_data,
                       pettermann_data,
                       renan_alloch_data,
                       renan_vertical_data,
                       renan_basic_data, 
                       rodrigo_data, 
                       sedney_data,
                       srivastava_site1_data,
                       srivastava_site2_data,
                       sweet_data,
                       thomas_data,
                       romero_stavirginia_data,
                       nakamura_site1_data,
                       nakamura_site2_data,
                       romero_campos_data,
                       romero_cardoso_data,
                       anikka_data,
                       claas_data,
                       romero_roof_japi_data,
                       martin_data_low,
                       martin_data_middle,
                       sam_data,
                       larrieu_burat_data, 
                       larrieu_fontainebleau_data,
                       yoshida_kusaki_data,
                       yoshida_karasawayama_data,
                       caliman_roof_natal_br,
                       cotriguacu_romero_data_mid,
                       cotriguacu_romero_data_high,
                       gonzales_site1_nonroof_data,
                       gonzales_site2_nonroof_data,
                       gonzales_site3_nonroof_data,
                       cotriguacu_romero_data_low_nonroof,
                       nock_data_mf2)

data_number <- column_id(nested_df, "MD")

data_number <- data_number %>%
  relocate(heigth_treatment, 
         .after = roof_treatment)

# Save in data_preprocessing
# This data is original traits by the authors
# salva no github
save(data_number,
     file = here::here("00_preprocessed_data",
                      "nested_df_original.RData"))
