# Mon Apr 15 18:45:16 2024 ------------------------------
# packages
library(tidyverse) # data science handling
library(readxl) # read .xlsx
library(here) # set directory 
library(renv) # versioning R and packages

#---
# Data dictiorary 
dict_data <- read_xlsx(
  here("dados_microcosmos",
       "data_dictionary.xlsx"),
  "measures_decomposition_geograph")

dict_names <- dict_data %>%
  select(new_name, old_name) %>%
  deframe()

# As pastas nesse diretorio correspondem aos autores e as localidades
# onde foram executadas os microcosmos. # Existem 4 planilhas dentro de 
# cada .xlsx. Alguns autores possuem dados temporais de loggers,
# e alguns também fizeram dois tratamentos a mais
# com telhado e sem telhado. Por isso, uma coluna "with_roof" foi criada
# para designar o tratamento aplicado. 
# with_roof
# 1 = present 
# 0 = ausent
# NA = non treatment apply

#--- Boukal_Czech (with roof)
boukal_roofs_fa <- read_xlsx(
  here(
  "dados_microcosmos",
  "Boukal_Czech",
  "Boukal_Site.2 Hluboka_roofs.xlsx"),
  "fauna_abundance")

boukal_roofs_list <- read_xlsx(
  here(
    "dados_microcosmos",
    "Boukal_Czech",
    "Boukal_Site.2 Hluboka_roofs.xlsx"),
  "Fauna_morphospecies_list")

boukal_roofs_traits <- read_xlsx(
  here(
    "dados_microcosmos",
    "Boukal_Czech",
    "Boukal_Site.2 Hluboka_roofs.xlsx"),
  "Fauna_traits")

boukal_roofs_measures <- read_xlsx(
  here(
    "dados_microcosmos",
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

# TODO needs revision by Gustavo Romero
# TODO needs encoding
glimpse(boukal_roofs_traits)

# rename variables with data dictionary
boukal_roofs_measures <- boukal_roofs_measures %>%
  rename(all_of(dict_names))
glimpse(boukal_roofs_measures)

# boukal_czech_roof
boukal_czech_roof <- tibble(
  researcher = "Boukal",
  locality = "Czech", 
  roof_treatment = 1,
  abundance = list(tibble(boukal_roofs_fa)),
  list = list(tibble(boukal_roofs_list)),
  traits=list(tibble(boukal_roofs_traits)),
  measures=list(tibble(boukal_roofs_measures))
  )

View(boukal_czech_roof)

#--- Boukal_Czech (without roof)
boukal_nonroofs_fa <- read_xlsx(
  here(
    "dados_microcosmos",
    "Boukal_Czech",
    "Boukal_Site.2 Hluboka_without-roofs.xlsx"),
  "fauna_abundance")

boukal_nonroofs_list <- read_xlsx(
  here(
    "dados_microcosmos",
    "Boukal_Czech",
    "Boukal_Site.2 Hluboka_without-roofs.xlsx"),
  "Fauna_morphospecies_list")

boukal_nonroofs_traits <- read_xlsx(
  here(
    "dados_microcosmos",
    "Boukal_Czech",
    "Boukal_Site.2 Hluboka_without-roofs.xlsx"),
  "Fauna_traits")

boukal_nonroofs_measures <- read_xlsx(
  here(
    "dados_microcosmos",
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

# TODO needs revision by Gustavo Romero
# TODO needs encoding
glimpse(boukal_nonroofs_traits)

# rename variables with data dictionary
boukal_nonroofs_measures <- boukal_nonroofs_measures %>%
  rename(all_of(dict_names))
glimpse(boukal_roofs_measures)

# NOTE: se eu colocar sem arg 'list' ele replica o dataset aninhado
boukal_czech_nonroof <- tibble(
  researcher = "Boukal",
  locality = "Czech", 
  roof_treatment = 0,
  abundance = list(tibble(boukal_nonroofs_fa)),
  list = list(tibble(boukal_nonroofs_list)),
  traits=list(tibble(boukal_nonroofs_traits)),
  measures=list(tibble(boukal_nonroofs_measures)))

###---- Boukal_Czech (TODO: third treatment?)
boukal_plesnelake_fa <- read_xlsx(
  here(
    "dados_microcosmos",
    "Boukal_Czech",
    "Boukal_Site.2 PlesneLake.xlsx"),
  "fauna_abundance")

boukal_plesnelake_list <- read_xlsx(
  here(
    "dados_microcosmos",
    "Boukal_Czech",
    "Boukal_Site.2 PlesneLake.xlsx"),
  "Fauna_morphospecies_list")

boukal_plesnelake_traits <- read_xlsx(
  here(
    "dados_microcosmos",
    "Boukal_Czech",
    "Boukal_Site.2 PlesneLake.xlsx"),
  "Fauna_traits")

boukal_plesnelake_measures <- read_xlsx(
  here(
    "dados_microcosmos",
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

# TODO needs revision by Gustavo Romero
# TODO needs encoding
glimpse(boukal_plesnelake_traits)

# rename variables with data dictionary
# gambiarra para renomear as colunas
boukal_plesnelake_measures <- 
  boukal_plesnelake_measures %>%
  rename("dissolved_O2" = "dissolved_O2 (%)")

boukal_plesnelake_measures <- 
  boukal_plesnelake_measures %>%
  rename(all_of(dict_names))

glimpse(boukal_plesnelake_measures)

boukal_czech_plesnelake <- tibble(
  researcher = "Boukal",
  locality = "Czech", 
  roof_treatment = NA,
  abundance = list(tibble(boukal_plesnelake_fa)),
  list = list(tibble(boukal_plesnelake_list)),
  traits=list(tibble(boukal_plesnelake_traits)),
  measures=list(tibble(boukal_plesnelake_measures)))


# save .RData from Boukal
save(boukal_czech_roof,
        boukal_czech_nonroof,
        boukal_czech_plesnelake,
        file = here("dados_microcosmos",
                    "Boukal_Czech",
                    "Boukal_Czech.RData")) 
# it works! 
load(here("dados_microcosmos",
          "Boukal_Czech",
          "Boukal_Czech.RData"))

# nested dataframes 
nested_df <- bind_rows(boukal_czech_roof,
                       boukal_czech_nonroof,
                       boukal_czech_plesnelake) 
View(nested_df)
nested_df$traits[2]

###----
#--- Caliman_Natal_BR
# era pouca coisa, fiz direto na mao

#--- Campos_do_Jordao_e_Sta_Virginia
# dados indisponiveis

#--- Cardinale_USA
cardinale_fa <- read_xlsx("Cardinale_USA/Cardinale_modified_microcosm_data.xlsx",
                          "fauna_abundance")
cardinale_fa[is.na(cardinale_fa)] <- 0

#--- Cardoso_Romero
# Dados incompletos, mas as coordenadas foram convertidas direto no xlsx.

#--- Collyer_Japan
# Dados estão ok

#--- Cornelissen_BR
# No arquivo São Bartolomeu_Site.2_MICROCOSMS (Without_roof)2.0.xlsx
# a aba "measures_decomposition_geograph" coluna H linha 8 tem
# um valor ausente que não está preenchido nem com NA. Precisa checar!

#--- Cotriguaçu_Romero
cotriguacu_list <- read_xlsx("Cotriguaçu_Romero/Romero.Amazon.xlsx",
                             "Fauna_morphospecies_list")

cotriguacu_list <- cotriguacu_list %>% mutate_all(~na_if(., "-"))

#--- Fabiola_Colombia
# tem uma coluna a mais de traits

#--- French_Guyana_Celine
# linhas 11-15 precisam ser deletadas, deletei direto no .xlsx

#--- Gonzalez_USA
gonzales_list <- read_xlsx("Gonzalez_USA/González.NJ_Site_Microcosm_Updated.xlsx",
                           "Fauna_morphospecies_list")
gonzales_list <- mutate_all(gonzales_list, ~(replace(., .=="?", NA)))

gonzales_traits <- read_xlsx("Gonzalez_USA/González.NJ_Site_Microcosm_Updated.xlsx",
                           "Fauna_traits")
gonzales_traits <- mutate_all(gonzales_traits, ~(replace(., .=="?", NA)))

#--- Horvath_HU
# dados do logger estão na mesma planilha

#--- Izzo_Chapada_BR
# Fauna_morphospecies_list: substituído '-' por 'NA' (fiz direto na planilha a remoção do hífen)

#--- GRomero_Japi
# coordenadas convertidas

#---Jari Finland
# tirar dúvidas

#--- Juen_Belém_BR
# dados ok após correção

#--- Knapp_Czech
# tirar dúvidas

#---Luciano_argentina
# parecem ok

#--- Martins_Hamada_Amazon
martins_fa <- read_xlsx("Martins_Hamada_Amazon/Martins&Hamada_Manaus_01_12_23.xlsx",
                          "fauna_abundance")

martins_traits <- read_xlsx("Martins_Hamada_Amazon/Martins&Hamada_Manaus_01_12_23.xlsx",
                        "Fauna_traits")

martins_fa[is.na(martins_fa)] <- 0

saveRDS(martins_fa, martins_traits, file = "martins3.rds")

#--- Mexico_Wesley
# retirei o undefined e deixei vazio para ler como NA.

#--- MMoretti Lab_BR
moretti_fa1 <- read_xlsx("MMoretti Lab_BR/PI.Marcelo.Moretti.Site.1.xlsx",
                          "Fauna_abundance")
moretti_fa1[is.na(moretti_fa1)] <- 0

moretti_fa2 <- read_xlsx("MMoretti Lab_BR/PI.Marcelo.Moretti.Site.2.xlsx",
                          "Fauna_abundance")

moretti_fa2[is.na(moretti_fa2)] <- 0

#--- Musa_SouthAfrica
#--- Nock
#--- Pavel Kratina_UK
#--- Petterman_Austria
#--- Renan_Chapecó
chapeco_list1 <- read_xlsx("Renan_Chapecó/Allochthonous_Chapeco_BR.xlsx",
                          "Fauna_morphospecies_list")
chapeco_list1  <- mutate_all(chapeco_list1, ~(replace(., .=="*", NA)))


chapeco_list2 <- read_xlsx("Renan_Chapecó/Basic_Chapeco_BR.xlsx",
                          "Fauna_morphospecies_list")
chapeco_list2  <- mutate_all(chapeco_list2, ~(replace(., .=="*", NA)))

chapeco_list3 <- read_xlsx("Renan_Chapecó/Vertical_Chapeco_BR.xlsx",
                          "Fauna_morphospecies_list")
chapeco_list3  <- mutate_all(chapeco_list3, ~(replace(., .=="*", NA)))