# Mon Apr 15 18:45:16 2024 ------------------------------
# packages
library(tidyverse) # data science handling
library(readxl) # read .xlsx
library(here) # set directory 
library(renv) # versioning R and packages

# Data dictiorary ----
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

#--- Boukal_Czech ----
# (with roof) 
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
  locality = "Hluboka_Czech", 
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
        file = here("dados_microcosmos",
                    "Boukal_Czech",
                    "Boukal_Czech.RData")) 
# it works! 
load(here("dados_microcosmos",
          "Boukal_Czech",
          "Boukal_Czech.RData"))


#--- Caliman_Natal_BR ----
caliman_fa <- read_xlsx(
  here(
    "dados_microcosmos",
    "Caliman_Natal_BR",
    "Adriano.Caliman_Natal,Restinga,Atlantic Forest.xlsx"),
  "fauna_abundance")

caliman_list <- read_xlsx(
  here(
    "dados_microcosmos",
    "Caliman_Natal_BR",
    "Adriano.Caliman_Natal,Restinga,Atlantic Forest.xlsx"),
  "Fauna_morphospecies_list")

caliman_traits <- read_xlsx(
  here(
    "dados_microcosmos",
    "Caliman_Natal_BR",
    "Adriano.Caliman_Natal,Restinga,Atlantic Forest.xlsx"),
  "Fauna_traits")

caliman_measures <- read_xlsx(
  here(
    "dados_microcosmos",
    "Caliman_Natal_BR",
    "Adriano.Caliman_Natal,Restinga,Atlantic Forest.xlsx"),
  "measures_decomposition_geograph")

# TODO: abundance 
# aqui temos 40 potinhos, 20 deles estão indicados como
#  (allochthonous detritus). Precisa ser separado em dois experimentos?
head(caliman_fa)

# list
head(caliman_list)

# traits
head(caliman_traits)

# measures
# rename variables with data dictionary
# gambiarra para renomear as colunas
caliman_measures <- 
  caliman_measures %>%
  rename("Elevation (m a.s.l.)" = "Elevation (m.s.l.)") #%>%
  #select(-"Other water bodies.1", -"Other water bodies.2")
caliman_measures <- 
  caliman_measures %>%
  rename(all_of(dict_names))

caliman_natal_br <- tibble(
  researcher = "Caliman",
  locality = "Natal, Brazil", 
  roof_treatment = NA,
  abundance = list(tibble(caliman_fa)),
  list = list(tibble(caliman_list)),
  traits=list(tibble(caliman_traits)),
  measures=list(tibble(caliman_measures)),
  obs= "experimento com 40 potes")

# save .RData from Boukal
save(caliman_natal_br,
     #boukal_czech_nonroof,
     file = here("dados_microcosmos",
                 "Caliman_Natal_BR",
                 "Caliman_Natal_BR.RData")) 

#--- Campos_do_Jordao_e_Sta_Virginia ----
# dados indisponiveis ainda
# TODO: precisa separar em duas pastas


#--- Cardinale_USA ----
cardinale_usa <- here("dados_microcosmos",
                      "Cardinale_USA")

cardinale_fa <- read_xlsx(
  here(
    cardinale_usa,
    "Cardinale_modified_microcosm_data.xlsx"),
  "fauna_abundance")

cardinale_list <- read_xlsx(
  here(
    cardinale_usa,
    "Cardinale_modified_microcosm_data.xlsx"),
  "Fauna_morphospecies_list")

cardinale_traits <- read_xlsx(
  here(
    cardinale_usa,
    "Cardinale_modified_microcosm_data.xlsx"),
  "Fauna_traits")

cardinale_measures <- read_xlsx(
  here(
    cardinale_usa,
    "Cardinale_modified_microcosm_data.xlsx"),
  "measures_decomposition_geograph")

# abundance
cardinale_fa[is.na(cardinale_fa)] <- 0

# list
cardinale_list

# traits
cardinale_traits

# measures
cardinale_measures

# rename variables with data dictionary
cardinale_measures <- 
  cardinale_measures %>%
  rename(all_of(dict_names))

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
     file = here(cardinale_usa,
                 "Cardinale_USA.RData")) 


#--- Cardoso_Romero ----
# TODO: Dados incompletos, mas as coordenadas foram convertidas direto no xlsx.


#--- Collyer_Japan ----
collyer_japan <- here("dados_microcosmos",
                      "Collyer_Japan")

collyer_fa <- read_xlsx(
  here(
    collyer_japan,
    "Giovanna Collyer & Takehito Yoshida_Japan.xlsx"),
  "fauna_abundance")

collyer_list <- read_xlsx(
  here(
    collyer_japan,
    "Giovanna Collyer & Takehito Yoshida_Japan.xlsx"),
  "Fauna_morphospecies_list")

collyer_traits <- read_xlsx(
  here(
    collyer_japan,
    "Giovanna Collyer & Takehito Yoshida_Japan.xlsx"),
  "Fauna_traits")

collyer_measures <- read_xlsx(
  here(
    collyer_japan,
    "Giovanna Collyer & Takehito Yoshida_Japan.xlsx"),
  "measures_decomposition_geograph")

# abundance
collyer_fa

# list
collyer_list

# traits
collyer_traits

# measures
collyer_measures
# rename variables with data dictionary
collyer_measures <- 
  collyer_measures %>%
  rename(all_of(dict_names))

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
     file = here(collyer_japan,
                 "Collyer_Japan.RData")) 


#---- Cornelissen_BR ----
cornelissen_br <- here("dados_microcosmos",
                      "Cornelissen_BR",
                      "dados_definitivos")

#--- Cornelissen_BR with roof
# TODO No arquivo São Bartolomeu_Site.2_MICROCOSMS (Without_roof)2.0.xlsx
# a aba "measures_decomposition_geograph" coluna H linha 8 tem
# um valor ausente que não está preenchido nem com NA. Precisa checar!
cornelissen_roof_fa <- read_xlsx(
  here(
    cornelissen_br,
    "Sao Bartolomeu_Site.2_MICROCOSMS (with_roof).xlsx"),
  "fauna_abundance")

cornelissen_roof_list <- read_xlsx(
  here(
    cornelissen_br,
    "Sao Bartolomeu_Site.2_MICROCOSMS (with_roof).xlsx"),
  "Fauna_morphospecies_list")

cornelissen_roof_traits <- read_xlsx(
  here(
    cornelissen_br,
    "Sao Bartolomeu_Site.2_MICROCOSMS (with_roof).xlsx"),
  "Fauna_traits")

cornelissen_roof_measures <- read_xlsx(
  here(
    cornelissen_br,
    "Sao Bartolomeu_Site.2_MICROCOSMS (with_roof).xlsx"),
  "measures_decomposition_geograph")

#abundance 
cornelissen_roof_fa

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
  rename(all_of(dict_names))

cornelissen_roof_BR_data <- tibble(
  researcher = "Cornelissen",
  locality = "MinasGerais_BR", 
  roof_treatment = 1,
  abundance = list(tibble(cornelissen_roof_fa)),
  list = list(tibble(cornelissen_roof_list)),
  traits=list(tibble(cornelissen_roof_traits)),
  measures=list(tibble(cornelissen_roof_measures)))

#View(cornelissen_roof_BR_data)

#--- Cornelissen_BR without roof
cornelissen_nonroof_fa <- read_xlsx(
  here(
    cornelissen_br,
    "Sao Bartolomeu_Site.2_MICROCOSMS (Without_roof)2.0.xlsx"),
  "fauna_abundance")

cornelissen_nonroof_list <- read_xlsx(
  here(
    cornelissen_br,
    "Sao Bartolomeu_Site.2_MICROCOSMS (Without_roof)2.0.xlsx"),
  "Fauna_morphospecies_list")

cornelissen_nonroof_traits <- read_xlsx(
  here(
    cornelissen_br,
    "Sao Bartolomeu_Site.2_MICROCOSMS (Without_roof)2.0.xlsx"),
  "Fauna_traits")

cornelissen_nonroof_measures <- read_xlsx(
  here(
    cornelissen_br,
    "Sao Bartolomeu_Site.2_MICROCOSMS (Without_roof)2.0.xlsx"),
  "measures_decomposition_geograph")

#abundance 
cornelissen_nonroof_fa

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
         -"...10", -"...11", -"...12")

# measures
# substituir o undetermined por NA
# o df nao tinha as colunas natural tree hole
# adicionar para renomear e manter o padrao
cornelissen_nonroof_measures <- cornelissen_nonroof_measures %>% 
  mutate_all(~ifelse(.=="undetermined", NA, .)) %>%
  mutate("Natural tree hole.1" = NA,
         "Natural tree hole.2" = NA) %>%
  rename(all_of(dict_names))

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
     file = here("dados_microcosmos",
                 "Cornelissen_BR",
                 "Cornelissen_BR.RData"))

#load(here("dados_microcosmos",
#          "Cornelissen_BR",
#          "Cornelissen_BR.RData"))

#--- Cotriguacu_Romero ----
cotriguacu_br <- here("dados_microcosmos",
                       "Cotriguacu_Romero")

cotriguacu_fa <- read_xlsx(
  here(
    cotriguacu_br,
    "Romero.Amazon.xlsx"),
  "fauna_abundance")

cotriguacu_list <- read_xlsx(
  here(
    cotriguacu_br,
    "Romero.Amazon.xlsx"),
  "Fauna_morphospecies_list")

cotriguacu_traits <- read_xlsx(
  here(
    cotriguacu_br,
    "Romero.Amazon.xlsx"),
  "Fauna_traits")

cotriguacu_measures <- read_xlsx(
  here(
    cotriguacu_br,
    "Romero.Amazon.xlsx"),
  "measures_decomposition_geograph")

# abundance
# TODO: precisa ver como organizar esses dados
# como tem dados de diferentes alturas e tem mais de 20 potinhos.
head(cotriguacu_fa)

# list
cotriguacu_list <- cotriguacu_list %>% mutate_all(~na_if(., "-"))
head(cotriguacu_list)

# traits
head(cotriguacu_traits)

# measures
cotriguacu_measures <- cotriguacu_measures %>%
  rename(all_of(dict_names))

# data 
cotriguacu_romero_data <- tibble(
  researcher = "Romero",
  locality = "Cotriguacu_Brazil", 
  roof_treatment = NA,
  abundance = list(tibble(cotriguacu_fa)),
  list = list(tibble(cotriguacu_list)),
  traits=list(tibble(cotriguacu_traits)),
  measures=list(tibble(cotriguacu_measures)),
  obs= "experimento com diferentes alturas")


#--- Fabiola_Colombia ----
# os dados dos tratamentos com telhado e sem telhado estao na mesma
# planilha, por isso irei separar em duas linhas distintas no df aninhado
# para ficar comparavel com o que esta sendo feito

# acrônimos usados para indicar os tratamentos sao:
# BC, BR, PC, and PR were a personal ID that I used. 
# B=forest; P=plantation; C=without roof; R=roof
fabiola_colombia <- here("dados_microcosmos",
                      "Fabiola_Colombia",
                      "Fabiola_Site.Colombia.xlsx")

fabiola_fa <- read_xlsx(
  here(
    fabiola_colombia),
  "fauna_abundance")

fabiola_list <- read_xlsx(
  here(
    fabiola_colombia),
  "Fauna_morphospecies_list")

fabiola_traits <- read_xlsx(
  here(
    fabiola_colombia),
  "Fauna_traits")

fabiola_measures <- read_xlsx(
  here(
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
         -"Isopoda.sp.1", -"Replicate...3")

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
         -"Isopoda.sp.1", -"Replicate...3")

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
list(fabiola_roof_list$Morfospecies_name) # ok
names(fabiola_roof_fa) # ok
list(fabiola_roof_traits$Morfospecies_name)# ok

# conferindo
list(fabiola_nonroof_list$Morfospecies_name) # ok
names(fabiola_nonroof_fa) # ok
list(fabiola_nonroof_traits$Morfospecies_name)# ok

# measures
fabiola_roof_measures <- fabiola_measures %>%
  filter(str_detect(fabiola_fa$`ID. Own`, "^BR|^PR")) %>%
  rename(all_of(dict_names))

fabiola_nonroof_measures <- fabiola_measures %>%
  filter(str_detect(fabiola_measures$`ID. Own`, "^BC|^PC")) %>%
  rename(all_of(dict_names))

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
     file = here("dados_microcosmos",
                 "Fabiola_Colombia",
                 "Fabiola_Colombia.RData"))

#--- French_Guyana_Celine ----
# linhas 11-15 precisam ser deletadas, deletei direto no .xlsx
# Canopy data
celine_guyana <- here("dados_microcosmos",
                         "French_Guyana_Celine")

celine_canopy_fa <- read_xlsx(
  here(
    celine_guyana, "Celine Leroy_French Guiana_canopy.xlsx"),
  "fauna_abundance")

celine_canopy_list <- read_xlsx(
  here(
    celine_guyana, "Celine Leroy_French Guiana_canopy.xlsx"),
  "Fauna_morphospecies_list")

celine_canopy_traits <- read_xlsx(
  here(
    celine_guyana, "Celine Leroy_French Guiana_canopy.xlsx"),
  "Fauna_traits")

celine_canopy_measures <- read_xlsx(
  here(
    celine_guyana, "Celine Leroy_French Guiana_canopy.xlsx"),
  "measures_decomposition_geograph")

# abundance
head(celine_canopy_fa)

# list
head(celine_canopy_list)

# traits
head(celine_canopy_traits)

# measures
celine_canopy_measures <- celine_canopy_measures %>%
  rename(all_of(dict_names))

# data 
celine_canopy_frenchguyana_data <- tibble(
  researcher = "Celine",
  locality = "FrenchGuyana", 
  roof_treatment = NA,
  abundance = list(tibble(celine_canopy_fa)),
  list = list(tibble(celine_canopy_list)),
  traits=list(tibble(celine_canopy_traits)),
  measures=list(tibble(celine_canopy_measures)),
  obs = "canopy data")

# general data
celine_general_fa <- read_xlsx(
  here(
    celine_guyana, "Celine Leroy_French Guiana_general.xlsx"),
  "fauna_abundance")

celine_general_list <- read_xlsx(
  here(
    celine_guyana, "Celine Leroy_French Guiana_general.xlsx"),
  "Fauna_morphospecies_list")

celine_general_traits <- read_xlsx(
  here(
    celine_guyana, "Celine Leroy_French Guiana_general.xlsx"),
  "Fauna_traits")

celine_general_measures <- read_xlsx(
  here(
    celine_guyana, "Celine Leroy_French Guiana_general.xlsx"),
  "measures_decomposition_geograph")

# abundance
head(celine_general_fa)

# list
head(celine_general_list)

# traits
head(celine_general_traits)

# measures
celine_general_measures <- celine_general_measures %>%
  rename(all_of(dict_names))

# data 
celine_general_frenchguyana_data <- tibble(
  researcher = "Celine",
  locality = "FrenchGuyana", 
  roof_treatment = NA,
  abundance = list(tibble(celine_general_fa)),
  list = list(tibble(celine_general_list)),
  traits=list(tibble(celine_general_traits)),
  measures=list(tibble(celine_general_measures)),
  obs = "general data")

save(celine_canopy_frenchguyana_data,
     celine_general_frenchguyana_data,
     file = here("dados_microcosmos",
                 "French_Guyana_Celine",
                 "Celine_FrenchGuyana.RData"))

#--- Gonzalez_USA ----
# aguardando retorno do email
# aparentemente temos tratamentos com e sem telhado.
gonzales_usa <- here("dados_microcosmos",
  "Gonzalez_USA",
  "González.NJ_Site_Microcosm_Updated_2.xlsx")

gonzales_fa <- read_xlsx(
  here(
    gonzales_usa),
  "fauna_abundance")

gonzales_list <- read_xlsx(
  here(
    gonzales_usa),
  "Fauna_morphospecies_list")

gonzales_traits <- read_xlsx(
  here(
    gonzales_usa),
  "Fauna_traits")

gonzales_measures <- read_xlsx(
  here(
    gonzales_usa),
  "measures_decomposition_geograph")

# abundance
gonzales_site1_fa <- gonzales_fa %>% 
  filter(Site == "Site 1") %>%
  select(-Morphospecies.4, -Morphospecies.5, -Morphospecies.7,
         -Morphospecies.8, -Morphospecies.9, -Morphospecies.10)

gonzales_site2_fa <- gonzales_fa %>% 
  filter(Site == "Site 2") %>%
  select(-Morphospecies.4, -Morphospecies.6, -Morphospecies.9)

gonzales_site3_fa <- gonzales_fa %>% 
  filter(Site == "Site 3") %>%
  select(-Morphospecies.5, -Morphospecies.6, -Morphospecies.7, 
         -Morphospecies.8)

#colSums(gonzales_site1_fa[,-c(1:3)]) 
#colSums(gonzales_site2_fa[,-c(1:3)])  
#colSums(gonzales_site3_fa[,-c(1:3)])  

# list
gonzales_list <- mutate_all(
  gonzales_list, ~(replace(., .=="?", NA)))

gonzales_site1_list <- gonzales_list %>%
  filter(Morfospecies_name != "Morphospecies.4" &
         Morfospecies_name != "Morphospecies.5" &
         Morfospecies_name != "Morphospecies.7" &
         Morfospecies_name != "Morphospecies.8" & 
         Morfospecies_name != "Morphospecies.9" & 
         Morfospecies_name != "Morphospecies.10")

gonzales_site2_list <- gonzales_list %>%
  filter(Morfospecies_name != "Morphospecies.4" &
           Morfospecies_name != "Morphospecies.6" &
           Morfospecies_name != "Morphospecies.9") 
           
gonzales_site3_list <- gonzales_list %>%
  filter(Morfospecies_name != "Morphospecies.5" &
           Morfospecies_name != "Morphospecies.6" &
           Morfospecies_name != "Morphospecies.7" &
           Morfospecies_name != "Morphospecies.8") 

# traits
gonzales_traits <- mutate_all(
  gonzales_traits, ~(replace(., .=="?", NA)))

gonzales_site1_traits <- gonzales_traits %>%
  filter(Morfospecies_name != "Morphospecies.4" &
           Morfospecies_name != "Morphospecies.5" &
           Morfospecies_name != "Morphospecies.7" &
           Morfospecies_name != "Morphospecies.8" & 
           Morfospecies_name != "Morphospecies.9" & 
           Morfospecies_name != "Morphospecies.10")

gonzales_site2_traits <- gonzales_traits %>%
  filter(Morfospecies_name != "Morphospecies.4" &
           Morfospecies_name != "Morphospecies.6" &
           Morfospecies_name != "Morphospecies.9") 

gonzales_site3_traits <- gonzales_traits %>%
  filter(Morfospecies_name != "Morphospecies.5" &
           Morfospecies_name != "Morphospecies.6" &
           Morfospecies_name != "Morphospecies.7" &
           Morfospecies_name != "Morphospecies.8")

# measures
gonzales_site1_measures <- gonzales_measures %>% 
  filter(Site == "Site 1") %>%
  rename(all_of(dict_names))

gonzales_site2_measures <- gonzales_measures %>% 
  filter(Site == "Site 2") %>%
  rename(all_of(dict_names))

gonzales_site3_measures <- gonzales_measures %>% 
  filter(Site == "Site 3") %>%
  rename(all_of(dict_names))

# data 
gonzales_site1_data <- tibble(
  researcher = "Gonzales",
  locality = "Philadelphia_USA", 
  roof_treatment = NA,
  abundance = list(tibble(gonzales_site1_fa)),
  list = list(tibble(gonzales_site1_list)),
  traits=list(tibble(gonzales_site1_traits)),
  measures=list(tibble(gonzales_site1_measures)),
  obs = "20 por tratamento. Site 1")

gonzales_site2_data <- tibble(
  researcher = "Gonzales",
  locality = "Philadelphia_USA", 
  roof_treatment = NA,
  abundance = list(tibble(gonzales_site2_fa)),
  list = list(tibble(gonzales_site2_list)),
  traits=list(tibble(gonzales_site2_traits)),
  measures=list(tibble(gonzales_site2_measures)),
  obs = "20 por tratamento. Site 2")

gonzales_site3_data <- tibble(
  researcher = "Gonzales",
  locality = "Philadelphia_USA", 
  roof_treatment = NA,
  abundance = list(tibble(gonzales_site3_fa)),
  list = list(tibble(gonzales_site3_list)),
  traits=list(tibble(gonzales_site3_traits)),
  measures=list(tibble(gonzales_site3_measures)),
  obs = "20 por tratamento. Site 3")

save(gonzales_site1_data,
     gonzales_site2_data,
     gonzales_site3_data,
     file = here("dados_microcosmos",
                 "Gonzalez_USA",
                 "Gonzalez_USA.RData"))


#--- Horvath_HU ----
# dados do logger estão na mesma planilha
horvath_hungria <- here("dados_microcosmos",
                     "Horvath_HU",
                     "Microcosm_HU_Horvath.xlsx")

horvath_fa <- read_xlsx(
  here(
    horvath_hungria),
  "fauna_abundance")

horvath_list <- read_xlsx(
  here(
    horvath_hungria),
  "Fauna_morphospecies_list")

horvath_traits <- read_xlsx(
  here(
    horvath_hungria),
  "Fauna_traits")

horvath_measures <- read_xlsx(
  here(
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
  mutate("Natural tree hole.1" = NA,
         "Natural tree hole.2" = NA) %>%
  rename(all_of(dict_names))

horvath_data <- tibble(
  researcher = "Horvath",
  locality = "PilisMountain_Hungria", 
  roof_treatment = NA,
  abundance = list(tibble(horvath_fa)),
  list = list(tibble(horvath_list)),
  traits=list(tibble(horvath_traits)),
  measures=list(tibble(horvath_measures)),
  obs = "dados do logger estão na mesma planilha")

save(horvath_data,
     file = here("dados_microcosmos",
                 "Horvath_HU",
                 "horvath_hungria.RData"))



#--- Izzo_Chapada_BR ----
# Fauna_morphospecies_list: substituído '-' por 'NA' (fiz direto na planilha a remoção do hífen)
izzo_br <- here("dados_microcosmos",
                        "Izzo_Chapada_BR",
                        "TJIZZO.Chapada.xlsx")

izzo_fa <- read_xlsx(
  here(
    izzo_br),
  "fauna_abundance")

izzo_list <- read_xlsx(
  here(
    izzo_br),
  "Fauna_morphospecies_list")

izzo_traits <- read_xlsx(
  here(
    izzo_br),
  "Fauna_traits")

izzo_measures <- read_xlsx(
  here(
    izzo_br),
  "measures_decomposition_geograph")

# abundance
head(izzo_fa)

# list
head(izzo_list)

# traits
head(izzo_traits)

# measures
izzo_measures <- izzo_measures %>%
  rename(all_of(dict_names))

izzo_data <- tibble(
  researcher = "Izzo",
  locality = "Chapada_Brazil", 
  roof_treatment = NA,
  abundance = list(tibble(izzo_fa)),
  list = list(tibble(izzo_list)),
  traits=list(tibble(izzo_traits)),
  measures=list(tibble(izzo_measures)),
  obs = "o pot.5 do tratamento 'Natural forest' esta marcado de amarelo
  e apenas com os parâmetros iniciais coletados")

save(izzo_data,
     file = here("dados_microcosmos",
                 "Izzo_Chapada_BR",
                 "izzo_brazil.RData"))

#--- GRomero_Japi ----
# coordenadas convertidas direto no xlsx
romero_japi <- here("dados_microcosmos",
                "Japi_romero",
                "GRomero_Japi.xlsx")

romero_japi_fa <- read_xlsx(
  here(
    romero_japi),
  "fauna_abundance")

romero_japi_list <- read_xlsx(
  here(
    romero_japi),
  "Fauna_morphospecies_list")

romero_japi_traits <- read_xlsx(
  here(
    romero_japi),
  "Fauna_traits")

romero_japi_measures <- read_xlsx(
  here(
    romero_japi),
  "measures_decomposition_geograph")

# abundance
head(romero_japi_fa)

# list
head(romero_japi_list)

# traits
head(romero_japi_traits)

# measures
#romero_japi_measures <- romero_japi_measures %>%
#  rename(all_of(dict_names))

romero_japi_data <- tibble(
  researcher = "Romero",
  locality = "Japi_Brazil", 
  roof_treatment = NA,
  abundance = list(tibble(romero_japi_fa)),
  list = list(tibble(romero_japi_list)),
  traits=list(tibble(romero_japi_traits)),
  measures=list(tibble(romero_japi_measures)),
  obs = "dados incompletos")

save(romero_japi_data,
     file = here("dados_microcosmos",
                 "Japi_romero",
                 "romero_japi_brazil.RData"))

#---Jari Finland ----
# tirar dúvidas
jari_finland <- here("dados_microcosmos",
                    "Jari_Finland",
                    "JariKouki-Finland-draft-data.xlsx")

jari_fa <- read_xlsx(
  here(
    jari_finland),
  "fauna_abundance")

jari_list <- read_xlsx(
  here(
    jari_finland),
  "Fauna_morphospecies_list")

jari_traits <- read_xlsx(
  here(
    jari_finland),
  "Fauna_traits")

jari_measures <- read_xlsx(
  here(
    jari_finland),
  "measures_decomposition_geograph")

# abundance
head(jari_fa)

# list
head(jari_list)

# traits
head(jari_traits)

# measures
jari_measures <- jari_measures %>%
  mutate("detritus dry mass (fine)" = NA) %>%
  rename(all_of(dict_names))

jari_data <- tibble(
  researcher = "Jari",
  locality = "Finland", 
  roof_treatment = NA,
  abundance = list(tibble(jari_fa)),
  list = list(tibble(jari_list)),
  traits=list(tibble(jari_traits)),
  measures=list(tibble(jari_measures)),
  obs = "Dados confusos. Precisamos tirar duvidas")

save(jari_data,
     file = here("dados_microcosmos",
                 "Jari_Finland",
                 "jari_finland.RData"))


#--- Juen_Belém_BR ----
# os dados dos diferentes tratamentos estao todos juntos
# precisamos separar em linhas distintas e limpar as abas correspondentes
# por ex, no tratamento com telhado, alguns taxons nao estao presentes, assim
# como nos outros experimentos. Pela estrutura dos dados, tem 3 experimentos 
# aqui, sendo um deles a comparacao entre UFPA vs. Utinga
juen_belem <- here("dados_microcosmos",
                     "Juen_Belém_BR")

juen_fa <- read_xlsx(
  here(
    juen_belem,
    "Juen_Belem_Amazon.xlsx"),
  "fauna_abundance")

juen_list <- read_xlsx(
  here(
    juen_belem,
    "Juen_Belem_Amazon.xlsx"),
  "Fauna_morphospecies_list")

juen_traits <- read_xlsx(
  here(
    juen_belem,
    "Juen_Belem_Amazon.xlsx"),
  "Fauna_traits")

juen_measures <- read_xlsx(
  here(
    juen_belem,
    "Juen_Belem_Amazon.xlsx"),
  "measures_decomposition_geograph")

# abundance
juen_roof_fa <- juen_fa %>%
  filter(Roof == "com roof") %>% # 20 amostras
  select(-"Morphospecies.1", -"Morphospecies.2", -"Morphospecies.6",
         -"Morphospecies.8", -"Morphospecies.10", -"Morphospecies.11",
         -"Morphospecies.16", -"Morphospecies.18", -"Morphospecies.20") 
#dim(juen_roof_fa)
#colSums(juen_roof_fa[,5:16])

juen_nonroof_fa <- juen_fa %>%
  filter(Roof == "Sem roof") %>% # 40 amostras
  filter(Local != "Utinga" & Local !="UFPA" ) %>% # agora 20 amostras
  select(-"Morphospecies.1", -"Morphospecies.2", -"Morphospecies.6",
         -"Morphospecies.9", -"Morphospecies.10", -"Morphospecies.13",
         -"Morphospecies.21")
#dim(juen_nonroof_fa)
#colSums(juen_nonroof_fa[,5:18])

juen_na_fa <- juen_fa %>%
  filter(Local == "Utinga" | Local == "UFPA") %>%
  select(-"Morphospecies.13", -"Morphospecies.14", -"Morphospecies.15",
         -"Morphospecies.16", -"Morphospecies.17", -"Morphospecies.18",
         -"Morphospecies.19", -"Morphospecies.20", -"Morphospecies.21")
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
         Morfospecies_name != "Morphospecies.20")

juen_nonroof_list <- juen_list %>%
  filter(Morfospecies_name != "Morphospecies.1" & 
           Morfospecies_name != "Morphospecies.2" &
           Morfospecies_name != "Morphospecies.6" &
           Morfospecies_name != "Morphospecies.9" &
           Morfospecies_name != "Morphospecies.10" &
           Morfospecies_name != "Morphospecies.13" &
           Morfospecies_name != "Morphospecies.21")

juen_na_list <- juen_list %>%
  filter(Morfospecies_name != "Morphospecies.13" & 
           Morfospecies_name != "Morphospecies.14" &
           Morfospecies_name != "Morphospecies.15" &
           Morfospecies_name != "Morphospecies.16" &
           Morfospecies_name != "Morphospecies.17" &
           Morfospecies_name != "Morphospecies.18" &
           Morfospecies_name != "Morphospecies.19" &
           Morfospecies_name != "Morphospecies.20" &
           Morfospecies_name != "Morphospecies.21")

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
           Morfospecies_name != "Morphospecies.20")

juen_nonroof_traits <- juen_traits %>%
  filter(Morfospecies_name != "Morphospecies.1" & 
           Morfospecies_name != "Morphospecies.2" &
           Morfospecies_name != "Morphospecies.6" &
           Morfospecies_name != "Morphospecies.9" &
           Morfospecies_name != "Morphospecies.10" &
           Morfospecies_name != "Morphospecies.13" &
           Morfospecies_name != "Morphospecies.21")

juen_na_traits <- juen_traits %>%
  filter(Morfospecies_name != "Morphospecies.13" & 
           Morfospecies_name != "Morphospecies.14" &
           Morfospecies_name != "Morphospecies.15" &
           Morfospecies_name != "Morphospecies.16" &
           Morfospecies_name != "Morphospecies.17" &
           Morfospecies_name != "Morphospecies.18" &
           Morfospecies_name != "Morphospecies.19" &
           Morfospecies_name != "Morphospecies.20" &
           Morfospecies_name != "Morphospecies.21")

# measures
juen_roof_measures <- juen_measures %>%
  filter(Roof == "com roof") %>%
  rename("dissolved_O2" = "dissolved_O2 (%)") %>%
  rename(all_of(dict_names))

juen_nonroof_measures <- juen_measures %>%
  filter(Roof == "Sem roof") %>%
  filter(Local != "Utinga" & Local !="UFPA") %>%
  rename("dissolved_O2" = "dissolved_O2 (%)") %>%
  rename(all_of(dict_names))

juen_na_measures <- juen_measures %>%
  filter(Roof == "Sem roof") %>%
  filter(Local == "Utinga" | Local == "UFPA") %>%
  rename("dissolved_O2" = "dissolved_O2 (%)") %>%
  rename(all_of(dict_names))

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
     file = here("dados_microcosmos",
                 "Juen_Belém_BR",
                 "juen_belem_br.RData"))



#--- Knapp_Czech ----
# tirar dúvidas
knapp_czech <- here("dados_microcosmos",
                   "Knapp_Czech")

knapp_fa <- read_xlsx(
  here(
    knapp_czech,
    "Knapp_Michal_Krivoklatsko.xlsx"),
  "fauna_abundance")[1,1]

knapp_list <- read_xlsx(
  here(
    knapp_czech,
    "Knapp_Michal_Krivoklatsko.xlsx"),
  "Fauna_morphospecies_list")[1,1]

knapp_traits <- read_xlsx(
  here(
    knapp_czech,
    "Knapp_Michal_Krivoklatsko.xlsx"),
  "Fauna_traits")[1,1]

knapp_measures <- read_xlsx(
  here(
    knapp_czech,
    "Knapp_Michal_Krivoklatsko.xlsx"),
  "measures_decomposition_geograph")[1:40, ]

# measures
knapp_roof_measures <- knapp_measures %>%
  filter(Experiment == "roof") %>%
  rename(all_of(dict_names)) %>%
  select(-Experiment)

knapp_nonroof_measures <- knapp_measures %>%
  filter(Experiment == "standard") %>%
  rename(all_of(dict_names)) %>%
  select(-Experiment)

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
  here(
    knapp_czech,
    "Knapp Michal_Krusne_hory.xlsx"),
  "fauna_abundance")

knapp_hory_list <- read_xlsx(
  here(
    knapp_czech,
    "Knapp Michal_Krusne_hory.xlsx"),
  "Fauna_morphospecies_list")

knapp_hory_traits <- read_xlsx(
  here(
    knapp_czech,
    "Knapp Michal_Krusne_hory.xlsx"),
  "Fauna_traits")

knapp_hory_measures <- read_xlsx(
  here(
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
knapp_hory_traits

# measures
knapp_hory_measures <- knapp_hory_measures %>%
  rename(all_of(dict_names)) %>%
  select(-Experiment)

knapp_data <- tibble(
  researcher = "Knapp",
  locality = "OreMountains_Czech",
  roof_treatment = NA,
  abundance = list(tibble(knapp_hory_fa)),
  list = list(tibble(knapp_hory_list)),
  traits= list(tibble(knapp_hory_traits)),
  measures=list(tibble(knapp_hory_measures)),
  obs = "")

save(knapp_roof_data,
     knapp_nonroof_data,
     knapp_data,
     file = here("dados_microcosmos",
                 "Knapp_Czech",
                 "knapp_czech.RData"))


#--- Luciano_argentina ----
luciano_argentina <- here("dados_microcosmos",
                    "Luciano_Argentina")

luciano_fa <- read_xlsx(
  here(
    luciano_argentina,
    "Microcosm_data_Cordoba.xlsx"),
  "Fauna_abundance")

luciano_list <- read_xlsx(
  here(
    luciano_argentina,
    "Microcosm_data_Cordoba.xlsx"),
  "Fauna_morphospecies_list")

luciano_traits <- read_xlsx(
  here(
    luciano_argentina,
    "Microcosm_data_Cordoba.xlsx"),
  "Fauna_traits")

luciano_measures <- read_xlsx(
  here(
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
  rename(all_of(dict_names)) 

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
     file = here("dados_microcosmos",
                 "Luciano_Argentina",
                 "luciano_argentina.RData"))


#--- Martins_Hamada_Amazon ----
# aqui existem alguns tratamentos juntos, com telhado, sem telhado
# e a diferenca de estratificacao com os microcosmos colocados a 15m de altura
# depois precisamos remover de cada experimento as faunas que nao estiveram
# presentes no respectivo tratamento (validado com Gustavo Romero)
martins_amazon <- here("dados_microcosmos",
                      "Martins_Hamada_Amazon")

martins_fa <- read_xlsx(
  here(
    martins_hamada,
    "Martins&Hamada_Manaus_01_12_23.xlsx"),
  "fauna_abundance")

martins_list <- read_xlsx(
  here(
    martins_hamada,
    "Martins&Hamada_Manaus_01_12_23.xlsx"),
  "Fauna_morphospecies_list")

martins_traits <- read_xlsx(
  here(
    martins_hamada,
    "Martins&Hamada_Manaus_01_12_23.xlsx"),
  "Fauna_traits")

martins_measures <- read_xlsx(
  here(
    martins_hamada,
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
         -Colembola, -Pompilidae, -Chilopoda)

martins_nonroof_fa <- martins_fa %>%
  filter(Treatment == "Managed forest - standard experiment" |
         Treatment == "Natural forest - standard experiment") %>%
  select(-Culicidae.Haemagogus, -Psychodidae, -Dytiscidae.sp2,
         -Curculionidae, -Termitidae, -Blaberidae, -Formicidae.morpho2,
         -Cicadidae, -Colembola, -"Anuro(girino)")
  

martins_na_fa <- martins_fa %>%
  filter(Treatment == "Managed forest - 15m" |
         Treatment == "Natural forest - 15m") %>%
  select(-Culicidae.Toxorhynchites, -Ceratopogonidae, -Psychodidae,
         -Scirtidae, -Stratiomidae, -Megapodagrionidae.Heteropodagrion,
         -Termitidae, -Blaberidae, -Pompilidae, -Chilopoda, -"Anuro(girino)")
  
martins_roof_fa[is.na(martins_roof_fa)] <- 0
martins_nonroof_fa[is.na(martins_nonroof_fa)] <- 0
martins_na_fa[is.na(martins_na_fa)] <- 0

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

martins_na_list <- martins_list %>% 
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

martins_na_traits <- martins_traits %>%
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
martins_roof_measures <- martins_measures %>%
  filter(Treatment == "Managed forest - roof" |
         Treatment == "Natural forest - roof") %>%
  rename(all_of(dict_names))

martins_nonroof_measures <- martins_measures %>%
  filter(Treatment == "Managed forest - standard experiment" |
         Treatment == "Natural forest - standard experiment") %>%
  rename(all_of(dict_names))

martins_na_measures <- martins_measures %>%
  filter(Treatment == "Managed forest - 15m" |
        Treatment == "Natural forest - 15m") %>%
  rename(all_of(dict_names))

# data
martins_roof_data <- tibble(
  researcher = "Martins",
  locality = "Amazon_Brazil", 
  roof_treatment = 1,
  abundance = list(tibble(martins_roof_fa)),
  list = list(tibble(martins_roof_list)),
  traits=list(tibble(martins_roof_traits)),
  measures=list(tibble(martins_roof_measures)),
  obs = "Tem invertebrado terrestre, revisar")

martins_nonroof_data <- tibble(
  researcher = "Martins",
  locality = "Amazon_Brazil", 
  roof_treatment = 0,
  abundance = list(tibble(martins_nonroof_fa)),
  list = list(tibble(martins_nonroof_list)),
  traits=list(tibble(martins_nonroof_traits)),
  measures=list(tibble(martins_nonroof_measures)),
  obs = "Tem invertebrado terrestre, revisar")

martins_na_data <- tibble(
  researcher = "Martins",
  locality = "Amazon_Brazil", 
  roof_treatment = NA,
  abundance = list(tibble(martins_na_fa)),
  list = list(tibble(martins_na_list)),
  traits=list(tibble(martins_na_traits)),
  measures=list(tibble(martins_na_measures)),
  obs = "Exp. estratificado 15m - Tem invertebrado terrestre, revisar")

save(martins_roof_data,
     martins_nonroof_data,
     martins_na_data,
     file = here("dados_microcosmos",
                 "Martins_Hamada_Amazon",
                 "martins_amazon_br.RData"))

# To do ----
# nested dataframes 
nested_df <- bind_rows(boukal_czech_roof,
                       boukal_czech_nonroof,
                       boukal_czech_plesnelake,
                       caliman_natal_br,
                       cardinale_usa_data,
                       collyer_japan_data,
                       cornelissen_roof_BR_data,
                       cornelissen_nonroof_BR_data,
                       cotriguacu_romero_data,
                       fabiola_roof_colombia_data,
                       fabiola_nonroof_colombia_data,
                       celine_canopy_frenchguyana_data,
                       celine_general_frenchguyana_data,
                       gonzales_data,
                       horvath_data,
                       izzo_data,
                       romero_japi_data,
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
                       martins_na_data)

View(nested_df)
save(nested_df,
     file = here("dados_microcosmos",
                 "nested_df.RData"))

load("dados_microcosmos/nested_df.RData")

#----
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

# Rodrigo_Argentina

# Sedney_Francis_Filipinas

# Srivastava_Canada

# Sweet_UK

# Thomas_Alemanha