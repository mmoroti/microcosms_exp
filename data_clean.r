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
#----
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


#----
#--- Caliman_Natal_BR
#----
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
View(caliman_traits)

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
  measures=list(tibble(caliman_measures)))

# save .RData from Boukal
save(caliman_natal_br,
     #boukal_czech_nonroof,
     file = here("dados_microcosmos",
                 "Caliman_Natal_BR",
                 "Caliman_Natal_BR.RData")) 
#----
#--- Campos_do_Jordao_e_Sta_Virginia
#----
# dados indisponiveis ainda
# TODO: precisa separar em duas pastas

#----
#--- Cardinale_USA
#----
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
  locality = "USA", 
  roof_treatment = NA,
  abundance = list(tibble(cardinale_fa)),
  list = list(tibble(cardinale_list)),
  traits=list(tibble(cardinale_traits)),
  measures=list(tibble(cardinale_measures)))

# save .RData from Boukal
save(cardinale_usa_data,
     file = here(cardinale_usa,
                 "Cardinale_USA.RData")) 

#----
#--- Cardoso_Romero
#----
# TODO: Dados incompletos, mas as coordenadas foram convertidas direto no xlsx.

#----
#--- Collyer_Japan
#----
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
  locality = "Japan", 
  roof_treatment = NA,
  abundance = list(tibble(collyer_fa)),
  list = list(tibble(collyer_list)),
  traits=list(tibble(collyer_traits)),
  measures=list(tibble(collyer_measures)))

save(collyer_japan_data,
     #boukal_czech_nonroof,
     file = here(collyer_japan,
                 "Collyer_Japan.RData")) 

#----
#---- Cornelissen_BR
#----
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
  locality = "Minas Gerais, BR", 
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
  locality = "Minas Gerais, BR", 
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
#----
#--- Cotriguacu_Romero
#----
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
View(cotriguacu_fa)

# list
cotriguacu_list <- cotriguacu_list %>% mutate_all(~na_if(., "-"))
View(cotriguacu_list)

# traits
View(cotriguacu_traits)

# measures
View(cotriguacu_measures)



#----
#--- Fabiola_Colombia
#----
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

colSums(fabiola_roof_fa[,5:15])
rowSums(fabiola_roof_fa[,5:15])

# as especies Eristalis.sp.1 Forcipomyia.sp.1 nao foram amostradas
# no tratamento sem telhado, por isso foram retiradas da list
# da traits 
# isopoda e terrestre, por isso foi retirado
fabiola_nonroof_fa <- fabiola_fa %>%
  filter(str_detect(fabiola_fa$`ID. Own`,
                    "^BC|^PC")) %>%
  select(-"Eristalis.sp.1", -"Forcipomyia.sp.1", 
         -"Isopoda.sp.1", -"Replicate...3")

colSums(fabiola_nonroof_fa[,5:15])
rowSums(fabiola_nonroof_fa[,5:15]) # alguns potes com zero

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
  locality = "Colombia", 
  roof_treatment = 1,
  abundance = list(tibble(fabiola_roof_fa)),
  list = list(tibble(fabiola_roof_list)),
  traits=list(tibble(fabiola_roof_traits)),
  measures=list(tibble(fabiola_roof_measures)))

fabiola_nonroof_colombia_data <- tibble(
  researcher = "Fabiola",
  locality = "Colombia", 
  roof_treatment = 0,
  abundance = list(tibble(fabiola_nonroof_fa)),
  list = list(tibble(fabiola_nonroof_list)),
  traits=list(tibble(fabiola_nonroof_traits)),
  measures=list(tibble(fabiola_nonroof_measures)))


#----
#--- French_Guyana_Celine
#----
# linhas 11-15 precisam ser deletadas, deletei direto no .xlsx

#----
# TODO
#----
# nested dataframes 
nested_df <- bind_rows(boukal_czech_roof,
                       boukal_czech_nonroof,
                       boukal_czech_plesnelake,
                       caliman_natal_br,
                       cardinale_usa_data,
                       collyer_japan_data,
                       cornelissen_roof_BR_data,
                       cornelissen_nonroof_BR_data,
                       fabiola_roof_colombia_data,
                       fabiola_nonroof_colombia_data) 
View(nested_df)

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