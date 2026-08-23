# Packages
library(tidyverse)

# Set directory 
local_directory <- "G:/.shortcut-targets-by-id/1zI08lv0MwVKAzyncAsVjf3Y3DHzV2Qfd/Cotton_strips"

# salva no drive do projeto
load(here::here("nested_df.RData"))

###---- Reading Cottonstrips data
# Este loop do codigo percorre automaticamente a estrutura de pastas dos experimentos
# e extrai, de cada arquivo de registro (.log), a observação correspondente ao
# maior valor de "Load".
#
# Assumindo que a estrutura dos diretórios é
# local_directory/
# ├── MD01/
# │   └── LOG/
# │       ├── MD01_MF_P1_COARSE_1.log
# │       ├── MD01_MF_P1_COARSE_2.log
# │       ├── MD01_MF_P1_FINE_1.log
# │       └── ...

# Cada pasta "MD" representa uma unidade experimental/estudo.
#
# Dentro de cada pasta MD existe uma pasta "LOG", contendo vários arquivos
# individuais. Cada arquivo representa uma replicate e seu nome contém os
# metadados necessários para identificá-la.
#
# Exemplo:
#
# MD01_MF_P1_COARSE_1.log
# │    │  │  │       │
# │    │  │  │       └── replicate
# │    │  │  └────────── cotton_type
# │    │  └───────────── pot
# │    └──────────────── treatment
# └───────────────────── ID

# identificando as pastas de MDs
mds_list <- list.files(local_directory) %>%
  purrr::keep(~ grepl("^MD", .x))

# Criando uma lista para guardar a leitura de cada arquivo
resultados <- list()

# inicio do loop
start <- Sys.time()
for (md in mds_list[88]) {
  # pasta LOG daquele MD
  file <- file.path(
    local_directory,
    md,
    "LOG"
  )
  # arquivos LOG dentro da pasta
  list_pots <- list.files(file) %>%
    purrr::keep(~ grepl("^MD", .x))
  cat(
    "Processando",
    md,
    "\n"
  )
  # loop para cada arquivo LOG
  for (arquivo_nome in list_pots) {
    # caminho completo do arquivo
    arquivo <- file.path(
      file,
      arquivo_nome
    )
    # read log files
    ext <- tools::file_ext(arquivo)
    
    if (ext == "log") {
      
      dados <- read.delim(
        arquivo,
        skip = 5,
        header = TRUE
      )
      
    } else if (ext == "csv") {
      # MD99 tem uns .csv no meio
      dados <- read.csv2(
        arquivo,
        header = TRUE
      ) %>%
        rename(
          "Load" = "Load..kgF.",
          "Travel" = "Travel..mm.") %>%
        mutate(
          "Load" = as.numeric("Load"),
          "Travel" = as.numeric("Travel"),
          "Time" = as.numeric("Time")
        )
    }
    # variables
    # selecionando o maior valor de Load
    max_load <- max(
      dados$Load,
      na.rm = TRUE
    )
    # metadata in file name
    partes <- strsplit(
      basename(arquivo),
      "_"
    )[[1]]
    
    ID        <- partes[1]
    treatment <- partes[2]
    pot       <- partes[3]
    cotton_type  <- partes[4]
    rep       <- sub(
      "\\.log$",
      "",
      partes[5]
    )
    # selecionando a linha com o maior Load
    dados_max <- dados %>%
      filter(Load == max_load) %>%
      mutate(
        ID = ID,
        treatment = treatment,
        pot = pot,
        cotton_type = cotton_type,
        rep = rep
      )
    
    resultados[[length(resultados) + 1]] <- dados_max
    
    # adicionando à tabela final
    data <- dplyr::bind_rows(resultados)
  }
}
end <- Sys.time()
end - start

save(
  data,
  file = file.path(
    "00_preprocessed_data",
    "data_cottonstrip.RData")
)

# Preparar planilhas para preenchimento das informacoes dos cotton strip ----
# checando por que alguns experimentos tem mais tiras do que esperado?
# se cada MD tem no maximo 20 potes, podemos ter no maximo 120 tiras 
# (6 tiras por pote x 20 = 120 amostras de tiras por MD)
check_load <- data %>%
  mutate(
    replicate = paste(treatment, pot, cotton_type, rep, sep = "_")
  ) %>%
  group_by(ID, replicate) %>%
  summarise(
    duplicated = n() > 1,
    same_load = n_distinct(Load) == 1,
    .groups = "drop"
  ) %>%
  filter(duplicated) %>%
  group_by(ID) %>%
  summarise(
    all_duplicated_same_load = all(same_load),
    .groups = "drop"
  )
any(!check_load$all_duplicated_same_load) 

# clean data
# TODO: isso pode ser adicionado no for?
# todos os replicates duplicados correspondem ao mesmo 'load', como pegamos
# o maior valor de load, ele pode ter acontecido mais de uma vez na mesma corrida
# portanto pegaremos o de menor 'TIME'
data_clean <- data %>%
  mutate(
    log_id = paste(treatment, pot, cotton_type, rep, sep = "_")
  ) %>%
  group_by(ID, log_id) %>%
  arrange(Time, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    # Substituir MF/NF
    treatment = recode(treatment,
                       "MF" = "Managed forest",
                       "NF" = "Natural forest"),
    # Renomear pot: P1 → pot.1, P2 → pot.2...
    pot = paste0("pot.", gsub("P", "", pot)),
    ID = sub("^MD0+", "MD", ID)
  ) %>%
  select(
    ID,
    treatment,
    pot,
    cotton_type,
    rep,
    Reading,
    Load,
    Travel,
    Time,
    log_id
  ) %>%
  rename(
    Treatment = treatment, 
    Replicate = pot
  ) %>%
  # ajustando algumas numeracoes dos arquivos log para dar match com as
  # planilhas de abundancia e measures
  mutate(
    pot_num = as.integer(str_remove(Replicate, "pot\\.")),
    pot_num = if_else(
      ID %in% c("MD27") & Treatment == "Managed forest",
      pot_num - 10,
      pot_num
    ),
    Replicate = paste0("pot.", pot_num)
  ) %>%
  mutate(
    pot_num = as.integer(str_remove(Replicate, "pot\\.")),
    pot_num = if_else(
      ID %in% c("MD21", "MD22") & Treatment == "Managed forest",
      pot_num - 40,
      pot_num
    ),
    Replicate = paste0("pot.", pot_num)
  ) %>%
  mutate(
    pot_num = as.integer(str_remove(Replicate, "pot\\.")),
    pot_num = if_else(
      ID %in% c("MD50") & Treatment == "Managed forest",
      pot_num - 50,
      pot_num
    ),
    Replicate = paste0("pot.", pot_num)
  ) %>%
  mutate(
    pot_num = as.integer(str_remove(Replicate, "pot\\.")),
    pot_num = if_else(
      ID %in% c("MD47") & Treatment == "Managed forest",
      pot_num - 30,
      pot_num
    ),
    Replicate = paste0("pot.", pot_num)
  ) %>%
  mutate(
    pot_num = as.integer(str_remove(Replicate, "pot\\.")),
    pot_num = if_else(
      ID %in% c("MD50") & Treatment == "Natural forest",
      pot_num - 40,
      pot_num
    ),
    Replicate = paste0("pot.", pot_num)
  ) %>%
  mutate(
    pot_num = as.integer(str_remove(Replicate, "pot\\.")),
    pot_num = if_else(
      ID %in% c("MD21", "MD22", "MD47", "") & Treatment == "Natural forest",
      pot_num - 20,
      pot_num
    ),
    Replicate = paste0("pot.", pot_num)
  ) %>%
  mutate(
    pot_num = as.integer(str_remove(Replicate, "pot\\.")),
    pot_num = if_else(
      ID %in% c("MD23", "MD51", "MD79") & Treatment == "Natural forest",
      pot_num - 10,
      pot_num
    ),
    Replicate = paste0("pot.", pot_num)
  ) %>%
  mutate(
    pot_num = as.integer(str_remove(Replicate, "pot\\.")),
    pot_num = if_else(
      ID %in% c("MD56") & Treatment == "Natural forest",
      pot_num + 10,
      pot_num
    ),
    Replicate = paste0("pot.", pot_num)
  ) %>%
  select(-pot_num) %>%
  # trocar managed e natural no MD20
  mutate(
    Treatment = if_else(
      ID == "MD20",
      case_when(
        Treatment == "Managed forest" ~ "Natural forest",
        Treatment == "Natural forest" ~ "Managed forest",
        TRUE ~ Treatment
      ),
      Treatment
    )
  )

md_nest <- data_clean %>%
  group_by(ID) %>%
  nest() %>%
  rename(cottonstrip = data) 

# conta linhas dentro de cada nested
# todos tem menos de 120, menos o MD6 mas ele tem mais potes mesmo
#md_nest %>%
#  mutate(n_strips = map_int(cottonstrip, nrow)) %>%
#  View()
# uniao com a tabela gerada no 02_trait_revised_join.R
nested_database <- nested_database_cleaned %>%
  left_join(md_nest) %>% 
  relocate(cottonstrip, .before = obs) 

# Salvar uma primeira versao; vou agora entender quais MDs estamos perdendo
# por erro no cruzamento, com isso a base vai poder estar 100% integrada entre
# os diferentes dataframes "abundance" e "measures"
local_directory <- "G:/.shortcut-targets-by-id/1zI08lv0MwVKAzyncAsVjf3Y3DHzV2Qfd/Cotton_strips"
save(
  nested_database,
  file = file.path(local_directory, "nested_df.RData")
)

save(
  nested_database,
  file = here::here("nested_df.RData")
)

# Em construção ----

# abundancia e measures teoricamente tem que ter o mesmo n de dados faltantes
# os dados tem que bater com a control list

# aqui na verdade so vou conferir integracao, os ajustes farei o maximo logo do 
# carregamento dos dados, no script 00_preprocessing_data.R
# a ideia é que esse script seja apenas para integração dos cottonstrips
# como essa parte de processamento demora mais que as outras (cerca de ~1h)
# ainda podemos optimizar esse carregamento; mas por hora importante
# é estar funcional - entao teoricamente ele termina antes dessa secao
# em construcao

# Checando as incongruencias entre as bases
# Agora precisamos conferir se os cotton strips estao batendo com a planilha
# de abundance & measures para que a tabela relacional fique completa
abundance_id <- nested_database_cleaned %>%
  select(ID, abundance) %>%
  unnest() %>%
  select(ID, Treatment, Replicate)

df_nested_cottonstrip <- left_join(
  abundance_id,
  data_clean,
  by = c("ID", "Treatment", "Replicate")) 

# TODO precisa padronizar os potes de abundance & measures para dar o match
df_na_abund <- df_nested_cottonstrip %>%
  filter(if_any(everything(), is.na)) %>%
  select(ID, Treatment, Replicate) %>%
  # esses potes nao tem dados de tiras
  filter(!ID %in% c(
    "MD18", "MD31", "MD48", "MD49", "MD57", "MD58", "MD59",
    "MD64", "MD70", "MD87", "MD100", "MD101", "MD102",
    "MD103", "MD105", "MD106", "MD107", "MD104", "MD108"
  ))

# quantas tiras faltam por experimento/tratamento no microcosmo?
list_missing_abund <- df_na_abund %>%
  group_by(ID, Treatment) %>%
  summarise(
    n_missing = n(),
    .groups = "drop"
  ) 

###----
control_list <- readxl::read_xlsx(
  file.path(local_directory,
            "df_fill_cottonstrip.xlsx")) %>%
  mutate(
    SAMPLE_OUTSIDE = na_if(SAMPLE_OUTSIDE, "NA"),
    SAMPLE_FINE    = na_if(SAMPLE_FINE, "NA"),
    SAMPLE_COARSE  = na_if(SAMPLE_COARSE, "NA")
  ) %>%
  mutate(
    with_strip = if_else(
      if_any(c(SAMPLE_OUTSIDE, SAMPLE_FINE, SAMPLE_COARSE), ~ !is.na(.x)),
      1,
      0
    )
  ) %>% select(ID, treatment, replicate, with_strip) %>%
  rename(Treatment = treatment,
         Replicate = replicate) %>%
  mutate(
    Treatment = case_when(
      str_detect(Treatment, "Natural forest") ~ "Natural forest",
      str_detect(Treatment, "Managed forest") ~ "Managed forest",
      TRUE ~ Treatment
    )
  )

control_summary <- control_list %>%
  group_by(ID, Treatment) %>%
  summarise(
    n_present = sum(with_strip == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # em relacao aos dados faltantes, 
  filter(ID %in% list_missing$ID)

# TODO tem que checar a origem dos NAs
# replicas com dados de tiras mas que estao vazias
left_join(df_na_abund,
          control_list, 
          by = c("ID", "Treatment", "Replicate")) %>%
  filter(with_strip == 1)

left_join(list_missing,
          control_summary, 
          by = c("ID", "Treatment")) %>% 
  mutate(soma = n_missing + n_present) %>%
  View()

# teoricamente todos tem que somar 10
# TODO checar o que não soma	
# MD22
# Managed forest
# MD33
# Natural forest
# MD54

# TODO Checar esses MDs
abundance <-unique (df_na_abund$ID)

# Precisamos conferir quem são os NA's na base agora. Esses são os
# MDs que tem potes sem correspondencia na tira. Precisamos também 
# que os dados de "cotton_strip" sejam relacionáveis aos dados em
# abundance e measures; permitindo a integracao dos dados de cada tira e suas
# replicas dentro de cada experimento - podendo ser entendido como a biodiversidade
# e o clima atuam na decomposicao ecossistemica. 

## Checar o restante em measures ----
df_cottonstrip <- nested_database_cleaned %>%
  select("ID", "measures") %>%
  mutate(measures = map(measures, ~ .x %>% select(treatment, replicate))) %>%
  unnest(measures) 

df_nested_cottonstrip <- left_join(
  df_cottonstrip,
  data_clean,
  by = c("ID", 
         c("treatment" = "Treatment"),
         c("replicate" = "Replicate")))

# TODO precisa padronizar os potes de abundance & measures para dar o match
df_na_measures <- df_nested_cottonstrip %>%
  filter(if_any(everything(), is.na)) %>%
  # esses potes nao tem dados de tiras
  filter(!ID %in% c(
    "MD18", "MD31", "MD48", "MD49", "MD57", "MD58", "MD59",
    "MD64", "MD70", "MD87", "MD100", "MD101", "MD102",
    "MD103", "MD105", "MD106", "MD107", "MD104", "MD108"
  ))

# quantas tiras faltam por experimento/tratamento no microcosmo?
list_missing_measures <- df_na_measures %>%
  group_by(ID, treatment) %>%
  summarise(
    n_missing_measures = n(),
    .groups = "drop"
  ) %>%
  rename(Treatment = treatment)

check_measures <- left_join(
  list_missing_measures,
  list_missing_abund,
  by = c("ID", "Treatment")
) %>%
  # as que estiverem batendo é por que o merge ta funcionando!
  filter(n_missing_measures != n_missing | is.na(n_missing)) #%>%

View(check_measures)

# TODO Checar esses MDs
measures <- unique(check_measures$ID)
# measures
# "MD34" "MD56" "MD69" "MD76"

setdiff(abundance, measures)
setdiff(measures, abundance)


## Integrar measures + abundance + cottonstrip ----

# aqui na verdade so vou conferir, os ajustes farei o maximo logo do 
# carregamento dos dados, no script 00_preprocessing_data.R
# a ideia é que esse script seja apenas para integração dos cottonstrips
# como essa parte de processamento demora mais que as outras (cerca de ~1h)
# ainda podemos optimizar esse carregamento; mas por hora importante
# é estar funcional

## Salvar a base integrada ----
save(
  nested_database,
  file = here::here("nested_cotton_df.RData")
)

save(
  nested_database,
  file = file.path(local_directory, "nested_cotton_df.RData")
)

