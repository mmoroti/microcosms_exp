# Packages
library(tidyverse)

# Set directory 
local_directory <- "G:/.shortcut-targets-by-id/1zI08lv0MwVKAzyncAsVjf3Y3DHzV2Qfd/Cotton_strips"

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
#View(check_load) 

# clean data
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
load("nested_df.RData")

nested_database <- nested_database_cleaned %>%
  left_join(md_nest) %>% 
  relocate(cottonstrip, .before = obs) 

# Salvar uma primeira versao; vou agora entender quais MDs estamos perdendo
# por erro no cruzamento, com isso a base vai poder estar 100% integrada entre
# os diferentes dataframes "abundance" e "measures"
save(
  nested_database,
  file = "nested_df.RData"
)

# Em construção ----
# Checando as incongruencias entre as bases
# Agora precisamos conferir se os cotton strips estao batendo com a planilha
# de abundance & measures para que a tabela relacional fique completa
test <- nested_database_cleaned %>%
  select(ID, abundance) %>%
  unnest() %>%
  select(ID, Treatment, Replicate)

df_nested_cottonstrip <- left_join(
  test,
  data_clean,
  by = c("ID", "Treatment", "Replicate")) 

# TODO precisa padronizar os potes de abundance & measures para dar o match
df_na <- df_nested_cottonstrip %>%
  filter(if_any(everything(), is.na))
View(df_na)

# TODO Checar esses MDs
abundance <-unique(df_na$ID)

# Precisamos conferir quem são os NA's na base agora. Esses são os
# MDs que tem potes sem correspondencia na tira. Precisamos também 
# que os dados de "cotton_strip" sejam relacionáveis aos dados em
# abundance e measures; permitindo a integracao dos dados de cada tira e suas
# replicas dentro de cada experimento - podendo ser entendido como a biodiversidade
# e o clima atuam na decomposicao ecossistemica. 

# faltam algumas tiras para esses MDs
# "MD1"   "MD2"   "MD5"   "MD7"  "MD17"

# sem dados de tira
# "MD18"; 

# conferir
#"MD20"  "MD21"  "MD22"  "MD23" 
# "MD26"  "MD27"  "MD28"  "MD29"  "MD30"  "MD31"
# "MD32"  "MD33"  "MD35"  "MD42"  "MD43" 
# "MD45"  "MD46"  "MD47"  "MD48"  "MD49"  "MD50"  
# "MD51"  "MD54"  "MD56"  "MD57"  "MD58" 
# "MD59"  "MD60"  "MD62"  "MD63"  "MD64"  "MD65"  
# "MD70"  "MD72"  "MD73"  "MD74"  "MD76" 
# "MD79"  "MD83"  "MD86"  "MD87"  "MD100" "MD101" 
# "MD102" "MD103" "MD105" "MD106" "MD107"

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
df_na <- df_nested_cottonstrip %>%
  filter(if_any(everything(), is.na))

# TODO Checar esses MDs
measures <- unique(df_na$ID)
# "MD1"   "MD2"   "MD5"   "MD7"   "MD10"  "MD17"  
# "MD18"  "MD20"  "MD21"  "MD22"  "MD23" 
# "MD26"  "MD27"  "MD28"  "MD29"  "MD30"  "MD31"
# "MD32"  "MD33"  "MD35"  "MD42"  "MD43" 
# "MD45"  "MD46"  "MD47"  "MD48"  "MD49"  "MD50"  
# "MD51"  "MD54"  "MD56"  "MD57"  "MD58" 
# "MD59"  "MD60"  "MD62"  "MD63"  "MD64"  "MD65"  
# "MD70"  "MD72"  "MD73"  "MD74"  "MD76" 
# "MD78"  "MD79"  "MD83"  "MD86"  "MD87"  "MD100" 
# "MD101" "MD102" "MD103" "MD105" "MD106"
#"MD107" "MD24"  "MD25"  "MD34"  "MD69"  "MD104" "MD108"

setdiff(abundance, measures)
setdiff(measures, abundance)

t <- nested_database_cleaned %>% filter(ID == "MD78") 
View(t)
