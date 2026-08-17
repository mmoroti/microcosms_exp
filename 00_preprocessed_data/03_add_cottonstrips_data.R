# Packages
library(tidyverse)

# Set directory 
local_directory <- "G:/.shortcut-targets-by-id/1zI08lv0MwVKAzyncAsVjf3Y3DHzV2Qfd/Cotton_strips"

###---- Reading Cottonstrips data
# MDs folders
mds_list <- list.files(local_directory) %>%
  purrr::keep(~ grepl("^MD", .x))


# results list
resultados <- list()
# tempo gasto: 15s para processar 5 MDs
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

save(data,
     file = "cottonstrip_data")

# Preparar planilhas para preenchimento das informacoes dos cotton strip ----
load("nested_df.RData")
load("cottonstrip_data")

md_nest <- data %>%
  group_by(ID) %>%  # Agrupa pelo speciesKey
  nest() %>%
  rename(cottonstrip = data) %>%
  mutate(
    ID = sub("^MD0+", "MD", ID)
  )

head(md_nest)

t <-left_join(
    nested_database_cleaned,
    md_nest,
    by = "ID") 

View(t)

## Checar o restante
df_cottonstrip <- nested_database_cleaned %>%
  select("ID", "measures") %>%
  mutate(measures = map(measures, ~ .x %>% select(treatment, replicate))) %>%
  unnest(measures) 

# adicionar dados ja existentes
pre_processing_data <- readxl::read_excel(file.path(
  local_directory, "df_fill_cottonstrip.xlsx"))

# join
names(df_cottonstrip)
names(pre_processing_data)

df_fill_cottonstrip <- left_join(
  df_cottonstrip,
  pre_processing_data,
  by = c("ID", "treatment", "replicate")) 

openxlsx::write.xlsx(
  df_fill_cottonstrip,
  "df_fill_cottonstrip.xlsx"
)

