options(scipen = 999) #retirando notacao cientifica

# Bibliotecas -------------------------------------------------------------

ifelse(!require(PNADcIBGE),install.packages("PNADcIBGE"),require(PNADcIBGE))
ifelse(!require(tidyverse),install.packages("tidyverse"),require(tidyverse))

## Loop para todo o periodo
periodo <- 2020:2023

for(i in seq_along(periodo)){
  ano <- periodo[i]
  print(paste0("Começando ano: ", ano,"..."))

  # Inputs por ano

  if(ano %in% 2020:2021){
    microdados <- paste0("./Analises/dados/raw/PNADC_",ano,"_visita5.txt")
    input <- paste0("./Analises/dados/raw/input_",ano,"_visita5.txt")
  }else{
    microdados <- paste0("./Analises/dados/raw/PNADC_",ano,"_visita1.txt")
    input <- paste0("./Analises/dados/raw/input_",ano,"_visita1.txt")
  }

  # variaveis

  if(ano %in% 2012:2015){
    vars <- c("Ano",
              "Trimestre",
              "UF",
              "UPA", # Unidade Primaria de Amostragem
              "Estrato",
              "V1008", # Controle do domicilio
              "V1014", # numero do painel
              "V1022", # situacao do domicilio (rural-urbano)
              "V1023", # tipo de area (Capital, Resto da RM, Resto da RIDE, Resto da UF)
              "V1032", # peso do domicilio e das pessoas
              "V2001", # controle pessoa no domicilio
              "V2003", # numero de ordem no domicilio
              "V2007", # sexo
              "V2009", # idade em anos completos
              "V2010", # cor ou raca
              "V5001", # Alguma pessoa do domicilio recebeu aposentadoria
              "V50011", # Se a respectiva pessoa recebeu aposentadoria
              "V500111", # Valor efetivamente recebido de aposentadoria
              "VD2002", # condicao no domicilio agregada
              "VD2003", # numero de componentes no domicilio
              "VD2004", # tipo de domicilio
              "VD3004", # nivel de instrucao mais elevado alcancado
              "VD3005", # anos de estudo
              "VD4001", # condicao em relacao a forca de trabalho
              "VD4002", # condicao de ocupacao
              "VD4046", # rendimento habitual recebido de todas as fontes
              "VD4048", # rendimento habitual recebido de outras fontes senão trabalho
              "VD5001", # rendimento efetivo domiciliar
              "VD5007" # rendimento habitual domiciliar de todas as fontes
    )
  } else{
    vars <- c("Ano",
              "Trimestre",
              "UF",
              "UPA", # Unidade Primaria de Amostragem
              "Estrato",
              "V1008", # Controle do domicilio
              "V1014", # numero do painel
              "V1022", # situacao do domicilio (rural-urbano)
              "V1023", # tipo de area (Capital, Resto da RM, Resto da RIDE, Resto da UF)
              "V1032", # peso do domicilio e das pessoas
              "V2001", # controle pessoa no domicilio
              "V2003", # numero de ordem no domicilio
              "V2007", # sexo
              "V2009", # idade em anos completos
              "V2010", # cor ou raca
              "V5004A", # Se a respectiva pessoa recebeu aposentadoria
              "V5004A2", # Valor efetivamente recebido de aposentadoria
              "VD2002", # condicao no domicilio agregada
              "VD2003", # numero de componentes no domicilio
              "VD2004", # tipo de domicilio
              "VD3004", # nivel de instrucao mais elevado alcancado
              "VD3005", # anos de estudo
              "VD4001", # condicao em relacao a forca de trabalho
              "VD4002", # condicao de ocupacao
              "VD4046", # rendimento habitual recebido de todas as fontes
              "VD4048", # rendimento habitual recebido de outras fontes senão trabalho
              "VD5001", # rendimento efetivo domiciliar
              "VD5007" # rendimento habitual domiciliar de todas as fontes
    )
  }

  # Importacao dos dados

  dados_pnadc <- read_pnadc(
    microdata = microdados, # ano selecionado
    input_txt = input
  )

  # Selecao de variaveis
  dados_pnadc <- dados_pnadc |> select(all_of(vars))

  # criacao de variaveis identificadoras

  dados_pnadc <- dados_pnadc |>
    mutate_if(is.character, as.numeric) |>
    mutate(id_dom = as.numeric(paste0(UPA,V1008,V1014)),
           id_pes = as.numeric(paste0(UPA, V1008, V1014, V2003))
    )  |>
    select(-c(UPA, V1008, V1014, V2003, V2001)) |>
    select(id_dom, id_pes, everything())

  ## Salvando base de dados

  dir <- paste0("./Analises/dados/pnadc_",ano,".rds")

  saveRDS(dados_pnadc, dir, compress = TRUE)
  print(paste0("Ano ", ano," finalizado!!!"))
  rm(dados_pnadc)
  invisible(gc())
}

