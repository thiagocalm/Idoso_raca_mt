options(scipen = 99999)

# Bibliotecas -------------------------------------------------------------

ifelse(!require(tidyverse),install.packages("tidyverse"),require(tidyverse))
ifelse(!require(srvyr),install.packages("srvyr"),require(srvyr))
ifelse(!require(survey),install.packages("survey"),require(survey))


# Tratamento dos dados ----------------------------------------------------

## Por ano
periodo <- 2012:2023

for(i in seq_along(periodo)){

  ano <- periodo[i]

  # Progress
  print(paste0("Estamos no ano ", ano,"..."))

  # Importacao dos dados
  dir <- paste0("./Analises/dados/pnadc_",ano,".rds")

  pnad <- readRDS(dir)

  pnad <- pnad |>
    mutate(
      # Flags idoso e se participa do MT
      flag_idoso = case_when(V2009 >= 60 ~ "Idoso", TRUE ~ "Nao_idoso"),
      flag_participa = case_when(VD4001 == 1 ~ "forca_trabalho", TRUE ~ "nao_forca_trabalho"),
      # Cor ou raça
      cor_raca = case_when(V2010 == 2 | V2010 == 4 ~ "Negro",
                           V2010 == 1 ~ "Branco", TRUE ~ "Outros")
    ) |>
    # Grupo etario quinquenal
    mutate(
      grupo_etario = cut(V2009, breaks = seq(0,max(V2009),5),right = FALSE),
    ) |>
    mutate(
      grupo_etario = str_extract(
        str_remove(grupo_etario, pattern = "\\["),
        pattern = "^\\d+")) |>
    mutate(grupo_etario = as.integer(grupo_etario)) |>
    mutate(
      # Sexo
      sexo = factor(V2007, levels = c(1,2), labels = c("Masculino", "Feminino")),
      # Escolaridade máxima atingida
      educ_atingida = as.factor(case_when(
        VD3004 == 1 ~ "Sem instrução",
        VD3004 <= 3 ~ "Ensino Fundamental",
        VD3004 <= 5 ~ "Ensino Médio",
        VD3004 <= 7 ~ "Ensino Superior",
        TRUE ~ "NA/NR"))) |>
    mutate(
      # Grau de urbanização e metropolizacao simplificado
      status_rm = as.factor(case_when(
        V1023 <= 2 & V1022 == 1 ~ "Urbano metropolitano",
        V1023 > 2 & V1022 == 1 ~ "Urbano não-metropolitano",
        V1022 == 2 ~ "Rural",
        TRUE ~ "NA/NR")),
      # Regiao geográfica
      regiao = as.factor(case_when(
        UF <= 17 ~ "Norte",
        UF <= 29 ~ "Nordeste",
        UF <= 35 ~ "Sudeste",
        UF <= 43 ~ "Sul",
        TRUE ~ "Centro-Oeste"))) |>
    mutate(
      # Posicao da renda do domicilio na renda domiciliar per capita da populacao
      inc_dom_pc = VD5007/VD2003,
      quintil_inc = ntile(inc_dom_pc, 5),
      # Proporcao da renda na renda domiciliar
      inc_prop_individuo = case_when(is.na(VD4046) ~ 0, TRUE ~VD4046/VD5007))

  # Status de aposentadoria - Para 2012 a 2015
  if(ano <= 2015){
    pnad <- pnad |>
      mutate(
        # flag de aposentadoria
        flag_aposentadoria = case_when(V50011 == 1 ~ "Aposentado", TRUE ~ "Não aposentado"),
        # Parcela da aposentadoria na renda habitual de todas as fontes do domicilio
        inc_prop_aposentado = V500111/VD4046,
        inc_prop_aposentado_dom = V500111/VD5007)
  } else{
    pnad <- pnad |>
      mutate(
        # flag de aposentadoria
        flag_aposentadoria = case_when(V5004A == 1 ~ "Aposentado", TRUE ~ "Não aposentado"),
        # Parcela da aposentadoria na renda habitual de todas as fontes do domicilio
        inc_prop_aposentado = V5004A2/VD4046,
        inc_prop_aposentado_dom = V5004A2/VD5007)
  }

  # Variaveis domiciliares

  pnad <- pnad |>
    mutate(tem_crianca = case_when(grupo_etario < 15 ~ 1, TRUE ~ 0),
           tem_idoso_dependente = case_when(grupo_etario > 60 & VD4046 == 0 ~ 1, TRUE ~ 0)) |>
    group_by(id_dom) |>
    mutate(tem_crianca = sum(tem_crianca),
           tem_idoso_dependente = sum(tem_idoso_dependente)) |>
    ungroup() |>
    mutate(tem_crianca = case_when(tem_crianca > 0 ~ 1, TRUE ~ 0),
           tem_idoso_dependente = case_when(tem_idoso_dependente > 0 ~ 1, TRUE ~ 0))

  # Selecionar variaveis para analises
  pnad <- pnad |>
    select(id_dom,id_pes,ano = Ano, trimestre = Trimestre, UF, Estrato, idade_simples = V2009,
           flag_aposentadoria, flag_idoso, flag_participa, cor_raca, grupo_etario, sexo, educ_atingida,
           status_rm, regiao, inc_dom_pc, quintil_inc, inc_prop_individuo, inc_prop_aposentado,
           inc_prop_aposentado_dom, tipo_dom = VD2004, tem_crianca, tem_idoso_dependente,
           peso = V1032)

  ## Salvando base de dados

  dir_analise <- paste0("./Analises/dados/pnadc_",ano,"_analise.rds")

  saveRDS(pnad, dir_analise, compress = TRUE)

  # criando base empilhada
  if(i == 1){
    pnad_pooled <- pnad
  } else{
    pnad_pooled <- pnad_pooled %>%
      bind_rows(pnad)
  }

  # salvando base empilhada
  if(i == length(periodo)){
    saveRDS(pnad_pooled, "./Analises/dados/pnadc_analise.rds", compress = TRUE)
    rm(pnad, pnad_pooled)
    invisible(gc())
  }

  # Progress
  print(paste0("Menos um! Ano ", ano," foi!!!!"))
}
