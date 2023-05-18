options(scipen = 99999)

# Bibliotecas -------------------------------------------------------------

ifelse(!require(tidyverse),install.packages("tidyverse"),require(tidyverse))
ifelse(!require(srvyr),install.packages("srvyr"),require(srvyr))
ifelse(!require(survey),install.packages("survey"),require(survey))

# Import data -------------------------------------------------------------

pnad <- readRDS("./Analises/dados/pnadc_analise.rds")

# Aplicacao do plano amostral -----------------------------------------------

pnad_plan <- pnad |>
  filter(idade_simples >= 10) |>
  filter(cor_raca != "Outros") |>
  srvyr::as_survey_design(ids = id_pes,
                          weights = peso)

pnad_idoso_plan <- pnad |>
  filter(flag_idoso == "Idoso") |>
  filter(cor_raca != "Outros") |>
  filter(idade_simples >= 10) |>
  srvyr::as_survey_design(ids = id_pes,
                          weights = peso)

rm(pnad)

# 1. Tabela agregada idosos e não idosos ------------------------------------

# Total

t_total <- pnad_plan |>
  group_by(ano, flag_idoso, cor_raca) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_plan |>
      filter(idade_simples >= 10) |>
      srvyr::as_survey_design(ids = id_pes,
                              weights = peso) |>
      group_by(ano, cor_raca) |>
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
      mutate(flag_idoso = "Total") |>
      select(ano, flag_idoso, everything())
  )

# Sexo

t_sexo <- pnad_plan |>
  group_by(ano, flag_idoso, cor_raca, sexo) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_plan |>
      filter(idade_simples >= 10) |>
      srvyr::as_survey_design(ids = id_pes,
                              weights = peso) |>
      group_by(ano, cor_raca, sexo) |>
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
      mutate(flag_idoso = "Total") |>
      select(ano, flag_idoso, everything())
  )

# Escolaridade

t_escolaridade <- pnad_plan |>
  group_by(ano, flag_idoso, cor_raca, educ_atingida) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_plan |>
      filter(idade_simples >= 10) |>
      srvyr::as_survey_design(ids = id_pes,
                              weights = peso) |>
      group_by(ano, cor_raca, educ_atingida) |>
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
      mutate(flag_idoso = "Total") |>
      select(ano, flag_idoso, everything())
  )

# Quintil de renda

t_renda <- pnad_plan |>
  group_by(ano, flag_idoso, cor_raca, quintil_inc) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_plan |>
      filter(idade_simples >= 10) |>
      srvyr::as_survey_design(ids = id_pes,
                              weights = peso) |>
      group_by(ano, cor_raca, quintil_inc) |>
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
      mutate(flag_idoso = "Total") |>
      select(ano, flag_idoso, everything())
  )

# Tipo de domicilio

t_tipodom <- pnad_plan |>
  group_by(ano, flag_idoso, cor_raca, tipo_dom) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_plan |>
      filter(idade_simples >= 10) |>
      srvyr::as_survey_design(ids = id_pes,
                              weights = peso) |>
      group_by(ano, cor_raca, tipo_dom) |>
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
      mutate(flag_idoso = "Total") |>
      select(ano, flag_idoso, everything())
  )

# Existencia de dependente

t_tem_dep <- pnad_plan |>
  group_by(ano, flag_idoso, cor_raca, tem_crianca, tem_idoso_dependente) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_plan |>
      filter(idade_simples >= 10) |>
      srvyr::as_survey_design(ids = id_pes,
                              weights = peso) |>
      group_by(ano, cor_raca, tem_crianca, tem_idoso_dependente) |>
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
      mutate(flag_idoso = "Total") |>
      select(ano, flag_idoso, everything())
  )

# Status de Aposentadoria

t_aposentadoria <- pnad_plan |>
  group_by(ano, flag_idoso, cor_raca, flag_aposentadoria) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_plan |>
      filter(idade_simples >= 10) |>
      srvyr::as_survey_design(ids = id_pes,
                              weights = peso) |>
      group_by(ano, cor_raca, flag_aposentadoria) |>
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
      mutate(flag_idoso = "Total") |>
      select(ano, flag_idoso, everything())
  )

# 2. Tabela agregada somente idosos por part. -------------------------------

# Total

t_part_total <- pnad_idoso_plan |>
  group_by(ano, cor_raca, flag_participa) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2))

# Sexo

t_part_sexo <- pnad_idoso_plan |>
  group_by(ano, cor_raca, flag_participa, sexo) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2))

# Escolaridade

t_part_escolaridade <- pnad_idoso_plan |>
  group_by(ano, cor_raca, flag_participa, educ_atingida) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2))

# Quintil de renda

t_part_renda <- pnad_idoso_plan |>
  group_by(ano, cor_raca, flag_participa, quintil_inc) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2))

# Tipo de domicilio

t_part_tipodom <- pnad_idoso_plan |>
  group_by(ano, cor_raca, flag_participa, tipo_dom) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2))

# Existencia de dependente

t_part_tem_dep <- pnad_idoso_plan |>
  group_by(ano, cor_raca, flag_participa, tem_crianca, tem_idoso_dependente) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2))

# Status de Aposentadoria

t_part_aposentadoria <- pnad_idoso_plan |>
  group_by(ano, cor_raca,  flag_participa, flag_aposentadoria) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2))

# Agrupando resultados ----------------------------------------------------

# 1. Composição entre grupo de idosos e não idosos
t_agregada <- t_total |>
  mutate(categoria = "Pop. total",
         variaveis = "Total") |>
  select(ano, flag_idoso, cor_raca, categoria, variaveis, prop) |>
  bind_rows(
    t_sexo |>
      mutate(categoria = "Sexo",
             variaveis = sexo) |>
      select(ano, flag_idoso, cor_raca, categoria, variaveis, prop)
  ) |>
  bind_rows(
    t_escolaridade |>
      mutate(categoria = "Escolaridade",
             variaveis = educ_atingida) |>
      select(ano, flag_idoso, cor_raca, categoria, variaveis, prop)
  ) |>
  bind_rows(
    t_renda |>
      mutate(quintil_inc = factor(
        quintil_inc,
        levels = c(1,2,3,4,5),
        labels = c("P20","P40","P60","P80","P100"))) |>
      mutate(categoria = "Dist. Renda",
             variaveis = quintil_inc) |>
      filter(!is.na(variaveis)) |>
      select(ano, flag_idoso, cor_raca, categoria, variaveis, prop)
  ) |>
  bind_rows(
    t_tipodom |>
      mutate(tipo_dom = as.factor(case_when(
        tipo_dom == 1 ~ "Unipessoal",
        tipo_dom == 2 ~ "Nuclear",
        tipo_dom == 3 ~ "Estendida",
        tipo_dom == 4 ~ "Composta"))) |>
      mutate(categoria = "Tipo de domicílio",
             variaveis = tipo_dom) |>
      filter(!is.na(variaveis)) |>
      select(ano, flag_idoso, cor_raca, categoria, variaveis, prop)
  ) |>
  bind_rows(
    t_tem_dep |>
    mutate(tem_dependente = as.factor(case_when(
      tem_crianca == 1 & tem_idoso_dependente == 0 ~ "Somente criança",
      tem_crianca == 0 & tem_idoso_dependente == 1 ~ "Somente idoso",
      tem_crianca == 1 & tem_idoso_dependente == 1 ~ "Criança e idoso",
      tem_crianca == 0 & tem_idoso_dependente == 0 ~ "Nenhum"))) |>
    select(-c(tem_crianca, tem_idoso_dependente)) |>
    ungroup() |>
    group_by(ano, flag_idoso, cor_raca, tem_dependente) |>
    summarise(n = n) |>
    summarise(ano = ano,
              flag_idoso = flag_idoso,
              cor_raca = cor_raca,
              tem_dependente = tem_dependente,
              n = n,
              prop = round(n/sum(n)*100,2)) |>
      mutate(categoria = "Existência de dependente",
             variaveis = tem_dependente) |>
      filter(!is.na(variaveis)) |>
      select(ano, flag_idoso, cor_raca, categoria, variaveis, prop)
  ) |>
  bind_rows(
    t_aposentadoria |>
      mutate(categoria = "Condição de aposentadoria",
             variaveis = flag_aposentadoria) |>
      filter(!is.na(variaveis)) |>
      select(ano, flag_idoso, cor_raca, categoria, variaveis, prop)
  ) |>
  select(ano, categoria, flag_idoso, cor_raca, variaveis, prop) |>
  arrange(categoria, flag_idoso, cor_raca)

# 2. Composição entre grupo de idosos que participam e nao participam

t_agregada_participa <- t_part_total |>
  mutate(categoria = "Pop. total",
         variaveis = "Total") |>
  select(ano, cor_raca, flag_participa, categoria, variaveis, prop) |>
  bind_rows(
    t_part_sexo |>
      mutate(categoria = "Sexo",
             variaveis = sexo) |>
      select(ano, cor_raca, flag_participa, categoria, variaveis, prop)
  ) |>
  bind_rows(
    t_part_escolaridade |>
      mutate(categoria = "Escolaridade",
             variaveis = educ_atingida) |>
      select(ano, cor_raca, flag_participa, categoria, variaveis, prop)
  ) |>
  bind_rows(
    t_part_renda |>
      mutate(quintil_inc = factor(
        quintil_inc,
        levels = c(1,2,3,4,5),
        labels = c("P20","P40","P60","P80","P100"))) |>
      mutate(categoria = "Dist. Renda",
             variaveis = quintil_inc) |>
      filter(!is.na(variaveis)) |>
      select(ano, cor_raca, flag_participa, categoria, variaveis, prop)
  ) |>
  bind_rows(
    t_part_tipodom |>
      mutate(tipo_dom = as.factor(case_when(
        tipo_dom == 1 ~ "Unipessoal",
        tipo_dom == 2 ~ "Nuclear",
        tipo_dom == 3 ~ "Estendida",
        tipo_dom == 4 ~ "Composta"))) |>
      mutate(categoria = "Tipo de domicílio",
             variaveis = tipo_dom) |>
      filter(!is.na(variaveis)) |>
      select(ano, cor_raca, flag_participa, categoria, variaveis, prop)
  ) |>
  bind_rows(
    t_part_tem_dep |>
      mutate(tem_dependente = as.factor(case_when(
        tem_crianca == 1 & tem_idoso_dependente == 0 ~ "Somente criança",
        tem_crianca == 0 & tem_idoso_dependente == 1 ~ "Somente idoso",
        tem_crianca == 1 & tem_idoso_dependente == 1 ~ "Criança e idoso",
        tem_crianca == 0 & tem_idoso_dependente == 0 ~ "Nenhum"))) |>
      select(-c(tem_crianca, tem_idoso_dependente)) |>
      ungroup() |>
      group_by(ano, cor_raca, flag_participa, tem_dependente) |>
      summarise(n = n) |>
      summarise(ano = ano,
                cor_raca = cor_raca,
                flag_participa = flag_participa,
                tem_dependente = tem_dependente,
                n = n,
                prop = round(n/sum(n)*100,2)) |>
      mutate(categoria = "Existência de dependente",
             variaveis = tem_dependente) |>
      filter(!is.na(variaveis)) |>
      select(ano, cor_raca, flag_participa, categoria, variaveis, prop)
  ) |>
  bind_rows(
    t_part_aposentadoria |>
      mutate(categoria = "Condição de aposentadoria",
             variaveis = flag_aposentadoria) |>
      filter(!is.na(variaveis)) |>
      select(ano, cor_raca, flag_participa, categoria, variaveis, prop)
  ) |>
  mutate(flag_participa = case_when(
    flag_participa == "forca_trabalho" ~ "Na FT",
    TRUE ~ "Fora da FT"
  )) |>
  select(ano, categoria, cor_raca, flag_participa, variaveis, prop) |>
  arrange(ano, categoria, cor_raca)

# Salvando resultados -----------------------------------------------------

save(t_agregada, file = "./Analises/outputs/pt1/descritivas_composicao_agregado.RData")

save(t_agregada_participa, file = "./Analises/outputs/pt1/descritivas_composicao_agregado_participa_mt.RData")

