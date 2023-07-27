options(scipen = 99999)

# Bibliotecas -------------------------------------------------------------

ifelse(!require(tidyverse),install.packages("tidyverse"),require(tidyverse))
ifelse(!require(srvyr),install.packages("srvyr"),require(srvyr))
ifelse(!require(survey),install.packages("survey"),require(survey))

# Import data -------------------------------------------------------------

pnad <- readRDS("./Analises/dados/pnadc_analise.rds")


# Manipulação dos dados ---------------------------------------------------

pnad_espacial <- pnad |>
  filter(ano == 2019) |>
  filter(!is.na(inc_dom_pc)) |>
  mutate(cond_residencia = case_when(
    status_rm == "Rural" ~ "Rural", TRUE ~ "Urbano")) |>
  group_by(cond_residencia, regiao) |>
  mutate(quintil_inc_esp = ntile(inc_dom_pc, 5)) #Calcula quintis de distribuicao


# Aplicacao do plano amostral ---------------------------------------------

pnad_esp_plan <- pnad_espacial |>
  filter(idade_simples >= 10) |>
  filter(cor_raca != "Outros") |>
  srvyr::as_survey_design(ids = id_pes,
                          weights = peso)

pnad_esp_idoso_plan <- pnad_espacial |>
  filter(flag_idoso == "Idoso") |>
  filter(cor_raca != "Outros") |>
  filter(idade_simples >= 10) |>
  srvyr::as_survey_design(ids = id_pes,
                          weights = peso)

rm(pnad)

# 1. Por condição de residência (desagregada) ------------------------------------

# Total

t_rm_total <- pnad_esp_idoso_plan |>
  group_by(cor_raca, status_rm) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2))

# Participação no mercado de trabalho

t_rm_participa <- pnad_esp_idoso_plan |>
  group_by(status_rm, cor_raca, flag_participa) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2))

# Sexo

t_rm_sexo <- pnad_esp_idoso_plan |>
  group_by(status_rm, cor_raca, sexo) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2))

# Escolaridade

t_rm_escolaridade <- pnad_esp_idoso_plan |>
  group_by(status_rm, cor_raca, educ_atingida) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2))

# Quintil de renda

t_rm_renda <- pnad_esp_idoso_plan |>
  group_by(status_rm, cor_raca, quintil_inc_esp) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2))

# Tipo de domicilio

t_rm_tipodom <- pnad_esp_idoso_plan |>
  group_by(status_rm, cor_raca, tipo_dom) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2))

# Existencia de dependente

t_rm_tem_dep <- pnad_esp_idoso_plan |>
  group_by(status_rm, cor_raca, tem_crianca, tem_idoso_dependente) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2))

# Status de Aposentadoria

t_rm_aposentadoria <- pnad_esp_idoso_plan |>
  group_by(status_rm, cor_raca, flag_aposentadoria) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2))

# 2. Por regiao geografica  ------------------------------------

# Total

t_regiao_total <- pnad_esp_idoso_plan |>
  group_by(regiao, cor_raca, flag_participa) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2))

# Sexo

t_regiao_sexo <- pnad_esp_idoso_plan |>
  group_by(regiao, cor_raca, flag_participa, sexo) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2))

# Escolaridade

t_regiao_escolaridade <- pnad_esp_idoso_plan |>
  group_by(regiao, cor_raca, flag_participa, educ_atingida) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2))

# Quintil de renda

t_regiao_renda <- pnad_esp_idoso_plan |>
  group_by(regiao, cor_raca, flag_participa, quintil_inc_esp) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2))

# Tipo de domicilio

t_regiao_tipodom <- pnad_esp_idoso_plan |>
  group_by(regiao, cor_raca, flag_participa, tipo_dom) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2))

# Existencia de dependente

t_regiao_tem_dep <- pnad_esp_idoso_plan |>
  group_by(regiao, cor_raca, flag_participa, tem_crianca, tem_idoso_dependente) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2))

# Status de Aposentadoria

t_regiao_aposentadoria <- pnad_esp_idoso_plan |>
  group_by(regiao, cor_raca,  flag_participa, flag_aposentadoria) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2))

# Agrupando resultados ----------------------------------------------------

# 1. Por condicao de residência

t_rm_agregada <- t_rm_total |>
  mutate(categoria = "População total",
         variaveis = "Total") |>
  select(status_rm, cor_raca, categoria, variaveis, prop) |>
  bind_rows(
    t_rm_participa |>
      mutate(flag_participa = as.factor(case_when(
        flag_participa == "forca_trabalho" ~ "Participa",
        TRUE ~ "Não participa"
      ))) |>
      mutate(categoria = "Participação laboral",
             variaveis = flag_participa) |>
      select(status_rm, cor_raca, categoria, variaveis, prop)
  ) |>
  bind_rows(
    t_rm_sexo |>
      mutate(categoria = "Sexo",
             variaveis = sexo) |>
      select(status_rm, cor_raca, categoria, variaveis, prop)
  ) |>
  bind_rows(
    t_rm_escolaridade |>
      mutate(categoria = "Escolaridade",
             variaveis = educ_atingida) |>
      select(status_rm, cor_raca, categoria, variaveis, prop)
  ) |>
  bind_rows(
    t_rm_renda |>
      mutate(quintil_inc_esp = factor(
        quintil_inc_esp,
        levels = c(1,2,3,4,5),
        labels = c("P20","P40","P60","P80","P100"))) |>
      mutate(categoria = "Dist. Renda",
             variaveis = quintil_inc_esp) |>
      filter(!is.na(variaveis)) |>
      select(status_rm, cor_raca, categoria, variaveis, prop)
  ) |>
  bind_rows(
    t_rm_tipodom |>
      mutate(tipo_dom = as.factor(case_when(
        tipo_dom == 1 ~ "Unipessoal",
        tipo_dom == 2 ~ "Nuclear",
        tipo_dom == 3 ~ "Estendida",
        tipo_dom == 4 ~ "Composta"))) |>
      mutate(categoria = "Tipo de domicílio",
             variaveis = tipo_dom) |>
      filter(!is.na(variaveis)) |>
      select(status_rm, cor_raca, categoria, variaveis, prop)
  ) |>
  bind_rows(
    t_rm_tem_dep |>
      mutate(tem_dependente = as.factor(case_when(
        tem_crianca == 1 & tem_idoso_dependente == 0 ~ "Somente criança",
        tem_crianca == 0 & tem_idoso_dependente == 1 ~ "Somente idoso",
        tem_crianca == 1 & tem_idoso_dependente == 1 ~ "Criança e idoso",
        tem_crianca == 0 & tem_idoso_dependente == 0 ~ "Nenhum"))) |>
      select(-c(tem_crianca, tem_idoso_dependente)) |>
      ungroup() |>
      group_by(status_rm, cor_raca, tem_dependente) |>
      summarise(n = n) |>
      summarise(status_rm = status_rm,
                cor_raca = cor_raca,
                tem_dependente = tem_dependente,
                n = n,
                prop = round(n/sum(n)*100,2)) |>
      mutate(categoria = "Existência de dependente",
             variaveis = tem_dependente) |>
      filter(!is.na(variaveis)) |>
      select(status_rm, cor_raca, categoria, variaveis, prop)
  ) |>
  bind_rows(
    t_rm_aposentadoria |>
      mutate(categoria = "Condição de aposentadoria",
             variaveis = flag_aposentadoria) |>
      filter(!is.na(variaveis)) |>
      select(status_rm, cor_raca, categoria, variaveis, prop)
  ) |>
  select(status_rm, categoria, cor_raca, variaveis, prop) |>
  arrange(categoria, cor_raca)

# 2. Por região geográfica

t_regiao_agregada <- t_regiao_total |>
  mutate(categoria = "Pop. total",
         variaveis = "Total") |>
  select(regiao, cor_raca, flag_participa, categoria, variaveis, prop) |>
  bind_rows(
    t_regiao_sexo |>
      mutate(categoria = "Sexo",
             variaveis = sexo) |>
      select(regiao, cor_raca, flag_participa, categoria, variaveis, prop)
  ) |>
  bind_rows(
    t_regiao_escolaridade |>
      mutate(categoria = "Escolaridade",
             variaveis = educ_atingida) |>
      select(regiao, cor_raca, flag_participa, categoria, variaveis, prop)
  ) |>
  bind_rows(
    t_regiao_renda |>
      mutate(quintil_inc_esp = factor(
        quintil_inc_esp,
        levels = c(1,2,3,4,5),
        labels = c("P20","P40","P60","P80","P100"))) |>
      mutate(categoria = "Dist. Renda",
             variaveis = quintil_inc_esp) |>
      filter(!is.na(variaveis)) |>
      select(regiao, cor_raca, flag_participa, categoria, variaveis, prop)
  ) |>
  bind_rows(
    t_regiao_tipodom |>
      mutate(tipo_dom = as.factor(case_when(
        tipo_dom == 1 ~ "Unipessoal",
        tipo_dom == 2 ~ "Nuclear",
        tipo_dom == 3 ~ "Estendida",
        tipo_dom == 4 ~ "Composta"))) |>
      mutate(categoria = "Tipo de domicílio",
             variaveis = tipo_dom) |>
      filter(!is.na(variaveis)) |>
      select(regiao, cor_raca, flag_participa, categoria, variaveis, prop)
  ) |>
  bind_rows(
    t_regiao_tem_dep |>
      mutate(tem_dependente = as.factor(case_when(
        tem_crianca == 1 & tem_idoso_dependente == 0 ~ "Somente criança",
        tem_crianca == 0 & tem_idoso_dependente == 1 ~ "Somente idoso",
        tem_crianca == 1 & tem_idoso_dependente == 1 ~ "Criança e idoso",
        tem_crianca == 0 & tem_idoso_dependente == 0 ~ "Nenhum"))) |>
      select(-c(tem_crianca, tem_idoso_dependente)) |>
      ungroup() |>
      group_by(regiao, cor_raca, flag_participa, tem_dependente) |>
      summarise(n = n) |>
      summarise(regiao = regiao,
                cor_raca = cor_raca,
                flag_participa = flag_participa,
                tem_dependente = tem_dependente,
                n = n,
                prop = round(n/sum(n)*100,2)) |>
      mutate(categoria = "Existência de dependente",
             variaveis = tem_dependente) |>
      filter(!is.na(variaveis)) |>
      select(regiao, cor_raca, flag_participa, categoria, variaveis, prop)
  ) |>
  bind_rows(
    t_regiao_aposentadoria |>
      mutate(categoria = "Condição de aposentadoria",
             variaveis = flag_aposentadoria) |>
      filter(!is.na(variaveis)) |>
      select(regiao, cor_raca, flag_participa, categoria, variaveis, prop)
  ) |>
  select(regiao, categoria, cor_raca, flag_participa, variaveis, prop) |>
  arrange(categoria, cor_raca)


# Salvando resultados -----------------------------------------------------

save(t_rm_agregada, file = "./Analises/outputs/pt2/descritiva_composicao_rm.RData")

save(t_regiao_agregada, file = "./Analises/outputs/pt2/descritiva_composicao_regiao.RData")
