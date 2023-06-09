options(scipen = 99999)

# Bibliotecas -------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, srvyr, survey, hutils, writexl)

# Import data -------------------------------------------------------------

pnad <- readRDS("./Analises/dados/pnadc_analise.rds")

# Manipulação dos dados ---------------------------------------------------

pnad_espacial <- pnad |>
  filter(ano == 2019) |>
  filter(!is.na(inc_dom_pc)) |>
  mutate(cond_residencia = case_when(
    status_rm == "Rural" ~ "Rural", TRUE ~ "Urbano")) |>
  group_by(cond_residencia, regiao) |>
  mutate(quintil_inc_esp = weighted_ntile(inc_dom_pc, weights = peso, n = 5)) #Calcula quintis de distribuicao

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
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_esp_idoso_plan |>
      group_by(status_rm) |>
      summarise(cor_raca = "Total",
                n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2))) |>
  arrange(cor_raca, status_rm)

# Sexo

t_rm_sexo <- pnad_esp_idoso_plan |>
  group_by(cor_raca, status_rm, sexo) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_esp_idoso_plan |>
      group_by(status_rm, sexo) |>
      summarise(cor_raca = "Total",
                n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
      select(cor_raca, status_rm, sexo, everything())
    ) |>
  arrange(cor_raca)

# Idade

t_rm_idade <- pnad_esp_idoso_plan |>
  group_by(cor_raca, status_rm, grupo_etario) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_esp_idoso_plan |>
      group_by(status_rm, grupo_etario) |>
      summarise(cor_raca = "Total",
                n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
      select(cor_raca, status_rm, grupo_etario, everything())
  ) |>
  arrange(cor_raca)

# 2. Por regiao geografica  ------------------------------------

# Total

t_regiao_total <- pnad_esp_idoso_plan |>
  group_by(cor_raca, regiao) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_esp_idoso_plan |>
      group_by(regiao) |>
      summarise(cor_raca = "Total",
                n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2))) |>
  arrange(cor_raca, regiao)

# Sexo

t_regiao_sexo <- pnad_esp_idoso_plan |>
  group_by(cor_raca, regiao, sexo) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_esp_idoso_plan |>
      group_by(regiao, sexo) |>
      summarise(cor_raca = "Total",
                n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
      select(cor_raca, regiao, sexo, everything())
  ) |>
  arrange(cor_raca)

# Idade

t_regiao_idade <- pnad_esp_idoso_plan |>
  group_by(cor_raca, regiao, grupo_etario) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_esp_idoso_plan |>
      group_by(regiao, grupo_etario) |>
      summarise(cor_raca = "Total",
                n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
      select(cor_raca, regiao, grupo_etario, everything())
  ) |>
  arrange(cor_raca)

# 3. Por condição de residência (agregada) e UF ------------------------------------

# Total

t_uf_total <- pnad_esp_idoso_plan |>
  group_by(cor_raca, cond_residencia, UF) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  arrange(cor_raca, cond_residencia)

# Agrupando resultados ----------------------------------------------------

# 1. Por condicao de residência

composicao_rm <- list(total = t_rm_total,
                      sexo = t_rm_sexo,
                      idade = t_rm_idade)



# 2. Por região geográfica

composicao_regiao <- list(total = t_regiao_total,
                      sexo = t_regiao_sexo,
                      idade = t_regiao_idade)

# 3. Por condicao de residencia e UF

composicao_uf <- list(total = t_uf_total)

# Salvando resultados -----------------------------------------------------

save(composicao_rm, file = "./Analises/outputs/pt2/descritiva_composicao_rm_dem.RData")

save(composicao_regiao, file = "./Analises/outputs/pt2/descritiva_composicao_regiao_dem.RData")

save(composicao_uf, file = "./Analises/outputs/pt2/descritiva_composicao_uf.RData")
