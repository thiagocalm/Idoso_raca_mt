rm(list = ls())
options(scipen = 99999)

# Bibliotecas -------------------------------------------------------------
ifelse(!require(pacman),install.packages("pacman"),require(pacman))
p_load(tidyverse, srvyr, survey, lmtest,car, openxlsx, gtsummary, margins)


# Importacao de dados -----------------------------------------------------

pnad <- readRDS("./Analises/dados/pnadc_analise.rds")

# Tratamento dos dados para modelo ----------------------------------------

pnad <- pnad |>
  filter(flag_idoso == "Idoso") |>
  filter(cor_raca != "Outros") |>
  filter(educ_atingida != "NA/NR") |>
  filter(idade_simples >= 10 & idade_simples <= 90) |>
  filter(!is.na(quintil_inc)) |>
  mutate(quintil_inc = factor(
    quintil_inc,
    levels = c(1,2,3,4,5),
    labels = c("P20","P40","P60","P80","P100")),
    tem_dependente = as.factor(case_when(
      tem_crianca == 1 & tem_idoso_dependente == 0 ~ "Somente criança",
      tem_crianca == 0 & tem_idoso_dependente == 1 ~ "Somente idoso",
      tem_crianca == 1 & tem_idoso_dependente == 1 ~ "Criança e idoso",
      tem_crianca == 0 & tem_idoso_dependente == 0 ~ "Nenhum")),
    tipo_dom = as.factor(case_when(
      tipo_dom == 1 ~ "Unipessoal",
      tipo_dom == 2 ~ "Nuclear",
      tipo_dom == 3 ~ "Estendida",
      tipo_dom == 4 ~ "Composta")),
    flag_aposentadoria = factor(flag_aposentadoria, levels = c("Não aposentado", "Aposentado"),
                                labels = c("Não aposentado", "Aposentado"))) |>
  mutate(
    inc_prop_individuo = case_when(is.na(inc_prop_individuo) ~ 0,
                                   TRUE ~ inc_prop_individuo),
    inc_prop_aposentado_dom = case_when(is.na(inc_prop_aposentado_dom) ~ 0,
                                        TRUE ~ inc_prop_aposentado_dom),
    inc_prop_individuo = factor(case_when(
      inc_prop_individuo == 0 ~ 1,
      inc_prop_individuo < .5 ~ 2,
      inc_prop_individuo > .5 ~ 3), levels = c(1,2,3), labels = c("Zero","Alguma","Maior")),
    inc_prop_aposentado_dom = factor(case_when(
      inc_prop_aposentado_dom == 0 ~ 1,
      inc_prop_aposentado_dom < .5 ~ 2,
      inc_prop_aposentado_dom > .5 ~ 3),levels = c(1,2,3), labels = c("Zero","Alguma","Maior")),
    anofct = as.factor(ano),
    grupo_etario = as.factor(grupo_etario),
    cor_raca = as.factor(cor_raca),
    flag_participa = as.integer(case_when(
      flag_participa == "forca_trabalho" ~ 1,
      TRUE ~ 0)),
    educ_atingida = factor(educ_atingida,
                           level = c("Sem instrução", "Ensino Fundamental", "Ensino Médio",
                                     "Ensino Superior"),
                           labels = c("Sem instrução", "Ensino Fundamental", "Ensino Médio",
                                      "Ensino Superior"))) |>
  select(-c(tem_crianca, tem_idoso_dependente))

pnad_idoso <- pnad %>%
  srvyr::as_survey_design(ids = id_pes, weights = peso)

invisible(gc())
# Modelagem ---------------------------------------------------------------

# Modelo 1

reg_mod1 <- svyglm(flag_participa ~ cor_raca + sexo + grupo_etario + quintil_inc + educ_atingida +
                     inc_prop_individuo + inc_prop_aposentado_dom + flag_aposentadoria + tipo_dom +
                     status_rm + regiao + anofct,
                   design = pnad_idoso, family = binomial())


# Modelo 2 - pop negra

reg_mod2 <- svyglm(flag_participa ~ sexo + grupo_etario + quintil_inc + educ_atingida +
                     inc_prop_individuo + inc_prop_aposentado_dom + flag_aposentadoria + tipo_dom +
                     status_rm + regiao + anofct,
                   design = pnad_idoso |> filter(cor_raca == "Negro"), family = binomial())

# Modelo 3 - pop branca


reg_mod3 <- svyglm(flag_participa ~ sexo + grupo_etario + quintil_inc + educ_atingida +
                     inc_prop_individuo + inc_prop_aposentado_dom + flag_aposentadoria + tipo_dom +
                     status_rm + regiao + anofct,
                   design = pnad_idoso |> filter(cor_raca == "Branco"), family = binomial())

rm(pnad_idoso)

# AME ---------------------------------------------------------------------

# Modelo 1

ame_mod1 <- margins(
  reg_mod1#,
  # design = pnad_idoso
)

summary(ame_mod1)

# Modelo 2

ame_mod2 <- margins(
  reg_mod2#,
  # design = pnad_idoso |> filter(cor_raca == "Negro")
)

summary(ame_mod2)

# Modelo 3

ame_mod3 <- margins(
  reg_mod3#,
  # design = pnad_idoso |> filter(cor_raca == "Branco")
)

summary(ame_mod3)


# Grafico dos efeitos -----------------------------------------------------

tabela <- summary(ame_mod2) %>%
  as_tibble() %>%
  select(factor, AME, lower, upper) %>%
  mutate(modelo = "Negros") %>%
  bind_rows(
    summary(ame_mod3) %>%
      as_tibble() %>%
      select(factor, AME, lower, upper) %>%
      mutate(modelo = "Brancos")
  )

tabela %>%
  ggplot() +
  aes(color = modelo) +
  geom_segment(aes(x = lower, xend = upper, y = rev(factor), yend = rev(factor)), linewidth = 1.02) +
  geom_point(aes(x = AME, y = rev(factor)), size = 2) +
  geom_vline(xintercept = 0, color = "grey50", linewidth = 1.05, linetype = "dashed") +
  theme_light()
