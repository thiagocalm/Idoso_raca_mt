options(scipen = 99999)

# Bibliotecas -------------------------------------------------------------

ifelse(!require(tidyverse),install.packages("tidyverse"),require(tidyverse))
ifelse(!require(srvyr),install.packages("srvyr"),require(srvyr))
ifelse(!require(survey),install.packages("survey"),require(survey))
ifelse(!require(lmtest),install.packages("lmtest"),require(lmtest))
ifelse(!require(car),install.packages("car"),require(car))
ifelse(!require(DescTools),install.packages("DescTools"),require(DescTools))
ifelse(!require(openxlsx),install.packages("openxlsx"),require(openxlsx))
ifelse(!require(gtsummary),install.packages("gtsummary"),require(gtsummary))

# Importacao dos dados ----------------------------------------------------

pnad <- readRDS("./Analises/dados/pnadc_analise.rds")

pnad_idoso <- pnad |>
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
  select(-c(tem_crianca, tem_idoso_dependente)) |>
  srvyr::as_survey_design(ids = id_pes,
                          weights = peso)

rm(pnad)

# Modelo 1 - Geral --------------------------------------------------------

reg_mod1 <- svyglm(flag_participa ~ cor_raca + sexo + grupo_etario + quintil_inc + educ_atingida +
                      inc_prop_individuo + inc_prop_aposentado_dom + flag_aposentadoria + tipo_dom +
                     status_rm + regiao + anofct,
                    design = pnad_idoso, family = binomial())

summary_mod1 <- summary(reg_mod1)

reg_mod1_s_dom <- svyglm(flag_participa ~ cor_raca + sexo + grupo_etario + quintil_inc + educ_atingida +
                     inc_prop_individuo + inc_prop_aposentado_dom + tipo_dom +
                     status_rm + regiao + anofct,
                   design = pnad_idoso, family = binomial())

anova(reg_mod1, reg_mod1_s_dom)

# Modelo 2 - Geral com interacao ------------------------------------------


reg_mod2 <- svyglm(flag_participa ~ sexo + grupo_etario + quintil_inc + educ_atingida +
                     inc_prop_individuo + inc_prop_aposentado_dom + flag_aposentadoria + tipo_dom +
                     status_rm + regiao + anofct,
                   design = pnad_idoso |> filter(cor_raca == "Negro"), family = binomial())

summary_mod2 <- summary(reg_mod2)

reg_mod1_s_dom <- svyglm(flag_participa ~ sexo + grupo_etario + quintil_inc + educ_atingida +
                           inc_prop_individuo + inc_prop_aposentado_dom + flag_aposentadoria +
                           status_rm + regiao + anofct,
                         design = pnad_idoso |> filter(cor_raca == "Negro"), family = binomial())

anova(reg_mod2, reg_mod1_s_dom)

# Modelo 3 - Urbano --------------------------------------------------------

reg_mod3 <- svyglm(flag_participa ~ sexo + grupo_etario + quintil_inc + educ_atingida +
                     inc_prop_individuo + inc_prop_aposentado_dom + flag_aposentadoria + tipo_dom +
                     status_rm + regiao + anofct,
                   design = pnad_idoso |> filter(cor_raca == "Branco"), family = binomial())

summary_mod3 <- summary(reg_mod3)

reg_mod1_s_dom <- svyglm(flag_participa ~ sexo + grupo_etario + quintil_inc + educ_atingida +
                           inc_prop_individuo + inc_prop_aposentado_dom + tipo_dom +
                           status_rm + regiao + anofct,
                         design = pnad_idoso |> filter(cor_raca == "Branco"), family = binomial())

anova(reg_mod3, reg_mod1_s_dom)

# Sintese -----------------------------------------------------------------

# Avaliacao das estimativas
# Pseudo R2 (McFadden)

1 - reg_mod1$deviance / reg_mod1$null.deviance # mod geral
1 - reg_mod2$deviance / reg_mod2$null.deviance # mod urbano
1 - reg_mod3$deviance / reg_mod3$null.deviance # mod rural

# AIC analysis

reg_mod1$aic #AIC MOD geral
reg_mod2$aic #AIC MOD urbano
reg_mod3$aic #AIC MOD rural

# VIF test

car::vif(reg_mod1) #VIF mod geral
car::vif(reg_mod2) #VIF mod urbano
car::vif(reg_mod3) #VIF mod rural

resultados <- summary_mod1$coefficients[,c(1:4)] |>
  as_tibble() |>
  mutate(coeficientes = Estimate,
         p_value = round(`Pr(>|t|)`,5),
         variaveis = row.names(summary_mod1$coefficients[,0]),
         se_95 = summary_mod1$coefficients[,2]*1.96,
         OR = exp(coeficientes),
         modelo = "Brasil") |>
  select(modelo, variaveis, coeficientes, se_95, OR, p_value) |>
  bind_rows(
    summary_mod2$coefficients[,c(1:4)] |>
      as_tibble() |>
      mutate(coeficientes = Estimate,
             p_value = round(`Pr(>|t|)`,5),
             variaveis = row.names(summary_mod2$coefficients[,0]),
             se_95 = summary_mod2$coefficients[,2]*1.96,
             OR = exp(coeficientes),
             modelo = "Negro(a)") |>
      select(modelo, variaveis, coeficientes, se_95, OR, p_value)
  ) |>
  bind_rows(
    summary_mod3$coefficients[,c(1:4)] |>
      as_tibble() |>
      mutate(coeficientes = Estimate,
             p_value = round(`Pr(>|t|)`,5),
             variaveis = row.names(summary_mod3$coefficients[,0]),
             se_95 = summary_mod3$coefficients[,2]*1.96,
             OR = exp(coeficientes),
             modelo = "Branco(a)") |>
      select(modelo, variaveis, coeficientes, se_95, OR, p_value)
  )

openxlsx::write.xlsx(resultados, file = "./Analises/outputs/pt3/tabela_sintese_regressao_cor.xlsx")


# Analise dos resultados --------------------------------------------------

sjPlot::plot_model(reg_mod2, type = "int", terms = c( "anofct", "cor_raca"), show.values = TRUE,
                   value.offset = .4)

reg_2_summary <- summary(reg_mod2)

exp(reg_2_summary$coefficients[,1])

predict_values <- predict(reg_mod2, pnad_idoso,type="response") |> as_tibble()

pnad_idoso |>
  mutate(prob_predita = predict_values$response) |>
  group_by(cor_raca, educ_atingida) |>
  summarise(prob = survey_mean(prob_predita, na.rm = TRUE)) |>
  ggplot() +
  aes(x = as.factor(educ_atingida), y = prob, linetype = cor_raca, group = cor_raca) +
  geom_line()


resumo_mod <- stargazer::stargazer(
  reg_mod1, reg_mod2, reg_mod3,
  type = "text",
  title = "Tabela síntese das estimativas do modelo de regressão",
  column.labels = c("Brasil","Pop. Negra","Pop. Branca"),
  dep.var.caption = c("Variável dependente:"),
  dep.var.labels = c("Se participa do mercado de trabalho"),
  decimal.mark = ",",
  digit.separator = ".",
  intercept.top = TRUE,
  intercept.bottom = FALSE
)


# Tabela resumo das variaveis utilizadas ----------------------------------

# Geral
tabela_geral <- pnad_idoso |>
  tbl_svysummary(
    #by = presenca_filho,
    statistic = list(all_continuous() ~ "{mean} ({sd})",        # stats and format for continuous columns
                     all_categorical() ~ "{n} ({p}%)"),   # stats and format for categorical columns
    digits = all_continuous() ~ 4,                              # rounding for continuous columns
    type   = list(all_categorical() ~ "categorical",
                  c(inc_prop_individuo, inc_prop_aposentado_dom) ~ "continuous"), # force all categorical levels to display
    label  = list(                                              # display labels for column names
      flag_participa ~ "Participa do merc. trabalho",
      cor_raca ~ "Cor ou raça",
      sexo ~ "Sexo",
      grupo_etario ~ "Idade (grupo etário quinquenal)",
      quintil_inc ~ "Estrato de renda (quintil)",
      educ_atingida ~ "Escolaridade atingida",
      inc_prop_individuo ~ "Parcela de contribuição na renda total do domicílio",
      inc_prop_aposentado_dom ~ "Parcela de contribuição da aposentadoria na renda total do domicílio",
      flag_aposentadoria ~ "Recebe aposentadoria",
      tipo_dom ~ "Tipo de domicílio de residência",
      status_rm ~ "Status do local de residência",
      regiao ~ "Região geográfica de residência"),
    include = c(flag_participa,cor_raca,sexo,grupo_etario,quintil_inc,educ_atingida,
                inc_prop_individuo,inc_prop_aposentado_dom,flag_aposentadoria,tipo_dom,
                status_rm,regiao),
    missing_text = "Missing")

tabela_geral |>
  flextable::as_flextable() |>
  flextable::save_as_docx(path = "tabela_reg_geral.docx")

# Por cor ou raca
tabela_raca <- pnad_idoso |>
  tbl_svysummary(
    by = cor_raca,
    statistic = list(all_continuous() ~ "{mean} ({sd})",        # stats and format for continuous columns
                     all_categorical() ~ "{n} ({p}%)"),   # stats and format for categorical columns
    digits = all_continuous() ~ 4,                              # rounding for continuous columns
    type   = list(all_categorical() ~ "categorical",
                  c(inc_prop_individuo, inc_prop_aposentado_dom) ~ "continuous"), # force all categorical levels to display
    label  = list(                                              # display labels for column names
      flag_participa ~ "Participa do merc. trabalho",
      sexo ~ "Sexo",
      grupo_etario ~ "Idade (grupo etário quinquenal)",
      quintil_inc ~ "Estrato de renda (quintil)",
      educ_atingida ~ "Escolaridade atingida",
      inc_prop_individuo ~ "Parcela de contribuição na renda total do domicílio",
      inc_prop_aposentado_dom ~ "Parcela de contribuição da aposentadoria na renda total do domicílio",
      flag_aposentadoria ~ "Recebe aposentadoria",
      tipo_dom ~ "Tipo de domicílio de residência",
      status_rm ~ "Status do local de residência",
      regiao ~ "Região geográfica de residência"),
    include = c(flag_participa,cor_raca,sexo,grupo_etario,quintil_inc,educ_atingida,
                inc_prop_individuo,inc_prop_aposentado_dom,flag_aposentadoria,tipo_dom,
                status_rm,regiao),
    missing_text = "Missing")
