options(scipen = 99999)

# Bibliotecas -------------------------------------------------------------

ifelse(!require(tidyverse),install.packages("tidyverse"),require(tidyverse))
ifelse(!require(srvyr),install.packages("srvyr"),require(srvyr))
ifelse(!require(survey),install.packages("survey"),require(survey))
ifelse(!require(lmtest),install.packages("lmtest"),require(lmtest))
ifelse(!require(car),install.packages("car"),require(car))
ifelse(!require(DescTools),install.packages("DescTools"),require(DescTools))
ifelse(!require(openxlsx),install.packages("openxlsx"),require(openxlsx))

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
    labels = c("P20","P40","P60","P80","P100"))) |>
  mutate(tem_dependente = as.factor(case_when(
    tem_crianca == 1 & tem_idoso_dependente == 0 ~ "Somente criança",
    tem_crianca == 0 & tem_idoso_dependente == 1 ~ "Somente idoso",
    tem_crianca == 1 & tem_idoso_dependente == 1 ~ "Criança e idoso",
    tem_crianca == 0 & tem_idoso_dependente == 0 ~ "Nenhum"))) |>
  mutate(
    inc_prop_individuo = case_when(is.na(inc_prop_individuo) ~ 0,
                                   TRUE ~ inc_prop_individuo),
    inc_prop_aposentado_dom = case_when(is.na(inc_prop_aposentado_dom) ~ 0,
                                        TRUE ~ inc_prop_aposentado_dom),
    inc_prop_individuo = scale(inc_prop_individuo, scale = FALSE), #Centralizar valores na media
    inc_prop_aposentado_dom = scale(inc_prop_aposentado_dom, scale = FALSE),  #Centralizar valores na media
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
                      inc_prop_individuo + inc_prop_aposentado_dom + status_rm + regiao + anofct,
                    design = pnad_idoso, family = binomial())

summary_mod1 <- summary(reg_mod1)

# Modelo 2 - Geral com interacao ------------------------------------------


reg_mod2 <- svyglm(flag_participa ~ sexo + grupo_etario + quintil_inc + educ_atingida +
                     inc_prop_individuo + inc_prop_aposentado_dom +
                     status_rm + regiao + anofct +
                     cor_raca*sexo + cor_raca*grupo_etario + cor_raca*quintil_inc +
                     cor_raca*educ_atingida + cor_raca*inc_prop_individuo +
                     cor_raca*inc_prop_aposentado_dom + cor_raca*status_rm +
                     cor_raca*regiao + cor_raca*anofct,
                    design = pnad_idoso, family = binomial())

summary_mod2 <- summary(reg_mod2)

# Modelo 3 - Urbano --------------------------------------------------------

pnad_idoso_urbano <- pnad_idoso |>
  filter(status_rm != "Rural")

reg_mod3 <- svyglm(flag_participa ~ cor_raca + sexo + grupo_etario + quintil_inc + educ_atingida +
                     inc_prop_individuo + inc_prop_aposentado_dom + status_rm + regiao + anofct,
                   design = pnad_idoso_urbano, family = binomial())

summary_mod3 <- summary(reg_mod3)

# Modelo 4 - Urbano com interacao ------------------------------------------

reg_mod4 <- svyglm(flag_participa ~ sexo + grupo_etario + quintil_inc + educ_atingida +
                     inc_prop_individuo + inc_prop_aposentado_dom + status_rm + regiao + anofct +
                     cor_raca*sexo + cor_raca*grupo_etario + cor_raca*quintil_inc +
                     cor_raca*educ_atingida + cor_raca*inc_prop_individuo +
                     cor_raca*inc_prop_aposentado_dom + cor_raca*status_rm +
                     cor_raca*regiao + cor_raca*anofct,
                   design = pnad_idoso_urbano, family = binomial())

summary_mod4 <- summary(reg_mod4)

# Modelo 5 - Rural --------------------------------------------------------

pnad_idoso_rural <- pnad_idoso |>
  filter(status_rm == "Rural")

reg_mod5 <- svyglm(flag_participa ~ cor_raca + sexo + grupo_etario + quintil_inc + educ_atingida +
                     inc_prop_individuo + inc_prop_aposentado_dom + regiao + anofct,
                   design = pnad_idoso_rural, family = binomial())

summary_mod5 <- summary(reg_mod5)

# Modelo 6 - Rural com interacao ------------------------------------------

reg_mod6 <- svyglm(flag_participa ~ sexo + grupo_etario + quintil_inc + educ_atingida +
                     inc_prop_individuo + inc_prop_aposentado_dom + regiao + anofct +
                     cor_raca*sexo + cor_raca*grupo_etario + cor_raca*quintil_inc +
                     cor_raca*educ_atingida + cor_raca*inc_prop_individuo +
                     cor_raca*inc_prop_aposentado_dom + cor_raca*regiao + cor_raca*anofct,
                   design = pnad_idoso_rural, family = binomial())

summary_mod6 <- summary(reg_mod6)

# Sintese -----------------------------------------------------------------

# Avaliacao das estimativas
# Pseudo R2 (McFadden)

1 - reg_mod2$deviance / reg_mod2$null.deviance # mod geral
1 - reg_mod4$deviance / reg_mod4$null.deviance # mod urbano
1 - reg_mod6$deviance / reg_mod6$null.deviance # mod rural

# AIC analysis

reg_mod2$aic #AIC MOD geral
reg_mod4$aic #AIC MOD urbano
reg_mod6$aic #AIC MOD rural

# VIF test

car::vif(reg_mod2) #VIF mod geral
car::vif(reg_mod4) #VIF mod urbano
car::vif(reg_mod6) #VIF mod rural

resultados <- summary_mod2$coefficients[,c(1:4)] |>
  as_tibble() |>
  mutate(coeficientes = Estimate,
         p_value = round(`Pr(>|t|)`,5),
         variaveis = row.names(summary_mod2$coefficients[,0]),
         se_95 = summary_mod2$coefficients[,2]*1.96,
         OR = exp(coeficientes),
         modelo = "Brasil") |>
  select(modelo, variaveis, coeficientes, se_95, OR, p_value) |>
  bind_rows(
    summary_mod4$coefficients[,c(1:4)] |>
      as_tibble() |>
      mutate(coeficientes = Estimate,
             p_value = round(`Pr(>|t|)`,5),
             variaveis = row.names(summary_mod4$coefficients[,0]),
             se_95 = summary_mod4$coefficients[,2]*1.96,
             OR = exp(coeficientes),
             modelo = "Urbano") |>
      select(modelo, variaveis, coeficientes, se_95, OR, p_value)
  ) |>
  bind_rows(
    summary_mod6$coefficients[,c(1:4)] |>
      as_tibble() |>
      mutate(coeficientes = Estimate,
             p_value = round(`Pr(>|t|)`,5),
             variaveis = row.names(summary_mod6$coefficients[,0]),
             se_95 = summary_mod6$coefficients[,2]*1.96,
             OR = exp(coeficientes),
             modelo = "Rural") |>
      select(modelo, variaveis, coeficientes, se_95, OR, p_value)
  )

openxlsx::write.xlsx(resultados, file = "./Analises/outputs/pt3/tabela_sintese_regressao.xlsx")


# Analise dos resultados --------------------------------------------------

predict_values <- predict(reg_mod2, pnad_idoso,type="response") |> as_tibble()

pnad_idoso |>
  mutate(prob_predita = predict_values$response) |>
  filter(ano == 2019) |>
  group_by(cor_raca, grupo_etario) |>
  summarise(prob = survey_mean(prob_predita, na.rm = TRUE)) |>
  ggplot() +
  aes(x = as.factor(grupo_etario), y = prob, linetype = cor_raca, group = cor_raca) +
  geom_line()

