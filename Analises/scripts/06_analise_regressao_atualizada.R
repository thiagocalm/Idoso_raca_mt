
## Commentarios
# Este script atualiza as analises realizadas no paper Almeida(2023), com um aprofundamento na modelagem
# Proximo passo: (i) testar significancia para interacao com sexo; (ii) analises de consistencia dos modelos

options(scipen = 99999)
rm(list = ls())

# Bibliotecas -------------------------------------------------------------

ifelse(!require(tidyverse),install.packages("tidyverse"),require(tidyverse))
ifelse(!require(srvyr),install.packages("srvyr"),require(srvyr))
ifelse(!require(survey),install.packages("survey"),require(survey))
ifelse(!require(lmtest),install.packages("lmtest"),require(lmtest))
ifelse(!require(car),install.packages("car"),require(car))
ifelse(!require(DescTools),install.packages("DescTools"),require(DescTools))
ifelse(!require(openxlsx),install.packages("openxlsx"),require(openxlsx))
ifelse(!require(gtsummary),install.packages("gtsummary"),require(gtsummary))
ifelse(!require(sjPlot),install.packages("sjPlot"),require(sjPlot))

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
                           inc_prop_individuo + inc_prop_aposentado_dom + flag_aposentadoria +
                           status_rm + regiao + anofct,
                         design = pnad_idoso, family = binomial())

anova(reg_mod1, reg_mod1_s_dom)

# Conclusao: #
# Vamos optar pelo modelo com domicilio, uma vez que segundo a analise de especificacao do modelo,
# sua retirada teria uma diferença estatisticamente significativa

rm(reg_mod1_s_dom)

# Modelo 2 - interacao ----------------------------------------------------

reg_mod1_interacao <- svyglm(
  flag_participa ~ cor_raca + sexo + grupo_etario +cor_raca * grupo_etario +
    quintil_inc + cor_raca * quintil_inc + educ_atingida + cor_raca * educ_atingida +
    inc_prop_individuo + cor_raca * inc_prop_individuo + inc_prop_aposentado_dom +
    cor_raca * inc_prop_aposentado_dom + flag_aposentadoria + cor_raca * flag_aposentadoria +
    status_rm + cor_raca * status_rm + regiao + cor_raca * regiao +
    tipo_dom + tipo_dom * cor_raca + anofct + cor_raca * anofct,
  design = pnad_idoso,
  family = binomial()
)

summary_mod1_int <- summary(reg_mod1_interacao)

# Comparacao --------------------------------------------------------------

anova(reg_mod1, reg_mod1_interacao)

# Conclusao: #
# Vamos optar pelo modelo com interacao, uma vez que a analise de especificacao do modelo se mostrou
# significativo.

# Validacao com modelos anteriores ----------------------------------------

# Restricao dos dados para 2012-2019 para ver se efeito continua o mesmo...

pnad_idoso_2 <- pnad_idoso %>%
  filter(anofct %in% 2012:2019)

# Modelo total

reg_modval <- svyglm(
  flag_participa ~ cor_raca + sexo + grupo_etario + quintil_inc + educ_atingida +
    inc_prop_individuo + inc_prop_aposentado_dom + flag_aposentadoria +
    status_rm + regiao + tipo_dom + anofct,
  design = pnad_idoso_2,
  family = binomial()
)

# Modelo total

reg_modval_int <- svyglm(
  flag_participa ~ cor_raca + sexo + grupo_etario +cor_raca * grupo_etario +
    quintil_inc + cor_raca * quintil_inc + educ_atingida + cor_raca * educ_atingida +
    inc_prop_individuo + cor_raca * inc_prop_individuo + inc_prop_aposentado_dom +
    cor_raca * inc_prop_aposentado_dom + flag_aposentadoria + cor_raca * flag_aposentadoria +
    status_rm + cor_raca * status_rm + regiao + cor_raca * regiao +
    tipo_dom + tipo_dom * cor_raca + anofct + cor_raca * anofct,
  design = pnad_idoso_2,
  family = binomial()
)

summary(reg_modval)
summary(reg_modval_int)

exp(coefficients(reg_modval))
exp(coefficients(reg_modval_int))
exp(coefficients(reg_mod1))
exp(coefficients(reg_mod1_interacao))

# Conclusoes #
# O mesmo padrao observado em 2012-2019 é tambem observado em 2012-2023. Ou seja, a ampliacao da serie nao
# afetou a mudanca de comportamento observado, e sim  a modelagem dos dados.
# Quando incluimos dados de 2019 a 2023 há duas distintas mudancas:
# 1 - para o modelo sem interacao: uma ligeira reducao aumento do diferencial (de 12,9% para 11,5% Odds ratio)
# 2 - para o modelo com interacao: um aumento significativo no diferencial (de 68,2% para 93,7% odds ratio)


rm(pnad_idoso_2,reg_modval_int, reg_modval)

# Teste - especificacao do modelo -----------------------------------------

## Interacao para sexo

reg_modval_int_csex <- svyglm(
  flag_participa ~ cor_raca + sexo + cor_raca * sexo + grupo_etario +cor_raca * grupo_etario +
    quintil_inc + cor_raca * quintil_inc + educ_atingida + cor_raca * educ_atingida +
    inc_prop_individuo + cor_raca * inc_prop_individuo + inc_prop_aposentado_dom +
    cor_raca * inc_prop_aposentado_dom + flag_aposentadoria + cor_raca * flag_aposentadoria +
    status_rm + cor_raca * status_rm + regiao + cor_raca * regiao +
    tipo_dom + tipo_dom * cor_raca + anofct + cor_raca * anofct,
  design = pnad_idoso,
  family = binomial()
)

reg_modval_int <- svyglm(
  flag_participa ~ cor_raca + sexo + grupo_etario +cor_raca * grupo_etario +
    quintil_inc + cor_raca * quintil_inc + educ_atingida + cor_raca * educ_atingida +
    inc_prop_individuo + cor_raca * inc_prop_individuo + inc_prop_aposentado_dom +
    cor_raca * inc_prop_aposentado_dom + flag_aposentadoria + cor_raca * flag_aposentadoria +
    status_rm + cor_raca * status_rm + regiao + cor_raca * regiao +
    tipo_dom + tipo_dom * cor_raca + anofct + cor_raca * anofct,
  design = pnad_idoso,
  family = binomial()
)

 anova(reg_modval_int_csex,reg_modval_int) # avaliacao - teste F para restricao do modelo "cor_raca * sexo"

 # Conclusao: #
 # Vamos optar pelo modelo sem interacao entre raca e sexo. A diferença principal observada se encontra no
 # diferencial por sexo, não tendo diferença interativa entre raça e sexo (i.e. entre homens e mulheres negros)

 rm(reg_modval_int_csex, reg_modval_int)

 # Analise de consistencia dos modelos -------------------------------------

#...


 # Plots modelo interativo -------------------------------------------------

 # By race
 plot_model(reg_mod1_interacao,
            type = "eff",
            terms = c("cor_raca"),
            title = "",
            axis.title = c(""),
            legend.title = "Raça ou cor")

 # By sex
 plot_model(reg_mod1_interacao,
            type = "eff",
            terms = c("sexo [all]", "cor_raca"),
            title = "",
            axis.title = c(""),
            legend.title = "Raça ou cor")

 # By age group
 plot_model(reg_mod1_interacao,
            type = "eff",
            terms = c("grupo_etario [all]", "cor_raca"),
            title = "",
            axis.title = c(""),
            legend.title = "Raça ou cor")

 # By income quarter
 plot_model(reg_mod1_interacao,
            type = "eff",
            terms = c("quintil_inc [all]", "cor_raca"),
            title = "",
            axis.title = c(""),
            legend.title = "Raça ou cor")

 # By education
 plot_model(reg_mod1_interacao,
            type = "eff",
            terms = c("educ_atingida [all]", "cor_raca"),
            title = "",
            axis.title = c(""),
            legend.title = "Raça ou cor")

 # By share of income in the household
 plot_model(reg_mod1_interacao,
            type = "eff",
            terms = c("inc_prop_individuo [all]", "cor_raca"),
            title = "",
            axis.title = c(""),
            legend.title = "Raça ou cor")

 # By share of retirement income in the household
 plot_model(reg_mod1_interacao,
            type = "eff",
            terms = c("inc_prop_aposentado_dom [all]", "cor_raca"),
            title = "",
            axis.title = c(""),
            legend.title = "Raça ou cor")

 # By retirement status
 plot_model(reg_mod1_interacao,
            type = "eff",
            terms = c("flag_aposentadoria [all]", "cor_raca"),
            title = "",
            axis.title = c(""),
            legend.title = "Raça ou cor")

 # By metropolitan region status
 plot_model(reg_mod1_interacao,
            type = "eff",
            terms = c("status_rm [all]", "cor_raca"),
            title = "",
            axis.title = c(""),
            legend.title = "Raça ou cor")

 # By region status
 plot_model(reg_mod1_interacao,
            type = "eff",
            terms = c("regiao [all]", "cor_raca"),
            title = "",
            axis.title = c(""),
            legend.title = "Raça ou cor")

 # By year status
 plot_model(reg_mod1_interacao,
            type = "eff",
            terms = c("anofct [all]", "cor_raca"),
            title = "",
            axis.title = c(""),
            legend.title = "Raça ou cor")

 # By household type
 plot_model(reg_mod1_interacao,
            type = "eff",
            terms = c("tipo_dom [all]", "cor_raca"),
            title = "",
            axis.title = c(""),
            legend.title = "Raça ou cor")

