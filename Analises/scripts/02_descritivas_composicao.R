options(scipen = 99999)

# Bibliotecas -------------------------------------------------------------

ifelse(!require(tidyverse),install.packages("tidyverse"),require(tidyverse))
ifelse(!require(srvyr),install.packages("srvyr"),require(srvyr))
ifelse(!require(survey),install.packages("survey"),require(survey))

# Import data -------------------------------------------------------------

pnad <- readRDS("./Analises/dados/pnadc_analise.rds")

# 1. Pop. idosa e nao idosa --------------------------------------------------

t_composicao_1 <- pnad |>
  filter(idade_simples >= 10) |>
  srvyr::as_survey_design(ids = id_pes,
                          weights = peso) |>
  group_by(ano, flag_idoso, cor_raca) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad |>
      filter(idade_simples >= 10) |>
      srvyr::as_survey_design(ids = id_pes,
                              weights = peso) |>
      group_by(ano, cor_raca) |>
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
      mutate(flag_idoso = "Total") |>
      select(ano, flag_idoso, everything())
  )

t_composicao_1_by_sex <- pnad |>
  filter(idade_simples >= 10) |>
  srvyr::as_survey_design(ids = id_pes,
                          weights = peso) |>
  group_by(ano, flag_idoso, cor_raca, sexo) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad |>
      filter(idade_simples >= 10) |>
      srvyr::as_survey_design(ids = id_pes,
                              weights = peso) |>
      group_by(ano, cor_raca, sexo) |>
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
      mutate(flag_idoso = "Total") |>
      select(ano, flag_idoso, everything())
  )

# 2. Por condição de participação no MT -------------------------------------

t_composicao_2 <- pnad |>
  filter(idade_simples >= 10) |>
  filter(cor_raca != "Outros") |>
  srvyr::as_survey_design(ids = id_pes,
                          weights = peso) |>
  group_by(ano, flag_idoso, cor_raca, flag_participa) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad |>
      filter(idade_simples >= 10) |>
      srvyr::as_survey_design(ids = id_pes,
                              weights = peso) |>
      group_by(ano, cor_raca, flag_participa) |>
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
      mutate(flag_idoso = "Total") |>
      select(ano, flag_idoso, everything())
  )

t_composicao_2_by_sex <- pnad |>
  filter(idade_simples >= 10) |>
  filter(cor_raca != "Outros") |>
  srvyr::as_survey_design(ids = id_pes,
                          weights = peso) |>
  group_by(ano, flag_idoso, cor_raca, sexo, flag_participa) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad |>
      filter(idade_simples >= 10) |>
      srvyr::as_survey_design(ids = id_pes,
                              weights = peso) |>
      group_by(ano, cor_raca, sexo, flag_participa) |>
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
      mutate(flag_idoso = "Total") |>
      select(ano, flag_idoso, everything())
  )


# Dados a serem trabalhados nas demais analises ---------------------------

pnad_idoso <- pnad |>
  filter(flag_idoso == "Idoso") |>
  filter(cor_raca != "Outros") |>
  filter(idade_simples >= 10) |>
  srvyr::as_survey_design(ids = id_pes,
                          weights = peso)

rm(pnad)

# 3. Por idade -------------------------------------

t_composicao_3 <- pnad_idoso |>
  group_by(ano, cor_raca, grupo_etario) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_idoso |>
      group_by(ano, grupo_etario) |>
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
      mutate(cor_raca = "Total") |>
      select(ano, cor_raca, everything())
  )

t_composicao_3_by_sex <- pnad_idoso |>
  group_by(ano, cor_raca, sexo, grupo_etario) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_idoso |>
      group_by(ano, sexo, grupo_etario) |>
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
      mutate(cor_raca = "Total") |>
      select(ano, cor_raca, everything())
  )

# 4. Por idade -------------------------------------

t_composicao_4 <- pnad_idoso |>
  group_by(ano, cor_raca, sexo) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_idoso |>
      group_by(ano, sexo) |>
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
      mutate(cor_raca = "Total") |>
      select(ano, cor_raca, everything())
  )

# 5. Por escolaridade -------------------------------------

t_composicao_5 <- pnad_idoso |>
  group_by(ano, cor_raca, educ_atingida) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_idoso |>
      group_by(ano, educ_atingida) |>
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
      mutate(cor_raca = "Total") |>
      select(ano, cor_raca, everything())
  )

t_composicao_5_by_sex <- pnad_idoso |>
  group_by(ano, cor_raca, sexo, educ_atingida) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_idoso |>
      group_by(ano, sexo, educ_atingida) |>
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
      mutate(cor_raca = "Total") |>
      select(ano, cor_raca, everything())
  )

# 6. Por quintil de renda -------------------------------------

t_composicao_6 <- pnad_idoso |>
  group_by(ano, cor_raca, quintil_inc) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_idoso |>
      group_by(ano, quintil_inc) |>
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
      mutate(cor_raca = "Total") |>
      select(ano, cor_raca, everything())
  )

t_composicao_6_by_sex <- pnad_idoso |>
  group_by(ano, cor_raca, sexo, quintil_inc) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_idoso |>
      group_by(ano, sexo, quintil_inc) |>
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
      mutate(cor_raca = "Total") |>
      select(ano, cor_raca, everything())
  )

# 7. Por quintil de renda -------------------------------------

t_composicao_7 <- pnad_idoso |>
  group_by(ano, cor_raca, tipo_dom) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_idoso |>
      group_by(ano, tipo_dom) |>
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
      mutate(cor_raca = "Total") |>
      select(ano, cor_raca, everything())
  )

t_composicao_7_by_sex <- pnad_idoso |>
  group_by(ano, cor_raca, sexo, tipo_dom) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_idoso |>
      group_by(ano, sexo, tipo_dom) |>
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
      mutate(cor_raca = "Total") |>
      select(ano, cor_raca, everything())
  )

# 8. Por existência de dependente -------------------------------------

t_composicao_8 <- pnad_idoso |>
  group_by(ano, cor_raca, tem_crianca, tem_idoso_dependente) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_idoso |>
      group_by(ano, tem_crianca, tem_idoso_dependente) |>
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
      mutate(cor_raca = "Total") |>
      select(ano, cor_raca, everything())
  )

t_composicao_8_by_sex <- pnad_idoso |>
  group_by(ano, cor_raca, sexo, tem_crianca, tem_idoso_dependente) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_idoso |>
      group_by(ano, sexo, tem_crianca, tem_idoso_dependente) |>
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
      mutate(cor_raca = "Total") |>
      select(ano, cor_raca, everything())
  )

# 9. Por posicao da renda -------------------------------------

t_composicao_9 <- pnad_idoso |>
  mutate(
    inc_prop_individuo = case_when(is.na(inc_prop_individuo) ~ 0,
                                   TRUE ~ inc_prop_individuo),
    inc_prop_aposentado = case_when(is.na(inc_prop_aposentado) ~ 0,
                                    TRUE ~ inc_prop_aposentado),
    inc_prop_aposentado_dom = case_when(is.na(inc_prop_aposentado_dom) ~ 0,
                                        TRUE ~ inc_prop_aposentado_dom)
  ) |>
  group_by(ano, cor_raca) |>
  summarise(
    prop_renda_ind_domicilio_mean = survey_mean(inc_prop_individuo, na.rm = TRUE),
    prop_renda_ind_domicilio_median = survey_median(inc_prop_individuo, na.rm = TRUE),
    prop_renda_apos_ind_mean = survey_mean(inc_prop_aposentado, na.rm = TRUE),
    prop_renda_apos_ind_median = survey_median(inc_prop_aposentado, na.rm = TRUE),
    prop_renda_apos_domicilio_mean = survey_mean(inc_prop_aposentado_dom, na.rm = TRUE),
    prop_renda_apos_domicilio_median = survey_median(inc_prop_aposentado_dom, na.rm = TRUE)
  ) |>
  bind_rows(
    pnad_idoso |>
      mutate(
        inc_prop_individuo = case_when(is.na(inc_prop_individuo) ~ 0,
                                       TRUE ~ inc_prop_individuo),
        inc_prop_aposentado = case_when(is.na(inc_prop_aposentado) ~ 0,
                                        TRUE ~ inc_prop_aposentado),
        inc_prop_aposentado_dom = case_when(is.na(inc_prop_aposentado_dom) ~ 0,
                                            TRUE ~ inc_prop_aposentado_dom)
      ) |>
      group_by(ano) |>
      summarise(
        prop_renda_ind_domicilio_mean = survey_mean(inc_prop_individuo, na.rm = TRUE),
        prop_renda_ind_domicilio_median = survey_median(inc_prop_individuo, na.rm = TRUE),
        prop_renda_apos_ind_mean = survey_mean(inc_prop_aposentado, na.rm = TRUE),
        prop_renda_apos_ind_median = survey_median(inc_prop_aposentado, na.rm = TRUE),
        prop_renda_apos_domicilio_mean = survey_mean(inc_prop_aposentado_dom, na.rm = TRUE),
        prop_renda_apos_domicilio_median = survey_median(inc_prop_aposentado_dom, na.rm = TRUE)
      ) |>
      mutate(cor_raca = "Total") |>
      select(ano, cor_raca, everything())
  )

t_composicao_9_by_sex <- pnad_idoso |>
  mutate(
    inc_prop_individuo = case_when(is.na(inc_prop_individuo) ~ 0,
                                   TRUE ~ inc_prop_individuo),
    inc_prop_aposentado = case_when(is.na(inc_prop_aposentado) ~ 0,
                                    TRUE ~ inc_prop_aposentado),
    inc_prop_aposentado_dom = case_when(is.na(inc_prop_aposentado_dom) ~ 0,
                                        TRUE ~ inc_prop_aposentado_dom)
  ) |>
  group_by(ano, cor_raca, sexo) |>
  summarise(
    prop_renda_ind_domicilio_mean = survey_mean(inc_prop_individuo, na.rm = TRUE),
    prop_renda_ind_domicilio_median = survey_median(inc_prop_individuo, na.rm = TRUE),
    prop_renda_apos_ind_mean = survey_mean(inc_prop_aposentado, na.rm = TRUE),
    prop_renda_apos_ind_median = survey_median(inc_prop_aposentado, na.rm = TRUE),
    prop_renda_apos_domicilio_mean = survey_mean(inc_prop_aposentado_dom, na.rm = TRUE),
    prop_renda_apos_domicilio_median = survey_median(inc_prop_aposentado_dom, na.rm = TRUE)
  ) |>
  bind_rows(
    pnad_idoso |>
      mutate(
        inc_prop_individuo = case_when(is.na(inc_prop_individuo) ~ 0,
                                       TRUE ~ inc_prop_individuo),
        inc_prop_aposentado = case_when(is.na(inc_prop_aposentado) ~ 0,
                                        TRUE ~ inc_prop_aposentado),
        inc_prop_aposentado_dom = case_when(is.na(inc_prop_aposentado_dom) ~ 0,
                                            TRUE ~ inc_prop_aposentado_dom)
      ) |>
      group_by(ano, sexo) |>
      summarise(
        prop_renda_ind_domicilio_mean = survey_mean(inc_prop_individuo, na.rm = TRUE),
        prop_renda_ind_domicilio_median = survey_median(inc_prop_individuo, na.rm = TRUE),
        prop_renda_apos_ind_mean = survey_mean(inc_prop_aposentado, na.rm = TRUE),
        prop_renda_apos_ind_median = survey_median(inc_prop_aposentado, na.rm = TRUE),
        prop_renda_apos_domicilio_mean = survey_mean(inc_prop_aposentado_dom, na.rm = TRUE),
        prop_renda_apos_domicilio_median = survey_median(inc_prop_aposentado_dom, na.rm = TRUE)
      ) |>
      mutate(cor_raca = "Total") |>
      select(ano, cor_raca, everything())
  )

# 10. Por status de aposentadoria -------------------------------------

t_composicao_10 <- pnad_idoso |>
  group_by(ano, cor_raca, flag_aposentadoria) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_idoso |>
      group_by(ano, flag_aposentadoria) |>
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
      mutate(cor_raca = "Total") |>
      select(ano, cor_raca, everything())
  )

t_composicao_10_by_sex <- pnad_idoso |>
  group_by(ano, cor_raca, sexo, flag_aposentadoria) |>
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_idoso |>
      group_by(ano, sexo, flag_aposentadoria) |>
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
      mutate(cor_raca = "Total") |>
      select(ano, cor_raca, everything())
  )

# Salvando resultados -----------------------------------------------------

descritivas_composicao <- list(t1 = t_composicao_1,
                               t1_by_sex = t_composicao_1_by_sex,
                               t2 = t_composicao_2,
                               t2_by_sex = t_composicao_2_by_sex,
                               t3 = t_composicao_3,
                               t3_by_sex = t_composicao_3_by_sex,
                               t4 = t_composicao_4,
                               t5 = t_composicao_5,
                               t5_by_sex = t_composicao_5_by_sex,
                               t6 = t_composicao_6,
                               t6_by_sex = t_composicao_6_by_sex,
                               t7 = t_composicao_7,
                               t7_by_sex = t_composicao_7_by_sex,
                               t8 = t_composicao_8,
                               t8_by_sex = t_composicao_8_by_sex,
                               t9 = t_composicao_9,
                               t9_by_sex = t_composicao_9_by_sex,
                               t10 = t_composicao_10,
                               t10_by_sex = t_composicao_10_by_sex)

save(descritivas_composicao, file = "./Analises/outputs/pt1/descritivas_composicao.RData")

# Salvando em arquivo excel
library(openxlsx)

wb <- createWorkbook()

modifyBaseFont(wb, fontSize = "11", fontName = "Calibri")

addWorksheet(wb, sheetName = "ReadMe")
addWorksheet(wb, sheetName = "1 - Por idoso, raca")
addWorksheet(wb, sheetName = "1.2 - Por idoso, raca, sexo")
addWorksheet(wb, sheetName = "2 - Por idoso, part. raca")
addWorksheet(wb, sheetName = "2.1 - Por part. raca e sexo")
addWorksheet(wb, sheetName = "3 - Por raca, idade")
addWorksheet(wb, sheetName = "3.1 - Por raca, idade e sexo")
addWorksheet(wb, sheetName = "4 - Por raca, sexo")
addWorksheet(wb, sheetName = "5 - Por escolaridade")
addWorksheet(wb, sheetName = "5.1 - Por escolaridade e sexo")
addWorksheet(wb, sheetName = "6 - Por quintil de renda")
addWorksheet(wb, sheetName = "6.1 - Por quintil e sexo")
addWorksheet(wb, sheetName = "7 - Por tipo de domicílio")
addWorksheet(wb, sheetName = "7.1 - Por tipo de dom. e sexo")
addWorksheet(wb, sheetName = "8 - Por exist. depend.")
addWorksheet(wb, sheetName = "8.1 - Por exist. depend. e sexo")
addWorksheet(wb, sheetName = "9 - Por prop. renda")
addWorksheet(wb, sheetName = "9.1 - Por prop. renda e sexo")
addWorksheet(wb, sheetName = "10 - Por status apos.")
addWorksheet(wb, sheetName = "10.1 - Por status apos. e sexo")

writeDataTable(wb, sheet = 2, x = t_composicao_1, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 3, x = t_composicao_1_by_sex, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 4, x = t_composicao_2, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 5, x = t_composicao_2_by_sex, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 6, x = t_composicao_3, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 7, x = t_composicao_3_by_sex, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 8, x = t_composicao_4, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 9, x = t_composicao_5, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 10, x = t_composicao_5_by_sex, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 11, x = t_composicao_6, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 12, x = t_composicao_6_by_sex, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 13, x = t_composicao_7, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 14, x = t_composicao_7_by_sex, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 15, x = t_composicao_8, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 16, x = t_composicao_8_by_sex, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 17, x = t_composicao_9, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 18, x = t_composicao_9_by_sex, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 19, x = t_composicao_10, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 20, x = t_composicao_10_by_sex, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")

saveWorkbook(wb,"./Analises/outputs/pt1/resultados_descritivas_composicao.xlsx", overwrite = TRUE)
