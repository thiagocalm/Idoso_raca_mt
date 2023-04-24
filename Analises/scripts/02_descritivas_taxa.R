options(scipen = 99999)

# Bibliotecas -------------------------------------------------------------

ifelse(!require(tidyverse),install.packages("tidyverse"),require(tidyverse))
ifelse(!require(srvyr),install.packages("srvyr"),require(srvyr))
ifelse(!require(survey),install.packages("survey"),require(survey))

# Import data -------------------------------------------------------------

pnad <- readRDS("./Analises/dados/pnadc_analise.rds")

pnad_idoso <- pnad |> 
  filter(flag_idoso == "Idoso") |> 
  filter(cor_raca != "Outros") |> 
  filter(idade_simples >= 10) |> 
  srvyr::as_survey_design(ids = id_pes,
                          weights = peso)


# 1. Taxa idosa e nao idosa --------------------------------------------------


taxa_1 <- pnad |> 
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

taxa_1_by_sex <- pnad |> 
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

# 2. Por idade -------------------------------------

taxa_2 <- pnad_idoso |> 
  group_by(ano, cor_raca, grupo_etario, flag_participa) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_idoso |> 
      group_by(ano, grupo_etario, flag_participa) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(cor_raca = "Total") |> 
      select(ano, cor_raca, everything())
  )

taxa_2_by_sex <- pnad_idoso |> 
  group_by(ano, cor_raca, sexo, grupo_etario, flag_participa) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_idoso |> 
      group_by(ano, sexo, grupo_etario, flag_participa) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(cor_raca = "Total") |> 
      select(ano, cor_raca, everything())
  )

# 3. Por escolaridade -------------------------------------

taxa_3 <- pnad_idoso |> 
  group_by(ano, cor_raca, educ_atingida, flag_participa) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_idoso |> 
      group_by(ano, educ_atingida, flag_participa) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(cor_raca = "Total") |> 
      select(ano, cor_raca, everything())
  )

taxa_3_by_sex <- pnad_idoso |> 
  group_by(ano, cor_raca, sexo, educ_atingida, flag_participa) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_idoso |> 
      group_by(ano, sexo, educ_atingida, flag_participa) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(cor_raca = "Total") |> 
      select(ano, cor_raca, everything())
  )

# 4. Por quintil de renda -------------------------------------

taxa_4 <- pnad_idoso |> 
  group_by(ano, cor_raca, quintil_inc, flag_participa) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_idoso |> 
      group_by(ano, quintil_inc, flag_participa) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(cor_raca = "Total") |> 
      select(ano, cor_raca, everything())
  )

taxa_4_by_sex <- pnad_idoso |> 
  group_by(ano, cor_raca, sexo, quintil_inc, flag_participa) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_idoso |> 
      group_by(ano, sexo, quintil_inc, flag_participa) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(cor_raca = "Total") |> 
      select(ano, cor_raca, everything())
  )

# 5. Por tipo de domicilio -------------------------------------

taxa_5 <- pnad_idoso |> 
  group_by(ano, cor_raca, tipo_dom, flag_participa) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_idoso |> 
      group_by(ano, tipo_dom, flag_participa) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(cor_raca = "Total") |> 
      select(ano, cor_raca, everything())
  )

taxa_5_by_sex <- pnad_idoso |> 
  group_by(ano, cor_raca, sexo, tipo_dom, flag_participa) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_idoso |> 
      group_by(ano, sexo, tipo_dom, flag_participa) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(cor_raca = "Total") |> 
      select(ano, cor_raca, everything())
  )

# 6. Por existência de dependente -------------------------------------

taxa_6 <- pnad_idoso |> 
  group_by(ano, cor_raca, tem_crianca, tem_idoso_dependente, flag_participa) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_idoso |> 
      group_by(ano, tem_crianca, tem_idoso_dependente, flag_participa) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(cor_raca = "Total") |> 
      select(ano, cor_raca, everything())
  )

taxa_6_by_sex <- pnad_idoso |> 
  group_by(ano, cor_raca, sexo, tem_crianca, tem_idoso_dependente, flag_participa) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_idoso |> 
      group_by(ano, sexo, tem_crianca, tem_idoso_dependente, flag_participa) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(cor_raca = "Total") |> 
      select(ano, cor_raca, everything())
  )

# 7. Por posicao da renda -------------------------------------

# taxa_7 <- pnad_idoso |> 
#   mutate(
#     inc_prop_individuo = case_when(is.na(inc_prop_individuo) ~ 0, 
#                                    TRUE ~ inc_prop_individuo),
#     inc_prop_aposentado = case_when(is.na(inc_prop_aposentado) ~ 0, 
#                                     TRUE ~ inc_prop_aposentado),
#     inc_prop_aposentado_dom = case_when(is.na(inc_prop_aposentado_dom) ~ 0, 
#                                         TRUE ~ inc_prop_aposentado_dom)
#   ) |> 
#   group_by(ano, cor_raca) |> 
#   summarise(
#     prop_renda_ind_domicilio_mean = survey_mean(inc_prop_individuo, na.rm = TRUE),
#     prop_renda_ind_domicilio_median = survey_median(inc_prop_individuo, na.rm = TRUE),
#     prop_renda_apos_ind_mean = survey_mean(inc_prop_aposentado, na.rm = TRUE),
#     prop_renda_apos_ind_median = survey_median(inc_prop_aposentado, na.rm = TRUE),
#     prop_renda_apos_domicilio_mean = survey_mean(inc_prop_aposentado_dom, na.rm = TRUE),
#     prop_renda_apos_domicilio_median = survey_median(inc_prop_aposentado_dom, na.rm = TRUE)
#   ) |>
#   bind_rows(
#     pnad_idoso |> 
#       mutate(
#         inc_prop_individuo = case_when(is.na(inc_prop_individuo) ~ 0, 
#                                        TRUE ~ inc_prop_individuo),
#         inc_prop_aposentado = case_when(is.na(inc_prop_aposentado) ~ 0, 
#                                         TRUE ~ inc_prop_aposentado),
#         inc_prop_aposentado_dom = case_when(is.na(inc_prop_aposentado_dom) ~ 0, 
#                                             TRUE ~ inc_prop_aposentado_dom)
#       ) |> 
#       group_by(ano) |> 
#       summarise(
#         prop_renda_ind_domicilio_mean = survey_mean(inc_prop_individuo, na.rm = TRUE),
#         prop_renda_ind_domicilio_median = survey_median(inc_prop_individuo, na.rm = TRUE),
#         prop_renda_apos_ind_mean = survey_mean(inc_prop_aposentado, na.rm = TRUE),
#         prop_renda_apos_ind_median = survey_median(inc_prop_aposentado, na.rm = TRUE),
#         prop_renda_apos_domicilio_mean = survey_mean(inc_prop_aposentado_dom, na.rm = TRUE),
#         prop_renda_apos_domicilio_median = survey_median(inc_prop_aposentado_dom, na.rm = TRUE)
#       ) |> 
#       mutate(cor_raca = "Total") |> 
#       select(ano, cor_raca, everything())
#   )
# 
# t_composicao_9_by_sex <- pnad_idoso |> 
#   mutate(
#     inc_prop_individuo = case_when(is.na(inc_prop_individuo) ~ 0, 
#                                    TRUE ~ inc_prop_individuo),
#     inc_prop_aposentado = case_when(is.na(inc_prop_aposentado) ~ 0, 
#                                     TRUE ~ inc_prop_aposentado),
#     inc_prop_aposentado_dom = case_when(is.na(inc_prop_aposentado_dom) ~ 0, 
#                                         TRUE ~ inc_prop_aposentado_dom)
#   ) |> 
#   group_by(ano, cor_raca, sexo) |> 
#   summarise(
#     prop_renda_ind_domicilio_mean = survey_mean(inc_prop_individuo, na.rm = TRUE),
#     prop_renda_ind_domicilio_median = survey_median(inc_prop_individuo, na.rm = TRUE),
#     prop_renda_apos_ind_mean = survey_mean(inc_prop_aposentado, na.rm = TRUE),
#     prop_renda_apos_ind_median = survey_median(inc_prop_aposentado, na.rm = TRUE),
#     prop_renda_apos_domicilio_mean = survey_mean(inc_prop_aposentado_dom, na.rm = TRUE),
#     prop_renda_apos_domicilio_median = survey_median(inc_prop_aposentado_dom, na.rm = TRUE)
#   ) |>
#   bind_rows(
#     pnad_idoso |> 
#       mutate(
#         inc_prop_individuo = case_when(is.na(inc_prop_individuo) ~ 0, 
#                                        TRUE ~ inc_prop_individuo),
#         inc_prop_aposentado = case_when(is.na(inc_prop_aposentado) ~ 0, 
#                                         TRUE ~ inc_prop_aposentado),
#         inc_prop_aposentado_dom = case_when(is.na(inc_prop_aposentado_dom) ~ 0, 
#                                             TRUE ~ inc_prop_aposentado_dom)
#       ) |> 
#       group_by(ano, sexo) |> 
#       summarise(
#         prop_renda_ind_domicilio_mean = survey_mean(inc_prop_individuo, na.rm = TRUE),
#         prop_renda_ind_domicilio_median = survey_median(inc_prop_individuo, na.rm = TRUE),
#         prop_renda_apos_ind_mean = survey_mean(inc_prop_aposentado, na.rm = TRUE),
#         prop_renda_apos_ind_median = survey_median(inc_prop_aposentado, na.rm = TRUE),
#         prop_renda_apos_domicilio_mean = survey_mean(inc_prop_aposentado_dom, na.rm = TRUE),
#         prop_renda_apos_domicilio_median = survey_median(inc_prop_aposentado_dom, na.rm = TRUE)
#       ) |> 
#       mutate(cor_raca = "Total") |> 
#       select(ano, cor_raca, everything())
#   )

# 8. Por status de aposentadoria -------------------------------------

taxa_8 <- pnad_idoso |> 
  group_by(ano, cor_raca, flag_aposentadoria, flag_participa) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_idoso |> 
      group_by(ano, flag_aposentadoria, flag_participa) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(cor_raca = "Total") |> 
      select(ano, cor_raca, everything())
  )

taxa_8_by_sex <- pnad_idoso |> 
  group_by(ano, cor_raca, sexo, flag_aposentadoria, flag_participa) |> 
  summarise(n = survey_total(),
            prop = round(survey_mean(na.rm = TRUE)*100,2)) |>
  bind_rows(
    pnad_idoso |> 
      group_by(ano, sexo, flag_aposentadoria, flag_participa) |> 
      summarise(n = survey_total(),
                prop = round(survey_mean(na.rm = TRUE)*100,2)) |> 
      mutate(cor_raca = "Total") |> 
      select(ano, cor_raca, everything())
  )

# Salvando resultados -----------------------------------------------------

descritivas_taxa <- list(t1 = taxa_1,
                         t1_by_sex = taxa_1_by_sex,
                         t2 = taxa_2,
                         t2_by_sex = taxa_2_by_sex,
                         t3 = taxa_3,
                         t3_by_sex = taxa_3_by_sex,
                         t4 = taxa_4,
                         t4_by_sex = taxa_4_by_sex,
                         t5 = taxa_5,
                         t5_by_sex = taxa_5_by_sex,
                         t6 = taxa_6,
                         t6_by_sex = taxa_6_by_sex,
                         # t7 = taxa_7,
                         # t7_by_sex = taxa_7_by_sex,
                         t8 = taxa_8,
                         t8_by_sex = taxa_8_by_sex)

save(descritivas_taxa, file = "./Analises/outputs/pt1/descritivas_taxa.RData")

# Salvando em arquivo excel
library(openxlsx)

wb <- createWorkbook()

modifyBaseFont(wb, fontSize = "11", fontName = "Calibri")

addWorksheet(wb, sheetName = "ReadMe")
addWorksheet(wb, sheetName = "1 - Por idoso, raca")
addWorksheet(wb, sheetName = "1.2 - Por idoso, raca, sexo")
addWorksheet(wb, sheetName = "2 - Por raca, idade")
addWorksheet(wb, sheetName = "2.1 - Por raca, idade e sexo")
addWorksheet(wb, sheetName = "3 - Por escolaridade")
addWorksheet(wb, sheetName = "3.1 - Por escolaridade e sexo")
addWorksheet(wb, sheetName = "4 - Por quintil de renda")
addWorksheet(wb, sheetName = "4.1 - Por quintil e sexo")
addWorksheet(wb, sheetName = "5 - Por tipo de domicílio")
addWorksheet(wb, sheetName = "5.1 - Por tipo de dom. e sexo")
addWorksheet(wb, sheetName = "6 - Por exist. depend.")
addWorksheet(wb, sheetName = "6.1 - Por exist. depend. e sexo")
addWorksheet(wb, sheetName = "7 - Por prop. renda")
addWorksheet(wb, sheetName = "7.1 - Por prop. renda e sexo")
addWorksheet(wb, sheetName = "8 - Por status apos.")
addWorksheet(wb, sheetName = "8.1 - Por status apos. e sexo")

writeDataTable(wb, sheet = 2, x = taxa_1, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 3, x = taxa_1_by_sex, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 4, x = taxa_2, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 5, x = taxa_2_by_sex, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 6, x = taxa_3, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 7, x = taxa_3_by_sex, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 8, x = taxa_4, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 9, x = taxa_4_by_sex, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 10, x = taxa_5, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 11, x = taxa_5_by_sex, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 12, x = taxa_6, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 13, x = taxa_6_by_sex, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
# writeDataTable(wb, sheet = 14, x = taxa_7, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
# writeDataTable(wb, sheet = 15, x = taxa_7_by_sex, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 16, x = taxa_8, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")
writeDataTable(wb, sheet = 17, x = taxa_8_by_sex, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight1")

saveWorkbook(wb,"./Analises/outputs/pt1/resultados_descritivas_taxa.xlsx", overwrite = TRUE)
