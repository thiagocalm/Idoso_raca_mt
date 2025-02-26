options(scipen = 99999)

# Bibliotecas -------------------------------------------------------------

ifelse(!require(tidyverse),install.packages("tidyverse"),require(tidyverse))
ifelse(!require(srvyr),install.packages("srvyr"),require(srvyr))
ifelse(!require(survey),install.packages("survey"),require(survey))


# Importacao dos dados ----------------------------------------------------

load("./Analises/outputs/pt1/descritivas_composicao.RData")

pnad <- readRDS("./Analises/dados/pnadc_analise.rds")

pnad_idoso <- pnad |>
  filter(flag_idoso == "Idoso") |>
  filter(cor_raca != "Outros") |>
  filter(idade_simples >= 10) |>
  srvyr::as_survey_design(ids = id_pes,
                          weights = peso)

rm(pnad)

# 1 - Composicao por raca e status de idoso --------------------------------------

# Total

descritivas_composicao$t1 |>
  filter(cor_raca != "Outros") |>
  ggplot() +
  aes(x = ano, y = prop, linetype = cor_raca, color = flag_idoso) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.05) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), width=.1,
                linetype = "solid") +
  scale_color_viridis_d(option = "D",begin = .1, end = .6) +
  coord_cartesian(ylim = c(30,max(descritivas_composicao$t1$prop+10))) +
  scale_x_continuous(breaks = seq(2012,2023,1)) +
  scale_y_continuous(breaks = seq(0,max(descritivas_composicao$t1$prop+5),10)) +
  labs(
    title = "Distribuição relativa da população por cor ou raça e se é idoso ou não - Brasil 2012-2019",
    x = "Ano Calendário",
    y = "População (%)",
    caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Anual, 2012-2019."
  ) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#525252"),
    axis.title = element_text(size = 11, hjust = .5, color = "#525252"),
    axis.text = element_text(size = 9, color = "#969696"),
    plot.caption = element_text(face = "bold", size = 8, hjust = 1, color = "#525252"),
    legend.title = element_blank(),
    legend.text = element_text(size = 9, color = "#525252"),
    legend.position = "top"
  )

# Por sexo

descritivas_composicao$t1_by_sex |>
  filter(cor_raca != "Outros") |>
  ggplot() +
  aes(x = ano, y = prop, linetype = cor_raca, color = flag_idoso) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.05) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), width=.1,
                linetype = "solid") +
  scale_color_viridis_d(option = "D",begin = .1, end = .6) +
  coord_cartesian(ylim = c(30,max(descritivas_composicao$t1$prop+10))) +
  scale_x_continuous(breaks = seq(2012,2023,1)) +
  scale_y_continuous(breaks = seq(0,max(descritivas_composicao$t1$prop+5),10)) +
  lemon::facet_rep_grid(. ~ sexo, repeat.tick.labels = TRUE) +
  labs(
    title = "Distribuição relativa da população por cor ou raça e se é idoso ou não - Brasil 2012-2019",
    x = "Ano Calendário",
    y = "População (%)",
    caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Anual, 2012-2019."
  ) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#525252"),
    axis.title = element_text(size = 11, hjust = .5, color = "#525252"),
    strip.text = element_text(face = "bold", size = 11, hjust = .5, color = "#525252"),
    strip.background = element_blank(),
    axis.text = element_text(size = 9, color = "#969696"),
    plot.caption = element_text(face = "bold", size = 8, hjust = 1, color = "#525252"),
    legend.title = element_blank(),
    legend.text = element_text(size = 9, color = "#525252"),
    legend.position = "top"
  )

# 2 - Composicao idoso por raca e idade --------------------------------------

# Total

descritivas_composicao$t3 |>
  filter(grupo_etario <= 90) |>
  filter(ano %in% c(2012,2015,2019,2021,2023)) |>
  filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = grupo_etario, y = prop, color = cor_raca, interaction(grupo_etario, grupo_etario)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.05) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), width=.1,
                linetype = "solid") +
  scale_color_viridis_d(option = "D",begin = .1, end = .6) +
  coord_cartesian(ylim = c(0,max(descritivas_composicao$t3$prop)+10)) +
  scale_x_continuous(breaks = seq(60,90,5)) +
  scale_y_continuous(breaks = seq(0,max(descritivas_composicao$t3$prop)+10,10)) +
  lemon::facet_rep_grid(. ~ as.factor(ano), repeat.tick.labels = TRUE) +
  labs(
    title = "Distribuição relativa da população por idade, segundo cor ou raça - Brasil 2012-2019",
    x = "Idade (grupos quinquenais)",
    y = "População (%)",
    caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Anual, 2012-2019."
  ) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#525252"),
    axis.title = element_text(size = 11, hjust = .5, color = "#525252"),
    strip.text = element_text(face = "bold", size = 11, hjust = .5, color = "#525252"),
    strip.background = element_blank(),
    axis.text = element_text(size = 9, color = "#969696"),
    plot.caption = element_text(face = "bold", size = 8, hjust = 1, color = "#525252"),
    legend.title = element_blank(),
    legend.text = element_text(size = 9, color = "#525252"),
    legend.position = "top"
  )

# Total (somente para 2019)

descritivas_composicao$t3 |>
  filter(grupo_etario <= 90) |>
  filter(ano %in% c(2019)) |>
  filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = grupo_etario, y = prop, color = cor_raca, interaction(grupo_etario, grupo_etario)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.05) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), width=.1,
                linetype = "solid") +
  scale_color_viridis_d(option = "D",begin = .1, end = .6) +
  coord_cartesian(ylim = c(0,max(descritivas_composicao$t3$prop)+10)) +
  scale_x_continuous(breaks = seq(60,90,5)) +
  scale_y_continuous(breaks = seq(0,max(descritivas_composicao$t3$prop)+10,10)) +
  # lemon::facet_rep_grid(. ~ as.factor(ano), repeat.tick.labels = TRUE) +
  labs(
    title = "(i)",
    x = "Idade (grupos quinquenais)",
    y = "População (%)",
    # caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Anual, 2012-2019."
  ) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 15, color = "#525252", hjust = .5),
    axis.title = element_text(size = 15, hjust = .5, color = "#525252"),
    strip.text = element_text(face = "bold", size = 11, hjust = .5, color = "#525252"),
    strip.background = element_blank(),
    axis.text = element_text(size = 13, color = "#969696"),
    plot.caption = element_text(face = "bold", size = 8, hjust = 1, color = "#525252"),
    legend.title = element_blank(),
    legend.text = element_text(size = 13, color = "#525252"),
    legend.position = "top"
  )
# Por sexo

descritivas_composicao$t3_by_sex |>
  filter(grupo_etario <= 90) |>
  filter(ano %in% c(2012,2015,2019,2021,2023)) |>
  filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = grupo_etario, y = prop, color = cor_raca, interaction(grupo_etario, grupo_etario)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.05) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), width=.1,
                linetype = "solid") +
  scale_color_viridis_d(option = "D",begin = .1, end = .6) +
  coord_cartesian(ylim = c(0,max(descritivas_composicao$t3_by_sex$prop)+10)) +
  scale_x_continuous(breaks = seq(60,90,5)) +
  scale_y_continuous(breaks = seq(0,max(descritivas_composicao$t3_by_sex$prop)+10,10)) +
  lemon::facet_rep_grid(sexo ~ as.factor(ano), repeat.tick.labels = TRUE) +
  labs(
    title = "Distribuição relativa da população por idade, segundo cor ou raça e sexo - Brasil 2012-2019",
    x = "Idade (grupos quinquenais)",
    y = "População (%)",
    caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Anual, 2012-2019."
  ) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#525252"),
    axis.title = element_text(size = 11, hjust = .5, color = "#525252"),
    strip.text = element_text(face = "bold", size = 11, hjust = .5, color = "#525252"),
    strip.background = element_blank(),
    axis.text = element_text(size = 9, color = "#969696"),
    plot.caption = element_text(face = "bold", size = 8, hjust = 1, color = "#525252"),
    legend.title = element_blank(),
    legend.text = element_text(size = 9, color = "#525252"),
    legend.position = "top"
  )

# 3 - Composicao idoso por raca e escolaridade --------------------------------------

# Total

descritivas_composicao$t5 |>
  filter(ano %in% c(2012,2015,2019,2021,2023)) |>
  filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = educ_atingida, y = prop, fill = cor_raca) +
  geom_col(position = position_dodge2(0.1)) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), position = position_dodge2(0.1)) +
  scale_fill_viridis_d(option = "D",begin = .2, end = .6) +
  coord_cartesian(ylim = c(0,max(descritivas_composicao$t5$prop)+10)) +
  # scale_x_continuous(breaks = seq(2012,2019,1)) +
  scale_y_continuous(breaks = seq(0,max(descritivas_composicao$t5$prop)+10,10)) +
  lemon::facet_rep_grid(. ~ as.factor(ano), repeat.tick.labels = TRUE) +
  labs(
    title = "Distribuição relativa da população por escolaridade atingida, segundo cor ou raça - Brasil 2012-2019",
    x = "Nível de escolaridade atingida",
    y = "População (%)",
    caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Anual, 2012-2019."
  ) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#525252"),
    axis.title = element_text(size = 11, hjust = .5, color = "#525252"),
    strip.text = element_text(face = "bold", size = 11, hjust = .5, color = "#525252"),
    strip.background = element_blank(),
    axis.text = element_text(size = 9, color = "#969696"),
    plot.caption = element_text(face = "bold", size = 8, hjust = 1, color = "#525252"),
    legend.title = element_blank(),
    legend.text = element_text(size = 9, color = "#525252"),
    legend.position = "top"
  )

# Por sexo

descritivas_composicao$t5_by_sex |>
  filter(ano %in% c(2012,2015,2019,2021,2023)) |>
  filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = educ_atingida, y = prop, fill = cor_raca) +
  geom_col(position = position_dodge2(0.1)) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), position = position_dodge2(0.1)) +
  scale_fill_viridis_d(option = "D",begin = .2, end = .6) +
  coord_cartesian(ylim = c(0,max(descritivas_composicao$t5_by_sex$prop)+10)) +
  # scale_x_continuous(breaks = seq(2012,2019,1)) +
  scale_y_continuous(breaks = seq(0,max(descritivas_composicao$t5_by_sex$prop)+10,10)) +
  lemon::facet_rep_grid(sexo ~ as.factor(ano), repeat.tick.labels = TRUE) +
  labs(
    title = "Distribuição relativa da população por escolaridade atingida, segundo cor ou raça e sexo - Brasil 2012-2019",
    x = "Nível de escolaridade atingida",
    y = "População (%)",
    caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Anual, 2012-2019."
  ) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#525252"),
    axis.title = element_text(size = 11, hjust = .5, color = "#525252"),
    strip.text = element_text(face = "bold", size = 11, hjust = .5, color = "#525252"),
    strip.background = element_blank(),
    axis.text = element_text(size = 9, color = "#969696"),
    plot.caption = element_text(face = "bold", size = 8, hjust = 1, color = "#525252"),
    legend.title = element_blank(),
    legend.text = element_text(size = 9, color = "#525252"),
    legend.position = "top"
  )

# 4 - Composicao idoso por raca e quintil de renda --------------------------------------

# Total

descritivas_composicao$t6 |>
  filter(!is.na(quintil_inc)) |>
  mutate(quintil_inc = factor(
    quintil_inc,
    levels = c(1,2,3,4,5),
    labels = c("P20","P40","P60","P80","P100"))) |>
  filter(ano %in% c(2012,2015,2019,2021,2023)) |>
  filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = quintil_inc, y = prop, fill = cor_raca) +
  geom_col(position = position_dodge2(0.1)) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), position = position_dodge2(0.1)) +
  scale_fill_viridis_d(option = "D",begin = .2, end = .6) +
  coord_cartesian(ylim = c(0,max(descritivas_composicao$t6$prop)+10)) +
  # scale_x_continuous(breaks = seq(2012,2019,1)) +
  scale_y_continuous(breaks = seq(0,max(descritivas_composicao$t6$prop)+10,10)) +
  lemon::facet_rep_grid(. ~ as.factor(ano), repeat.tick.labels = TRUE) +
  labs(
    title = "Distribuição relativa da população por quintil de renda domiciliar per capita de todas as fontes, segundo cor ou raça - Brasil 2012-2019",
    x = "Quintil de renda domiciliar per capita",
    y = "População (%)",
    caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Anual, 2012-2019."
  ) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#525252"),
    axis.title = element_text(size = 11, hjust = .5, color = "#525252"),
    strip.text = element_text(face = "bold", size = 11, hjust = .5, color = "#525252"),
    strip.background = element_blank(),
    axis.text = element_text(size = 9, color = "#969696"),
    plot.caption = element_text(face = "bold", size = 8, hjust = 1, color = "#525252"),
    legend.title = element_blank(),
    legend.text = element_text(size = 9, color = "#525252"),
    legend.position = "top"
  )

# Por sexo

descritivas_composicao$t6_by_sex |>
  filter(!is.na(quintil_inc)) |>
  mutate(quintil_inc = factor(
    quintil_inc,
    levels = c(1,2,3,4,5),
    labels = c("P20","P40","P60","P80","P100"))) |>
  filter(ano %in% c(2012,2015,2019,2021,2023)) |>
  filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = quintil_inc, y = prop, fill = cor_raca) +
  geom_col(position = position_dodge2(0.1)) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), position = position_dodge2(0.1)) +
  scale_fill_viridis_d(option = "D",begin = .2, end = .6) +
  coord_cartesian(ylim = c(0,max(descritivas_composicao$t6_by_sex$prop)+10)) +
  # scale_x_continuous(breaks = seq(2012,2019,1)) +
  scale_y_continuous(breaks = seq(0,max(descritivas_composicao$t6_by_sex$prop)+10,10)) +
  lemon::facet_rep_grid(sexo ~ as.factor(ano), repeat.tick.labels = TRUE) +
  labs(
    title = "Distribuição relativa da população por quintil de renda domiciliar per capita de todas as fontes, segundo cor ou raça e sexo - Brasil 2012-2019",
    x = "Quintil de renda domiciliar per capita",
    y = "População (%)",
    caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Anual, 2012-2019."
  ) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#525252"),
    axis.title = element_text(size = 11, hjust = .5, color = "#525252"),
    strip.text = element_text(face = "bold", size = 11, hjust = .5, color = "#525252"),
    strip.background = element_blank(),
    axis.text = element_text(size = 9, color = "#969696"),
    plot.caption = element_text(face = "bold", size = 8, hjust = 1, color = "#525252"),
    legend.title = element_blank(),
    legend.text = element_text(size = 9, color = "#525252"),
    legend.position = "top"
  )

# 5 - Composicao idoso por raca e tipo de domicilio ---------------------------------

# Total

descritivas_composicao$t7 |>
  mutate(tipo_dom = as.factor(case_when(
    tipo_dom == 1 ~ "Unipessoal",
    tipo_dom == 2 ~ "Nuclear",
    tipo_dom == 3 ~ "Estendida",
    tipo_dom == 4 ~ "Composta"))) |>
  filter(ano %in% c(2012,2015,2019,2021,2023)) |>
  filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = tipo_dom, y = prop, fill = cor_raca) +
  geom_col(position = position_dodge2(0.1)) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), position = position_dodge2(0.1)) +
  scale_fill_viridis_d(option = "D",begin = .2, end = .6) +
  coord_cartesian(ylim = c(0,max(descritivas_composicao$t7$prop)+10)) +
  # scale_x_continuous(breaks = seq(2012,2019,1)) +
  scale_y_continuous(breaks = seq(0,max(descritivas_composicao$t7$prop)+10,10)) +
  lemon::facet_rep_grid(. ~ as.factor(ano), repeat.tick.labels = TRUE) +
  labs(
    title = "Distribuição relativa da população por tipo de domicílio de residência, segundo cor ou raça - Brasil 2012-2019",
    x = "Tipo de domicílio",
    y = "População (%)",
    caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Anual, 2012-2019."
  ) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#525252"),
    axis.title = element_text(size = 11, hjust = .5, color = "#525252"),
    strip.text = element_text(face = "bold", size = 11, hjust = .5, color = "#525252"),
    strip.background = element_blank(),
    axis.text = element_text(size = 9, color = "#969696"),
    plot.caption = element_text(face = "bold", size = 8, hjust = 1, color = "#525252"),
    legend.title = element_blank(),
    legend.text = element_text(size = 9, color = "#525252"),
    legend.position = "top"
  )

# Por sexo

descritivas_composicao$t7_by_sex |>
  mutate(tipo_dom = as.factor(case_when(
    tipo_dom == 1 ~ "Unipessoal",
    tipo_dom == 2 ~ "Nuclear",
    tipo_dom == 3 ~ "Estendida",
    tipo_dom == 4 ~ "Composta"))) |>
  filter(ano %in% c(2012,2015,2019,2021,2023)) |>
  filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = tipo_dom, y = prop, fill = cor_raca) +
  geom_col(position = position_dodge2(0.1)) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), position = position_dodge2(0.1)) +
  scale_fill_viridis_d(option = "D",begin = .2, end = .6) +
  coord_cartesian(ylim = c(0,max(descritivas_composicao$t7_by_sex$prop)+10)) +
  # scale_x_continuous(breaks = seq(2012,2019,1)) +
  scale_y_continuous(breaks = seq(0,max(descritivas_composicao$t7_by_sex$prop)+10,10)) +
  lemon::facet_rep_grid(sexo ~ as.factor(ano), repeat.tick.labels = TRUE) +
  labs(
    title = "Distribuição relativa da população por tipo de domicílio de residência, segundo cor ou raça e sexo - Brasil 2012-2019",
    x = "Tipo de domicílio",
    y = "População (%)",
    caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Anual, 2012-2019."
  ) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#525252"),
    axis.title = element_text(size = 11, hjust = .5, color = "#525252"),
    strip.text = element_text(face = "bold", size = 11, hjust = .5, color = "#525252"),
    strip.background = element_blank(),
    axis.text = element_text(size = 9, color = "#969696"),
    plot.caption = element_text(face = "bold", size = 8, hjust = 1, color = "#525252"),
    legend.title = element_blank(),
    legend.text = element_text(size = 9, color = "#525252"),
    legend.position = "top"
  )

# 6 - Composicao idoso por raca e dependentes ---------------------------------

# Total

descritivas_composicao$t8 |>
  mutate(tem_dependente = as.factor(case_when(
    tem_crianca == 1 & tem_idoso_dependente == 0 ~ "Somente criança",
    tem_crianca == 0 & tem_idoso_dependente == 1 ~ "Somente idoso",
    tem_crianca == 1 & tem_idoso_dependente == 1 ~ "Criança e idoso",
    tem_crianca == 0 & tem_idoso_dependente == 0 ~ "Nenhum"))) |>
  select(-c(tem_crianca, tem_idoso_dependente)) |>
  ungroup() |>
  group_by(ano, cor_raca, tem_dependente) |>
  summarise(n = n,
            prop_se = prop_se) |>
  summarise(ano = ano, cor_raca = cor_raca, tem_dependente = tem_dependente,
            n = n,
            prop = round(n/sum(n)*100,2),
            prop_se = prop_se) |>
  filter(ano %in% c(2012,2015,2019,2021,2023)) |>
  # filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = tem_dependente, y = prop, fill = cor_raca) +
  geom_col(position = position_dodge2(0.1)) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), position = position_dodge2(0.1)) +
  scale_fill_viridis_d(option = "D",begin = .2, end = .6) +
  coord_cartesian(ylim = c(0,100)) +
  # scale_x_continuous(breaks = seq(2012,2019,1)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  lemon::facet_rep_grid(. ~ as.factor(ano), repeat.tick.labels = TRUE) +
  labs(
    # title = "Distribuição relativa da população por existência de dependente, segundo cor ou raça - Brasil 2012-2019",
    x = "Existência de dependentes",
    y = "População (%)",
    # caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Anual, 2012-2019."
  ) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#525252"),
    axis.title = element_text(size = 11, hjust = .5, color = "#525252"),
    strip.text = element_text(face = "bold", size = 11, hjust = .5, color = "#525252"),
    strip.background = element_blank(),
    axis.text = element_text(size = 9, color = "#969696"),
    plot.caption = element_text(face = "bold", size = 8, hjust = 1, color = "#525252"),
    legend.title = element_blank(),
    legend.text = element_text(size = 9, color = "#525252"),
    legend.position = "top"
  )

# Por sexo

descritivas_composicao$t8_by_sex |>
  mutate(tem_dependente = as.factor(case_when(
    tem_crianca == 1 & tem_idoso_dependente == 0 ~ "Somente criança",
    tem_crianca == 0 & tem_idoso_dependente == 1 ~ "Somente idoso",
    tem_crianca == 1 & tem_idoso_dependente == 1 ~ "Criança e idoso",
    tem_crianca == 0 & tem_idoso_dependente == 0 ~ "Nenhum"))) |>
  select(-c(tem_crianca, tem_idoso_dependente)) |>
  ungroup() |>
  group_by(ano, cor_raca, sexo, tem_dependente) |>
  summarise(n = n,
            prop_se = prop_se) |>
  summarise(ano = ano, cor_raca = cor_raca, sexo = sexo, tem_dependente = tem_dependente,
            n = n,
            prop = round(n/sum(n)*100,2),
            prop_se = prop_se) |>
  filter(ano %in% c(2012,2015,2019,2021,2023)) |>
  # filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = tem_dependente, y = prop, fill = cor_raca) +
  geom_col(position = position_dodge2(0.1)) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), position = position_dodge2(0.1)) +
  scale_fill_viridis_d(option = "D",begin = .2, end = .6) +
  coord_cartesian(ylim = c(0,100)) +
  # scale_x_continuous(breaks = seq(2012,2019,1)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  lemon::facet_rep_grid(sexo ~ as.factor(ano), repeat.tick.labels = TRUE) +
  labs(
    title = "Distribuição relativa da população por existência de dependente, segundo cor ou raça e sexo - Brasil 2012-2019",
    x = "Existência de dependentes",
    y = "População (%)",
    caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Anual, 2012-2019."
  ) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#525252"),
    axis.title = element_text(size = 11, hjust = .5, color = "#525252"),
    strip.text = element_text(face = "bold", size = 11, hjust = .5, color = "#525252"),
    strip.background = element_blank(),
    axis.text = element_text(size = 9, color = "#969696"),
    plot.caption = element_text(face = "bold", size = 8, hjust = 1, color = "#525252"),
    legend.title = element_blank(),
    legend.text = element_text(size = 9, color = "#525252"),
    legend.position = "top"
  )

# 7 - Composicao idoso por raca e aposentadoria ---------------------------------

# Total

descritivas_composicao$t10 |>
  filter(ano %in% c(2019)) |>
  filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = flag_aposentadoria, y = prop, fill = cor_raca) +
  geom_col(position = position_dodge2(0.1)) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), position = position_dodge2(0.1)) +
  scale_fill_viridis_d(option = "D",begin = .2, end = .6) +
  coord_cartesian(ylim = c(0,max(descritivas_composicao$t10$prop)+10)) +
  scale_y_continuous(breaks = seq(0,max(descritivas_composicao$t10$prop)+10,10)) +
  lemon::facet_rep_grid(. ~ as.factor(ano), repeat.tick.labels = TRUE) +
  labs(
    title = "(ii)",
    # title = "Distribuição relativa da população por status de aposentadoria, segundo cor ou raça - Brasil 2012-2019",
    x = "Status de aposentadoria",
    y = "População (%)",
    # caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Anual, 2012-2019."
  ) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#525252", hjust = .5),
    axis.title = element_text(size = 11, hjust = .5, color = "#525252"),
    strip.text = element_text(face = "bold", size = 11, hjust = .5, color = "#525252"),
    strip.background = element_blank(),
    axis.text = element_text(size = 9, color = "#969696"),
    plot.caption = element_text(face = "bold", size = 8, hjust = 1, color = "#525252"),
    legend.title = element_blank(),
    legend.text = element_text(size = 9, color = "#525252"),
    legend.position = "top"
  )

# Por sexo

descritivas_composicao$t10_by_sex |>
  filter(ano %in% c(2012,2015,2019,2021,2023)) |>
  filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = flag_aposentadoria, y = prop, fill = cor_raca) +
  geom_col(position = position_dodge2(0.1)) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), position = position_dodge2(0.1)) +
  scale_fill_viridis_d(option = "D",begin = .2, end = .6) +
  coord_cartesian(ylim = c(0,max(descritivas_composicao$t10_by_sex$prop)+10)) +
  scale_y_continuous(breaks = seq(0,max(descritivas_composicao$t10_by_sex$prop)+10,10)) +
  lemon::facet_rep_grid(sexo ~ as.factor(ano), repeat.tick.labels = TRUE) +
  labs(
    title = "Distribuição relativa da população por status de aposentadoria, segundo cor ou raça e sexo - Brasil 2012-2019",
    x = "Status de aposentadoria",
    y = "População (%)",
    caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Anual, 2012-2019."
  ) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#525252"),
    axis.title = element_text(size = 11, hjust = .5, color = "#525252"),
    strip.text = element_text(face = "bold", size = 11, hjust = .5, color = "#525252"),
    strip.background = element_blank(),
    axis.text = element_text(size = 9, color = "#969696"),
    plot.caption = element_text(face = "bold", size = 8, hjust = 1, color = "#525252"),
    legend.title = element_blank(),
    legend.text = element_text(size = 9, color = "#525252"),
    legend.position = "top"
  )

# 8.1 - Composicao idoso por raca e parcela da renda individual no domicilio ---

# Total

pnad_idoso |>
  filter(ano %in% c(2019)) |>
  mutate(
    inc_prop_individuo = case_when(is.na(inc_prop_individuo) ~ 0,
                                   TRUE ~ inc_prop_individuo),
    inc_prop_aposentado = case_when(is.na(inc_prop_aposentado) ~ 0,
                                    TRUE ~ inc_prop_aposentado),
    inc_prop_aposentado_dom = case_when(is.na(inc_prop_aposentado_dom) ~ 0,
                                        TRUE ~ inc_prop_aposentado_dom),
    anofct = fct_rev(as.factor(ano)),
    flag_participa = as.factor(case_when(
      flag_participa == "forca_trabalho" ~ "Na força de trabalho",
      TRUE ~ "Fora da força de trabalho"))
  ) |>
  as_tibble() |>
  ggplot() +
  aes(y = flag_participa) +
  ggridges::geom_density_ridges(
    aes(x = round(inc_prop_individuo*100,2), fill = paste(cor_raca)),
    alpha = .6, color = "black", from = 0, to = 100
  ) +
  scale_fill_viridis_d(option = "D",begin = .2, end = .6) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  labs(
    # title = "(i)",
    title = "Distribuição de densidade da população por parcela da contribuição da renda do idoso na renda domiciliar, \nsegundo cor ou raça - Brasil 2012-2019",
    x = "Contribuição da renda do idoso na renda domiciliar total",
    y = "Densidade",
    # caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Anual, 2012-2019."
  ) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#525252", hjust = .5),
    axis.title = element_text(size = 11, hjust = .5, color = "#525252"),
    strip.text = element_text(face = "bold", size = 11, hjust = .5, color = "#525252"),
    strip.background = element_blank(),
    axis.text = element_text(size = 9, color = "#969696"),
    plot.caption = element_text(face = "bold", size = 8, hjust = 1, color = "#525252"),
    legend.title = element_blank(),
    legend.text = element_text(size = 9, color = "#525252"),
    legend.position = "top"
  )

# Por sexo

pnad_idoso |>
  filter(ano %in% c(2012,2015,2019,2021,2023)) |>
  mutate(
    inc_prop_individuo = case_when(is.na(inc_prop_individuo) ~ 0,
                                   TRUE ~ inc_prop_individuo),
    inc_prop_aposentado = case_when(is.na(inc_prop_aposentado) ~ 0,
                                    TRUE ~ inc_prop_aposentado),
    inc_prop_aposentado_dom = case_when(is.na(inc_prop_aposentado_dom) ~ 0,
                                        TRUE ~ inc_prop_aposentado_dom),
    anofct = as.factor(ano)
  ) |>
  as_tibble() |>
  ggplot() +
  aes(y = flag_participa) +
  ggridges::geom_density_ridges(
    aes(x = round(inc_prop_individuo*100,2), fill = paste(cor_raca)),
    alpha = .6, color = "black", from = 0, to = 100
  ) +
  scale_fill_viridis_d(option = "D",begin = .2, end = .6) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  lemon::facet_rep_grid(sexo ~ anofct, repeat.tick.labels = TRUE) +
  labs(
    title = "Distribuição de densidade da população por parcela da contribuição da renda do idoso na renda domiciliar, \nsegundo cor ou raça e sexo - Brasil 2012-2019",
    x = "Contribuição da renda do idoso na renda domiciliar total",
    y = "Densidade",
    caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Anual, 2012-2019."
  ) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#525252", hjust = .5),
    axis.title = element_text(size = 11, hjust = .5, color = "#525252"),
    strip.text = element_text(face = "bold", size = 11, hjust = .5, color = "#525252"),
    strip.background = element_blank(),
    axis.text = element_text(size = 9, color = "#969696"),
    plot.caption = element_text(face = "bold", size = 8, hjust = 1, color = "#525252"),
    legend.title = element_blank(),
    legend.text = element_text(size = 9, color = "#525252"),
    legend.position = "top"
  )

# 8.2 - Composicao idoso por raca e parcela da renda de aposentadoria na renda individual

# Total

pnad_idoso |>
  filter(ano %in% c(2012,2015,2019,2021,2023)) |>
  mutate(
    inc_prop_individuo = case_when(is.na(inc_prop_individuo) ~ 0,
                                   TRUE ~ inc_prop_individuo),
    inc_prop_aposentado = case_when(is.na(inc_prop_aposentado) ~ 0,
                                    TRUE ~ inc_prop_aposentado),
    inc_prop_aposentado_dom = case_when(is.na(inc_prop_aposentado_dom) ~ 0,
                                        TRUE ~ inc_prop_aposentado_dom),
    anofct = fct_rev(as.factor(ano))
  ) |>
  as_tibble() |>
  ggplot() +
  aes(y = anofct) +
  ggridges::geom_density_ridges(
    aes(x = round(inc_prop_aposentado*100,2), fill = paste(cor_raca)),
    alpha = .6, color = "black", from = 0, to = 100
  ) +
  scale_fill_viridis_d(option = "D",begin = .2, end = .6) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  labs(
    title = "Distribuição de densidade da população por parcela da contribuição da renda do idoso oriunda da aposentadoria na renda individual\nde todas as fontes, segundo cor ou raça - Brasil 2012-2019",
    x = "Contribuição da renda de aposentadoria na renda total do idoso",
    y = "Densidade",
    caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Anual, 2012-2019."
  ) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#525252", hjust = .5),
    axis.title = element_text(size = 11, hjust = .5, color = "#525252"),
    strip.text = element_text(face = "bold", size = 11, hjust = .5, color = "#525252"),
    strip.background = element_blank(),
    axis.text = element_text(size = 9, color = "#969696"),
    plot.caption = element_text(face = "bold", size = 8, hjust = 1, color = "#525252"),
    legend.title = element_blank(),
    legend.text = element_text(size = 9, color = "#525252"),
    legend.position = "top"
  )

# Por sexo

pnad_idoso |>
  filter(ano %in% c(2012,2015,2019,2021,2023)) |>
  mutate(
    inc_prop_individuo = case_when(is.na(inc_prop_individuo) ~ 0,
                                   TRUE ~ inc_prop_individuo),
    inc_prop_aposentado = case_when(is.na(inc_prop_aposentado) ~ 0,
                                    TRUE ~ inc_prop_aposentado),
    inc_prop_aposentado_dom = case_when(is.na(inc_prop_aposentado_dom) ~ 0,
                                        TRUE ~ inc_prop_aposentado_dom),
    anofct = as.factor(ano)
  ) |>
  as_tibble() |>
  ggplot() +
  aes(y = flag_participa) +
  ggridges::geom_density_ridges(
    aes(x = round(inc_prop_aposentado*100,2), fill = paste(cor_raca)),
    alpha = .6, color = "black", from = 0, to = 100
  ) +
  scale_fill_viridis_d(option = "D",begin = .2, end = .6) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  lemon::facet_rep_grid(sexo ~ anofct, repeat.tick.labels = TRUE) +
  labs(
    title = "Distribuição de densidade da população por parcela da contribuição da renda do idoso oriunda da aposentadoria na renda individual\nde todas as fontes, segundo cor ou raça - Brasil 2012-2019",
    x = "Contribuição da renda de aposentadoria na renda total do idoso",
    y = "Densidade",
    caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Anual, 2012-2019."
  ) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#525252", hjust = .5),
    axis.title = element_text(size = 11, hjust = .5, color = "#525252"),
    strip.text = element_text(face = "bold", size = 11, hjust = .5, color = "#525252"),
    strip.background = element_blank(),
    axis.text = element_text(size = 9, color = "#969696"),
    plot.caption = element_text(face = "bold", size = 8, hjust = 1, color = "#525252"),
    legend.title = element_blank(),
    legend.text = element_text(size = 9, color = "#525252"),
    legend.position = "top"
  )

# 8.3 - Composicao idoso por raca e parcela da renda de aposentadoria na renda domiciliar

# Total

pnad_idoso |>
  filter(ano %in% c(2019)) |>
  mutate(
    inc_prop_individuo = case_when(is.na(inc_prop_individuo) ~ 0,
                                   TRUE ~ inc_prop_individuo),
    inc_prop_aposentado = case_when(is.na(inc_prop_aposentado) ~ 0,
                                    TRUE ~ inc_prop_aposentado),
    inc_prop_aposentado_dom = case_when(is.na(inc_prop_aposentado_dom) ~ 0,
                                        TRUE ~ inc_prop_aposentado_dom),
    anofct = fct_rev(as.factor(ano)),
    flag_participa = as.factor(case_when(
      flag_participa == "forca_trabalho" ~ "Na força de trabalho",
      TRUE ~ "Fora da força de trabalho"))
  ) |>
  as_tibble() |>
  ggplot() +
  aes(y = flag_participa) +
  ggridges::geom_density_ridges(
    aes(x = round(inc_prop_aposentado_dom*100,2), fill = paste(cor_raca)),
    alpha = .6, color = "black", from = 0, to = 100
  ) +
  scale_fill_viridis_d(option = "D",begin = .2, end = .6) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  labs(
    title = "(ii)",
    # title = "Distribuição de densidade da população por parcela da contribuição da renda do idoso \noriunda da aposentadoria na renda domiciliar total, segundo cor ou raça - Brasil 2012-2019",
    x = "Contribuição da renda de aposentadoria na renda total do domicílio",
    y = "Densidade",
    # caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Anual, 2012-2019."
  ) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#525252", hjust = .5),
    axis.title = element_text(size = 11, hjust = .5, color = "#525252"),
    strip.text = element_text(face = "bold", size = 11, hjust = .5, color = "#525252"),
    strip.background = element_blank(),
    axis.text = element_text(size = 9, color = "#969696"),
    plot.caption = element_text(face = "bold", size = 8, hjust = 1, color = "#525252"),
    legend.title = element_blank(),
    legend.text = element_text(size = 9, color = "#525252"),
    legend.position = "top"
  )

# Por sexo

pnad_idoso |>
  filter(ano %in% c(2012,2015,2019,2021,2023)) |>
  mutate(
    inc_prop_individuo = case_when(is.na(inc_prop_individuo) ~ 0,
                                   TRUE ~ inc_prop_individuo),
    inc_prop_aposentado = case_when(is.na(inc_prop_aposentado) ~ 0,
                                    TRUE ~ inc_prop_aposentado),
    inc_prop_aposentado_dom = case_when(is.na(inc_prop_aposentado_dom) ~ 0,
                                        TRUE ~ inc_prop_aposentado_dom),
    anofct = as.factor(ano)
  ) |>
  as_tibble() |>
  ggplot() +
  aes(y = flag_participa) +
  ggridges::geom_density_ridges(
    aes(x = round(inc_prop_aposentado_dom*100,2), fill = paste(cor_raca)),
    alpha = .6, color = "black", from = 0, to = 100
  ) +
  scale_fill_viridis_d(option = "D",begin = .2, end = .6) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  lemon::facet_rep_grid(sexo ~ anofct, repeat.tick.labels = TRUE) +
  labs(
    title = "Distribuição de densidade da população por parcela da contribuição da renda do idoso oriunda da aposentadoria na renda domiciliar total,\n segundo cor ou raça e sexo - Brasil 2012-2019",
    x = "Contribuição da renda de aposentadoria na renda total do domicílio",
    y = "Densidade",
    caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Anual, 2012-2019."
  ) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#525252", hjust = .5),
    axis.title = element_text(size = 11, hjust = .5, color = "#525252"),
    strip.text = element_text(face = "bold", size = 11, hjust = .5, color = "#525252"),
    strip.background = element_blank(),
    axis.text = element_text(size = 9, color = "#969696"),
    plot.caption = element_text(face = "bold", size = 8, hjust = 1, color = "#525252"),
    legend.title = element_blank(),
    legend.text = element_text(size = 9, color = "#525252"),
    legend.position = "top"
  )

# Tabela

t7_tabela <- pnad_idoso |>
  filter(ano %in% c(2019)) |>
  mutate(
    inc_prop_individuo = case_when(is.na(inc_prop_individuo) ~ 0,
                                   TRUE ~ inc_prop_individuo),
    inc_prop_aposentado = case_when(is.na(inc_prop_aposentado) ~ 0,
                                    TRUE ~ inc_prop_aposentado),
    inc_prop_aposentado_dom = case_when(is.na(inc_prop_aposentado_dom) ~ 0,
                                        TRUE ~ inc_prop_aposentado_dom),
    anofct = fct_rev(as.factor(ano)),
    flag_participa = as.factor(case_when(
      flag_participa == "forca_trabalho" ~ "Na força de trabalho",
      TRUE ~ "Fora da força de trabalho"))
  ) |>
  group_by(cor_raca, flag_participa) |>
  summarise("Contribuição da renda do idoso de todas as fontes" = 100*srvyr::survey_mean(inc_prop_individuo, na.rm = T),
            "Contribuição da renda do idoso oriunda de aposentadoria" = 100*srvyr::survey_mean(inc_prop_aposentado_dom, na.rm = T))

clipr::write_clip(t7_tabela, dec = ",")
