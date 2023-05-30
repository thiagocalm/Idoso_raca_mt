options(scipen = 99999)

# Bibliotecas -------------------------------------------------------------

ifelse(!require(tidyverse),install.packages("tidyverse"),require(tidyverse))
ifelse(!require(srvyr),install.packages("srvyr"),require(srvyr))
ifelse(!require(survey),install.packages("survey"),require(survey))


# Importacao dos dados ----------------------------------------------------

load("./Analises/outputs/pt1/descritivas_taxa.RData")

# 1 - TAE por raca e status de idoso --------------------------------------

# Total

descritivas_taxa$t1 |>
  filter(flag_participa == "forca_trabalho") |>
  filter(cor_raca != "Outros") |>
  ggplot() +
  aes(x = ano, y = prop, linetype = cor_raca, color = flag_idoso) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.05) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), width=.1,
                linetype = "solid") +
  scale_color_viridis_d(option = "D",begin = .1, end = .6) +
  coord_cartesian(ylim = c(15,75)) +
  scale_x_continuous(breaks = seq(2012,2019,1)) +
  scale_y_continuous(breaks = seq(15,80,10)) +
  labs(
    # title = "Taxa Bruta de Participação no Mercado de Trabalho por cor ou raça e se é idoso ou não - Brasil 2012-2019",
    x = "Ano Calendário",
    y = "Taxa Bruta de Participação no Mercado de Trabalho",
    # caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Anual, 2012-2019."
  ) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#525252"),
    axis.title = element_text(size = 15, hjust = .5, color = "#525252"),
    axis.text = element_text(size = 13, color = "#969696"),
    plot.caption = element_text(face = "bold", size = 8, hjust = 1, color = "#525252"),
    legend.title = element_blank(),
    legend.text = element_text(size = 13, color = "#525252"),
    legend.position = "top"
  )

# Por sexo

descritivas_taxa$t1_by_sex |>
  filter(flag_participa == "forca_trabalho") |>
  filter(cor_raca != "Outros") |>
  ggplot() +
  aes(x = ano, y = prop, linetype = cor_raca, color = flag_idoso) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.05) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), width=.1,
                linetype = "solid") +
  scale_color_viridis_d(option = "D",begin = .1, end = .6) +
  coord_cartesian(ylim = c(15,75)) +
  scale_x_continuous(breaks = seq(2012,2019,1)) +
  scale_y_continuous(breaks = seq(15,80,10)) +
  lemon::facet_rep_grid(. ~ sexo, repeat.tick.labels = TRUE) +
  labs(
    title = "Taxa Bruta de Participação no Mercado de Trabalho por cor ou raça e se é idoso ou não, segundo o sexo - Brasil 2012-2019",
    x = "Ano Calendário",
    y = "Taxa Bruta de Participação no Mercado de Trabalho",
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

# 2 - TAE idoso por raca e idade --------------------------------------

# Total

descritivas_taxa$t2 |>
  filter(flag_participa == "forca_trabalho") |>
  filter(grupo_etario <= 90) |>
  filter(ano %in% c(2012,2015,2019)) |>
  filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = grupo_etario, y = prop, color = cor_raca, interaction(grupo_etario, grupo_etario)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.05) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), width=.1,
                linetype = "solid") +
  scale_color_viridis_d(option = "D",begin = .1, end = .6) +
  coord_cartesian(ylim = c(0,75)) +
  scale_x_continuous(breaks = seq(60,90,5)) +
  scale_y_continuous(breaks = seq(0,80,10)) +
    lemon::facet_rep_grid(. ~ as.factor(ano), repeat.tick.labels = TRUE) +
  labs(
    # title = "Taxa Específica de Participação no Mercado de Trabalho idade, segundo cor ou raça - Brasil 2012-2019",
    x = "Ano Calendário",
    y = "Taxa Específica de Participação no Mercado de Trabalho",
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

# Total (somente 2019)

descritivas_taxa$t2 |>
  filter(flag_participa == "forca_trabalho") |>
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
  coord_cartesian(ylim = c(0,75)) +
  scale_x_continuous(breaks = seq(60,90,5)) +
  scale_y_continuous(breaks = seq(0,80,10)) +
  # lemon::facet_rep_grid(. ~ as.factor(ano), repeat.tick.labels = TRUE) +
  labs(
    title = "(ii)",
    x = "Idade (grupos quinquenais)",
    y = "Taxa Específica de Participação no Mercado de Trabalho",
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

descritivas_taxa$t2_by_sex |>
  filter(flag_participa == "forca_trabalho") |>
  filter(grupo_etario <= 90) |>
  filter(ano %in% c(2012,2015,2019)) |>
  filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = grupo_etario, y = prop, color = cor_raca, interaction(grupo_etario, grupo_etario)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.05) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), width=.1,
                linetype = "solid") +
  scale_color_viridis_d(option = "D",begin = .1, end = .6) +
  coord_cartesian(ylim = c(0,70)) +
  scale_x_continuous(breaks = seq(60,90,5)) +
  scale_y_continuous(breaks = seq(0,80,10)) +
  lemon::facet_rep_grid(sexo ~ as.factor(ano), repeat.tick.labels = TRUE) +
  labs(
    title = "Taxa Específica de Participação no Mercado de Trabalho idade, segundo cor ou raça e sexo - Brasil 2012-2019",
    x = "Ano Calendário",
    y = "Taxa Específica de Participação no Mercado de Trabalho",
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

# 3 - TAE idoso por raca e escolaridade --------------------------------------

# Total

descritivas_taxa$t3 |>
  filter(flag_participa == "forca_trabalho") |>
  filter(ano %in% c(2012,2015,2019)) |>
  filter(cor_raca != "Total") |>
  mutate(educ_atingida = factor(educ_atingida, levels = c("Sem instrução","Ensino Fundamental", "Ensino Médio",
                                                          "Ensino Superior"))) |>
  ggplot() +
  aes(x = educ_atingida, y = prop, fill = cor_raca) +
  geom_col(position = position_dodge2(0.1)) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), position = position_dodge2(0.1)) +
  scale_fill_viridis_d(option = "D",begin = .2, end = .6) +
  coord_cartesian(ylim = c(0,60)) +
  # scale_x_continuous(breaks = seq(2012,2019,1)) +
  scale_y_continuous(breaks = seq(0,80,10)) +
  lemon::facet_rep_grid(. ~ as.factor(ano), repeat.tick.labels = TRUE) +
  labs(
    # title = "Taxa Específica de Participação no Mercado de Trabalho por escolaridade atingida, segundo cor ou raça - Brasil 2012-2019",
    x = "Ano Calendário",
    y = "Taxa Específica de Participação no Mercado de Trabalho",
    # caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Anual, 2012-2019."
  ) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#525252"),
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

descritivas_taxa$t3_by_sex |>
  filter(flag_participa == "forca_trabalho") |>
  filter(ano %in% c(2012,2015,2019)) |>
  filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = educ_atingida, y = prop, fill = cor_raca) +
  geom_col(position = position_dodge2(0.1)) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), position = position_dodge2(0.1)) +
  scale_fill_viridis_d(option = "D",begin = .2, end = .6) +
  coord_cartesian(ylim = c(0,60)) +
  # scale_x_continuous(breaks = seq(2012,2019,1)) +
  scale_y_continuous(breaks = seq(0,80,10)) +
  lemon::facet_rep_grid(sexo ~ as.factor(ano), repeat.tick.labels = TRUE) +
  labs(
    title = "Taxa Específica de Participação no Mercado de Trabalho por escolaridade atingida, segundo cor ou raça e sexo - Brasil 2012-2019",
    x = "Ano Calendário",
    y = "Taxa Específica de Participação no Mercado de Trabalho por escolaridade",
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

# 4 - TAE idoso por raca e quintil de renda --------------------------------------

# Total

descritivas_taxa$t4 |>
  filter(!is.na(quintil_inc)) |>
  mutate(quintil_inc = as.factor(case_when(
    quintil_inc == 1 ~ "P20",
    quintil_inc == 2 ~ "P40",
    quintil_inc == 3 ~ "P60",
    quintil_inc == 4 ~ "P80",
    quintil_inc == 5 ~ "P100"
  ))) |>
  filter(flag_participa == "forca_trabalho") |>
  filter(ano %in% c(2012,2015,2019)) |>
  filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = quintil_inc, y = prop, fill = cor_raca) +
  geom_col(position = position_dodge2(0.1)) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), position = position_dodge2(0.1)) +
  scale_fill_viridis_d(option = "D",begin = .2, end = .6) +
  coord_cartesian(ylim = c(0,60)) +
  # scale_x_continuous(breaks = seq(2012,2019,1)) +
  scale_y_continuous(breaks = seq(0,80,10)) +
  lemon::facet_rep_grid(. ~ as.factor(ano), repeat.tick.labels = TRUE) +
  labs(
    # title = "Taxa Específica de Participação no Mercado de Trabalho por quintil de renda domiciliar per capita de todas as fontes, segundo cor ou raça - Brasil 2012-2019",
    x = "Ano Calendário",
    y = "Taxa Específica de Participação no Mercado de Trabalho",
    # caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Anual, 2012-2019."
  ) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#525252"),
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

descritivas_taxa$t4_by_sex |>
  filter(!is.na(quintil_inc)) |>
  mutate(quintil_inc = as.factor(case_when(
    quintil_inc == 1 ~ "P20",
    quintil_inc == 2 ~ "P40",
    quintil_inc == 3 ~ "P60",
    quintil_inc == 4 ~ "P80",
    quintil_inc == 5 ~ "P100"
  ))) |>
  filter(flag_participa == "forca_trabalho") |>
  filter(ano %in% c(2012,2015,2019)) |>
  filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = quintil_inc, y = prop, fill = cor_raca) +
  geom_col(position = position_dodge2(0.1)) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), position = position_dodge2(0.1)) +
  scale_fill_viridis_d(option = "D",begin = .2, end = .6) +
  coord_cartesian(ylim = c(0,60)) +
  # scale_x_continuous(breaks = seq(2012,2019,1)) +
  scale_y_continuous(breaks = seq(0,80,10)) +
  lemon::facet_rep_grid(sexo ~ as.factor(ano), repeat.tick.labels = TRUE) +
  labs(
    # title = "Taxa Específica de Participação no Mercado de Trabalho por quintil de renda domiciliar per capita de todas as fontes,\n segundo cor ou raça e sexo - Brasil 2012-2019",
    x = "Ano Calendário",
    y = "Taxa Específica de Participação no Mercado de Trabalho",
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

# 5 - TAE idoso por raca e tipo de domicilio ---------------------------------

# Total

descritivas_taxa$t5 |>
  mutate(tipo_dom = as.factor(case_when(
    tipo_dom == 1 ~ "Unipessoal",
    tipo_dom == 2 ~ "Nuclear",
    tipo_dom == 3 ~ "Estendida",
    tipo_dom == 4 ~ "Composta"))) |>
  filter(flag_participa == "forca_trabalho") |>
  filter(ano %in% c(2012,2015,2019)) |>
  filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = tipo_dom, y = prop, fill = cor_raca) +
  geom_col(position = position_dodge2(0.1)) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), position = position_dodge2(0.1)) +
  scale_fill_viridis_d(option = "D",begin = .2, end = .6) +
  coord_cartesian(ylim = c(0,60)) +
  # scale_x_continuous(breaks = seq(2012,2019,1)) +
  scale_y_continuous(breaks = seq(0,80,10)) +
  lemon::facet_rep_grid(. ~ as.factor(ano), repeat.tick.labels = TRUE) +
  labs(
    title = "Taxa Específica de Participação no Mercado de Trabalho por tipo de domicílio de residência, segundo cor ou raça - Brasil 2012-2019",
    x = "Ano Calendário",
    y = "Taxa Específica de Participação no Mercado de Trabalho por tipo de domicilio",
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

descritivas_taxa$t5_by_sex |>
  mutate(tipo_dom = as.factor(case_when(
    tipo_dom == 1 ~ "Unipessoal",
    tipo_dom == 2 ~ "Nuclear",
    tipo_dom == 3 ~ "Estendida",
    tipo_dom == 4 ~ "Composta"))) |>
  filter(flag_participa == "forca_trabalho") |>
  filter(ano %in% c(2012,2015,2019)) |>
  filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = tipo_dom, y = prop, fill = cor_raca) +
  geom_col(position = position_dodge2(0.1)) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), position = position_dodge2(0.1)) +
  scale_fill_viridis_d(option = "D",begin = .2, end = .6) +
  coord_cartesian(ylim = c(0,60)) +
  # scale_x_continuous(breaks = seq(2012,2019,1)) +
  scale_y_continuous(breaks = seq(0,80,10)) +
  lemon::facet_rep_grid(sexo ~ as.factor(ano), repeat.tick.labels = TRUE) +
  labs(
    title = "Taxa Específica de Participação no Mercado de Trabalho por tipo de domicílio de residência, segundo cor ou raça e sexo - Brasil 2012-2019",
    x = "Ano Calendário",
    y = "Taxa Específica de Participação no Mercado de Trabalho por tipo de domicilio",
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

# 6 - TAE idoso por raca e dependentes ---------------------------------

# Total

descritivas_taxa$t6 |>
  mutate(tem_dependente = as.factor(case_when(
    tem_crianca == 1 & tem_idoso_dependente == 0 ~ "Somente criança",
    tem_crianca == 0 & tem_idoso_dependente == 1 ~ "Somente idoso",
    tem_crianca == 1 & tem_idoso_dependente == 1 ~ "Criança e idoso",
    tem_crianca == 0 & tem_idoso_dependente == 0 ~ "Nenhum"))) |>
  filter(flag_participa == "forca_trabalho") |>
  filter(ano %in% c(2012,2015,2019)) |>
  # filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = tem_dependente, y = prop, fill = cor_raca) +
  geom_col(position = position_dodge2(0.1)) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), position = position_dodge2(0.1)) +
  scale_fill_viridis_d(option = "D",begin = .2, end = .6) +
  coord_cartesian(ylim = c(0,60)) +
  # scale_x_continuous(breaks = seq(2012,2019,1)) +
  scale_y_continuous(breaks = seq(0,80,10)) +
  lemon::facet_rep_grid(. ~ as.factor(ano), repeat.tick.labels = TRUE) +
  labs(
    # title = "Taxa Específica de Participação no Mercado de Trabalho por existência de dependente, \n segundo cor ou raça - Brasil 2012-2019",
    x = "Ano Calendário",
    y = "Taxa Específica de Participação no Mercado de Trabalho",
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

descritivas_taxa$t6_by_sex |>
  mutate(tem_dependente = as.factor(case_when(
    tem_crianca == 1 & tem_idoso_dependente == 0 ~ "Somente criança",
    tem_crianca == 0 & tem_idoso_dependente == 1 ~ "Somente idoso",
    tem_crianca == 1 & tem_idoso_dependente == 1 ~ "Criança e idoso",
    tem_crianca == 0 & tem_idoso_dependente == 0 ~ "Nenhum"))) |>
  filter(flag_participa == "forca_trabalho") |>
  filter(ano %in% c(2012,2015,2019)) |>
  filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = tem_dependente, y = prop, fill = cor_raca) +
  geom_col(position = position_dodge2(0.1)) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), position = position_dodge2(0.1)) +
  scale_fill_viridis_d(option = "D",begin = .2, end = .6) +
  coord_cartesian(ylim = c(0,60)) +
  # scale_x_continuous(breaks = seq(2012,2019,1)) +
  scale_y_continuous(breaks = seq(0,80,10)) +
  lemon::facet_rep_grid(sexo ~ as.factor(ano), repeat.tick.labels = TRUE) +
  labs(
    title = "Taxa Específica de Participação no Mercado de Trabalho por existência de dependente, segundo cor ou raça e sexo - Brasil 2012-2019",
    x = "Ano Calendário",
    y = "Taxa Específica de Participação no Mercado de Trabalho",
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

# 7 - TAE idoso por raca e aposentadoria ---------------------------------

# Total

descritivas_taxa$t8 |>
  filter(flag_participa == "forca_trabalho") |>
  filter(ano %in% c(2019)) |>
  filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = flag_aposentadoria, y = prop, fill = cor_raca) +
  geom_col(position = position_dodge2(0.1)) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), position = position_dodge2(0.1)) +
  scale_fill_viridis_d(option = "D",begin = .2, end = .6) +
  coord_cartesian(ylim = c(0,60)) +
  # scale_x_continuous(breaks = seq(2012,2019,1)) +
  scale_y_continuous(breaks = seq(0,80,10)) +
  # lemon::facet_rep_grid(. ~ as.factor(ano), repeat.tick.labels = TRUE) +
  labs(
    title = "(i)",
    # title = "Taxa Específica de Participação no Mercado de Trabalho por status de aposentadoria, segundo cor ou raça - Brasil 2012-2019",
    x = "Status de aposentado",
    y = "Taxa Específica de Participação no Mercado de Trabalho",
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

descritivas_taxa$t8_by_sex |>
  filter(flag_participa == "forca_trabalho") |>
  filter(ano %in% c(2012,2015,2019)) |>
  filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = flag_aposentadoria, y = prop, fill = cor_raca) +
  geom_col(position = position_dodge2(0.1)) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), position = position_dodge2(0.1)) +
  scale_fill_viridis_d(option = "D",begin = .2, end = .6) +
  coord_cartesian(ylim = c(0,80)) +
  # scale_x_continuous(breaks = seq(2012,2019,1)) +
  scale_y_continuous(breaks = seq(0,80,10)) +
  lemon::facet_rep_grid(sexo ~ as.factor(ano), repeat.tick.labels = TRUE) +
  labs(
    title = "Taxa Específica de Participação no Mercado de Trabalho por status de aposentadoria, segundo cor ou raça e sexo - Brasil 2012-2019",
    x = "Ano Calendário",
    y = "Taxa Específica de Participação no Mercado de Trabalho",
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
