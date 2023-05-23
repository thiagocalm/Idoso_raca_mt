options(scipen = 99999)

# Bibliotecas -------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, srvyr, survey, hutils, writexl)

# Import data -------------------------------------------------------------

# Composicao
load("./Analises/outputs/pt2/descritiva_composicao_rm_dem.RData")

load("./Analises/outputs/pt2/descritiva_composicao_regiao_dem.RData")

# Taxa
load("./Analises/outputs/pt2/descritiva_taxa_rm_dem.RData")

load("./Analises/outputs/pt2/descritiva_taxa_regiao_dem.RData")


# Graficos composicao -----------------------------------------------------

## Status Metropolitano

# Total
composicao_rm$total |>
  filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = status_rm, y = prop, fill = cor_raca) +
  geom_col(position = position_dodge2(0.1)) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), position = position_dodge2(0.1)) +
  scale_fill_viridis_d(option = "D",begin = .2, end = .6) +
  coord_cartesian(ylim = c(0,max(composicao_rm$total$prop)+15)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  labs(
    title = "Distribuição relativa da população idosa por condição de residência metropolitana, segundo cor ou raça - Brasil 2019",
    x = "Condição de residência metropolitana",
    y = "População (%)",
    caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Contínua, 2019."
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

composicao_rm$sexo |>
  filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = sexo, y = prop, fill = cor_raca) +
  geom_col(position = position_dodge2(0.1)) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), position = position_dodge2(0.1)) +
  scale_fill_viridis_d(option = "D",begin = .2, end = .6) +
  coord_cartesian(ylim = c(0,max(composicao_rm$sexo$prop)+15)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  lemon::facet_rep_wrap(.~ status_rm, repeat.tick.labels = T) +
  labs(
    title = "Distribuição relativa da população idosa por condição de residência metropolitana, segundo cor ou raça e sexo - Brasil 2019",
    x = "sexo",
    y = "População (%)",
    caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Contínua, 2019."
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

# Idade

composicao_rm$idade |>
  # filter(grupo_etario <= 90) |>
  filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = grupo_etario, y = prop, color = cor_raca, interaction(grupo_etario, grupo_etario)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.05) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), width=.1,
                linetype = "solid") +
  scale_color_viridis_d(option = "D",begin = .1, end = .6) +
  coord_cartesian(ylim = c(0,max(composicao_rm$idade$prop)+10)) +
  scale_x_continuous(breaks = seq(60,110,5)) +
  scale_y_continuous(breaks = seq(0,max(composicao_rm$idade$prop)+10,10)) +
  lemon::facet_rep_grid(. ~ status_rm, repeat.tick.labels = TRUE) +
  labs(
    title = "Distribuição relativa da população idosa por condição de residência metropolitana, segundo cor ou raça e idade - Brasil 2019",
    x = "Grupo etário quinquenal",
    y = "População (%)",
    caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Contínua, 2019."
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

## Região geográfica

# Total
composicao_regiao$total |>
  filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = regiao, y = prop, fill = cor_raca) +
  geom_col(position = position_dodge2(0.1)) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), position = position_dodge2(0.1)) +
  scale_fill_viridis_d(option = "D",begin = .2, end = .6) +
  coord_cartesian(ylim = c(0,max(composicao_rm$total$prop)+15)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  labs(
    title = "Distribuição relativa da população idosa por região geográfica, segundo cor ou raça - Brasil 2019",
    x = "Região geográfica",
    y = "População (%)",
    caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Contínua, 2019."
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

composicao_regiao$sexo |>
  filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = sexo, y = prop, fill = cor_raca) +
  geom_col(position = position_dodge2(0.1)) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), position = position_dodge2(0.1)) +
  scale_fill_viridis_d(option = "D",begin = .2, end = .6) +
  coord_cartesian(ylim = c(0,max(composicao_regiao$sexo$prop)+15)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  lemon::facet_rep_wrap(.~ regiao, repeat.tick.labels = T) +
  labs(
    title = "Distribuição relativa da população idosa por região geográfica, segundo cor ou raça e sexo - Brasil 2019",
    x = "sexo",
    y = "População (%)",
    caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Contínua, 2019."
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

# Idade

composicao_regiao$idade |>
  filter(grupo_etario <= 90) |>
  filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = grupo_etario, y = prop, color = cor_raca, interaction(grupo_etario, grupo_etario)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.05) +
  geom_errorbar(aes(ymin=prop-prop_se*1.96, ymax=prop+prop_se*1.96), width=.1,
                linetype = "solid") +
  scale_color_viridis_d(option = "D",begin = .1, end = .6) +
  coord_cartesian(ylim = c(0,max(composicao_regiao$idade$prop)+10)) +
  scale_x_continuous(breaks = seq(60,110,5)) +
  scale_y_continuous(breaks = seq(0,max(composicao_regiao$idade$prop)+10,10)) +
  lemon::facet_rep_grid(. ~ regiao, repeat.tick.labels = TRUE) +
  labs(
    title = "Distribuição relativa da população idosa por região geográfica, segundo cor ou raça e idade - Brasil 2019",
    x = "Grupo etário quinquenal",
    y = "População (%)",
    caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Contínua, 2019."
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


# Graficos taxa -----------------------------------------------------

## Status Metropolitano

# Total
taxa_rm$total |>
  filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = status_rm, y = taxa, fill = cor_raca) +
  geom_col(position = position_dodge2(0.1)) +
  geom_errorbar(aes(ymin=taxa-taxa_se*1.96, ymax=taxa+taxa_se*1.96), position = position_dodge2(0.1)) +
  scale_fill_viridis_d(option = "D",begin = .2, end = .6) +
  coord_cartesian(ylim = c(0,max(taxa_rm$total$taxa)+15)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  labs(
    title = "Taxa de participação no mercado de trabalho por condição de residência metropolitana, segundo cor ou raça - Brasil 2019",
    x = "Condição de residência metropolitana",
    y = "Taxa de participação (%)",
    caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Contínua, 2019."
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

taxa_rm$sexo |>
  filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = status_rm, y = taxa, fill = cor_raca) +
  geom_col(position = position_dodge2(0.1)) +
  geom_errorbar(aes(ymin=taxa-taxa_se*1.96, ymax=taxa+taxa_se*1.96), position = position_dodge2(0.1)) +
  scale_fill_viridis_d(option = "D",begin = .2, end = .6) +
  coord_cartesian(ylim = c(0,max(taxa_rm$total$taxa)+15)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  lemon::facet_rep_wrap(.~ sexo, repeat.tick.labels = T) +
  labs(
    title = "Taxa de participação no mercado de trabalho da população idosa por condição de residência metropolitana, segundo cor ou raça e sexo - Brasil 2019",
    x = "Condição de residência",
    y = "Taxa de participação (%)",
    caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Contínua, 2019."
  ) +
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

# Idade

taxa_rm$idade |>
  filter(grupo_etario <= 90) |>
  filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = grupo_etario, y = taxa, color = cor_raca, interaction(grupo_etario, grupo_etario)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.05) +
  geom_errorbar(aes(ymin=taxa-taxa_se*1.96, ymax=taxa+taxa_se*1.96), width=.1,
                linetype = "solid") +
  scale_color_viridis_d(option = "D",begin = .1, end = .6) +
  coord_cartesian(ylim = c(0,max(taxa_rm$idade$taxa)+10)) +
  scale_x_continuous(breaks = seq(60,110,5)) +
  scale_y_continuous(breaks = seq(0,max(taxa_rm$idade$taxa)+10,10)) +
  lemon::facet_rep_grid(. ~ status_rm, repeat.tick.labels = TRUE) +
  labs(
    title = "Taxa de participação da população idosa por condição de residência metropolitana, segundo cor ou raça e idade - Brasil 2019",
    x = "Grupo etário quinquenal",
    y = "Taxa de participação (%)",
    caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Contínua, 2019."
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

## Região geográfica

# Total
taxa_regiao$total |>
  filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = regiao, y = taxa, fill = cor_raca) +
  geom_col(position = position_dodge2(0.1)) +
  geom_errorbar(aes(ymin=taxa-taxa_se*1.96, ymax=taxa+taxa_se*1.96), position = position_dodge2(0.1)) +
  scale_fill_viridis_d(option = "D",begin = .2, end = .6) +
  coord_cartesian(ylim = c(0,max(taxa_rm$total$taxa)+15)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  labs(
    title = "Taxa de participação da população idosa por região geográfica, segundo cor ou raça - Brasil 2019",
    x = "Região geográfica",
    y = "Taxa de participação (%)",
    caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Contínua, 2019."
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

taxa_regiao$sexo |>
  filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = regiao, y = taxa, fill = cor_raca) +
  geom_col(position = position_dodge2(0.1)) +
  geom_errorbar(aes(ymin=taxa-taxa_se*1.96, ymax=taxa+taxa_se*1.96), position = position_dodge2(0.1)) +
  scale_fill_viridis_d(option = "D",begin = .2, end = .6) +
  coord_cartesian(ylim = c(0,max(taxa_regiao$sexo$taxa)+15)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  lemon::facet_rep_wrap(.~ sexo, repeat.tick.labels = T) +
  labs(
    title = "Taxa de participação da população idosa por região geográfica, segundo cor ou raça e sexo - Brasil 2019",
    x = "Região geográfica",
    y = "Taxa de participação (%)",
    caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Contínua, 2019."
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

# Idade

taxa_regiao$idade |>
  filter(grupo_etario <= 90) |>
  filter(cor_raca != "Total") |>
  ggplot() +
  aes(x = grupo_etario, y = taxa, color = cor_raca, interaction(grupo_etario, grupo_etario)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.05) +
  geom_errorbar(aes(ymin=taxa-taxa_se*1.96, ymax=taxa+taxa_se*1.96), width=.1,
                linetype = "solid") +
  scale_color_viridis_d(option = "D",begin = .1, end = .6) +
  coord_cartesian(ylim = c(0,max(taxa_regiao$idade$taxa)+10)) +
  scale_x_continuous(breaks = seq(60,110,5)) +
  scale_y_continuous(breaks = seq(0,max(taxa_regiao$idade$taxa)+10,10)) +
  lemon::facet_rep_grid(. ~ regiao, repeat.tick.labels = TRUE) +
  labs(
    title = "Taxa de participação da população idosa por região geográfica, segundo cor ou raça e idade - Brasil 2019",
    x = "Grupo etário quinquenal",
    y = "Taxa de participação (%)",
    caption = "Fonte: IBGE, Pesquisa Nacional Por Amostra de Domicílios Contínua, 2019."
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
