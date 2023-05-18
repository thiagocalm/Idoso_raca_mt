options(scipen = 99999)

# Bibliotecas -------------------------------------------------------------

ifelse(!require(tidyverse),install.packages("tidyverse"),require(tidyverse))
ifelse(!require(srvyr),install.packages("srvyr"),require(srvyr))
ifelse(!require(survey),install.packages("survey"),require(survey))
ifelse(!require(patchwork),install.packages("patchwork"),require(patchwork))


# Importacao dos dados ----------------------------------------------------

load("./Analises/outputs/pt2/descritiva_composicao_rm.RData")

load("./Analises/outputs/pt2/descritiva_composicao_regiao.RData")

# Grafico por RM -------------------------------------------------

graf_rm <- t_rm_agregada |>
  mutate(prop_fct = case_when(
           prop <= 11 ~ "0-10",
           prop <= 21 ~ "11-20",
           prop <= 31 ~ "21-30",
           prop <= 41 ~ "31-40",
           prop <= 51 ~ "41-50",
           prop <= 61 ~ "51-60",
           prop <= 71 ~ "61-70",
           prop <= 81 ~ "71-80",
           prop <= 91 ~ "81-90",
           prop <= 100 ~ "91-100"),
         prop_fct = factor(
           prop_fct,
           level = c("0-10","11-20","21-30","31-40", "41-50", "51-60", "61-70",
                     "71-80", "81-90", "91-100"),
           labels = c("0-10","11-20","21-30","31-40", "41-50", "51-60", "61-70",
                      "71-80", "81-90", "91-100"))) |>
  mutate(
    variaveis = case_when(
      variaveis == "Sem instrução" ~ "Escolaridade - Nenhum",
      variaveis == "Ensino Fundamental" ~ "Escolaridade - Fund.",
      variaveis == "Ensino Médio" ~ "Escolaridade - Médio",
      variaveis == "Ensino Superior" ~ "Escolaridade - Superior",
      variaveis == "Somente idoso" ~ "Tem dependente - Idoso",
      variaveis == "Somente criança" ~ "Tem dependente - Criança",
      variaveis == "Total" ~ "Pop. total",
      variaveis == "Feminino" ~ "Sexo - Feminino",
      variaveis == "Masculino" ~ "Sexo - Masculino",
      variaveis == "Unipessoal" ~ "Tipo dom. - Unipessoal",
      variaveis == "Nuclear" ~ "Tipo dom. - Nuclear",
      variaveis == "Estendida" ~ "Tipo dom. - Estendido",
      TRUE ~ variaveis)) |>
  filter(variaveis != "Não aposentado") |>
  filter(variaveis !=  "Nenhum") |>
  filter(variaveis != "Estendida") |>
  filter(variaveis != "Criança e idoso") |>
  filter(variaveis != "Composta")

# grafico

graf_rm |>
  mutate(
    variaveis = factor(
      variaveis,
      levels = c("Pop. total","Sexo - Feminino","Sexo - Masculino",
                 "Escolaridade - Nenhum", "Escolaridade - Fund.",
                 "Escolaridade - Médio", "Escolaridade - Superior",
                 "P20", "P40", "P60", "P80","P100", "Aposentado",
                 "Tem dependente - Criança", "Tem dependente - Idoso",
                 "Tipo dom. - Unipessoal", "Tipo dom. - Nuclear",
                 "Tipo dom. - Estendido"),
      labels = c("Pop. total","Sexo - Feminino","Sexo - Masculino",
                 "Escolaridade - Nenhum", "Escolaridade - Fund.",
                 "Escolaridade - Médio", "Escolaridade - Superior",
                 "Dist. renda - P20", "Dist. renda - P40", "Dist. renda - P60",
                 "Dist. renda - P80","Dist. renda - P100", "Aposentado",
                 "Tem dependente - Criança", "Tem dependente - Idoso",
                 "Tipo dom. - Unipessoal", "Tipo dom. - Nuclear",
                 "Tipo dom. - Estendido"))) |>
  mutate(flag_participa = case_when(
    flag_participa == "forca_trabalho" ~ "Na FT",
    TRUE ~ "Fora da FT"
  )) |>
  filter(variaveis != "Pop. total") |>
  ggplot() +
  aes(x = status_rm, y = fct_rev(variaveis), fill = prop_fct) +
  geom_tile(color = "white", linewidth = 1.5, linetype = 1) +
  coord_fixed() +
  scale_fill_brewer(palette = "RdBu", direction = -1) +
  theme_minimal() +
  labs(
    caption = "Obs.: Para cada cor ou raça, status e condição na força de trabalho, soma-se 100% em cada categoria \n (sexo, escolaridade, etc.).",
    fill = "Prop (%)",
    y = "Características",
    x = "Ano calendário"
  ) +
  facet_wrap(~cor_raca + flag_participa, nrow = 1, ncol = 4) +
  theme(
    plot.caption = element_text(size = 8, face = "italic", hjust = 1),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    axis.text.x = element_text(angle = 90, size = 8),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 10, face = "bold", hjust = 1),
    strip.text = element_text(size = 10, face = "bold")
  )

# Grafico por regiao -------------------------------------------------

graf_regiao <- t_regiao_agregada |>
  mutate(prop_fct = case_when(
    prop <= 11 ~ "0-10",
    prop <= 21 ~ "11-20",
    prop <= 31 ~ "21-30",
    prop <= 41 ~ "31-40",
    prop <= 51 ~ "41-50",
    prop <= 61 ~ "51-60",
    prop <= 71 ~ "61-70",
    prop <= 81 ~ "71-80",
    prop <= 91 ~ "81-90",
    prop <= 100 ~ "91-100"),
    prop_fct = factor(
      prop_fct,
      level = c("0-10","11-20","21-30","31-40", "41-50", "51-60", "61-70",
                "71-80", "81-90", "91-100"),
      labels = c("0-10","11-20","21-30","31-40", "41-50", "51-60", "61-70",
                 "71-80", "81-90", "91-100"))) |>
  mutate(
    variaveis = case_when(
      variaveis == "Sem instrução" ~ "Escolaridade - Nenhum",
      variaveis == "Ensino Fundamental" ~ "Escolaridade - Fund.",
      variaveis == "Ensino Médio" ~ "Escolaridade - Médio",
      variaveis == "Ensino Superior" ~ "Escolaridade - Superior",
      variaveis == "Somente idoso" ~ "Tem dependente - Idoso",
      variaveis == "Somente criança" ~ "Tem dependente - Criança",
      variaveis == "Total" ~ "Pop. total",
      variaveis == "Feminino" ~ "Sexo - Feminino",
      variaveis == "Masculino" ~ "Sexo - Masculino",
      variaveis == "Unipessoal" ~ "Tipo dom. - Unipessoal",
      variaveis == "Nuclear" ~ "Tipo dom. - Nuclear",
      variaveis == "Estendida" ~ "Tipo dom. - Estendido",
      TRUE ~ variaveis)) |>
  filter(variaveis != "Não aposentado") |>
  filter(variaveis !=  "Nenhum") |>
  filter(variaveis != "Estendida") |>
  filter(variaveis != "Criança e idoso") |>
  filter(variaveis != "Composta")

# grafico

graf_regiao |>
  mutate(
    variaveis = factor(
      variaveis,
      levels = c("Pop. total","Sexo - Feminino","Sexo - Masculino",
                 "Escolaridade - Nenhum", "Escolaridade - Fund.",
                 "Escolaridade - Médio", "Escolaridade - Superior",
                 "P20", "P40", "P60", "P80","P100", "Aposentado",
                 "Tem dependente - Criança", "Tem dependente - Idoso",
                 "Tipo dom. - Unipessoal", "Tipo dom. - Nuclear",
                 "Tipo dom. - Estendido"),
      labels = c("Pop. total","Sexo - Feminino","Sexo - Masculino",
                 "Escolaridade - Nenhum", "Escolaridade - Fund.",
                 "Escolaridade - Médio", "Escolaridade - Superior",
                 "Dist. renda - P20", "Dist. renda - P40", "Dist. renda - P60",
                 "Dist. renda - P80","Dist. renda - P100", "Aposentado",
                 "Tem dependente - Criança", "Tem dependente - Idoso",
                 "Tipo dom. - Unipessoal", "Tipo dom. - Nuclear",
                 "Tipo dom. - Estendido"))) |>
  mutate(flag_participa = case_when(
    flag_participa == "forca_trabalho" ~ "Na FT",
    TRUE ~ "Fora da FT"
  )) |>
  filter(variaveis != "Pop. total") |>
  ggplot() +
  aes(x = regiao, y = fct_rev(variaveis), fill = prop_fct) +
  geom_tile(color = "white", linewidth = 1.5, linetype = 1) +
  coord_fixed() +
  scale_fill_brewer(palette = "RdBu", direction = -1) +
  theme_minimal() +
  labs(
    caption = "Obs.: Para cada cor ou raça, status e condição na força de trabalho, soma-se 100% em cada categoria \n (sexo, escolaridade, etc.).",
    fill = "Prop (%)",
    y = "Características",
    x = "Ano calendário"
  ) +
  facet_wrap(~cor_raca + flag_participa, nrow = 1, ncol = 4) +
  theme(
    plot.caption = element_text(size = 8, face = "italic", hjust = 1),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    axis.text.x = element_text(angle = 90, size = 8),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 10, face = "bold", hjust = 1),
    strip.text = element_text(size = 10, face = "bold")
  )
