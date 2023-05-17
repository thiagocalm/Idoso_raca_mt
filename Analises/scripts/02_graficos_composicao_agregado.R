options(scipen = 99999)

# Bibliotecas -------------------------------------------------------------

ifelse(!require(tidyverse),install.packages("tidyverse"),require(tidyverse))
ifelse(!require(srvyr),install.packages("srvyr"),require(srvyr))
ifelse(!require(survey),install.packages("survey"),require(survey))
ifelse(!require(patchwork),install.packages("patchwork"),require(patchwork))


# Importacao dos dados ----------------------------------------------------

load("./Analises/outputs/pt1/descritivas_composicao_agregado.RData")


# Graficos ----------------------------------------------------------------


graf_df <- t_agregada |>
  filter(flag_idoso != "Total") |>
  filter(ano %in% c(2012,2015,2019)) |>
  mutate(ano = as.factor(ano),
         prop_fct = case_when(
           prop <= 11 ~ "0-10",
           prop <= 21 ~ "11-20",
           prop <= 31 ~ "21-30",
           prop <= 41 ~ "31-40",
           prop <= 51 ~ "41-50",
           prop <= 61 ~ "51-60",
           prop <= 71 ~ "61-70",
           prop <= 81 ~ "71-80",
           prop <= 91 ~ "81-90",
           prop <= 100 ~ "91-100",
           TRUE ~ ano),
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

(
  (graf_df |>
     filter(flag_idoso == "Nao_idoso" & cor_raca == "Branco") |>
     ggplot() +
     aes(x = ano, y = variaveis, fill = prop_fct) +
     geom_tile(color = "white", linewidth = 1.5, linetype = 1, show.legend = F) +
     coord_fixed() +
     scale_fill_brewer(palette = "RdBu", direction = -1) +
     theme_minimal() +
     labs(
       title = "Não idoso",
       y = "Branco",
       fill = "Prop (%)"
     ) +
     theme(
       plot.title = element_text(size = 10, face = "bold", hjust = 0),
       axis.text.x = element_text(angle = 90, size = 8),
       axis.text.y = element_text(size = 9),
       axis.title.y = element_text(size = 10, face = "bold", hjust = 1),
       axis.title.x = element_blank(),
     )) +
    (graf_df |>
       filter(flag_idoso == "Idoso" & cor_raca == "Branco") |>
       ggplot() +
       aes(x = ano, y = variaveis, fill = prop_fct) +
       geom_tile(color = "white", linewidth = 1.5, linetype = 1, show.legend = F) +
       coord_fixed() +
       scale_fill_brewer(palette = "RdBu", direction = -1) +
       theme_minimal() +
       labs(
         title = "Idoso",
         y = "Branco",
         fill = "Prop (%)"
       ) +
       theme(
         plot.title = element_text(size = 10, face = "bold", hjust = 0),
         axis.text.x = element_text(angle = 90, size = 8),
         axis.text.y = element_blank(),
         # axis.title.y = element_text(size = 10, face = "bold", hjust = 1),
         axis.title = element_blank(),
       ))
)+
(
  (graf_df |>
     filter(flag_idoso == "Nao_idoso" & cor_raca == "Negro") |>
     ggplot() +
     aes(x = ano, y = variaveis, fill = prop_fct) +
     geom_tile(color = "white", linewidth = 1.5, linetype = 1, show.legend = F) +
     coord_fixed() +
     scale_fill_brewer(palette = "RdBu", direction = -1) +
     theme_minimal() +
     labs(
       title = "Não idoso",
       y = "Negro",
       fill = "Prop (%)"
     ) +
     theme(
       plot.title = element_text(size = 10, face = "bold", hjust = 0),
       axis.text.x = element_text(angle = 90, size = 8),
       axis.text.y = element_text(size = 9),
       axis.title.y = element_text(size = 10, face = "bold", hjust = 1),
       axis.title.x = element_blank(),
     )) +
    (graf_df |>
       filter(flag_idoso == "Idoso" & cor_raca == "Negro") |>
       ggplot() +
       aes(x = ano, y = variaveis, fill = prop_fct) +
       geom_tile(color = "white", linewidth = 1.5, linetype = 1) +
       coord_fixed() +
       scale_fill_brewer(palette = "RdBu", direction = -1) +
       theme_minimal() +
       labs(
         title = "Idoso",
         y = "Negro",
         fill = "Prop (%)"
       ) +
       theme(
         plot.title = element_text(size = 10, face = "bold", hjust = 0),
         axis.text.x = element_text(angle = 90, size = 8),
         axis.text.y = element_blank(),
         # axis.title.y = element_text(size = 10, face = "bold", hjust = 1),
         axis.title = element_blank(),
       ))
)


