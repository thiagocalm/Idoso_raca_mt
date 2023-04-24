options(scipen = 99999)


# Bibliotecas -------------------------------------------------------------

ifelse(!require(tidyverse),install.packages("tidyverse"),require(tidyverse))
ifelse(!require(srvyr),install.packages("srvyr"),require(srvyr))
ifelse(!require(survey),install.packages("survey"),require(survey))

# Import data -------------------------------------------------------------

pnad <- readRDS("./Analises/dados/pnadc_analise.rds")

# 1. Taxa Bruta de Atividade Economica ------------------------------------

tbae <- pnad |>  
  filter(idade_simples >= 10) |> 
  select(ano, flag_idoso, flag_participa, cor_raca, id_pes, peso) |> 
  srvyr::as_survey_design(ids = id_pes,
                          weights = peso) |> 
  group_by(ano, flag_idoso, cor_raca, flag_participa) |> 
  summarise(n = srvyr::survey_total()) |>
  select(-n_se) |> 
  pivot_wider(names_from = "flag_participa", values_from = "n") |> 
  mutate(pia = forca_trabalho + nao_forca_trabalho,
         TBAE = round((forca_trabalho/pia)*100,3))

# tabela

tbae |> 
  select(ano, flag_idoso, cor_raca, taxa = TBAE) |> 
  pivot_wider(names_from = "ano", values_from = "taxa") |> 
  mutate(var_2019_2012 = (`2019`-`2012`)/(`2012`)*100) |> 
  View("TBAE")

# grafico

tbae |> 
  select(ano, flag_idoso, cor_raca, taxa = TBAE) |> 
  ggplot() +
  aes(x = ano, y = taxa, group = cor_raca, linetype = cor_raca,
      interaction(cor_raca,cor_raca)) +
  geom_point() +
  geom_line() +
  lemon::facet_rep_grid(flag_idoso ~.,repeat.tick.labels = TRUE) +
  theme_minimal()

tbae |> 
  filter(cor_raca != "Outros") |> 
  select(ano,flag_idoso, cor_raca, taxa = TBAE) |> 
  ggplot() +
  aes(x = ano, y = taxa, group = cor_raca, linetype = cor_raca,
      interaction(cor_raca,cor_raca)) +
  geom_point() +
  geom_line() +
  lemon::facet_rep_grid(flag_idoso ~.,repeat.tick.labels = TRUE) +
  theme_minimal()

tbae |> 
  filter(cor_raca != "Outros") |> 
  select(ano,flag_idoso, cor_raca, taxa = TBAE) |> 
  mutate(cor_var = cor_raca,
         flag_var = flag_idoso) |> 
  pivot_wider(names_from = c("flag_idoso","cor_raca"), values_from = "taxa") |> 
  pivot_longer(cols = "Idoso_Branco":"Nao_idoso_Negro", names_to = "grupo_cor", 
               values_to = "taxa") |>
  filter(!is.na(taxa)) |>  
  ggplot() +
  aes(x = ano, y = taxa, group = grupo_cor, linetype = cor_var, color = flag_var,
      interaction(grupo_cor,grupo_cor)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d(option = "D",begin = .2, end = .5) +
  theme_minimal()
