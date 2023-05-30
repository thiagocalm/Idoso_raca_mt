options(scipen = 99999)

# Bibliotecas -------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, srvyr, survey, hutils, writexl, ggalt, brazilmaps, sf, ggspatial, ggsn)

# Import data -------------------------------------------------------------

# Composicao
load("./Analises/outputs/pt2/descritiva_composicao_uf.RData")

# Taxa
load("./Analises/outputs/pt2/descritiva_taxa_uf.RData")

# Spatial analysis --------------------------------------------------------

# Codigos das UFs

cods_uf <- tibble(cod = c("11","12","13","14","15","16","17","21","22","23","24","25","26","27",
                          "28","29","31","32","33","35","41","42","43","50","51","52","53","BR"),
                  names = c("Rondônia","Acre","Amazonas", "Roraima","Pará","Amapá","Tocantins","Maranhão",
                            "Piauí", "Ceará","Rio Grande do Norte","Paraíba","Pernambuco","Alagoas","Sergipe",
                            "Bahia","Minas Gerais","Espírito Santo","Rio de Janeiro","São Paulo","Paraná","Santa Catarina",
                            "Rio Grande do Sul","Mato Grosso do Sul","Mato Grosso","Goiás","Distrito Federal","Brasil"))


cod <- cods_uf |> select(cod) |> as.vector()
cod <- rep(cod$cod, 4)


# Composição --------------------------------------------------------------

comp_spatial <- composicao_uf$total

# Import spatial data

uf_map <- get_brmap(geo = "State",
                    class = "sf")

# Joining data

comp_estimates_spatial <- uf_map |>
  bind_rows(uf_map) |>
  bind_rows(uf_map) |>
  bind_rows(uf_map) |>
  arrange(State) |>
  bind_cols(
    comp_spatial |>
      mutate(UF = as.numeric(UF)) |>
      arrange(UF)
  ) |>
  select(cond_residencia, REGIAO = Region, UF, cor_raca, prop, geometry)

comp_estimates_spatial |>
  ggplot() +
  geom_sf(aes(fill = prop),
          colour = "black", size = 0.1) +
  geom_sf(data = get_brmap("State"),
          fill = "transparent",
          colour = "black", size = 0.6) +
  scale_fill_viridis_c(option = 2, direction = -1, limits = c(0,50)) +
  lemon::facet_rep_grid(cor_raca~cond_residencia, repeat.tick.labels = TRUE) +
  guides(fill = guide_colourbar(title = "População (%)")) +
  labs(
    # title = "Mapinha de composição",
    # caption = "Fonte: IBGE. Pesquisa Nacional por Amostra de Domicílios Contínua, 2019."
  ) +
  ## Inserindo Escala e Norte
  # Alternativa 1 - via `ggspatial`
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  # tira sistema cartesiano
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.caption = element_text(size = 8),
    legend.title = element_text(face = "bold", size = 13, hjust = 0, vjust = .5),
    legend.text = element_text(size = 10, hjust = 0, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 15, hjust = .5, vjust = .5),
    axis.text = element_text(face = "bold", size = 8, color = "#636363", hjust = .5, vjust = .5),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 13, color = "#636363", hjust = .9, vjust = .5),
    panel.grid = element_line(color = "#f0f0f0",linewidth = .01),
    # panel.grid = element_line(colour = "grey"),
    panel.background = element_blank())

# Taxas --------------------------------------------------------------

taxa_spatial <- taxa_uf$total |>
  ungroup() |>
  add_row(cor_raca = "Branco",cond_residencia = "Rural",UF = 16,n = 0,n_se = 0,
          taxa = 0, taxa_se = 0)

# Import spatial data

uf_map <- get_brmap(geo = "State",
                    class = "sf")

# Joining data

taxa_estimates_spatial <- uf_map |>
  bind_rows(uf_map) |>
  bind_rows(uf_map) |>
  bind_rows(uf_map) |>
  arrange(State) |>
  bind_cols(
    taxa_spatial |>
      mutate(UF = as.numeric(UF)) |>
      arrange(UF)
  ) |>
  select(cond_residencia, REGIAO = Region, UF, cor_raca, taxa, geometry)

taxa_estimates_spatial |>
  ggplot() +
  geom_sf(aes(fill = taxa),
          colour = "black", size = 0.1) +
  geom_sf(data = get_brmap("State"),
          fill = "transparent",
          colour = "black", size = 0.6) +
  scale_fill_viridis_c(option = 2, direction = -1, limits = c(0,50)) +
  lemon::facet_rep_grid(cor_raca~cond_residencia, repeat.tick.labels = TRUE) +
  guides(fill = guide_colourbar(title = "Taxa de participação (%)")) +
  labs(
    # title = "Mapinha de composição",
    # caption = "Fonte: IBGE. Pesquisa Nacional por Amostra de Domicílios Contínua, 2019."
  ) +
  ## Inserindo Escala e Norte
  # Alternativa 1 - via `ggspatial`
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  # tira sistema cartesiano
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.caption = element_text(size = 8),
    legend.title = element_text(face = "bold", size = 13, hjust = 0, vjust = .5),
    legend.text = element_text(size = 10, hjust = 0, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 15, hjust = .5, vjust = .5),
    axis.text = element_text(face = "bold", size = 8, color = "#636363", hjust = .5, vjust = .5),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 13, color = "#636363", hjust = .9, vjust = .5),
    panel.grid = element_line(color = "#f0f0f0",linewidth = .01),
    # panel.grid = element_line(colour = "grey"),
    panel.background = element_blank())
