# ------------------------------------------------------------
# Pacotes
# ------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)   # opcional (não estritamente necessário, mas útil)

# ------------------------------------------------------------
# 0) Ler dados
# ------------------------------------------------------------
dim_teachers                 <- readr::read_csv("dim_teachers.csv", show_col_types = FALSE)
teachers_contents_interactions <- readr::read_csv("fct_teachers_contents_interactions.csv", show_col_types = FALSE)
teachers_entries             <- readr::read_csv("fct_teachers_entries.csv", show_col_types = FALSE)

formation        <- readr::read_csv("stg_formation.csv", show_col_types = FALSE)
mari_ia_conv     <- readr::read_csv("stg_mari_ia_conversation.csv", show_col_types = FALSE)
mari_ia_reports  <- readr::read_csv("stg_mari_ia_reports.csv", show_col_types = FALSE)

# ------------------------------------------------------------
# 1) Preparar timestamps e variáveis
# ------------------------------------------------------------
# tenta converter para datetime; se já estiver em datetime, não há problema
teachers_entries <- teachers_entries %>%
  mutate(
    data_inicio = parse_datetime(as.character(data_inicio), guess_formats = TRUE),
    data_fim    = parse_datetime(as.character(data_fim),    guess_formats = TRUE)
  )

# tempo_sessao em horas
teachers_entries <- teachers_entries %>%
  mutate(
    tempo_sessao = as.numeric(difftime(data_fim, data_inicio, units = "hours")),
    ano          = year(data_inicio)
  )

# ------------------------------------------------------------
# 2) Agregado por unique_id x ano
# ------------------------------------------------------------
teachers_entries_agg_ano <- teachers_entries %>%
  group_by(unique_id, ano) %>%
  summarise(
    sessao_medio = mean(tempo_sessao, na.rm = TRUE),
    sessao_total = sum(tempo_sessao, na.rm = TRUE),
    frequencia   = dplyr::n()
    # opcional: n_tipos = n_distinct(user_type)
  ) %>%
  ungroup() %>%
  arrange(unique_id, ano)

cat(format(nrow(teachers_entries_agg_ano), big.mark = ".", decimal.mark = ","), "linhas no df agregado.\n")

# ------------------------------------------------------------
# 3) Plot 2×2: scatter (X log), por ano (até 4 anos)
# ------------------------------------------------------------
xcol <- "sessao_total"; ycol <- "frequencia"

df_12 <- teachers_entries_agg_ano %>%
  select(all_of(c(xcol, ycol, "ano"))) %>%
  filter(!is.na(.data[[xcol]]), !is.na(.data[[ycol]]), !is.na(ano)) %>%
  mutate(
    !!xcol := as.numeric(.data[[xcol]]),
    !!ycol := as.numeric(.data[[ycol]])
  ) %>%
  filter(.data[[xcol]] > 0, .data[[xcol]] <= 200, .data[[ycol]] >= 0, .data[[ycol]] <= 400)

anos <- sort(unique(df_12$ano))
anos_plot <- if (length(anos) > 4) tail(anos, 4) else anos

# paleta fixa (seguindo sua ordem)
palette4 <- c("#ec4899", "#f59d0a", "#8b5cf5", "#3b2087")
pal_usada <- setNames(palette4[seq_along(anos_plot)], as.character(anos_plot))

df_12p <- df_12 %>%
  filter(ano %in% anos_plot) %>%
  mutate(
    ano = factor(ano, levels = anos_plot)
  )

p1 <- ggplot(df_12p, aes(x = .data[[xcol]], y = .data[[ycol]], color = ano)) +
  geom_point(size = 1.6, alpha = 0.7) +
  scale_color_manual(values = pal_usada, guide = "none") +
  scale_x_log10(
    limits = c(1, 200),
    breaks = 10^(0:2),
    minor_breaks = as.numeric(outer(10^(0:2), c(2, 5), "*")),
    labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
  ) +
  coord_cartesian(ylim = c(0, 400)) +
  facet_wrap(~ ano, ncol = 2) +
  labs(
    title = "Horas na plataforma vs Frequência de uso - por indivíduo",
    x = "Duração do uso em horas (X em log)",
    y = "Frequência"
  ) +
  theme_bw() +
  theme(panel.grid.minor = element_line(linetype = "dashed", linewidth = 0.2, colour = "grey80"))

print(p1)

# ------------------------------------------------------------
# 4) Quadrantes + tabelas (contagem/percentual) + plot 2×2 com cortes
# ------------------------------------------------------------
x_max <- 100; y_max <- 400
x_thr <- 6;   y_thr <- 100

df_q <- teachers_entries_agg_ano %>%
  select(ano, all_of(c(xcol, ycol))) %>%
  filter(!is.na(ano)) %>%
  mutate(
    !!xcol := as.numeric(.data[[xcol]]),
    !!ycol := as.numeric(.data[[ycol]])
  ) %>%
  filter(.data[[xcol]] > 0, .data[[xcol]] <= x_max, .data[[ycol]] >= 0, .data[[ycol]] <= y_max) %>%
  mutate(
    quadrante = case_when(
      .data[[xcol]] <= x_thr & .data[[ycol]] <= y_thr ~ "Q1 (≤6, ≤100)",
      .data[[xcol]] >  x_thr & .data[[ycol]] <= y_thr ~ "Q2 (>6, ≤100)",
      .data[[xcol]] <= x_thr & .data[[ycol]] >  y_thr ~ "Q3 (≤6, >100)",
      .data[[xcol]] >  x_thr & .data[[ycol]] >  y_thr ~ "Q4 (>6, >100)",
      TRUE ~ "Outros"
    )
  )

ord_quads <- c("Q1 (≤6, ≤100)", "Q2 (>6, ≤100)", "Q3 (≤6, >100)", "Q4 (>6, >100)")

counts_by_year <- df_q %>%
  count(ano, quadrante) %>%
  mutate(quadrante = factor(quadrante, levels = ord_quads)) %>%
  tidyr::pivot_wider(names_from = quadrante, values_from = n, values_fill = 0) %>%
  arrange(ano)

perc_by_year <- df_q %>%
  count(ano, quadrante) %>%
  group_by(ano) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  mutate(quadrante = factor(quadrante, levels = ord_quads)) %>%
  select(-n) %>%
  tidyr::pivot_wider(names_from = quadrante, values_from = pct, values_fill = 0) %>%
  arrange(ano)

counts_total <- df_q %>%
  count(quadrante) %>%
  filter(quadrante %in% ord_quads) %>%
  mutate(quadrante = factor(quadrante, levels = ord_quads)) %>%
  arrange(quadrante)

perc_total <- counts_total %>%
  mutate(pct = n / sum(n)) %>%
  select(quadrante, pct)

cat("Contagem por ano:\n"); print(counts_by_year)
cat("\nPercentual por ano:\n"); print(perc_by_year)
cat("\nContagem total:\n"); print(counts_total)
cat("\nPercentual total:\n"); print(perc_total)

anos2 <- sort(unique(df_q$ano))
anos2_plot <- if (length(anos2) > 4) tail(anos2, 4) else anos2
pal2 <- setNames(palette4[seq_along(anos2_plot)], as.character(anos2_plot))

df_qp <- df_q %>%
  filter(ano %in% anos2_plot) %>%
  mutate(ano = factor(ano, levels = anos2_plot))

p2 <- ggplot(df_qp, aes(x = .data[[xcol]], y = .data[[ycol]], color = ano)) +
  geom_point(size = 1.6, alpha = 0.7) +
  scale_color_manual(values = pal2, guide = "none") +
  scale_x_log10(
    limits = c(1, x_max),
    breaks = 10^(0:2),
    minor_breaks = as.numeric(outer(10^(0:2), c(2, 5), "*")),
    labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
  ) +
  coord_cartesian(ylim = c(0, y_max)) +
  geom_vline(xintercept = x_thr, color = "black", linewidth = 0.4) +
  geom_hline(yintercept = y_thr, color = "black", linewidth = 0.4) +
  facet_wrap(~ ano, ncol = 2) +
  labs(
    title = "Horas na plataforma vs Frequência de uso - por indivíduo",
    subtitle = paste0("X em log; corte X=", x_thr, " e Y=", y_thr),
    x = "Duração do uso em horas (X em log; corte em 100)",
    y = paste0("Frequência (corte em ", y_thr, ")")
  ) +
  theme_bw()

print(p2)

# ------------------------------------------------------------
# 5) Grid 3×2: primeiros acessos vs retornos (cores por coorte)
# ------------------------------------------------------------
x_max <- 100; y_max <- 400
x_thr <- 6;   y_thr <- 100
anos_plot_vec <- c(2023, 2024, 2025)  # defina conforme necessário

color_by_year <- c(`2023`="#f59d0a", `2024`="#8b5cf5", `2025`="#3b2087")
color_other   <- "#ec4899"

fmt_milhar <- function(v) format(v, big.mark = ".", decimal.mark = ",", scientific = FALSE)

# 1) primeiro ano por unique_id
first_year_map <- teachers_entries_agg_ano %>%
  filter(!is.na(unique_id), !is.na(ano)) %>%
  group_by(unique_id) %>%
  summarise(first_year_for_id = min(ano), .groups = "drop")

# 2) preparar DF + flags
df3 <- teachers_entries_agg_ano %>%
  select(unique_id, ano, all_of(c(xcol, ycol))) %>%
  filter(!is.na(unique_id), !is.na(ano), !is.na(.data[[xcol]]), !is.na(.data[[ycol]])) %>%
  mutate(
    !!xcol := as.numeric(.data[[xcol]]),
    !!ycol := as.numeric(.data[[ycol]])
  ) %>%
  inner_join(first_year_map, by = "unique_id") %>%
  mutate(
    is_first_year = (ano == first_year_for_id)
  ) %>%
  filter(ano %in% anos_plot_vec,
         .data[[xcol]] > 0, .data[[xcol]] <= x_max,
         .data[[ycol]] >= 0, .data[[ycol]] <= y_max) %>%
  mutate(
    tipo = if_else(is_first_year, "Primeiros acessos", "Acessos reincidentes"),
    # grupo de cor: nos primeiros, usa o próprio ano; nos retornos, usa coorte do primeiro acesso
    color_group = case_when(
      is_first_year ~ as.character(ano),
      !is_first_year & first_year_for_id %in% anos_plot_vec ~ as.character(first_year_for_id),
      TRUE ~ "Outros"
    ),
    ano  = factor(ano, levels = anos_plot_vec),
    tipo = factor(tipo, levels = c("Primeiros acessos","Acessos reincidentes"))
  )

color_map <- c(color_by_year, `Outros` = color_other)

# contadores por painel para "n = ..."
counts_panel <- df3 %>%
  group_by(ano, tipo) %>%
  summarise(n = n(), .groups = "drop")

p3 <- ggplot(df3, aes(x = .data[[xcol]], y = .data[[ycol]], color = color_group)) +
  geom_point(size = 1.6, alpha = 0.8, show.legend = TRUE) +
  scale_color_manual(
    name = "Corte (ano do 1º acesso)",
    values = color_map,
    breaks = c("2023","2024","2025","Outros")
  ) +
  scale_x_log10(
    limits = c(1, x_max),
    breaks = 10^(0:2),
    minor_breaks = as.numeric(outer(10^(0:2), c(2, 5), "*")),
    labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
  ) +
  coord_cartesian(ylim = c(0, y_max)) +
  geom_vline(xintercept = x_thr, color = "black", linewidth = 0.4) +
  geom_hline(yintercept = y_thr, color = "black", linewidth = 0.4) +
  facet_grid(rows = vars(ano), cols = vars(tipo)) +
  labs(
    title = "Horas na plataforma vs Frequência — 1º acesso × retorno de usuários por ano",
    x = paste0("Duração do uso em horas (X em log; corte em ", x_thr, ")"),
    y = paste0("Frequência (corte em ", y_thr, ")")
  ) +
  theme_bw()

# adicionar "n=..." no canto superior direito de cada painel
p3 <- p3 +
  geom_text(
    data = counts_panel,
    aes(x = Inf, y = Inf, label = paste0("n=", fmt_milhar(n))),
    inherit.aes = FALSE,
    hjust = 1.05, vjust = 1.5, size = 3.2
  )

print(p3)
