# --- 1. Carregar Pacotes ---
library(shiny)
library(ggplot2)
library(dplyr)
library(patchwork) # Para combinar múltiplos gráficos ggplot
library(tidyverse)
library(readr)
library(lubridate)

# --- 1. Carrega os dados e gera as novas Features ---

# Para exibir todas as linhas de um tibble
options(tibble.print_max = Inf)

# Para exibir todas as colunas e o conteúdo completo delas
options(tibble.width = Inf)

# Agora, leia os arquivos CSV usando read_csv()

dim_teachers <- read_csv("dim_teachers.csv")
teachers_contents_interactions <- read_csv("fct_teachers_contents_interactions.csv")
teachers_entries <- read_csv("fct_teachers_entries.csv")

formation <- read_csv("stg_formation.csv")
mari_ia_conversation <- read_csv("stg_mari_ia_conversation.csv")
mari_ia_reports <- read_csv("stg_mari_ia_reports.csv")


teachers_entries <- teachers_entries %>%
  mutate(
    # Converte as colunas para o formato data-hora
    # A função ymd_hms() assume o formato "Ano-Mês-Dia Hora:Minuto:Segundo"
    # O lubridate tem outras funções (dmy(), mdy_hms(), etc.) para formatos diferentes.
    # Ele automaticamente converte erros de parsing em NA, igual a errors="coerce".
    
    data_inicio = ymd_hms(data_inicio),
    data_fim = ymd_hms(data_fim),
    
    # Calcula a diferença entre as datas diretamente em horas
    tempo_sessao = as.numeric(data_fim - data_inicio, units = "hours"),
    
    # Extrai o ano da data de início
    ano = year(data_inicio)
  )

df <- teachers_entries %>%
  group_by(unique_id, ano) %>% 
  summarise(                    
    sessao_medio = mean(tempo_sessao, na.rm = TRUE),
    sessao_total = sum(tempo_sessao, na.rm = TRUE),
    frequencia   = n(),
    .groups = 'drop'
  ) %>%
  arrange(unique_id, ano) 

# --- 2. Definições do Gráfico (fora do Shiny para não recalcular) ---
anos_plot <- sort(unique(df$ano))
palette <- c("#ec4899", "#f59d0a", "#8b5cf5", "#3b2087")

# Mapeia cores para os anos de forma segura
color_map <- setNames(palette, anos_plot)

# Valores para as linhas de corte
cut_freq <- 100
cut_duration <- 10


# --- 3. Interface do Usuário (UI) ---
ui <- fluidPage(
  # Título da Aplicação
  titlePanel("Horas na plataforma vs Frequência de uso - por indivíduo"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Controles de Filtro"),
      sliderInput(
        "freq_range",
        "Filtrar por Frequência:",
        min = 0,
        max = 400,
        value = c(0, 400), # Valor inicial (range completo)
        step = 10
      ),
      sliderInput(
        "duration_range",
        "Filtrar por Duração do uso (horas):",
        min = 1,
        max = 200,
        value = c(1, 200), # Valor inicial (range completo)
        step = 5
      )
    ),
    
    mainPanel(
      # Onde o gráfico será renderizado
      plotOutput("scatterPlot", height = "600px")
    )
  )
)

# --- 4. Lógica do Servidor (Server) ---
server <- function(input, output) {
  
  # Cria um dataframe reativo que muda com os inputs do usuário
  filtered_data <- reactive({
    df %>%
      filter(
        frequencia >= input$freq_range[1],
        frequencia <= input$freq_range[2],
        sessao_total >= input$duration_range[1],
        sessao_total <= input$duration_range[2]
      )
  })
  
  # Renderiza o gráfico
  output$scatterPlot <- renderPlot({
    
    # Lista para armazenar os 4 gráficos individuais
    plots <- list()
    
    # Loop para criar um gráfico para cada ano
    for (i in seq_along(anos_plot)) {
      ano_atual <- anos_plot[i]
      cor_atual <- color_map[[as.character(ano_atual)]]
      
      # Filtra os dados reativos para o ano específico
      data_ano <- filtered_data() %>%
        filter(ano == ano_atual)
      
      p <- ggplot(data_ano, aes(x = sessao_total, y = frequencia)) +
        # Pontos do gráfico
        geom_point(
          color = cor_atual,
          size = 2.5,
          alpha = 0.7
        ) +
        # Linha horizontal de corte
        geom_hline(yintercept = cut_freq, color = "black", linetype = "solid", linewidth = 0.8) +
        # Linha vertical de corte
        geom_vline(xintercept = cut_duration, color = "black", linetype = "solid", linewidth = 0.8) +
        # Escala logarítmica no eixo X
        scale_x_log10(
          breaks = c(1, 10, 100), # Principais marcas
          minor_breaks = c(seq(1, 9), seq(10, 90, 10), 100, 200) # Marcas menores
        ) +
        # Define os limites dos eixos (zoom)
        coord_cartesian(
          xlim = c(1, 200),
          ylim = c(0, 400),
          expand = FALSE # Impede que o ggplot adicione padding
        ) +
        # Título de cada painel (facet)
        labs(title = paste("Ano", ano_atual)) +
        # Tema visual
        theme_light() +
        theme(
          # Estilo do título do painel
          plot.title = element_text(
            face = "bold", 
            size = 14, 
            color = cor_atual,
            hjust = 0.5
          ),
          # Estilo da grade (grid)
          panel.grid.major = element_line(linetype = "dashed", linewidth = 0.3, color = "gray70"),
          panel.grid.minor = element_line(linetype = "dashed", linewidth = 0.2, color = "gray80")
        )
      
      # Adiciona os rótulos dos eixos apenas nos gráficos corretos
      if (ano_atual %in% c(2022, 2024)) {
        p <- p + ylab("Frequência (corte em 100)")
      } else {
        p <- p + ylab(NULL) + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
      }
      
      if (ano_atual %in% c(2024, 2025)) {
        p <- p + xlab("Duração do uso em horas (X em log; corte em 100)") # Correção no rótulo X
      } else {
        p <- p + xlab(NULL) + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
      }
      
      plots[[i]] <- p
    }
    
    # Combina os 4 gráficos em um grid 2x2 usando o pacote 'patchwork'
    (plots[[1]] | plots[[2]]) / (plots[[3]] | plots[[4]])
    
  })
}

# --- 5. Executar a Aplicação ---
shinyApp(ui = ui, server = server)
