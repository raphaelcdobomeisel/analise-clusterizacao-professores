library(shiny)
library(dplyr)
library(ggplot2) 

data(diamonds)
diamonds
ui <- fluidPage(
  radioButtons('var_x', 
               label = 'Escolha a variável para o eixo x:',
               choices = colnames(diamonds)[-c(2,3,4)]),
  sliderInput('lim_y', 'Escolha os limites do eixo y:',
              value = c(0, 20000), min = 0, max = 30000),
  plotOutput('grafico1')
)
server <- function(input, output,session){
  output$grafico1 = renderPlot({diamonds %>% 
      ggplot(aes(x = .data[[input$var_x]],
                 y = price))+
      geom_point(col = 'blue')+
      scale_y_continuous(limits = input$lim_y)})
}
shinyApp(ui, server)
