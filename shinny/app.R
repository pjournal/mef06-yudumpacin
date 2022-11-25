
library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)


# Prepare data
data <- readRDS("my_data.rds")

data_univ <- data %>% 
  rename(ad = 'Üniversite Adı',
         tur = 'Üniversite Türü',
         il = 'İl Adı',
         uyruk = 'Uyruk')


data_univ <- data_univ %>%
              filter(ad!='TOPLAM')


data_univ$T <- suppressWarnings(as.numeric(data_univ$T))
data_univ$E <- suppressWarnings(as.numeric(data_univ$E))
data_univ$K <- suppressWarnings(as.numeric(data_univ$K))
data_gr <- data_univ %>%
  group_by(ad,uyruk) %>%
  summarise(total = sum(T))


ui <- fluidPage(
  titlePanel("Türkiye Üniversitelerindeki Yabancı Öğrenci Dağılımı"),
  
  sidebarLayout(
    sidebarPanel(
      
  selectInput(inputId = "il", label = "İl", choices = sort(unique(data_univ$il))),
  radioButtons(inputId = "tur", label = "Tür", choices =  unique(data_univ$tur)),
  selectInput(inputId = "ad", label = "Üniversite", choices = unique(data_univ$ad),multiple = FALSE),

    ),
  
  mainPanel(
    
    # Output: Histogram ----
    plotOutput(outputId = "distPlot")
    
  )
  )
  )



server <- function(input, output,session) {
  
 
  observeEvent(input$il, {
    updateSelectInput(session,
                      "ad",
                      choices = sort(unique(data_univ$ad[(data_univ$il == input$il) & (data_univ$tur==input$tur)]))
    )
  })
  observeEvent(input$tur, {
    updateSelectInput(session,
                      "ad",
                      choices = sort(unique(data_univ$ad[(data_univ$il == input$il) & (data_univ$tur==input$tur)]))
    )
  })
  
  
  
  output$distPlot <- renderPlot({
    
    data_selected <- data_gr %>%
                      filter(ad==input$ad)
    
    value    <- data_selected$total
    uyruk <- data_selected$uyruk
    
    ggplot(data_selected, aes(x=uyruk, y=value)) + 
      geom_bar(stat = "identity")+
      theme(axis.text.x = element_text(angle = 90))
    
    
  })
  
} 
shinyApp(ui = ui, server = server)
