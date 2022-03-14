#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# BSV2025 - Brandschadenanalyse GVZ

library(shiny)
library(shinythemes)
library(tidyverse)
library(RColorBrewer)

# Load data
# ---------
dat <- read_delim("data/Brandtote_IRV_1987.csv",
                  delim = ";", col_names = TRUE,
                  locale = locale(encoding = 'ISO-8859-1'),
                  # col_types = cols(),
                  col_types = cols_only(
                    'schadenjahr' = col_integer(),
                    'kanton' = col_character(),
                    'zweckcode_vkf' = col_factor(),
                    'stockwerk' = col_factor(),
                    'schadencode' = col_factor(),
                    'todesopfer_anzahl' = col_integer(),
                    'zweckgruppe' = col_factor(),
                    'schadengruppe' = col_factor()))


# Define UI for application
# -------------------------
ui <- fluidPage(
  # set shiny theme
  theme = shinytheme("simplex"),
  
  # Application title
  titlePanel("BSV2025 - Datenanalyse GVZ"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("IRV Brandschäden seit 1987 - Schweizweit"),
      # Define the sidebar with one input
      selectInput("kanton", "Kanton:",
                  choices = unique(dat$kanton)),
      # hr(),
      br(),
      

      #taking inputs using radiobuttons
      radioButtons(inputId  = "x",
                   label = "X-axis:",
                   choices = list("Jahr" = "schadenjahr",
                                  "Zweckcode VKF" = "zweckcode_vkf",
                                  "Stockwerk" = "stockwerk",
                                  "Schadencode" = "schadencode",
                                  "Anzahl Todesopfer" = "todesopfer_anzahl"),
                   selected = "schadenjahr"),
      
      radioButtons(inputId  = "y",
                   label = "Y-axis:",
                   choices = list("Jahr" = "schadenjahr",
                                  "Zweckcode VKF" = "zweckcode_vkf",
                                  "Stockwerk" = "stockwerk",
                                  "Schadencode" = "schadencode",
                                  "Anzahl Todesopfer" = "todesopfer_anzahl"),
                   selected = "todesopfer_anzahl")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plots",
                 plotOutput("distPlot"),
                 plotOutput("mosaicplot1")),
        tabPanel("Code Beschrieb",
                 tableOutput("tbl_zwck"),
                 tableOutput("tbl_schad"))
      )
    )
  )
)

# für Achsenbeschriftung
choice_Vec <- c("Jahr" = "schadenjahr",
                "Zweckcode VKF" = "zweckcode_vkf",
                "Stockwerk" = "stockwerk",
                "Schadencode" = "schadencode",
                "Anzahl Todesopfer" = "todesopfer_anzahl")

# Define server logic
# -------------------------
server <- function(input, output) {
  
  # aktualisiere den kanton
  rt <- reactive({
    dat %>%
      filter(kanton == input$kanton)
  })
  
  #writing server function
  output$distPlot <- renderPlot({
    dat_plot <- rt()
    
    plot(dat_plot[[input$x]], dat_plot[[input$y]],
         main = paste(names(choice_Vec)[choice_Vec == input$x], "und",
                      names(choice_Vec)[choice_Vec == input$y], "im Kanton", input$kanton),
         xlab = names(choice_Vec)[choice_Vec == input$x],
         ylab = names(choice_Vec)[choice_Vec == input$y])
  })
  
  # aktualisiere den kanton
  rt_2 <- reactive({
    dat %>%
      filter(kanton == input$kanton) %>% 
    
    # frequency table: Anzahl Brandtote per ...
      group_by(zweckcode_vkf) %>% 
      # 2 way cross table
      select(zweckcode_vkf, schadencode) %>%
      table()
  })
  
  # render mosaicplot
  output$mosaicplot1 <- renderPlot({
    freq_table <- rt_2()
    
    mosaicplot(t(freq_table),
             sort = 2:1,
             # off = 30,
             color = brewer.pal(7,"Paired"),
             main = paste("Brandopfer pro Zweck und Ursache", "im Kanton", input$kanton),
             # sub = "subtitle",
             xlab = "Zweck VKF",
             ylab = "Brandursache",
             las = 2)
  })

  # render zweck table
  output$tbl_zwck <- renderTable({
    unique(dat[c("zweckcode_vkf", "zweckgruppe")])
    })
  
  # render schad table
  output$tbl_schad <- renderTable({
    unique(dat[c("schadencode", "schadengruppe")])
  })
    
} # END Server fkt

# Run the application
# -------------------
shinyApp(ui = ui, server = server)
