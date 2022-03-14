#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/
#
# BSV2025 - Brandschadenanalyse GVZ: Choose the Scatterplotted variables

library(shiny)
library(shinythemes)
library(tidyverse)

# load data
schad <- read_csv("data/schad_shiny.csv",
                  #locale(encoding = "iso-8859-1"), col_names = TRUE,
                  col_types = cols(
                    # Ausdr3 = col_factor(NULL),
                    # GebaeudeBaujahr = col_factor(NULL),
                    # zweckcode = col_factor(NULL),
                    # zweckcode_kat = col_factor(NULL),
                    # schadencode = col_factor(NULL),
                    volumen = col_number()))

schad <- as.data.frame(schad) %>% 
  # filtern der schadenfaelle wenn: indexierter schaden ueber 0, 
  # Versicherungssumme ueber 0, volumen nicht NA und und > 0,
  # schadensatz kleiner 1.3
  filter(SbwVerWert > 0 & schad_index > 0 & schadensatz < 1.3 &
           !is.na(volumen) & volumen > 0) %>% 
  # from nummeric to factor by reordering factor levels...
  mutate(lg_versSumme = log(SbwVerWert),
         lg_schad_index = log(schad_index),
         lg_schadensatz = log(schadensatz),
         lg_volumen = log(volumen)) %>% 
  arrange(Ausdr3) %>%
  mutate(schadenjahr_f = factor(Ausdr3)) %>% 
  arrange(GebaeudeBaujahr) %>%
  mutate(Gebaeudebaujahr_f = factor(GebaeudeBaujahr)) %>% 
  arrange(zweckcode) %>%
  mutate(zweckcode_f = factor(zweckcode),
         zweckcode_einstlg = floor(zweckcode/1000),
         zweckcode_einstlg_f = factor(zweckcode_einstlg)) %>% 
  arrange(zweckcode_kat) %>%
  mutate(zweckcode_zweistlg_f = factor(zweckcode_kat)) %>% 
  arrange(schadencode) %>%
  mutate(schadencode_f = factor(schadencode)) %>% 
  # ... just keep the desired columns
  dplyr::select(c("Gebaeudebaujahr_f", "SbwVerWert", "lg_versSumme", "volumen",
                "lg_volumen", "zweckcode_einstlg_f", "zweckcode_zweistlg_f",
                "zweckcode_f", "schadenjahr_f", "schadencode_f", "schad_index",
                "lg_schad_index", "schadensatz", "lg_schadensatz", "verletzte",
                "todesopfer"))

# Define UI for application that draws a scatterplot
ui <- fluidPage(
  # set shiny theme
  fluidPage(theme = shinytheme("united")),
  
  # Application title
  titlePanel("BSV2025 - Brandschadenanalyse GVZ"),
  
  sidebarLayout(
    sidebarPanel(

      #taking inputs using radiobuttons
      radioButtons("s", "Select X-axis:",
                   list("Gebaeudebaujahr"='a1', "Versicherunssumme"='b1',
                        "lg Versicherunssumme"='c1', "Volumen"='d1',
                        "lg Volumen"='e1', "Zweckcode einstellig"='f1',
                        "Zweckcode zweistellig"='g1', "Zweckcode"='h1',
                        "Schadenjahr"='i1', "Schadencode"='j1',
                        "Index_Schadensumme"='k1', "lg Index_Schadensumme"='l1',
                        "schadensatz"='m1', "lg schadensatz"='n1',
                        "Verletzte"='o1', "Todesopfer"='p1'),
                   selected = "f1"),
      
      #taking input k using radiobuttons
      radioButtons("k", "Select Y-axis:",
                   list("Gebaeudebaujahr"='a2', "Versicherunssumme"='b2',
                        "lg Versicherunssumme"='c2', "Volumen"='d2',
                        "lg Volumen"='e2', "Zweckcode einstellig"='f2',
                        "Zweckcode zweistellig"='g2', "Zweckcode"='h2',
                        "Schadenjahr"='i2', "Schadencode"='j2',
                        "Index_Schadensumme"='k2', "lg Index_Schadensumme"='l2',
                        "schadensatz"='m2', "lg schadensatz"='n2',
                        "Verletzte"='o2', "Todesopfer"='p2'),
                   selected = "c2")
    ),
    
    #displaying the output plot
    mainPanel(plotOutput("distPlot"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #writing server function
  output$distPlot <- renderPlot({
    if(input$s=='a1') { i<-1 }
    if(input$s=='b1') { i<-2 }
    if(input$s=='c1') { i<-3 }
    if(input$s=='d1') { i<-4 }
    if(input$s=='e1') { i<-5 }
    if(input$s=='f1') { i<-6 }
    if(input$s=='g1') { i<-7 }
    if(input$s=='h1') { i<-8 }
    if(input$s=='i1') { i<-9 }
    if(input$s=='j1') { i<-10 }
    if(input$s=='k1') { i<-11 }
    if(input$s=='l1') { i<-12 }
    if(input$s=='m1') { i<-13 }
    if(input$s=='n1') { i<-14 }
    if(input$s=='o1') { i<-15 }
    if(input$s=='p1') { i<-16 }
    if(input$k=='a2') { j<-1 }
    if(input$k=='b2') { j<-2 }
    if(input$k=='c2') { j<-3 }
    if(input$k=='d2') { j<-4 }
    if(input$k=='e2') { j<-5 }
    if(input$k=='f2') { j<-6 }
    if(input$k=='g2') { j<-7 }
    if(input$k=='h2') { j<-8 }
    if(input$k=='i2') { j<-9 }
    if(input$k=='j2') { j<-10 }
    if(input$k=='k2') { j<-11 }
    if(input$k=='l2') { j<-12 }
    if(input$k=='m2') { j<-13 }
    if(input$k=='n2') { j<-14 }
    if(input$k=='o2') { j<-15 }
    if(input$k=='p2') { j<-16 }
    
    # Plot data
    X    <- schad[, i]
    Y    <- schad[, j]
    plot(X,Y)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
