#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Unconstrained ordination in vegan"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput (inputId = "dataset",
                   label = 'Dataset:',
                   choices = list ('Vltava', 'Grasslands', 'Simul-1'),
                   selected = 'vltava'),
      radioButtons(inputId = "ordimethod",
                   label = 'Ordination method:',
                   choices = list ('PCA', 'CA', 'DCA', "tb-PCA", 'PCoA', 'NMDS'),
                   inline = TRUE),
      radioButtons(inputId = "transformation",
                   label = 'Transformation of species data:',
                   choices = list ('none', 'sqrt', 'log', 'presence-absence'),
                   inline = TRUE),
      checkboxGroupInput(inputId = "display",
                         label = 'Display in ordination diagram:',
                         choices = list ('species', 'sites', 'groups', 'spiderplot', 'envelope'),
                         selected = c('species', 'sites'),
                         inline = TRUE)
      
    ),
    
    # Show a result
    mainPanel(
      verbatimTextOutput("text")
#      plotOutput("distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  inputData <- reactiveVal({
    # load data according to input$dataset
    list (spe = read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/vltava-spe.txt', row.names = 1),
          env = read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/vltava-env.txt', row.names = 1))
  })
  output$text <- renderPrint ({
    input$dataset
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

