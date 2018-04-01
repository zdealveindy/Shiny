#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Download data into Shiny ./data folder
require (vegan)
require (RColorBrewer)
read.delim.downl <- function (file, url = paste ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/', file, sep = ''), destfile = paste ('data/', file, sep = ''), ...)
{
  if (!file.exists (destfile)) download.file (url = url, destfile = destfile)
  read.delim (destfile, ...)
}

# files.to.download <- c('vltava-spe.txt', 'vltava-env.txt',
#                        'grasslands-spe.txt', 'grasslands-env.txt',
#                        'simul1-spe.txt', 'simul1-env.txt')
# 
# lapply (files.to.download, FUN = function (x) download.file (paste ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/', x, sep = ''), destfile = paste ('data/', x, sep = '')))


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
                      choices = list ('vltava', 'grasslands', 'simul1', 'simul.short', 'simul.long')),
         conditionalPanel(condition = "input.dataset == 'vltava'",
           p(a('vltava', href = 'http://anadat-r.davidzeleny.net/doku.php/en:data:vltava', target = '_blank'),'- forest vegetation data, compositionally highly heterogenous; plots: 97, species: 274, species data: percentage cover.')),
         conditionalPanel(condition = "input.dataset == 'grasslands'",
                          p(a('grasslands', href = 'http://anadat-r.davidzeleny.net/doku.php/en:data:grasslands', target = '_blank'),'- grassland vegetation data, compositionally rather homogeneous; plots: 48, species: 171, species data: percentage cover.')),
         conditionalPanel(condition = "input.dataset == 'simul1'",
                          p(a('simul1', href = 'http://anadat-r.davidzeleny.net/doku.php/en:data:simul', target = '_blank'),'- simulated community data along one environmental gradient, very heterogeneous; plots: 500, species: 296, species data: presence-absence.')),
         conditionalPanel(condition = "input.dataset == 'simul.short'",
                          p(a('simul.short', href = 'http://anadat-r.davidzeleny.net/doku.php/en:data:simul', target = '_blank'),'- simulated community data along two unequally long environmental gradient, rather homogeneous; plots: 70, species: 300, species data: presence-absence.')),
         conditionalPanel(condition = "input.dataset == 'simul.long'",
                          p(a('simul.long', href = 'http://anadat-r.davidzeleny.net/doku.php/en:data:simul', target = '_blank'),'- simulated community data along two unequally long environmental gradient, rather heterogeneous; plots: 70, species: 300, species data: presence-absence.')),
         radioButtons(inputId = "ordimethod",
                      label = 'Ordination method:',
                      choices = list ('PCA', 'CA', 'DCA', "tb-PCA", 'PCoA', 'NMDS'),
                      inline = TRUE),
         conditionalPanel(
           condition = "input.ordimethod == 'tb-PCA'",
           radioButtons(inputId = "pretransf_spe",
                        label = "Pre-transformation of data for tb-PCA:",
                        choiceValues = list("hell", "normalize"),
                        choiceNames = list ("Hellinger", "chord"),
                        selected = "hell",
                        inline = TRUE)
         ),
         conditionalPanel(
           condition = "input.ordimethod == 'PCoA'",
           radioButtons(inputId = "distance_PCoA",
                        label = "Distance index for PCoA:",
                        choiceValues = list("bray", "euclidean", "manhattan"),
                        choiceNames = list ("Bray-Curtis", "Euclidean", "Manhattan"),
                        selected = "bray",
                        inline = TRUE)
         ),
         conditionalPanel(
           condition = "input.ordimethod == 'NMDS'",
           radioButtons(inputId = "distance_NMDS",
                        label = "Distance index for NMDS:",
                        choiceValues = list("bray", "euclidean", "manhattan"),
                        choiceNames = list ("Bray-Curtis", "Euclidean", "Manhattan"),
                        selected = "bray",
                        inline = TRUE)
         ),
         radioButtons(inputId = "transformation",
                      label = 'Transformation of species data:',
                      choices = list ('none', 'sqrt', 'log', 'presence-absence'),
                      inline = TRUE),
         checkboxGroupInput(inputId = "display",
                      label = 'Display in ordination diagram:',
                      choices = list ('species', 'sites', 'groups', 'ordispider', 'ordihull'),
                      selected = c('species', 'sites'),
                      inline = TRUE)
      ),
      
      # Show a result
      mainPanel(
        plotOutput("distPlot", height = "600px"),
        verbatimTextOutput("code"),
        verbatimTextOutput("text")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  inputData <- reactive({
    # load data according to input$dataset
    switch (input$dataset,
            "vltava" = {spe <- read.delim.downl ('vltava-spe.txt', row.names = 1)
                        env <- read.delim.downl ('vltava-env.txt')
                        groups <- env$GROUP
                        },
            "grasslands" = {spe <- read.delim.downl ('grasslands-spe.txt', row.names = 1)
                            env <- read.delim.downl ('grasslands-env.txt')
                            groups <- as.numeric (env$classification)
                            },
            "simul1" = {spe <- read.delim.downl ('simul1-spe.txt', row.names = 1)
                        env <- read.delim.downl ('simul1-env.txt', row.names = 1)
                        groups <- env$groups
                        },
            "simul.short" = {spe <- read.delim.downl ('simul.short-spe.txt', row.names = 1)
                        env <- read.delim.downl ('simul.short-env.txt', row.names = 1)
                        groups <- env$groups
                        },
            "simul.long" = {spe <- read.delim.downl ('simul.long-spe.txt', row.names = 1)
                        env <- read.delim.downl ('simul.long-env.txt', row.names = 1)
                        groups <- env$groups
            }
    )
    
    spe <- switch (input$transformation,
            none = spe,
            sqrt = sqrt (spe),
            log = log1p (spe),
            'presence-absence' = decostand (spe, method = 'pa'))
    list (spe = spe, env = env, groups = groups)
  })
   
  calcOrdi <- reactive ({
    spe <- inputData ()$spe
    ordi <- switch (input$ordimethod,
                    PCA = rda (spe),
                    CA = cca (spe),
                    DCA = decorana (spe),
                    "tb-PCA" = rda (decostand (spe, method = input$pretransf_spe)),
                    PCoA = cmdscale (vegdist (spe, method = input$distance_PCoA), k = nrow (spe)-1),
                    NMDS = metaMDS (spe, distance = input$distance_NMDS, try = 1, trymax = 1))
    ordi
  })

  output$distPlot <- renderPlot(width = 600, height = 600, expr = {
    data_in <- inputData ()
    ordi <- calcOrdi ()
     ordiplot (ordi, type = 'n')
     if ('species' %in% input$display  & input$ordimethod != 'PCoA') points (ordi, display = 'species', col = 'red', pch = '+')
     if ('sites' %in% input$display) points (ordi, display = 'sites', pch = if ('groups' %in% input$display) as.character (data_in$groups) else 1, col = if ('groups' %in% input$display) brewer.pal (n = max(data_in$groups), 'Set1')[data_in$groups] else 'black')
     if ('ordispider' %in% input$display) for (gr in unique (data_in$groups)) ordispider (ordi, groups = data_in$groups, show.groups = gr, col = if ('groups' %in% input$display) brewer.pal (n = max(data_in$groups), 'Set1')[gr] else 'black')
     if ('ordihull' %in% input$display) for (gr in unique (data_in$groups)) ordihull (ordi, groups = data_in$groups, show.groups = gr, col = if ('groups' %in% input$display) brewer.pal (n = max(data_in$groups), 'Set1')[gr] else 'black')
   })
  
  output$code <- renderPrint ({
    code.intro <- paste ("# The R code which can be used to reproduce the ordination", 
                         "library (vegan) # uploading vegan library",
                         sep = '\n')
    which.files <- switch (input$dataset,
                          "vltava" = list (spe = 'vltava-spe.txt', env = 'vltava-env.txt', groups = 'groups <- env$GROUP'),
                          "grasslands" = list (spe = 'grasslands-spe.txt', env = 'grasslands-env.txt', groups = 'groups <- as.numeric (env$classification)'),
                          "simul1" = list (spe = 'simul1-spe.txt', env = 'simul1-env.txt', groups = 'groups <- env$groups'),
                          "simul.short" = list (spe = "simul.short-spe.txt", env = "simul.short-env.txt", groups = "groups <- env$groups"),
                          "simul.long" = list (spe = "simul.long-spe.txt", env = "simul.long-env.txt", groups = "groups <- env$groups")
                          )
    read.files <- paste(paste ("spe <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/", which.files$spe, "', row.names = 1)", sep = ''), paste ("env <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/", which.files$env, "', row.names = 1)", sep = ''), which.files$groups, sep = '\n') 
    transf.output <- switch (input$transformation,
            'none' = NULL,
            'sqrt' = '\n# Transforming species data\nspe <- sqrt (spe)',
            'log' = '\n# Transforming species data\nspe <- log1p (spe)',
            'presence-absence' = "\n# Transforming species data\nspe <- decostand (spe, method = 'pa')")
    ordi.output <- switch (input$ordimethod,
            'PCA' = 'ordi <- rda (spe)',
            'CA' = 'ordi <- cca (spe)',
            'DCA' = 'ordi <- decorana (spe)',
            'tb-PCA' = paste (if (input$pretransf_spe == 'hell')
              "spe <- decostand (spe, method = 'hell') # Pre-transforming species data for tb-PCA" else "spe <- decostand (spe, method = 'normalize') # Pre-transforming species data for tb-PCA",
              "ordi <- rda (spe)", sep = '\n'),
            'PCoA' = paste (paste ("spe.dist <- vegdist (spe, method = '", input$distance_PCoA, "') # Calculating distance matrix", sep = ''), "ordi <- cmdscale (spe.dist)", sep = '\n'),
            'NMDS' = paste (paste ("spe.dist <- vegdist (spe, method = '", input$distance_NMDS, "') # Calculating distance matrix", sep = ''), "ordi <- metaMDS (spe.dist)", sep = '\n'))
      {
      out <- "ordiplot (ordi, type = 'n')"
      if ("species" %in% input$display)
        if (input$ordimethod != "PCoA") out <- paste(out, "points (ordi, display = 'species', pch = '+', col = 'red')", sep = '\n') else
          out <- paste(out, "# Species scores are not available in PCoA ordination", sep = '\n')
      if ("sites" %in% input$display)
        if ("groups" %in% input$display) out <- paste(out, "points (ordi, display = 'sites', pch = as.character (groups), col = groups)", sep = '\n') else
          out <- paste (out, "points (ordi, display = 'sites')", sep = '\n')
      if ('ordispider' %in% input$display)
        if ("groups" %in% input$display) out <- paste(out, "for (gr in unique (groups)) ordispider (ordi, groups = groups, show.group = gr, col = gr)", sep = '\n') else
          out <- paste(out, "ordispider (ordi, groups = groups)", sep = '\n')
      if ('ordihull' %in% input$display)
        if ("groups" %in% input$display) out <- paste(out, "for (gr in unique (groups)) ordihull (ordi, groups = groups, show.group = gr, col = gr)", sep = '\n') else
          out <- paste(out, "ordihull (ordi, groups = groups)", sep = '\n')        
      diag.output <- out
      }
    cat (paste (code.intro, '\n# Import of data from GitHub repository:', read.files, transf.output, '\n# Calculating ordination:', ordi.output, '\n# Ploting ordination diagram:', diag.output, '\n# Summary of ordination:', 'ordi', sep = '\n'))
  }) 
  output$text <- renderPrint ({if (input$ordimethod != 'PCoA') calcOrdi () else cat("The function 'cmdscale' does not return summary output.")})
}

# Run the application 
shinyApp(ui = ui, server = server)

