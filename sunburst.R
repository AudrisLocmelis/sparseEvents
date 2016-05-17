
source("common/ipak.R")
packages <- c("devtools", "shiny", "sunburstR", "rPython")
ipak(packages)


devtools::install_github("timelyportfolio/sunburstR")
# library(devtools)
# library(shiny)
# library(sunburstR)

if (!require("rPython")) install.packages("rPython")
install.packages("http://genome.crg.es/~didac/ggsunburst/ggsunburst_0.0.5.tar.gz", repos=NULL, type="source")


sequences <- read.csv(
  system.file("examples/visit-sequences.csv",package = "sunburstR")
  ,header = F
  ,stringsAsFactors = FALSE
)


server <- function(input,output,session) {
  output$sunburst <- renderSunburst({
    invalidateLater(1000, session)
    
    sequences <-
      sequences[sample(nrow(sequences),1000),]
    
    sunburst(sequences)
  })
}


ui <- fluidPage(sidebarLayout(sidebarPanel(),
                              
                              # plot sunburst
                              mainPanel(sunburstOutput("sunburst"))))

shinyApp(ui,server)