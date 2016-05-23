
#server.R

# source("pfa_visualisation2_app.R")

library(htmlwidgets)
library(devtools)

# install_github("smartinsightsfromdata/rpivotTable")
library(rpivotTable)



function(input, output){
         # , session) {
  
  #lapply(x, function(v) {
  #  output[[paste0('graf_', v)]] <- renderPlot({
  #    get_plot(X,v)
  #  })
  #})
  output$ptab = renderTable({ptab})
  output$test = renderTable({do_tests(X,input$variable)})
  output$graf = renderPlot({get_plot(X,input$variable)})
  output$density = renderPlot({plot_density(X,input$variable)})
  output$pivot <- renderRpivotTable({
    rpivotTable(data = as.data.frame(X),
                rows = "age", cols = "claims",
                aggregatorName = "Count as Fraction of Rows",
                rendererName = "Col Heatmap")})
}