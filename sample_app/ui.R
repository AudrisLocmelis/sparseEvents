
#ui.R

# source("pfa_visualisation2_app.R")


navbarPage("Lung Cancer Claims",
  tabPanel("Descriptive Overview",
             fluidRow(
               column(4,
                      selectInput("variable", "Variable:",vec),
                      br(),
                      
                      strong("Mean (location) equallty tests"),
                      br(),
                      "comparing claim / no claim",
                      tableOutput("test"),
                      br(),
                      # Kruskal-Wallis Test
                      strong("Ordered p-values"),
                      br(),
                      "of Kruskal-Wallis / Chi-squared tests for all variables",
                      tableOutput("ptab")
               ),
               
               column(8,
                      plotOutput("graf"),
                      br(),
                      plotOutput("density")
               )
             )
    ),
  tabPanel("Pivot Table", rpivotTable::rpivotTableOutput('pivot'))
)