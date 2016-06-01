library(shiny)

shinyUI(fluidPage(
  sidebarPanel(fileInput('file1', 'Choose CSV File',
                         accept=c('text/csv', 
                                  'text/comma-separated-values,text/plain', 
                                  '.csv')),
               selectInput("inSelect", "Choose a variable to plot: ", choices = "Pending File Upload"),
  		selectInput("inSelect2", "Choose a second variable (grouping variable) to plot: ", choices = "Pending File Upload"),
  		actionButton("createg", "Create graphs")
  ),
  mainPanel(
    plotOutput("plot1")
  )
))
