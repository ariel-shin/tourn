library(shiny)

shinyUI(fluidPage(
  sidebarPanel(
        selectInput("dataset", "Choose a file:",
            choices = c("Grapevine", "Gold", "Gold Plus", "Platinum")),

        downloadButton('downloadData', 'Download')
  ),

  mainPanel(
        tableOutput('table')
  )

))

