library(shiny)
gold <- read.csv("data/gold.csv")


shinyUI(fluidPage(
  sidebarPanel(
        selectInput("dataset", "Choose a file:",
            choices = c("Grapevine", "Gold", "Gold Plus", "Platinum"),
            selected = "Gold",
        ),

        selectInput("inSelect", "Choose a variable to plot: ", choices = names(gold), selected = names(gold)[1]),

  		selectInput("inSelect2", "Choose a second variable (grouping variable) to plot: ", choices = names(gold), selected = names(gold)[2]),

  		actionButton("createg", "Create graphs")
  ),

  mainPanel(
    plotOutput("plot1")
  )
))
