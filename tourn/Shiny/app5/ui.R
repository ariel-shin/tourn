#app5
library(shiny)
gold <- read.csv("data/gold.csv")

num1 <- sapply(gold, is.numeric)
ngold <- gold[,num1]

num2 <- sapply(gold, is.factor)
fgold <- gold[,num2]


shinyUI(pageWithSidebar(
#Application Title
headerPanel("Analyzing Debate Trends"),

sidebarPanel(
    wellPanel(
        selectInput(inputId = "dataset", label = "Choose a file:",
        choices = c("Grapevine", "Gold", "Gold Plus", "Platinum"),
        selected = "Gold",
        )
    ),

    uiOutput("Box1"),
    uiOutput("Box2"),
    uiOutput("Box3"),
    uiOutput("Box4")
),

#conditionalPanel(
#       condition = "input.conditionedPanels == 1",
#    wellPanel(
#        selectInput("inSelect", "Choose a numeric variable to plot: ", choices = names(ngold),
#            selected = names(ngold)[1]),
#        selectInput("inSelect2", "Choose a second variable (grouping variable) to plot: ",
#            choices = names(ngold), selected = names(ngold)[2])
#    ),

#  conditionalPanel(
#       condition = "input.conditionedPanels == 2",

#    wellPanel(
#        selectInput("inSelect3", "Choose a factor variable to plot: ", choices = names(fgold),
#            selected = names(fgold)[1]),
#        selectInput("inSelect4", "Choose a second variable (grouping variable) to plot: ",
#            choices = names(fgold), selected = names(fgold)[2])
#    )
#),

mainPanel(
    tabsetPanel(
       tabPanel("Numeric Table", tableOutput("table1"), value = 1),
       tabPanel("Factor Table", tableOutput("table2"), value = 2),
       tabPanel("Numeric Plot", plotOutput("plot1"), value = 1),
       tabPanel("Factor Plot", plotOutput("plot2"), value = 2)
    ),
    id = "conditionedPanels"
    )
))

