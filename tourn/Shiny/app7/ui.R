#app6
library(shiny)
gold <- read.csv("data/gold2.csv")

#creates a subset of the pre-selected dataframe, gold, that only contains columns with numeric variables
num1 <- sapply(gold, is.numeric)
ngold <- gold[,num1]

#creates a subset of columns with factor variables
num2 <- sapply(gold, is.factor)
fgold <- gold[,num2]

shinyUI(pageWithSidebar(
headerPanel("Analyzing Debate Trends"),

sidebarPanel(
wellPanel( #creates a box inside the box
        selectInput(inputId = "dataset", label = "Choose a file:",
        choices = c("Grapevine", "Gold", "Platinum"),
        selected = "Gold",
        ),

actionButton("createg", "Create Tables and Graphs") #creates a button to display
    ),

    conditionalPanel(
        condition = "input.conditionedPanels == 'Numeric Plot'",
        wellPanel(
            uiOutput("Box1"), #numeric variable 1
            uiOutput("Box2"), #numeric variable 2
            uiOutput("Box2B") #checkboxes for variable 2
        )
    ),

    conditionalPanel(
        condition = "input.conditionedPanels == 'Factor Plot'",
        wellPanel(
            uiOutput("Box3"), #factor variable 1
            uiOutput("Box4"), #factor variable 2
            uiOutput("Box4B") #checkboxes for variable 2
        )
    )
),

mainPanel(
    tabsetPanel(
       tabPanel("Numeric Table", tableOutput("table1"), helpText("Table of Numeric Variables")),
       tabPanel("Factor Table", tableOutput("table2"), helpText("Table of Factor Variables")),
       tabPanel("Numeric Plot", plotOutput("plot1"), helpText("Plot of Numeric Variables")),
       tabPanel("Factor Plot", plotOutput("plot2"), helpText("Plot of Numeric Variables")),
        id = "conditionedPanels"

        )
    )
))

