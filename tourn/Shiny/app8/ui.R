#app8
library(shiny)
gold <- read.csv("data/goldPlusBid.csv")

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
        choices = c("Grapevine", "Gold", "Gold Plus", "Platinum"),
        selected = "Gold Plus",
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
       tabPanel("Numeric Table", tableOutput("table1"), helpText("This table is a subset of the selected dataframe that only includes numeric variables. Some factor variables have been converted to numbers and have the prefix num. Any of these variables can be plotted as a the Numeric Plot.")),
       tabPanel("Factor Table", tableOutput("table2"), helpText("This table is a subset of the selected dataframe that only includes factor or categorical variables. Some numeric variables have been converted to categorical variables and have the prefix fac. Any of these variables can be plotted as a the Factor Plot.")),
       tabPanel("Numeric Plot", plotOutput("plot1"), helpText("These plots are created through two density plots, which are created from two dataframes. With the numeric table, I subset the dataframe into two columns. The first column is the first variable selected and the second column is the second variable selected. I then create two dataframes. The first dataframe includes the rows in which the second column is equal to the first checkbox clicked. For instance, if my first column was seed and my second column was phouse and I clicked phouse = 1, my dataframe would only include the rows of the first and second column in which phouse = 1. We do the same for the second dataframe except we only include the rows of both columns in which phouse = 0. Density plots are useful because they do not rely on the quantity of a variable, e.g it does not rely on how many female debaters there are. We use the mean performance of a female debater to determine the curve. Moreover, the reason why we cannot plot all the variables in one plot is becuase you cannot plot cateogorical variables, such as Male and Female, with Numerical Variables, such as 1 and 0. Thus some categorical variables have been transformed to numerical variables and have the prefix num. Additionally, for those unfamiliar with numerical variables, 1 generally means yes and 0 usually generally no. For example, for the phouse variable, 1 means the student is from a powerhouse school and 0 means the student is not from a powerhouse house school. In terms of gender, 1 is male and 0 is female.")),
       tabPanel("Factor Plot", plotOutput("plot2"), helpText("Factor variables are plotted through simple plots. We use the same method of creating two dataframes that we used from Numeric Variable then we create two simple transparent plots on top of eachother. Some numerical variables have been transformed to categorical variables and have the prefix fac.")),
        id = "conditionedPanels"

        )
    )
))

