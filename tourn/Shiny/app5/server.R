#app5
library(shiny)
grapevine <- read.csv("data/bronze.csv")
gold <- read.csv("data/gold.csv")
goldPlus <- read.csv("data/goldPlusElim.csv")
platinum <- read.csv("data/danieldf.csv")

shinyServer(function(input, output, session) {

dataInput <- reactive({
    switch(input$dataset,
    "Grapevine" = grapevine,
    "Gold" = gold,
    "Gold Plus" = goldPlus,
    "Platinum" = platinum,
    )
})

output$Box1 <- renderUI({
    data <- dataInput()
    nums00 <- sapply(data, is.numeric)
    ndat <- data[,nums00]
    
    selectInput("inSelect", "Choose a numeric variable", choices = names(ndat), selected = names(ndat)[1])
})
    
output$Box2 <- renderUI({
    data <- dataInput()
    nums00 <- sapply(data, is.numeric)
    ndat <- data[,nums00]
    
    selectInput("inSelect2", "Choose a second numeric variable", choices = names(ndat), selected = names(ndat)[2])
})

output$Box3 <- renderUI({
    datb <- dataInput()
    nums01 <- sapply(datb, is.factor)
    fdat <- datb[,nums01]
    
    selectInput("inSelect3", "Choose a factor variable", choices = names(fdat), selected = names(fdat)[1])
})

output$Box4 <- renderUI({
    datb <- dataInput()
    nums01 <- sapply(datb, is.factor)
    fdat <- datb[,nums01]
    
    selectInput("inSelect4", "Choose a second factor variable", choices = names(fdat), selected = names(fdat)[2])
})

output$table1 <- renderTable({
    #We check to see if a dataframe was submitted, otherwise ERROR
    if(is.null(input$dataset)) return(NULL)
    #We make dat our dataframe
    dat <- dataInput()
    nums <- sapply(dat, is.numeric)
    dat[,nums]
})

output$table2 <- renderTable({
    #We check to see if a dataframe was submitted, otherwise ERROR
    if(is.null(input$dataset)) return(NULL)
    #We make dat our dataframe
    dat <- dataInput()
    nums <- sapply(dat, is.factor)
    dat[,nums]
    
})

nSplitData1 <- reactive ({
    if(is.null(input$dataset)) return(NULL)
    #We make dat our dataframe
    dat <- dataInput()
    nums <- sapply(dat, is.numeric)
    dat <- dat[,nums]
    
    #We create a smaller dataframe with only the two columns selected
    dat1 <- dat[,c(input$inSelect2, input$inSelect), drop = FALSE]
    
    #We create an even smaller dataframe that takes when the grouping variable's numeric value is equal to the first instance
    #eg if the grouping variable is gender and the other variable is seed
    #This would make the gender variable numeric and just take which variable is equal to 1
    #Following our example, this would create a new dataframe with seed and when gender == 1
    dat1 <- subset(dat1, as.numeric(dat1[[1]]) == 1)
    dat1 <- droplevels(dat1)
    
    #This is the dataframe we return
    #This return the column that the user wanted to see with the grouping variable
    #eg if the grouping variable is gender and the other variable is seed
    #This would just return seed column
    dat1 <- dat1[,c(input$inSelect), drop = FALSE]
})

nSplitData2 <- reactive ({
    if(is.null(input$dataset)) return(NULL)
    #We make dat our dataframe
    dat <- dataInput()
    nums <- sapply(dat, is.numeric)
    dat <- dat[,nums]
    dat1 <- dat[,c(input$inSelect2, input$inSelect), drop = FALSE]

    #This is the only line that has changed from SplitData2
    dat1 <- subset(dat1, as.numeric(dat1[[1]]) == 2)
    dat1 <- droplevels(dat1)
    
    dat1 <- dat1[,c(input$inSelect), drop = FALSE]
})

fSplitData1 <- reactive ({
    #We check to see if a dataframe was submitted, otherwise ERROR
    if(is.null(input$dataset)) return(NULL)
    #We make dat our dataframe
    dat <- dataInput()
    nums <- sapply(dat, is.factor)
    dat <- dat[,nums]
    dat1 <- dat[,c(input$inSelect4, input$inSelect3), drop = FALSE]


    dat1 <- subset(dat1, as.numeric(dat1[[1]]) == 1)
    dat1 <- droplevels(dat1)

    #This is the dataframe we return
    #This return the column that the user wanted to see with the grouping variable
    #eg if the grouping variable is gender and the other variable is seed
    #This would just return seed column
    dat1 <- dat1[,c(input$inSelect3), drop = FALSE]
})

fSplitData2 <- reactive ({
    #We check to see if a dataframe was submitted, otherwise ERROR
    if(is.null(input$dataset)) return(NULL)
    dat <- dataInput()
    nums <- sapply(dat, is.factor)
    dat <- dat[,nums]
    
    dat1 <- dat[,c(input$inSelect4, input$inSelect3), drop = FALSE]


    #This is the only line that has changed from SplitData2
    dat1 <- subset(dat1, as.numeric(dat1[[1]]) == 2)
    dat1 <- droplevels(dat1)
    
    dat1 <- dat1[,c(input$inSelect3), drop = FALSE]
})

output$plot1 <- renderPlot ({
    #plot(mtcars$wt, mtcars$mpg)
    aplot <- density(as.numeric(nSplitData1()[,1]))
    bplot <- density(as.numeric(nSplitData2()[,1]))
    
    if(max(aplot$y) >= max(bplot$y))
    {
        plot(aplot, col = "red")
        lines(bplot, col = "blue")
    }
    
    else if(max(aplot$y) < max(bplot$y))
    {
        plot(bplot, col = "blue")
        lines(aplot, col = "red")
    }
    
    #plot(nSplitData1(), col = "red") #creates histograms of the first grouping variable
    #lines(nSplitData2(), col = "blue") #creates histograms of the second grouping variable
})

output$plot2 <- renderPlot ({
    plot(fSplitData1(), col = "red") #creates histograms of the first grouping variable
    lines(fSplitData2(), col = "blue") #creates histograms of the second grouping variable
})

})