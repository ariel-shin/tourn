#app8
library(shiny)
library(ggplot2)
grapevine <- read.csv("data/grapevine2.csv")
gold <- read.csv("data/gold2.csv")
goldPlusBid <- read.csv("data/goldPlusBid.csv")
platinum <- read.csv("data/danieldf.csv")

shinyServer(function(input, output, session) {

dataInput <- reactive({
    switch(input$dataset,
    "Grapevine" = grapevine,
    "Gold" = gold,
    "Gold Plus" = goldPlusBid,
    "Platinum" = platinum,
    )
})

#numeric variable 1 dropdown menu
output$Box1 <- renderUI({
    data <- dataInput()
    nums00 <- sapply(data, is.numeric)
    ndat <- data[,nums00]
    
    selectInput("inSelect", "Choose a numeric variable", choices = names(ndat), selected = names(ndat)[1]) #preset is seed
})

#numeric variable 2 dropdown menu
output$Box2 <- renderUI({
    data <- dataInput()
    nums00 <- sapply(data, is.numeric)
    ndat <- data[,nums00]
    
    selectInput("inSelect2", "Choose a second numeric variable", choices = names(ndat), selected = names(ndat)[6]) #preset is numGender
})

#numeric variable 2 checkboxes
output$Box2B <- renderUI({
    dat <- dataInput()
    var2 <- dat[input$inSelect2]
    mkc <- var2[,1]
    unirows <- sort(unique(mkc))
    checkboxGroupInput('nshow_vars', 'Choose two levels of the second variable to compare', unirows, selected = unirows[c(1,2)])
})

#factor variable 1 dropdown menu
output$Box3 <- renderUI({
    datb <- dataInput()
    nums01 <- sapply(datb, is.factor)
    fdat <- datb[,nums01]
    
    selectInput("inSelect3", "Choose a factor variable", choices = names(fdat), selected = names(fdat)[5]) #preset is gender
})

#factor variable 2 dropdown menu
output$Box4 <- renderUI({
    datb <- dataInput()
    nums01 <- sapply(datb, is.factor)
    fdat <- datb[,nums01]
    
    selectInput("inSelect4", "Choose a second factor variable", choices = names(fdat), selected = names(fdat)[10]) #preset is facGotBid
})

#factor variable 2 checkboxes
output$Box4B <- renderUI({
    dat <- dataInput()
    var2 <- dat[input$inSelect4]
    mkc <- var2[,1]
    unirows <- sort(levels(mkc))
    checkboxGroupInput('fshow_vars', 'Choose two levels of the second variable to compare', unirows, selected = unirows[c(1,2)])
})

#the go (create tables and graph) button
go <- eventReactive(input$vreateg, {
    table1()
    table2()
    plot1()
    plot2()
})

#displays a table of numeric variables
output$table1 <- renderTable({
    #We check to see if a dataframe was submitted, otherwise ERROR
    if(is.null(input$dataset)) return(NULL)
    #We make dat our dataframe
    dat <- dataInput()
    #we make dat a smaller dataframe that only includes numeric columns
    nums <- sapply(dat, is.numeric)
    dat[,nums]
})

#displays a table of factor variables
output$table2 <- renderTable({
    #We check to see if a dataframe was submitted, otherwise ERROR
    if(is.null(input$dataset)) return(NULL)
    #We make dat our dataframe
    dat <- dataInput()
    #we make dat a smaller dataframe that only includes factor columns
    nums <- sapply(dat, is.factor)
    dat[,nums]
    
})

#n -- numeric
#f -- factor

#The surpose of nSplitData1 and nSplitData2 is to create two smaller dataframe
#the first step is to only include numeric columns
#Then we only include columns of the first and variable selected
#nSplitData1 creates a subset of the first column in which the second column has the value of the first checkbox
#nSplitData2 creates a subset of the first column in which the second column has the value of the second checkbox

nSplitData1 <- reactive ({
    if(is.null(input$dataset)) return(NULL)
    #We make dat our dataframe that only includes numeric columns
    dat <- dataInput()
    nums <- sapply(dat, is.numeric)
    dat <- dat[,nums]
    
    #We create a smaller dataframe with only the two columns selected
    dat1 <- dat[,c(input$inSelect2, input$inSelect), drop = FALSE]
    
    #We create an even smaller dataframe that takes when the grouping variable's numeric value is equal to the first instance
    #eg if the grouping variable is gender and the other variable is seed
    #This would make the gender variable numeric and just take which variable is equal to the checbox
    #Following our example, this would create a new dataframe with seed and when gender == checkbox
    dat1 <- subset(dat1, as.numeric(dat1[[1]]) == input$nshow_vars[1])
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
    dat1 <- subset(dat1, as.numeric(dat1[[1]]) == input$nshow_vars[2])
    dat1 <- droplevels(dat1)
    
    dat1 <- dat1[,c(input$inSelect), drop = FALSE]
})

#same as nSplitData except this splits data for factor variables
fSplitData1 <- reactive ({
    #We check to see if a dataframe was submitted, otherwise ERROR
    if(is.null(input$dataset)) return(NULL)
    #We make dat our dataframe
    dat <- dataInput()
    nums <- sapply(dat, is.factor)
    dat <- dat[,nums]
    dat1 <- dat[,c(input$inSelect4, input$inSelect3), drop = FALSE]


    dat1 <- subset(dat1, dat1[[1]] == input$fshow_vars[1])
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
    dat1 <- subset(dat1, dat1[[1]] == input$fshow_vars[2])
    dat1 <- droplevels(dat1)
    
    dat1 <- dat1[,c(input$inSelect3), drop = FALSE]
})

output$plot1 <- renderPlot ({
    #creates density plots
    aplot <- density(as.numeric(nSplitData1()[,1]),na.rm = TRUE)
    bplot <- density(as.numeric(nSplitData2()[,1]),na.rm = TRUE)
    
    if(max(aplot$y) >= max(bplot$y))
    {
        plot(aplot, col = "red", main = "Plot of Two Numeric Variables", xlab = input$inSelect)
        lines(bplot, col = "blue")
    }
    
    else if(max(aplot$y) < max(bplot$y))
    {
        plot(bplot, col = "blue", main = "Plot of Two Numeric Variables", xlab = input$inSelect)
        lines(aplot, col = "red")
    }

    legend("topright", inset = .01, c( paste(input$inSelect2, " == ", input$nshow_vars[1] ), paste(input$inSelect2, " == ", input$nshow_vars[2] )), lty = c(1,1), lwd = c(1,1), col = c("red", "blue"))
    
})

output$plot2 <- renderPlot ({


    a <- max(table(as.numeric(unlist(fSplitData1()))))
    b <- max(table(as.numeric(unlist(fSplitData2()))))

    if (a >= b)
    {
        plot(fSplitData1(), col = rgb(1,0,0,0.5), main = "Plot of Two Factor Variables", xlab = input$inSelect3) #creates histograms of the first grouping variable
        
        par(new = TRUE)
        plot(fSplitData2(), col = rgb(0,0,1,0.5), add = T, axes = FALSE) #creates histograms of the second grouping variable
    }
    
    else if (a < b)
    {
       plot(fSplitData2(), col = rgb(0,0,1,0.5), main = "Plot of Two Factor Variables", xlab = input$inSelect3) #creates histograms of the second grouping variable

        par(new = TRUE)
        plot(fSplitData1(), col = rgb(1,0,0,0.5), add = T, axes = FALSE) #creates histograms of the first grouping variable
   }
    
    legend("bottom", c( paste(input$inSelect4, " == ", input$fshow_vars[1] ), paste(input$inSelect4, " == ", input$fshow_vars[2] )), xpd = TRUE, horiz = TRUE, inset = c(0,0), bty = "n", pch = c(15, 15), col = c("red", "blue"))


})

})