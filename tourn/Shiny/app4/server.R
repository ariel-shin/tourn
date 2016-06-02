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
    
    #Creates variables for the columns selected
    observe({
        updateSelectInput(session, "inSelect", choices = names(dataInput()), selected = names(dataInput())[1])
        updateSelectInput(session, "inSelect2", choices = names(dataInput()), selected = names(dataInput())[2])
    })

#This creates the "Create" button
  plot1 <- eventReactive(input$createg, {
      splitData1()
      splitData2()
})
  
#We create smaller dataframes here
  splitData1 <- reactive({
      #We check to see if a dataframe was submitted, otherwise ERROR
      if(is.null(input$dataset)) return(NULL)
      #We make dat our dataframe
      dat <- dataInput()
      #We create a smaller dataframe with only the two columns selected
      dat1 <- dat[,c(input$inSelect2, input$inSelect), drop = FALSE]
      
      #We create an even smaller datafram that takes when the grouping variable's numeric value is equal to the first instance
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

#Second function that creates smaller dataframes
  splitData2 <- reactive({
      if(is.null(input$dataset)) return(NULL)
      dat <- dataInput()
      dat1 <- dat[,c(input$inSelect2, input$inSelect), drop = FALSE]
      
      #This is the only line that has changed from SplitData2
      dat1 <- subset(dat1, as.numeric(dat1[[1]]) == 2)
      dat1 <- droplevels(dat1)
      
      dat1 <- dat1[,c(input$inSelect), drop = FALSE]
      })

#This creates our plots
  output$plot1 <- renderPlot({
      plot1() #displays the graph after the create button
      
      #density graphs are not working
      #error: argument 'x' must be numeric
      #dens <- density(splitData1())
      #plot(dens, col = "red")
      #dens2 <- density(splitData2())
      #plot(dens, col = "blue")
      
      #only histograms and lines seem to be working
            plot(splitData1(), col = "red") #creates histograms of the first grouping variable
            lines(splitData2(), col = "blue") #creates histograms of the second grouping variable
        })

})
