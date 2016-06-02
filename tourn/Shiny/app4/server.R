library(shiny)

grapevine <- read.csv("data/bronze.csv")
gold <- read.csv("data/gold.csv")
goldPlus <- read.csv("data/goldPlusElim.csv")
platinum <- read.csv("data/danieldf.csv")

shinyServer(function(input, output, session) {
    
    datasetInput <- reactive({
        switch(input$dataset,
            "Grapevine" = grapevine,
            "Gold" = gold,
            "Gold Plus" = goldPlus,
            "Platinum" = platinum,
            )
      })

      output$table <- renderTable({
          datasetInput()
      })
      
      output$downloadData <- downloadHandler(
      filename = function() { paste(input$dataset) },
      content = function(file) {
          write.csv(datasetInput(), file)
      }
      )
  })
