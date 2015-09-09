library(shiny)
source("base.R")
shinyServer(
  function(input,output){
    output$plot1<-renderPlot({
      if(input$plot == "VsWeekday"){
        my$plot1
      }
      else
        if(input$plot == "VsMonth"){
          my$plot2
        }
      else
        if(input$plot == "VsYear"){
          my$plot3
        }
      else
        if(input$plot == "VsHour"){
          my$plot4
        } 
    }
    )
    output$den1<-renderImage({
      filename <- str_replace_all(str_replace_all(input$den, ' / ', '-'),'\\s','_')
        myfile <- normalizePath(file.path('./projectImages',
                                            paste(filename, '.png', sep='')))
      list(src = myfile,
           width=1100, height=800,
           alt = "Somethings worng with file path")
    }, deleteFile = FALSE )
  }
  )