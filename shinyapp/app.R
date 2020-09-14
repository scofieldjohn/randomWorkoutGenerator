library(shiny)
library(shinyWidgets)

ui <- shinyUI(
  fluidPage(
    shinyWidgets::panel(
      fluidRow(
        column(12, align="center",
               actionButton("rmd", "Create Workout")
        )
      ))
    
    ,uiOutput("test")
  )
)

server <- function(input, output) {
  observeEvent(input$rmd, {
    output$test <- renderUI({
      withMathJax(includeHTML(rmarkdown::render("create_dailyWorkout.Rmd")))
    })
  })
}


shinyApp(ui, server) 
