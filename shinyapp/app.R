library(shiny)
library(shinyWidgets)

ui <- shinyUI(
  fluidPage(
    shinyWidgets::panel(
      fluidRow(
        column(12, align="center",
               actionButton("rmd", "Create Workout"),
               downloadButton("report", "Download WOD")
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
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      

      
      withMathJax(includeHTML(rmarkdown::render("create_dailyWorkout.Rmd",
                                                output_file = file,
                                                envir = new.env(parent = globalenv()))))
      
    }
  )

  
}


shinyApp(ui, server) 
