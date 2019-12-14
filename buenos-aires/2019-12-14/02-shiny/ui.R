## ui.R ##

shinyUI(
  function(request) {
    fluidPage(
      theme = "css/custom.min.css",
      
      fluidRow(
        div(
          id = "controls",
          column(
            3,
            # Controls ----------------------------------------------------------------
            
            selectInput(
              "y",
              "Year:",
              choices = available_years_min:available_years_max,
              selected = NULL,
              selectize = TRUE
            ),
            
            selectInput(
              "r",
              "Reporter:",
              choices = c("Select", available_reporters_iso),
              selected = NULL,
              selectize = TRUE
            ),
            
            selectInput(
              "p",
              "Partner:",
              choices = c("Select", available_reporters_iso),
              selected = NULL,
              selectize = TRUE
            ),
            
            actionButton("go", "Go!")
          )
        ),
        
        div(
          id = "content",
          column(
            9,
            style = "height:100vh",
            htmlOutput("title", container = tags$h2),
            highchartOutput("exports_treemap_detailed", height = "95%")
          )
        ),
        
        tags$footer(
          tags$link(rel = "shortcut icon", href = "img/favicon.ico")
        )
      )
    )
  }
)
