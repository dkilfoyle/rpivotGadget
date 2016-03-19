library(shiny)
library(rpivotTable)
library(DT)
library(miniUI)

rpivotAddin <- function() {

  ui <- miniPage(
    gadgetTitleBar("Shiny gadget example"),
    miniTabstripPanel(
      miniTabPanel("Pivot", icon = icon("sliders"),
        miniContentPanel(
          rpivotTableOutput("mypivot")
        )
      ),
      miniTabPanel("Data", icon = icon("table"),
        miniContentPanel(
          DT::dataTableOutput("table")
        )
      )
    )
  )

  server <- function(input, output, session) {

    output$mypivot <- renderRpivotTable({
     rpivotTable(iris, onRefresh=htmlwidgets::JS("function(config) { Shiny.onInputChange('myPivotData', config); }"))
    })

    output$table <- DT::renderDataTable({
      diamonds
    })

    observeEvent(input$done, {
      stopApp(TRUE)
    })
  }

  runGadget(shinyApp(ui, server), viewer = paneViewer())
}

