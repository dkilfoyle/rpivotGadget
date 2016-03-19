library(shiny)
library(rpivotTable)
library(DT)
library(miniUI)

list_to_string <- function(obj, listname) {
  if (is.null(names(obj))) {
    paste(listname, "[[", seq_along(obj), "]] = ", obj,
      sep = "", collapse = "\n")
  } else {
    paste(listname, "$", names(obj), " = ", obj,
      sep = "", collapse = "\n")
  }
}

rpivotAddin <- function() {

  ui <- miniPage(
    gadgetTitleBar("Pivot Table Gadget"),
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
      ),
      miniTabPanel("PivotData", icon = icon("table"),
        miniContentPanel(
          verbatimTextOutput("pivotRefresh")
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

    output$pivotRefresh <- renderText({

      cnames <- list("cols","rows","vals", "exclusions","inclusions", "aggregatorName", "rendererName")
      # Apply a function to all keys, to get corresponding values
      allvalues <- lapply(cnames, function(name) {
        item <- input$myPivotData[[name]]
        if (is.list(item)) {
          list_to_string(item, name)
        } else {
          paste(name, item, sep=" = ")
        }
      })
      paste(allvalues, collapse = "\n")
    })

    observeEvent(input$done, {
      stopApp(TRUE)
    })
  }

  runGadget(shinyApp(ui, server), viewer = paneViewer())
}

