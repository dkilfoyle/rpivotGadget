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

      miniTabPanel("Pivot", icon = icon("table"),
        miniContentPanel(
          rpivotTableOutput("mypivot")
        )
      ),

      miniTabPanel("Setup", icon = icon("bars"),
        miniContentPanel(
          fillRow(
            fillCol(
              selectInput("dataset", "Dataframe:", choices=getDataFrames()),
              verbatimTextOutput("pivotRefresh"),
              verbatimTextOutput("rcode")
            )
          )
        )
      )

    )
  )

  server <- function(input, output, session) {

    getSelectedDF <- reactive({
      eval(parse(text=input$dataset))
    })

    output$mypivot <- renderRpivotTable({
     rpivotTable(getSelectedDF(), onRefresh=htmlwidgets::JS("function(config) { Shiny.onInputChange('myPivotData', config); }"))
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

