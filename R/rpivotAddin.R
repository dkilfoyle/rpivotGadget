library(shiny)
library(rpivotTable)
library(DT)
library(miniUI)
library(whisker)
library(shinyAce)
library(rstudioapi)

list_to_string <- function(obj, listname) {
  if (is.null(names(obj))) {
    paste(listname,
      "[[",
      seq_along(obj),
      "]] = ",
      obj,
      sep = "",
      collapse = "\n")
  } else {
    paste(listname,
      "$",
      names(obj),
      " = ",
      obj,
      sep = "",
      collapse = "\n")
  }
}

tmplSummariseN =
  '# selected = {{groupby}}
{{df}} %>%
  group_by({{groupby}}) %>%
  summarise(n=n())'

tmplSummariseAgg =
  '# selected = {{groupby}}, {{vals}}
{{df}} %>%
  group_by({{groupby}}) %>%
  summarise(n=n(), {{agg}}={{agg}}({{vals}}, na.rm=T)) {{#bar}} %>%
    ggplot2(aes(x={{groupby}}, y={{agg}})) + geom_bar(stat="identity") {{/bar}}'

rpivotAddin <- function() {
  ui <- miniPage(
    gadgetTitleBar("Pivot Table Gadget"),

    miniTabstripPanel(
      id = "gadgetTabstrip",

      miniTabPanel(
        "Pivot",
        icon = icon("table"),
        miniContentPanel(rpivotTableOutput("mypivot"))
      ),

      miniTabPanel(
        "Setup",
        icon = icon("bars"),
        miniContentPanel(
          fillCol(
            flex = c(NA, NA, 1),
            selectInput("dataset", "Dataframe:", choices = getDataFrames()),
            verbatimTextOutput("pivotRefresh"),
            aceEditor("rcode", "# R code will appear here", mode = "r", height="100%")
          )
        ),
        miniButtonBlock(
          actionButton("code2clipboard", "Copy to clipboard", icon("clipboard")),
          actionButton("code2console", "Execute in console", icon("play"))
        )
      )
    )
  )

  server <- function(input, output, session) {
    getSelectedDF <- reactive({
      eval(parse(text = input$dataset))
    })

    output$mypivot <- renderRpivotTable({
      rpivotTable(
        getSelectedDF(),
        onRefresh = htmlwidgets::JS("function(config) { Shiny.onInputChange('myPivotData', config); }")
      )
      # updateTabsetPanel(session, "gadgetTabstrip", selected="pivot")
    })

    output$pivotRefresh <- renderText({
      cnames <-
        list("cols", "rows", "vals", "exclusions", "inclusions", "aggregatorName", "rendererName")
      allvalues <- lapply(cnames, function(name) {
        item <- input$myPivotData[[name]]
        if (is.list(item)) {
          list_to_string(item, name)
        } else {
          paste(name, item, sep = " = ")
        }
      })
      paste(allvalues, collapse = "\n")
    })

    observeEvent(input$code2clipboard, {
      writeClipboard(input$rcode)
    })

    observeEvent(input$code2console, {
      sendToConsole(input$rcode)
    })

    observeEvent(input$myPivotData, {

      template=NULL

        if (length(input$myPivotData[["rows"]]) == 1) {

          if (input$myPivotData[["aggregatorName"]] == "Count") {
            template = whisker.render(tmplSummariseN, list(df=input$dataset, groupby=input$myPivotData[["rows"]][1]))
          }

          else if (input$myPivotData[["aggregatorName"]] %in% c("Average", "Minimum", "Maximum", "Sum")) {
            template = whisker.render(tmplSummariseAgg, list(
              df=input$dataset,
              groupby=input$myPivotData[["rows"]][1],
              vals=input$myPivotData[["vals"]][1],
              agg=c("mean","min","max","sum")[match(input$myPivotData[["aggregatorName"]], c("Average","Minimum","Maximum","Sum"))],
              bar=(input$myPivotData[["rendererName"]]=="Bar Chart"))
            )
          }
        }

      if (!is.null(template)) {
        updateAceEditor(session,  "rcode",
          whisker.render(
            template,
            list(
              df = input$dataset,
              groupby = input$myPivotData[["rows"]][1]
            )
          ))
      }
    })

    observeEvent(input$done, {
      stopApp(TRUE)
    })
  }

  runGadget(shinyApp(ui, server), viewer = paneViewer())
}
