library(shiny)
library(rpivotTable)
library(DT)
library(miniUI)
library(whisker)
library(shinyAce)
library(rstudioapi)

options(shiny.trace=F)

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
{{^bar}}
{{df}} %>%
  group_by({{groupby}}) %>%
  summarise(n=n())
{{/bar}}{{#bar}}
ggplot({{df}}, aes(x={{groupby}})) +
  geom_bar()
{{/bar}}
'

tmplSummariseAgg =
  '# selected = {{groupby}}, {{vals}}
{{df}} %>%
  group_by({{groupby}}) %>%
  summarise(n=n(), {{agg}}={{agg}}({{vals}}, na.rm=T)) {{#bar}} %>%
    ggplot2(aes(x={{groupby}}, y={{agg}})) + geom_bar(stat="identity") {{/bar}}'

rpivotAddin <- function() {
  ui <- miniPage(

    tags$head(tags$style(HTML("
      .gadget-title .shiny-input-container {
        position: relative;
        height: 30px;
        margin: 6px 10px 0;
        z-index: 10;
      }"))),

    gadgetTitleBar("Pivot Table Gadget", left=miniTitleBarButton("done", "Done", primary=T), right=selectInput("dataset",NULL, choices = getDataFrames())),

    miniTabstripPanel(id = "gadgetTabstrip",

      miniTabPanel(
        "Pivot",
        icon = icon("table"),
        miniContentPanel(

          rpivotTableOutput("mypivot")
          )
      ),

      miniTabPanel(
        "Setup",
        icon = icon("bars"),
        miniContentPanel(
          fillCol(
            flex = c(NA, 1),
            verbatimTextOutput("pivotRefresh"),
            aceEditor("rcode", "# R code will appear here", mode = "r", height="100%")
          )
        ),
        miniButtonBlock(
          actionButton("code2clipboard", "Copy to clipboard", icon("clipboard")),
          actionButton("code2doc", "Copy to editor", icon("align-left")),
          actionButton("code2console", "Execute in console", icon("play"))
        )
      )
    )
  ) # minipage

  server <- function(input, output, session) {
    getSelectedDF <- reactive({
      eval(parse(text = input$dataset))
    })

    output$mypivot <- renderRpivotTable({
      rpivotTable(
        getSelectedDF(),
        onRefresh = htmlwidgets::JS("function(config) { Shiny.onInputChange('myPivotData', config); }")
      )
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

    observeEvent(input$code2doc, {
      insertText(input$rcode)
    })

    observeEvent(input$code2console, {
      sendToConsole(input$rcode)
      stopApp(TRUE)
    })

    observeEvent(input$gadgetTabstrip, {
      if (input$gadgetTabstrip == "Setup") {
        # hack to fix ace editor not firing change event when update is called but editor not visible
        updateAceEditor(session,  "rcode", getRcode())
      }
    })

    # observe({
    #   updateAceEditor(session,  "rcode", getRcode())
    #   # TODO: fix aceeditor offscreen update problem. ? need to call editor.resize when tab shown
    # })

    getRcode = reactive({
      template=NULL
      if (length(input$myPivotData[["rows"]]) == 1) {

        if (input$myPivotData[["aggregatorName"]] == "Count") {
          template = whisker.render(tmplSummariseN, list(df=input$dataset,
            groupby=input$myPivotData[["rows"]][1],
            bar=(input$myPivotData[["rendererName"]]=="Bar Chart")))
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
      return(template)
    })

    observeEvent(input$done, {
      stopApp(TRUE)
    })

  } # server

  runGadget(shinyApp(ui, server), viewer = paneViewer())
}
