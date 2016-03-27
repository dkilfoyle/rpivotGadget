library(shiny)
library(rpivotTable)
library(DT)
library(miniUI)
library(whisker)
library(shinyAce)
library(rstudioapi)
library(brew)

options(shiny.trace=F)

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
          aceEditor("rcode", "# R code will appear here", mode = "r", height="100%")
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
        # instead force update only once the aceeditor containing tab is activated
        updateAceEditor(session,  "rcode", getRcode())
      }
    })

    getRcode = reactive({
      template=NULL

      pd = input$myPivotData

      if (length(pd$rows) + length(pd$cols) == 0) {
        # nothing selected = quit
        return (NULL)
      }

      wdata = list(
        df=input$dataset,
        groupby=paste(c(unlist(pd$rows), unlist(pd$cols)), collapse=","),
        group1 = c(unlist(pd$rows), unlist(pd$cols))[1],
        group2 = c(unlist(pd$rows), unlist(pd$cols))[2],
        group3 = c(unlist(pd$rows), unlist(pd$cols))[3],
        vals=paste(pd$vals, collapse=","),
        agg = c("mean","min","max","sum")[match(pd[["aggregatorName"]], c("Average","Minimum","Maximum","Sum"))]
      )

      if (pd$rendererName == "Table") {
        if (pd[["aggregatorName"]] == "Count") {
          template = whisker.render(tmplTableCount, wdata)
        }
        else if (pd[["aggregatorName"]] %in% c("Average", "Minimum", "Maximum", "Sum")) {
          template = whisker.render(tmplTableAgg, wdata)

        }
      }
      else if (pd$rendererName == "Bar Chart") {
        if (pd[["aggregatorName"]] == "Count") {
          if (!is.na(wdata$group3))
            template = whisker.render(tmplBarCount3, wdata) # faceted dodged
          else if (!is.na(wdata$group2))
            template = whisker.render(tmplBarCount2, wdata) # dodged
          else
            template = whisker.render(tmplBarCount1, wdata) # simple bar
        }
        else if (pd[["aggregatorName"]] %in% c("Average", "Minimum", "Maximum", "Sum")) {
          if (!is.na(wdata$group3))
            template = whisker.render(tmplBarAgg3, wdata) # faceted dodged
          else if (!is.na(wdata$group2))
            template = whisker.render(tmplBarAgg2, wdata) # dodged
          else
            template = whisker.render(tmplBarAgg1, wdata) # simple bar
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
