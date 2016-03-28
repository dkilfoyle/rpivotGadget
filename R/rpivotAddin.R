library(shiny)
library(rpivotTable)
library(miniUI)
library(whisker)
library(shinyAce)
library(rstudioapi)
library(ggplot2)
library(dplyr)

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
        "R Preview",
        icon = icon("eye"),
        miniContentPanel(
          conditionalPanel(condition='output.getOutputType=="Table"', tableOutput("table")),
          conditionalPanel(condition='output.getOutputType=="Plot"', plotOutput("plot")),
          conditionalPanel(condition='output.getOutputType=="Text"', verbatimTextOutput("rtext"))
        )
      ),

      miniTabPanel(
        "R Code",
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

    ) # minitabstrippanel
  ) # minipage

  server <- function(input, output, session) {



    # Outputs

    output$mypivot <- renderRpivotTable({
      rpivotTable(
        getSelectedDF(),
        onRefresh = htmlwidgets::JS("function(config) { Shiny.onInputChange('myPivotData', config); }")
      )
    })

    output$table = renderTable({
      code = getR()$code
      x=eval(parse(text=code))
      x
    })

    output$plot = renderPlot({
      code = getR()$code
      x=eval(parse(text=code))
      x
    })

    output$rtext = renderPrint({
      code = getR()$code
      print(eval(parse(text=code)))
    })

    # EVENTS

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
      if (input$gadgetTabstrip == "R Code") {
        # hack to fix ace editor not firing change event when update is called but editor not visible
        # instead force update only once the aceeditor containing tab is activated
        # TODO: try outputOptions - suspendWhenHidden = false
        updateAceEditor(session,  "rcode", getR()$code)
      }
    })

    observeEvent(input$done, {
      stopApp(TRUE)
    })

    # REACTIVES

    getSelectedDF <- reactive({
      eval(parse(text = input$dataset))
    })

    output$getOutputType = reactive({
      getR()$wdata$renderer
    })

    getR = reactive({
      template=NULL

      if (length(input$myPivotData$rows) + length(input$myPivotData$cols) == 0) {
        # nothing selected = quit
        return (NULL)
      }

      wdata = list(
        df=input$dataset,
        groupby=paste(c(unlist(input$myPivotData$rows), unlist(input$myPivotData$cols)), collapse=","),
        groupbyPlus=paste(c(unlist(input$myPivotData$rows), unlist(input$myPivotData$cols)), collapse="+"),
        group1 = c(unlist(input$myPivotData$rows), unlist(input$myPivotData$cols))[1],
        group2 = c(unlist(input$myPivotData$rows), unlist(input$myPivotData$cols))[2],
        group3 = c(unlist(input$myPivotData$rows), unlist(input$myPivotData$cols))[3],
        vals=paste(input$myPivotData$vals, collapse=","),
        agg = c("mean","min","max","sum")[match(input$myPivotData[["aggregatorName"]], c("Average","Minimum","Maximum","Sum"))],
        renderer = "Table",
        rown = length(input$myPivotData$rows),
        coln = length(input$myPivotData$cols)
      )

      if (input$myPivotData$rendererName == "Table") {
        if (input$myPivotData[["aggregatorName"]] == "Count") {
          if (wdata$coln + wdata$rown <=2)
            template = whisker.render(tmplTableCount, wdata)
          else
          {
            wdata$renderer = "Text"
            template = whisker.render(tmplTableFtable, wdata)
          }
        }
        else if (input$myPivotData[["aggregatorName"]] %in% c("Average", "Minimum", "Maximum", "Sum")) {
          template = whisker.render(tmplTableAgg, wdata)
        }
      }
      else if (input$myPivotData$rendererName %in% c("Bar Chart","Stacked Bar Chart")) {
        wdata$bar = (input$myPivotData$rendererName == "Bar Chart")
        wdata$renderer = "Plot"
        if (input$myPivotData[["aggregatorName"]] == "Count") {
          if (!is.na(wdata$group3))
            template = whisker.render(tmplBarCount3, wdata) # faceted dodged
          else if (!is.na(wdata$group2))
            template = whisker.render(tmplBarCount2, wdata) # dodged
          else
            template = whisker.render(tmplBarCount1, wdata) # simple bar
        }
        else if (input$myPivotData[["aggregatorName"]] %in% c("Average", "Minimum", "Maximum", "Sum")) {
          if (!is.na(wdata$group3))
            template = whisker.render(tmplBarAgg3, wdata) # faceted dodged
          else if (!is.na(wdata$group2))
            template = whisker.render(tmplBarAgg2, wdata) # dodged
          else
            template = whisker.render(tmplBarAgg1, wdata) # simple bar
        }
      }

      outputOptions(output, 'getOutputType', suspendWhenHidden=FALSE)

      return(list(code=template, wdata=wdata))
    })



  } # server

  runGadget(shinyApp(ui, server), viewer = paneViewer())
}
