

options(shiny.trace=F)
quant80 = function(x) quantile(x,0.8)
quant20 = function(x) quantile(x,0.2)

rpivotAddin <- function() {
  library(shiny)
  library(rpivotTable)
  library(miniUI)
  library(whisker)
  library(shinyAce)
  library(rstudioapi)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(DT)

  ui <- miniPage(

    # css hack to provide space for a select input in the gadgetTitleBar
    tags$head(tags$style(HTML("
      .gadget-title .shiny-input-container {
        position: relative;
        height: 30px;
        margin: 6px 10px 0;
        z-index: 10;
      }"))),

    # a nasty hack to fix AceEditor bug that updated content wont display if updated while aceeditor invisible
    tags$head(tags$script('Shiny.addCustomMessageHandler("resizeACE",
      function(message) {
        var $el = $("#rcode");
        var editor = $el.data("aceEditor");
        editor.resize();
      })')),

    # another hack - this time to fix bug in miniUI that prevents observing tabpanel changes
    tags$head(tags$script('$(document).ready(function() {
      $("#gadgetTabstrip").addClass("nav");
    })')),

    gadgetTitleBar("Pivot Table Gadget",
      left=miniTitleBarButton("done", "Done", primary=T),
      right=selectInput("dataset", NULL, choices = getDataFrames())),

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
          conditionalPanel(condition='output.getOutputType=="DataTable"', dataTableOutput("datatable")),
          conditionalPanel(condition='output.getOutputType=="Plot"', plotOutput("plot")),
          conditionalPanel(condition='output.getOutputType=="Text"', verbatimTextOutput("rtext")),
          conditionalPanel(condition='output.getOutputType=="Undefined"', h3("No R conversion defined for this setting"))
        )
      ),

      miniTabPanel(
        "R Code",
        icon = icon("bars"),
        miniContentPanel(
          checkboxInput("spread","Spread", value=F),
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

    values <- reactiveValues(outputType="Undefined")

    # Outputs ==================================

    output$mypivot <- renderRpivotTable({
      rpivotTable(getSelectedDF(), onRefresh = htmlwidgets::JS("function(config) { Shiny.onInputChange('myPivotData', config); }")
      )
    })

    output$table = renderTable({
      eval(parse(text=input$rcode))
    })

    output$plot = renderPlot({
      eval(parse(text=input$rcode))
    })

    output$rtext = renderPrint({
      print(eval(parse(text=input$rcode)))
    })

    output$datatable = DT::renderDataTable(datatable(eval(parse(text=input$rcode))) %>%
        formatStyle('n',  background = styleColorBar(eval(parse(text=input$rcode))$n, 'steelblue'))
      )

    # EVENTS ==================================

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

    observeEvent(input$done, {
      stopApp(TRUE)
    })

    # hack to force aceeditor to display contents - update content wont update display if called while div is not visible
    observeEvent(input$gadgetTabstrip, {
      if (input$gadgetTabstrip == "R Code")
      {
        session$sendCustomMessage("resizeACE", list())
      }
    })

    # REACTIVES =================================

    getSelectedDF <- reactive({
      eval(parse(text = input$dataset))
    })

    output$getOutputType <- reactive({
      values$outputType
    })

    # getR <- reactive({
    observe({

      groups = c(unlist(input$myPivotData$rows), unlist(input$myPivotData$cols))
      groups.n = length(groups)

      if (groups.n==0) { # nothing selected = quit
        updateAceEditor(session,  "rcode", "# R code will appear here")
        return()
      }
      else
        template="# No R code for this pivotTable setting" # default template

      wdata = list(
        df           = input$dataset,
        groupby      = paste(groups, collapse=","),
        groupbyPlus  = paste(groups, collapse="+"),
        group1       = groups[1],
        group2       = ifelse(groups.n > 1, groups[2], F),
        group3       = ifelse(groups.n > 2, groups[3], F),
        vals         = paste(input$myPivotData$vals, collapse=","),
        agg          = c("mean","min","max","sum", "quant80", "quant20")[match(input$myPivotData[["aggregatorName"]], c("Average","Minimum","Maximum","Sum"))],
        renderer     = "Undefined",
        rown         = length(input$myPivotData$rows),
        coln         = length(input$myPivotData$cols),
        spread       = input$spread
      )

      if (input$myPivotData$rendererName %in% c("Table", "Table Barchart")) {
        if (input$myPivotData[["aggregatorName"]] == "Count") {
          if (wdata$coln + wdata$rown >2) wdata$renderer = "Text" else wdata$renderer="Table"
          template = whisker.render(tmplTableCount, wdata)
        }
        else if (!is.na(wdata$agg)) {
          wdata$renderer="Table"
          template = whisker.render(tmplTableAgg, wdata)
        }
        if (input$myPivotData$rendererName == "Table Barchart")
          wdata$renderer="DataTable"
      }
      else if (input$myPivotData$rendererName %in% c("Bar Chart","Stacked Bar Chart")) {
        wdata$bar = (input$myPivotData$rendererName == "Bar Chart")
        wdata$renderer = "Plot"
        if (input$myPivotData[["aggregatorName"]] == "Count") {
          template = whisker.render(tmplBarCount, wdata) # simple bar
        }
        else if (input$myPivotData[["aggregatorName"]] %in% c("Average", "Minimum", "Maximum", "Sum")) {
          template = whisker.render(tmplBarAgg, wdata) # simple bar
        }
      }

      updateAceEditor(session,  "rcode", template)
      values$outputType = wdata$renderer

    })

    outputOptions(output, 'getOutputType', suspendWhenHidden=FALSE)

  } # server

  runGadget(shinyApp(ui, server), viewer = paneViewer())
}
