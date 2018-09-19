###
### The gauge module renders a gauge with a title and statistics below the gauge
###
piesModuleUI <- function(id){

  # set namespace via id
  ns <- NS(id)

  tagList(uiOutput(ns("Pies")))
}

# module server
piesModule <- function(input, output, session, moduleData){

  ns <- session$ns

  #### Render pies ----
  output$Pies <- renderUI({

    L <- list(
      br(),

      fluidRow(
        column(3,
               h4("Oncology", style = "text-align:center"),
               FrissC3PieChartOutput(ns("PieOncology"))
        ),
        column(3,
               h4("CV", style = "text-align:center"),
               FrissC3PieChartOutput(ns("PieCV"))
        ),
        column(3,
               h4("MS", style = "text-align:center"),
               FrissC3PieChartOutput(ns("PieMS"))
        ),
        column(3, style = "overflow: hidden;",
               h4("PAH", style = "text-align:center"),
               FrissC3PieChartOutput(ns("PiePAH"))
        )
      )
    )

    return(L)

  })

  # pie charts

  PieWidth <- "50%"

  output$PieOncology <-  renderFrissC3PieChart({

    lstData <- moduleData()
    FrissC3PieChart(lstData$OncologyHours,height=250,width = PieWidth,maxGroups=5,legendPosition='right',dataHidden=diff)
  })

  output$PieCV <-  renderFrissC3PieChart({

    lstData <- moduleData()
    FrissC3PieChart(lstData$CVHours,height=250,width = PieWidth,maxGroups=5,legendPosition='right')
  })

  output$PieMS <-  renderFrissC3PieChart({

    lstData <- moduleData()
    FrissC3PieChart(lstData$MSHours,height=250,width = PieWidth, maxGroups=5,legendPosition='right')
  })

  output$PiePAH <-  renderFrissC3PieChart({

    lstData <- moduleData()
    FrissC3PieChart(lstData$PAHHours,height=250,width = PieWidth, maxGroups=5,legendPosition='right')
  })

  returnItems <- reactive({
    # This check makes sure the filter app has been fully loaded
    # This way we prevent an unnecessary trigger
    if(is.null(input$PieOncology))
      return(NULL) 
    else
      return(list(PieOncology = input$PieOncology,PieCV = input$PieCV,PieMS = input$PieMS,PiePAH = input$PiePAH))
  })

  return(returnItems)

}
