# 2017-01-05 in response to Google Groups thread by mattek on 2017-01-04 
# Response from Stephen McDaniel at PowerTrip Analytics
#
#   Topic: Shiny, managing inputs, reset and error 
#             check inputs, with an interactive, ggvis historgram
# Program: app.R 
#    Data: randomly generated
#
# Joe Cheng answered the original question
# I took the angle of simplifying the original code 
# Adding interactive plots via ggvis (vega/D3 plots)
# Adding additional error handling for incorrect clipping
#    boundaries, missing clipping boundaries, making the clip
#    values readable (less precision) and a few other tweaks
# I also left in the simplified, traditional plot, but commented 
#    it out, feel free to try both or the original
#
# License: MIT License
# Attribution, package authors for shiny and ggvis on CRAN.

library(dplyr)
library(ggvis)
library(shiny)

ui <- shinyUI(fluidPage(
  titlePanel("Interactive graph with hover and clipping"),
  
    absolutePanel(
       bottom = 10,
       left = 10,
       HTML('<a href="http://www.powertripanalytics.com/" target="_new"><span style="color:#CC0000">Power</span><span style="color:#004de6">Trip</span> 
          <span style="color:#009933">Analytics</span></a>	 
	       </span></span>') # your organization name/logo here
    ),
    sidebarLayout(
    
    sidebarPanel(
      actionButton('genNewData', 'Generate dataset'),
      br(),
      br(),
      uiOutput('resetable_input_clip'),
      actionButton('clipCurrData', 'Reset data clipping')
    ),
    mainPanel(
      # for traditional graph, uncomment next line
      #plotOutput("plotHist", width = "100%"), 
      ggvisOutput("ggvisPlot1")
    )
  )
))

server <- shinyServer(function(input, output, session) {
  
   summaryStats <- reactiveValues(
      min = NA,
      max = NA
   )

  # generate random dataset
  randomData <- reactive({
     if(input$genNewData > 0) { # start with no data
        # only input$genNewData will trigger execution, a single time per click
        newData <- data.frame(x = rnorm(1000))
        # the summaryStats reactive values don't trigger execution 
        #    as they are being assigned, not called
        summaryStats$min = min(newData$x)
        summaryStats$max = max(newData$x)
        cat(file = stderr(), '\ngenNewData\n')
        return(newData)
     } else return(NULL)
  })
  
  output$resetable_input_clip <- renderUI({
    cat(file = stderr(), 'output$resetable_input_clip\n')
    
    input$clipCurrData
    
    div(
      id = "min_max", #letters[(times %% length(letters)) + 1],
      numericInput(
        'clipMin',
        'Clip data below threshold:',
        value = round(summaryStats$min - 0.0005, 3),
        width = 200,
        step = .1
      ),
      numericInput(
        'clipMax',
        'Clip data above threshold:',
        value = round(summaryStats$max + 0.0005, 3),
        width = 200,
        step = .1
      )
    )
  })
  
# for traditional graph, uncomment next section  
#  output$plotHist <- renderPlot({
#    cat(file = stderr(), 'plotHist \n')
#    cat("input$clipMin", input$clipMin)
#      
#    randData <- randomData()
#    
#    if (is.null(randData$x))
#          return(NULL)
#    else {
#         randData <- randData %>%
#            filter(
#               x > ifelse(is.na(input$clipMin), -Inf, input$clipMin) & 
#               x < ifelse(is.na(input$clipMax), Inf, input$clipMax)
#            )
#      if(nrow(randData) > 0) {
#         return(plot(hist(randData$x)))
#      } else return(NULL)
#    }
#  })
  
  reactive({
     cat(file = stderr(), 'plotHist \n')
    cat("input$clipMin", input$clipMin)
      
    randData <- randomData()
    
    if (is.null(randData$x)) {
         randData <- data.frame(x = 0, y = 0) 
    } else {
         randData <- randData %>%
            filter(
               x > ifelse(is.na(input$clipMin), -Inf, input$clipMin) & 
               x < ifelse(is.na(input$clipMax), Inf, input$clipMax)
            )
    }
    
    if(nrow(randData) > 1) {
       histogram <- randData %>% 
          ggvis(~x) %>% 
          layer_histograms(width=0.2, fill := "#004de6") %>%
          add_axis("x", title = "x (hover over bar for count)", grid = FALSE) %>%
          add_axis("y", title = "Count (n = 1,000)", grid = FALSE) 
    } else {
       histogram <- randData %>% 
          ggvis(~x, ~y) %>%
          layer_points() %>%
          add_axis("x", grid = FALSE) %>%
          add_axis("y", grid = FALSE)
    }
     
    return(histogram %>%
          add_tooltip(function(df){ 
             paste0("n: ", df$stack_upr_ - df$stack_lwr_)
          }))
  }) %>% bind_shiny("ggvisPlot1")
  
})

shinyApp(ui = ui, server = server)
