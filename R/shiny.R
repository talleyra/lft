
library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Hello Shiny!"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of bins ----
      sliderInput(
        inputId = "n",
        label = "Number of bins:",
        min = 1,
        max = 50,
        value = 30
      )
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Histogram ----
      plotly::plotlyOutput(outputId = "distPlot")
    )
  )
)


server <- function(input, output) {

  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- plotly::renderPlotly({
    error %>%
      select(date, ll, diff, err, rain, niederschlag) %>%
      fill(rain) %>%
      mutate(rr = TTR::EMA(rain, n = input$n)) %>%
      select(-err, -diff, -niederschlag) %>%
      pivot_longer(-date) %>%
      plot_time_series(
        date, value, name,
        .smooth = FALSE
      )
  })
}



shinyApp(ui = ui, server = server)
