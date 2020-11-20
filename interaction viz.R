library(tidyverse)
library(shiny)
library(plotly)

# ---------- a shiny app for visulizing interactions in regression ----------
ui <- fluidPage(
  sidebarLayout(
    position = "right",
    sidebarPanel(
      h4("Toggle Coefficients"),
      sliderInput(
        inputId = "b0",
        label = "Intercept",
        min = -2,
        max = 2,
        value = 0,
        step = 0.1
      ),
      sliderInput(
        inputId = "b1",
        label = "X1",
        min = -2,
        max = 2,
        value = 0,
        step = 0.1
      ),
      sliderInput(
        inputId = "b2",
        label = "X2",
        min = -2,
        max = 2,
        value = 0,
        step = 0.1
      ),
      sliderInput(
        inputId = "b3",
        label = "Interaction (X1*X2)",
        min = -2,
        max = 2,
        value = 0,
        step = 0.1
      ),
      sliderInput(
        inputId = "std.err",
        label = "Residual standard error",
        min = 0,
        max = 5,
        value = 1,
        step = 1
      )
    ),
    mainPanel(
      plotlyOutput({"plot"}, height = "500px")
    )
  )
)

server <- function(input, output, session){
  tmp <- expand.grid(seq(-5, 5), seq(-5, 5))
  x1 <- tmp[, 1]
  x2 <- tmp[, 2]
  
  vals <- reactiveValues()
  observe({
    b0 <- input$b0
    b1 <- input$b1
    b2 <- input$b2
    b3 <- input$b3
    z1 <- b0 + x1*b1 + x2*b2 + x1*x2*b3
    vals$z2 <- z1 + rnorm(121, 0, input$std.err)
    vals$z1 <- matrix(z1, nrow = 11L, ncol = 11L)
  })
  
  output$plot <- renderPlotly({
    plot_ly()%>%
      add_surface(
        x = seq(-5, 5),
        y = seq(-5, 5),
        z = vals$z1,
        type = "surface",
        colorscale = list(c(0, 1), c("lavender", "indigo"))
      )%>%
      add_trace(
        x = x2, 
        y = x1, 
        z = vals$z2, 
        type = "scatter3d", 
        mode = "markers",
        opacity = 0.7,
        marker = list(color = "silver")
      )%>%
      layout(
        scene = list(
          xaxis = list(title = "X2", range = c(5, -5)),
          yaxis = list(title = "X1", range = c(5, -5)),
          zaxis = list(title = "Y"),
          margin = list(b = 50)
        ),
        showlegend = FALSE
      )%>%
      hide_colorbar()
  })
}

shinyApp(ui, server)
