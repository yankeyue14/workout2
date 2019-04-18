# 


library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Saving/Investing Calculator"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("amount",
                  "Initial Amount",
                  min = 0,
                  max = 100000,
                  value = 1000,
                  step = 500,
                  pre = "$"),
      sliderInput("contrib",
                  "Annual Contribution",
                  min = 0,
                  max = 100000,
                  value = 2000,
                  step = 500,
                  pre = "$"),
      sliderInput("rate",
                  "Return Rate (in %)",
                  min = 0,
                  max = 20,
                  value = 5,
                  step = 0.1,
                  pre = "%"),
      sliderInput("growth",
                  "Growth Rate (in %)",
                  min = 0,
                  max = 20,
                  value = 2,
                  step = 0.1,
                  pre = "%"),
      sliderInput("years",
                  "Years",
                  min = 0,
                  max = 50,
                  value = 20,
                  step = 1),
      selectInput('facet', 'Facet?', choices = c("No", "Yes"), selected = "No")
    ),
    mainPanel(
      plotOutput("plot"),
      tableOutput("table")
    )
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    future_value <- function(amount, rate, years) {
      return(amount*(1+rate/100)**years)
    }
    annuity <- function(contrib, rate, years) {
      return(contrib*(((1+rate/100)**years)-1)/(rate)/100)
    }
    growing_annuity <- function(contrib, rate, growth, years) {
      return(contrib*(((1+rate/100)**years)-(1+growth)**years)/(rate/100-growth/100))
    }
    no_contrib <- c(0:input$years)
    for (i in no_contrib) {
      no_contrib[i+1] = future_value(input$amount, input$rate, i)
    }
    fixed_contrib <- c(0:input$years)
    for (i in fixed_contrib) {
      fixed_contrib[i+1] = (future_value(input$amount, input$rate, i)+annuity(input$contrib, input$rate, i))
    }
    growing_contrib <- c(0:input$years)
    for (i in growing_contrib) {
      growing_contrib[i+1] = future_value(input$amount, input$rate, i)+growing_annuity(input$contrib, input$rate, input$growth, i)
    }
    years <- c(0:input$years)
    x_values <- years
    y1 <- no_contrib
    y2 <- fixed_contrib
    y3 <- growing_contrib
    plot(x_values, y1, type='l', col='red', xlab='Years', ylab='Futrue Values', ylim = c(1000, 10000))
    lines(x_values, y2, col="blue", type="l")
    lines(x_values, y3, col="green", type="l")
    legend('topleft', legend = c('no_contirb', 'fixed_contrib', 'growing_contrib'))
  })
  output$table <- renderTable({
    future_value <- function(amount, rate, years) {
      return(amount*(1+rate/100)**years)
    }
    annuity <- function(contrib, rate, years) {
      return(contrib*(((1+rate/100)**years)-1)/(rate)/100)
    }
    growing_annuity <- function(contrib, rate, growth, years) {
      return(contrib*(((1+rate/100)**years)-(1+growth/100)**years)/(rate/100-growth/100))
    }
    no_contrib <- c(0:input$years)
    for (i in no_contrib) {
      no_contrib[i+1] = future_value(input$amount, input$rate, i)
    }
    fixed_contrib <- c(0:input$years)
    for (i in fixed_contrib) {
      fixed_contrib[i+1] = (future_value(input$amount, input$rate, i)+annuity(input$contrib, input$rate, i))
    }
    growing_contrib <- c(0:input$years)
    for (i in growing_contrib) {
      growing_contrib[i+1] = future_value(input$amount, input$rate, i)+growing_annuity(input$contrib, input$rate, input$growth, i)
    }
    years <- c(0:input$years)
    modalities <- data.frame(years, no_contrib, fixed_contrib, growing_contrib)
  })
}

shinyApp(ui = ui, server = server)



