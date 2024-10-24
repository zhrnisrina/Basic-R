
setwd("C:/Users/HP/OneDrive - Institut Teknologi Sepuluh Nopember/SEMESTER 4/SIM")
install.packages("shiny")
library(shiny)

ui <- fluidPage("Hello World",
                textInput("name", "What's your name?"),
                passwordInput("password", "What's your password?"),
                textAreaInput("story", "Tell me about yourself", rows = 3),
                numericInput("num", "Number one", value = 0, min = 0, max =
                               100),
                sliderInput("num2", "Number two", value = 50, min = 0, max =
                              100),
                sliderInput("rng", "Range", value = c(10, 20), min = 0, max = 100),
                sliderInput(inputId = "num",
                            label = "Choose a number",
                            value = 25, min = 1, max = 100),
                plotOutput("hist"),
                dateInput("dob", "When were you born?"),
                dateRangeInput("holiday", "When do you want to go on vacation next?")
)
server <- function(input, output) {
  output$hist <- renderPlot({
    title <- "Histogram of number"
    hist(rnorm(input$num), main = title)})
}
shinyApp(ui = ui, server = server)


#Exercises 1.8

#1
ui <- fluidPage(numericInput("age", "How old are you?", value = NA),
                textInput("name", "What's your name?"),
                textOutput("greeting"),
                plotOutput("histogram"))
server <- function(input, output) {
  output$greeting <- renderText({
    paste("Hello ", input$name, "u r", input$age, "years old")})
  output$histogram <- renderPlot({
    hist(rnorm(1000))
  }, res = 96)
}
shinyApp(ui = ui, server = server)

#2
ui <- fluidPage(
  sliderInput("x", label = "If x is", min = 1, max = 50, value = 30),
  "then x times 5 is",
  textOutput("product")
)
server <- function(input, output) {
  output$product <- renderText({ 
    input$x * 5
  })
}
shinyApp(ui = ui, server = server)


#3
ui <- fluidPage(
  sliderInput("x", label = "If x is", min = 1, max = 50, value = 30),
  sliderInput("y", label = "and y is", min = 1, max = 50, value = 30),
  "then x times y is",
  textOutput("product")
)
server <- function(input, output) {
  output$product <- renderText({ 
    input$x * input$y
  })
}
shinyApp(ui = ui, server = server)


#4 
#Before reactive

ui <- fluidPage(
  sliderInput("x", "If x is", min = 1, max = 50, value = 30),
  sliderInput("y", "and y is", min = 1, max = 50, value = 5),
  "then, (x * y) is", textOutput("product"),
  "and, (x * y) + 5 is", textOutput("product_plus5"),
  "and (x * y) + 10 is", textOutput("product_plus10")
)

server <- function(input, output, session) {
  output$product <- renderText({ 
    product <- input$x * input$y
    product
  })
  output$product_plus5 <- renderText({ 
    product <- input$x * input$y
    product + 5
  })
  output$product_plus10 <- renderText({ 
    product <- input$x * input$y
    product + 10
  })
}

shinyApp(ui, server)


#After reactive 

ui <- fluidPage(
  sliderInput("x", "If x is", min = 1, max = 50, value = 30),
  sliderInput("y", "and y is", min = 1, max = 50, value = 5),
  "then, (x * y) is", textOutput("product"),
  "and, (x * y) + 5 is", textOutput("product_plus5"),
  "and (x * y) + 10 is", textOutput("product_plus10")
)

server <- function(input, output, session) {
  product <- reactive({ input$x * input$y
  })
  output$product <- renderText({ product() })
  output$product_plus5 <- renderText({ product()+5 })
  output$product_plus10 <- renderText({ product()+10 })
}

shinyApp(ui, server)


#5
library(ggplot2)

datasets <- c("economics", "faithfuld", "seals")
ui <- fluidPage(
  selectInput("dataset", "Dataset", choices = datasets),
  verbatimTextOutput("summary"),
  plotOutput("plot")
)

server <- function(input, output, session) {
  dataset <- reactive({
    get(input$dataset, "package:ggplot2")
  })
  output$summary <- renderPrint({
    summary(dataset())
  })
  output$plot <- renderPlot({
    plot(dataset())
  }, res = 96)
}

shinyApp(ui, server)


#21/4/2022

install.packages("shinydashboard")
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", height = 250)),
      
      box(
        title = "Controls",
        sliderInput("slider", "Nilai Math:", 1, 100, 50)
      )
    )
  )
)

server <- function(input, output) {
  histdata <-k
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)


