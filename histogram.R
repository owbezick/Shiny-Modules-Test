# Standard UI ----

# ui <- fluidPage(
#   selectInput("var", "Variable", names(mtcars)),
#   numericInput("bins", "bins", 10, min = 1),
#   plotOutput("hist")
# )

# Standard Server ----

# server <- function(input, output, session) {
#   data <- reactive(mtcars[[input$var]])
#   output$hist <- renderPlot({
#     hist(data(), breaks = input$bins, main = input$var)
#   }, res = 96)
# }
# histogramUI <- function(id) {
#   tagList(
#     selectInput(NS(id, "var"), "Variable", names(mtcars)),
#     numericInput(NS(id, "bins"), "bins", 10, min = 1),
#     plotOutput(NS(id, "hist"))
#   )
# }

# Module UI ----

# Put the UI code inside a function that has an id argument.

# Wrap each existing ID in a call to NS(), so that (e.g.) "var" turns into NS(id, "var").
histogramUI <- function(id) {
  tagList(
    selectInput(NS(id, "var"), "Variable", names(mtcars)),
    numericInput(NS(id, "bins"), "bins", 10, min = 1),
    plotOutput(NS(id, "hist"))
  )
}
# Here I’ve returned the UI components in a tagList(), but you could also return them in 
# an HTML container like column() or a fluidRow(). Returning a list is more flexible because
# it allows the caller of the module to choose the container. If you always place the module
# in the same container, you should return that instead, saving a little code in the UI.

# Module Server ----
# This gets wrapped inside another function which must have an id argument. This function 
# calls moduleServer() with the id, and a function that looks like a regular server function:
histogramServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive(mtcars[[input$var]])
    output$hist <- renderPlot({
      hist(data(), breaks = input$bins, main = input$var)
    }, res = 96)
  })
}
# Note that moduleServer() takes care of the namespacing automatically: inside of moduleServer(id)
# , input$var and input$bins refer to the inputs with names NS(id, "var") and NS(id, "bins").

# App function ----
# it’s good practice to write a function that uses them to generate an app which we can use for experimentation and testing:
histogramApp <- function() {
  ui <- fluidPage(
    histogramUI("hist1")
  )
  server <- function(input, output, session) {
    histogramServer("hist1")
  }
  shinyApp(ui, server)  
}
# Note that, like all Shiny control, you need to use the same id in both UI and server, otherwise the two pieces will not be connected.
# 
# Namespacing turns modules into black boxes. From outside of the module, you can’t see any of the inputs, outputs, or reactives 
# inside of it. For example, take the app below. The text output output$out will never get updated because there is no input$bins;
# the bins input can only be seen inside of the hist1 module.If you want to take input from reactives elsewhere in the app, you’ll 
# need to pass them to the module function explicitly

# Note that the module UI and server differ in how the namespacing is expressed:
#   
#   In the module UI, the namespacing is explicit: you have to call NS() every time you create an input or output.
# 
# In the module server, the namespacing is implicit. You only need to use id in the call to moduleServer() and then 
# Shiny automatically namespaces input and output so that your module code can only access elements with the matching id.