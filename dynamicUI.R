# As usual, we’ll start with the module UI. It’s very simple here, because we’re just generating a “hole” that the server functions will fill in dynamically.
filterUi <- function(id) {
  uiOutput(NS(id, "controls"))
}

# To create the module server, we’ll first copy in the helper functions from Section 10.3.2: make_ui() makes a control 
# for each column, and filter_var() helps generate the final logical vector. There’s only one difference here: make_ui()
# gains an additional id argument so that we can namespace the controls to the module.


library(purrr)

make_ui <- function(x, id, var) {
  if (is.numeric(x)) {
    rng <- range(x, na.rm = TRUE)
    sliderInput(id, var, min = rng[1], max = rng[2], value = rng)
  } else if (is.factor(x)) {
    levs <- levels(x)
    selectInput(id, var, choices = levs, selected = levs, multiple = TRUE)
  } else {
    # Not supported
    NULL
  }
}
filter_var <- function(x, val) {
  if (is.numeric(x)) {
    !is.na(x) & x >= val[1] & x <= val[2]
  } else if (is.factor(x)) {
    x %in% val
  } else {
    # No control, so don't filter
    TRUE
  }
}

# Now we create the module server. There are two main parts:
#   
# We generate the controls using purrr::map() and make_ui(). Note the explicit use of NS() here. 
# That’s needed because even though we’re inside the module server, the automatic namespacing only applies to input, output, and session.
# 
# We return the logical filtering vector as the module output.

filterServer <- function(id, df) {
  stopifnot(is.reactive(df))
  
  moduleServer(id, function(input, output, session) {
    vars <- reactive(names(df()))
    
    output$controls <- renderUI({
      map(vars(), function(var) make_ui(df()[[var]], NS(id, var), var))
    })
    
    reactive({
      each_var <- map(vars(), function(var) filter_var(df()[[var]], input[[var]]))
      reduce(each_var, `&`)
    })
  })
}

filterApp <- function() {
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        datasetInput("data", is.data.frame),
        textOutput("n"),
        filterUi("filter"),
      ),
      mainPanel(
        tableOutput("table")    
      )
    )
  )
  server <- function(input, output, session) {
    df <- datasetServer("data")
    filter <- filterServer("filter", df)
    
    output$table <- renderTable(df()[filter(), , drop = FALSE])
    output$n <- renderText(paste0(sum(filter()), " rows"))
  }
  shinyApp(ui, server)
}

filterApp()