library(shiny)

ui <- fluidPage(
  titlePanel("Package MECfda: fcQR - beta(t)"),
  sidebarLayout(
    sidebarPanel(
      fileInput(   inputId = "Y",           label = "Data of response variable, Y"),
      fileInput(   inputId = "Z",           label = "Data of scalar-valued covariate(s), Z"),
      fileInput(   inputId = "FC",          label = "Data of function-valued covariate(s), X"),
      textInput(   inputId = "formula.Z",   label = "formula.Z",   value = "NULL"),
      numericInput(inputId = "tau",         label = "quantile",    value = 0.5, min = 0, max = 1),
      selectInput( inputId = "basis.type",  label = "basis.type",  choices = c("Bspline", "Fourier")),
      numericInput(inputId = "basis.order", label = "basis.order", value = 5L, min = 1L),
      numericInput(inputId = "bs_degree",  label = "bs_degree",  value = 3,  min = 1),
      numericInput(inputId = "plot_range_min",  label = "left end of plot range",  value = 0),
      numericInput(inputId = "plot_range_max",  label = "right end of plot range", value = 1),
      numericInput(inputId = "show_which",  label = "Which function-valued coefficient to be plotted?",  value = 1, min = 1)
    ),
    mainPanel(
      plotOutput("beta_plot"),
    )
  )
)

server <- function(input, output, session) {
  library(MECfda)
  output$beta_plot = renderPlot(
    { 
      for (input_var_name in c('Y','Z','FC')) {
        file_type = tools::file_ext(input[[input_var_name]]$name)
        switch (file_type,
                'csv' = {
                  assign(input_var_name, 
                         read.csv(input[[input_var_name]]$datapath, header = TRUE))},
                'rds' = {
                  assign(input_var_name,
                         readRDS( input[[input_var_name]]$datapath))
                }
        )
      }
      basis.type = reactive(input$basis.type)
      res = fcQR(
        Y = Y,
        FC = FC,
        Z = Z,
        formula.Z = eval(parse(text=input$formula.Z)),
        tau = input$tau,
        basis.type = basis.type(),
        basis.order = input$basis.order,
        bs_degree = input$bs_degree
      )
      t = input$plot_range_min + ((0:100)/100)*(input$plot_range_max-input$plot_range_min)
      plot(x = t, y = fc.beta(res,input$show_which,t), ylab = expression(beta(t)))
    }
  )
}

shinyApp(ui, server)


