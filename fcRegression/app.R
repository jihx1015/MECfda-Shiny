# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# For MEM measurement error bias correct method proposed by Luan et. al.
# This is the interactive app to get the prediction of X(t)

library(shiny)

ui <- fluidPage(
  # Application title
  titlePanel("Package MECfda: fcRegression - beta(t)"),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput(   inputId = "Y",           label = "Data of response variable, Y"),
      fileInput(   inputId = "Z",           label = "Data of scalar-valued covariate(s), Z"),
      fileInput(   inputId = "FC",          label = "Data of function-valued covariate(s), X"),
      textInput(   inputId = "formula.Z",   label = "formula.Z",   value = "NULL"),
      textInput(   inputId = "family",      label = "family",      value = 'gaussian(link = "identity")'),
      selectInput( inputId = "basis.type",  label = "basis.type",  choices = c("Bspline", "Fourier")),
      numericInput(inputId = "basis.order", label = "basis.order", value = 5L, min = 1L),
      numericInput(inputId = "bs_degree",  label = "bs_degree",  value = 3,  min = 1),
      numericInput(inputId = "plot_range_min",  label = "left end of plot range",  value = 0),
      numericInput(inputId = "plot_range_max",  label = "right end of plot range", value = 1),
      numericInput(inputId = "show_which",  label = "Which function-valued coefficient to be plotted?",  value = 1, min = 1)
    ),
    mainPanel(
      # textOutput("test"),
      textOutput("beta_plot_title"),
      # "beta_plot_title",
      plotOutput("beta_plot"),
    )
  )
)

server <- function(input, output, session) {
  library(MECfda)
  output$beta_plot = renderPlot(
    { 
      req(input$Y,input$Z,input$FC)
      
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
      # if(is.null(input$Z)){Z = NULL}
      
      basis.type = reactive(input$basis.type)
      # bs_degree = reactive(input$bs_degree)
      res = fcRegression(
        Y = Y,
        FC = FC,
        Z = Z,
        basis.order = input$basis.order,
        basis.type = basis.type(),
        formula.Z = eval(parse(text=input$formula.Z)),
        family = eval(parse(text=input$family)),
        bs_degree = input$bs_degree
      )
      t = input$plot_range_min + ((0:100)/100)*(input$plot_range_max-input$plot_range_min)
      plot(x = t, y = fc.beta(res,input$show_which,t), ylab = expression(beta(t)))
      # plot(x = t, y = fc.beta(res,1,t), ylab = expression(beta[1](t)))
    }
    )
  output$beta_plot_title = renderText(
    paste0("The plot of ",input$show_which,"-th beta(t)") 
  )
  # output$test = renderText({
  #   is.null(input$Z)
  #   # "Z" %in% names(input)
  #   # exists(input$Z)
  # })
}

shinyApp(ui, server)

# library(MECfda)
# data(MECfda.data.sim.0.0)
# Y = MECfda.data.sim.0.0$Y
# Z = MECfda.data.sim.0.0$Z
# FC = MECfda.data.sim.0.0$FC
# write.csv(Z,"C:/Users/jihx1/Desktop/Z.csv", row.names = FALSE)
# write.csv(Y,"C:/Users/jihx1/Desktop/Y.csv", row.names = FALSE)
# saveRDS(FC,file = "C:/Users/jihx1/Desktop/FC.rds")
# saveRDS(Z,file = "C:/Users/jihx1/Desktop/Z.rds")
# a = readRDS(file = "C:/Users/jihx1/Desktop/FC.rds")
