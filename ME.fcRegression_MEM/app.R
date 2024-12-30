#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Package MECfda: ME.fcRegression_MEM - beta(t)"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "data.Y",label = "Data of Y"),
      fileInput(inputId = "data.W",label = "Data of W"),
      fileInput(inputId = "data.Z",label = "Data of Z"),
      selectInput(inputId = "method",  label = "method",  choices = c('UP_MEM', 'MP_MEM', 'average')),
      numericInput(inputId = "t_0",  label = "start of the time interval",  value = 0),
      numericInput(inputId = "t_1",  label = "end of the time interval",  value = 1),
      selectInput(inputId = "family.W",  label = "family.W",  choices = c("gaussian","poisson")),
      textInput(  inputId = "family.Y",  label = "family.Y",  value = 'gaussian(link = "identity")'),
      textInput(  inputId = "formula.Z", label = "formula.Z", value = ''),
      numericInput(inputId = "d",  label = "d",  value = 3),
      selectInput( inputId = "basis.type",  label = "basis.type",  choices = c("Bspline", "Fourier")),
      numericInput(inputId = "basis.order", label = "basis.order", value = 5L, min = 1L),
      numericInput(inputId = "bs_degree",  label = "bs_degree",  value = 3,  min = 1),
      selectInput(inputId = "smooth",  label = "smooth",  choices = c(FALSE,TRUE)),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      "Plot of beta(t)",
      plotOutput("beta_plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  library(MECfda)
  output$beta_plot = renderPlot({
    req(input$data.Y,input$data.Z,input$data.W)
    for (input_var_name in c('data.Y','data.Z','data.W')) {
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
    res = ME.fcRegression_MEM(
      data.Y = data.Y,
      data.W = data.W,
      data.Z = data.Z,
      method = input$method,
      t_interval = c(input$t_0, input$t_1),
      d = input$d,
      family.W = input$family.W,
      family.Y = eval(parse(text=input$family.Y)),
      formula.Z = eval(parse(text=input$formula.Z)),
      basis.type = input$basis.type,
      basis.order = input$basis.order,
      bs_degree = input$bs_degree,
      smooth = input$smooth
    )
    t = input$t_0 + ((0:100)/100)*(input$t_1-input$t_0)
    plot(x = t, y = fc.beta(res,1,t), ylab = expression(beta(t)))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


# library(MECfda)
# data(MECfda.data.sim.0.1)
# Y = MECfda.data.sim.0.1$Y
# Z = MECfda.data.sim.0.1$Z
# W = MECfda.data.sim.0.1$W
# write.csv(Z,"C:/Users/jihx1/Desktop/Z.csv", row.names = FALSE)
# write.csv(Y,"C:/Users/jihx1/Desktop/Y.csv", row.names = FALSE)
# saveRDS(W,file = "C:/Users/jihx1/Desktop/W.rds")




