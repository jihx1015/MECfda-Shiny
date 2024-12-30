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

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Package MECfda: MEM_X_hat"),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
      sidebarPanel(
        fileInput(inputId = "data.W",label = "Data of W"),
        selectInput(inputId = "method",  label = "method",  choices = c('UP_MEM', 'MP_MEM', 'average')),
        selectInput(inputId = "family.W",  label = "family.W",  choices = c("gaussian","poisson")),
        numericInput(inputId = "d",  label = "d",  value = 3),
        selectInput(inputId = "smooth",  label = "smooth",  choices = c(FALSE,TRUE)),
        "Download Prediction of X(t)",
        downloadButton("downloadData", "Download")
      ),

      mainPanel(
        "Prediction of X(t)",
        tableOutput("Xtable"),
        # textOutput("test")
      )
      
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  library(MECfda)
  data.W = reactive({ readRDS(input$data.W$datapath) })
  output$Xtable <- renderTable({
    req(input$data.W)
    X_hat = MEM_X_hat(data.W = data.W(),
                      method = input$method,
                      d = input$d,
                      family.W = input$family.W,
                      smooth = input$smooth)
    output$downloadData <- downloadHandler(
      filename = "X_hat.csv",
      content = function(file) {
        write.csv(X_hat, 
                  file, row.names = FALSE)
      }
    )
    X_hat
  })
  # output$downloadData <- downloadHandler(
  #   filename = "X_hat.csv",
  #   content = function(file) {
  #     write.csv(MEM_X_hat(data.W = data.W(),
  #                         method = input$method,
  #                         d = input$d,
  #                         family.W = input$family.W,
  #                         smooth = input$smooth), 
  #               file, row.names = FALSE)
  #   }
  # )
  # output$test = renderText({
  #   as.character(dim(X_hat))
  # })
}

# Run the application
shinyApp(ui = ui, server = server)

# library(MECfda)
# data(MECfda.data.sim.0.1)
# W = MECfda.data.sim.0.1$W
# saveRDS(W,file = "C:/Users/jihx1/Desktop/W.rds")
# data.W = readRDS(file = "C:/Users/jihx1/Desktop/W.rds")
# tab = MEM_X_hat(data.W = data.W,
#                 method = 'UP_MEM',
#                 d = 3,
#                 family.W = 'gaussian',
#                 smooth = FALSE)


