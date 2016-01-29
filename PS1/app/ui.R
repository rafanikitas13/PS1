library(shiny)

#User Interface commands
shinyUI(fluidPage(
  # app title
  titlePanel('LoanData Decision Boundaries'),
  sidebarLayout(
    sidebarPanel( # functions for modifying 8 parameters
      numericInput("muSA",
                   label = h4(strong("Mean Solvency for Approved"),style = "color:green"),
                   value = 1),
      numericInput("muSD",
                   label = h4(strong("Mean Solvency for Denied"),style = "color:red"),
                   value = 1),
      numericInput("sdSA",
                   label = h4(strong("Standard Deviation Solvency for Approved"),style = "color:green"),
                   value = 1),
      numericInput("sdSD",
                   label = h4(strong("Standard Deviation Solvency for Denied"),style = "color:red"),
                   value = 1),
      numericInput("muPA",
                   label = h4(strong("Mean PI ratio for Approved"),style = "color:green"),
                   value = 1),
      numericInput("muPD",
                   label = h4(strong("Mean PI ratio for Denied"),style = "color:red"),
                   value = 1),
      numericInput("sdPA",
                   label = h4(strong("Standard Deviation PI ratio for Approved"),style = "color:green"),
                   value = 1),
      numericInput("sdPD",
                   label = h4(strong("Standard Deviation PI ratio for Denied"),style = "color:red"),
                   value = 1)
    ),
    mainPanel( # displaying plot with decision boundaries and confusion matrix
      plotOutput("plot", height = "500px"),
      div(tableOutput("confusion.mtrx"), style = "font-size:200%",align="center")
    )
   )
  )
 )
