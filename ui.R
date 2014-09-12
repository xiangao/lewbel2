library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Lewbel model"),

  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    sliderInput("n", "Sample size (exponentiated):",
                min=4, max=8, value=4, step=1),
    sliderInput("k", "heteroskedasticity",
                min=1, max=10, value=1, step=1)

      ),
      mainPanel(plotOutput("plot1")
                )
      ))
