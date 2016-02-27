
shinyUI(

  pageWithSidebar(
      # Application title
      headerPanel("Hospital Transparency Tool:  Quality of Care"),

      mainPanel(
          fluidRow(
              column(4,
                  h5("1. Choose a condition(s):"),
                  uiOutput("conditionListDropDown")
               ),
              column(8,
                  h5("2. Choose a location (Optional)"),
                  uiOutput("stateListDropDown"),
                  uiOutput("inputZipText")
                  
                  #Future expansion items
                  #uiOutput("inputCityText"),
                  #uiOutput("inputCountyText")
              )
          ),
          fluidRow(
              column(2, actionButton("run","Update Results")),
              column(2, actionButton("resetAll","Reset"))
          ),
          h3("")

      ),

      # Show a tabset that includes different view of the data
      mainPanel(
          tabsetPanel(
              tabPanel("Plot", plotOutput("scatter")),
              tabPanel("Top 5 Hopitals- Death Rate", tableOutput("top5death")),
              tabPanel("Top 5 Hopitals - Readmission Rate", tableOutput("top5readmit")),
              tabPanel("Interactive Dataset", dataTableOutput("mytable"))
          )
      )
  )
)
