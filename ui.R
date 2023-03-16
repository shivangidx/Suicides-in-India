fluidPage(
tags$style(type="text/css", css),
shinyjs::useShinyjs(),

# Title
h2("Analysis of suicides in India"),
#sub title
h4("(2001-2012)"),

sidebarLayout(
  
  sidebarPanel(
    
    # Input: Selector for choosing variables
    
    radioButtons("radio", "Select variable:",
                 c("Year"="year",
                   "Gender" = "gender",
                   "Age group" = "agegroup",
                   "States/UT" = "states",
                   "Professional Profile"="profile",
                   "Suicide causes"="causes",
                   "Suicide methods"="means",
                   "Status of suicide victims" = "type")),
    
    # Input: Selector for choosing status
    
    hidden(
      div(id = "type",
          selectInput(inputId="type",
                      label="", 
                      choices=c("Education_Status","Social_Status"),
                      selected="Education_Status")
          
      )),
    
    downloadButton("report", "Download report"),
    br(),
    h5("(Hover over the graph to see the values of bars)"),
    h5("(Click to select/deselect the categories shown on the right side of page)"),
    h5("Data source: "),
    a("Link", href="https://www.kaggle.com/rajanand/suicides-in-india",target = "_blank")
  ), #sidebar panel
  
  
  mainPanel(
    
    tabsetPanel(type = "tabs",
                tabPanel("Visualization", 
                         
                         #display plot  
                         br(),
                         plotlyOutput("distplot",height = "480",width="865"),
                         br(),
                         hidden(
                           div(id = "head",
                               div(
                                 h3("Insight:"),
                                 h4("Suicide rate kept on rising over the years in India.")
                               )
                           )),
                         br(),
                         #textOutput("description")
                         plotlyOutput("distplot1",height = "600",width="875"),
                         br(),
                         br(),
                         htmlOutput("insight"),
                         br(),
                         br(),
                         br(),
                         br()
                         
                ),
                
                tabPanel("Table",
                         
                         #display table
                         br(),
                         htmlOutput("heading"),
                         br(),
                         dataTableOutput("table")
                         
                )
    ) #tabset panel
    
  ) #main panel
  
) #side bar layout

) #fluid page
