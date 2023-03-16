#setwd("D:/suicide_project")

#load libraries
library(DT)
library(tidyverse)
library(readr,warn.conflicts = FALSE)
library(ggplot2,warn.conflicts = FALSE)
library(shiny,warn.conflicts = FALSE)
library(plotly,warn.conflicts = FALSE)
library(dplyr,warn.conflicts = FALSE)
library(shinyjs,warn.conflicts = FALSE)
library(gridExtra,warn.conflicts = FALSE)
library(tidyr,warn.conflicts = FALSE)
library(lubridate,warn.conflicts = FALSE)
library(scales)
library(ggpubr)
library(grid)

css <- "
.shiny-output-error { visibility: hidden; }
.shiny-output-error:before {
visibility: visible;
content: ''; }
}
"

#load file
suicide<-read.csv("Suicides in India 2001-2012.csv")

dim(suicide)
suicide<-suicide[,c(2,1,3,4,5,6,7)]
names(suicide)<-c("Year","States_UT","Type_code","Type","Gender","Age_group","Suicide_count")
summary(suicide)
suicide$year<-suicide$Year
suicide$Year<-as.factor(suicide$Year)
str(suicide)



#Define ui function
ui <- source(file.path("ui", "ui.R"),  local = TRUE)$value


# Define server function 
server <- function(input, output) {
  
  source(file.path("server", "server.R"),  local = TRUE)$value
  
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)   