#Suicide counts based on years
suic_count<-suicide%>%
  group_by(Year)%>%
  summarise(Suicide_count=sum(Suicide_count))

#Suicide counts based on years and Gender
suic_count1<-suicide%>%
  group_by(Gender)%>%
  summarise(Suicide_count=sum(Suicide_count))%>%
  mutate(Suicide_count_in_percent=round(Suicide_count/sum(Suicide_count)*100))

#Suicide counts based on years and Age group
suic_count2<-suicide%>%
  group_by(Age_group)%>%
  filter(!Age_group=="0-100+")%>%
  summarise(Suicide_count=sum(Suicide_count))%>%
  mutate(Suicide_count_in_percent=round(Suicide_count/sum(Suicide_count)*100))

#Suicide counts based on years and states
suic_count3<-suicide%>%
  group_by(States_UT)%>%
  filter(!(States_UT=="Total (All India)" | States_UT=="Total (States)" | States_UT=="Total (Uts)"))%>%
  summarise(Suicide_count=sum(Suicide_count))%>%
  mutate(Suicide_count_in_percent=round(Suicide_count/sum(Suicide_count)*100))

#Suicide counts based on years and Professional Profile
suic_count4<-suicide%>%
  filter(Type_code=="Professional_Profile")%>%
  group_by(Type)%>%
  summarise(Suicide_count=sum(Suicide_count))%>%
  mutate(Suicide_count_in_percent=round(Suicide_count/sum(Suicide_count)*100))

#Suicide counts based on years and Causes
suic_count5<-suicide%>%
  filter(Type_code=="Causes")%>%
  group_by(Type)%>%
  summarise(Suicide_count=sum(Suicide_count))%>%
  mutate(Suicide_count_in_percent=round(Suicide_count/sum(Suicide_count)*100))

#Suicide counts based on years and Means_adopted
suic_count6<-suicide%>%
  filter(Type_code=="Means_adopted")%>%
  group_by(Type)%>%
  summarise(Suicide_count=sum(Suicide_count))%>%
  mutate(Suicide_count_in_percent=round(Suicide_count/sum(Suicide_count)*100))

#Suicide counts based on years and Education status
suic_count7<-suicide%>%
  filter(Type_code=="Education_Status")%>%
  group_by(Type)%>%
  summarise(Suicide_count=sum(Suicide_count))%>%
  mutate(Suicide_count_in_percent=round(Suicide_count/sum(Suicide_count)*100))

#Suicide counts based on years and Social status
suic_count8<-suicide%>%
  filter(Type_code=="Social_Status")%>%
  group_by(Type)%>%
  summarise(Suicide_count=sum(Suicide_count))%>%
  mutate(Suicide_count_in_percent=round(Suicide_count/sum(Suicide_count)*100))


# for distplot2

#Suicide counts based on years and Gender
suic_count11<-suicide%>%
  group_by(Year,Gender)%>%
  summarise(Suicide_count=sum(Suicide_count))%>%
  mutate(Suicide_count_in_percent=round(Suicide_count/sum(Suicide_count)*100))

#Suicide counts based and Age group & Gender
suic_count22<-suicide%>%
  group_by(Age_group,Gender)%>%
  filter(!Age_group=="0-100+")%>%
  summarise(Suicide_count=sum(Suicide_count))%>%
  mutate(Suicide_count_in_percent=round(Suicide_count/sum(Suicide_count)*100))

#Suicide counts based on and states & Gender
suic_count33<-suicide%>%
  group_by(States_UT,Gender)%>%
  filter(!(States_UT=="Total (All India)" | States_UT=="Total (States)" | States_UT=="Total (Uts)"))%>%
  summarise(Suicide_count=sum(Suicide_count))%>%
  mutate(Suicide_count_in_percent=round(Suicide_count/sum(Suicide_count)*100))

#Suicide counts based on Professional Profile and gender
suic_count44<-suicide%>%
  filter(Type_code=="Professional_Profile")%>%
  group_by(Type,Gender)%>%
  summarise(Suicide_count=sum(Suicide_count))%>%
  mutate(Suicide_count_in_percent=round(Suicide_count/sum(Suicide_count)*100))

#Suicide counts based on Causes and gender
suic_count55<-suicide%>%
  filter(Type_code=="Causes")%>%
  group_by(Type,Gender)%>%
  summarise(Suicide_count=sum(Suicide_count))%>%
  mutate(Suicide_count_in_percent=round(Suicide_count/sum(Suicide_count)*100))

#Suicide counts based on suicide methods & gender
suic_count66<-suicide%>%
  filter(Type_code=="Means_adopted")%>%
  group_by(Type,Gender)%>%
  summarise(Suicide_count=sum(Suicide_count))%>%
  mutate(Suicide_count_in_percent=round(Suicide_count/sum(Suicide_count)*100))


#Suicide counts based on Education status & gender
suic_count77<-suicide%>%
  filter(Type_code=="Education_Status")%>%
  group_by(Type,Gender)%>%
  summarise(Suicide_count=sum(Suicide_count))%>%
  mutate(Suicide_count_in_percent=round(Suicide_count/sum(Suicide_count)*100))

#Suicide counts based on Social status & gender
suic_count88<-suicide%>%
  filter(Type_code=="Social_Status")%>%
  group_by(Type,Gender)%>%
  summarise(Suicide_count=sum(Suicide_count))%>%
  mutate(Suicide_count_in_percent=round(Suicide_count/sum(Suicide_count)*100))

#plot1 code

options(scipen=10000)

output$distplot <- renderPlotly({
  
  if(input$radio =="year"){
    
    shinyjs::hide("type")
    shinyjs::show("head")
    
    p<-ggplot(data=suic_count,aes(x=Year,y=Suicide_count))
    p<-p+geom_bar(stat = "identity",fill = "lightcoral")
    p<-p+labs(y="No. of suicides",x="Years",title = "Year wise total no. of suicides")
    
  }
  
  else if(input$radio =="gender"){
    
    shinyjs::hide("type")
    shinyjs::hide("head")
    
    p<-ggplot(data=suic_count1,aes(x=Gender,y=Suicide_count_in_percent,fill=Gender))
    p<-p+geom_bar(stat = "identity",position="dodge",aes(text=paste("Gender: ",Gender,"<br>Suicide count: ",Suicide_count_in_percent,"%",sep="")))
    p<-p+labs(y="Suicides (%) ",x="Years",title = "Gender wise percentage of suicides")
    ggplotly(p,tooltip = "text")
  }
  
  else if(input$radio =="agegroup"){
    
    shinyjs::hide("type")
    shinyjs::hide("head")
    
    p<-ggplot(data=suic_count2,aes(x=Age_group,y=Suicide_count_in_percent,fill=Age_group))
    p<-p+geom_bar(stat = "identity",aes(text=paste("Age group: ",Age_group,"<br>Suicide count: ",Suicide_count_in_percent,"%",sep="")))
    p<-p+labs(y="Suicides (%) ",x="Years",title = "Age group wise percentage of suicides")
    ggplotly(p,tooltip = "text")
  }
  
  else if(input$radio =="states"){
    
    shinyjs::hide("type")
    shinyjs::hide("head")
    
    p<-ggplot(data=suic_count3,aes(x=States_UT,y=Suicide_count,fill=States_UT))
    p<-p+geom_bar(stat = "identity",aes(text=paste("State/UT: ",States_UT,"<br>Suicide count: ",Suicide_count)))
    p<-p+labs(y="Suicides ",x="States/UT",title = "States/UT wise no. of suicides")
    p<-p + theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())
    ggplotly(p,tooltip = "text")
  }
  
  else  if(input$radio=="profile"){
    
    shinyjs::hide("type")
    shinyjs::hide("head")
    
    p<-ggplot(data=suic_count4,aes(x=Type,y=Suicide_count_in_percent,fill=Type))
    p<-p+geom_bar(stat = "identity",aes(text=paste("Professional Profile: ",Type,"<br>Suicide count: ",Suicide_count_in_percent,"%",sep="")))
    p<-p+labs(y="Suicides (%) ",x="Professional Profile of suicide victims",title = "Suicides % based on Professional Profile of suicide victims")
    p<-p + theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())
    ggplotly(p,tooltip = "text")
    
  }
  
  else  if(input$radio=="causes"){
    
    shinyjs::hide("type")
    shinyjs::hide("head")
    
    p<-ggplot(data=suic_count5,aes(x=Type,y=Suicide_count_in_percent,fill=Type))
    p<-p+geom_bar(stat = "identity",aes(text=paste("Suicide cause: ",Type,"<br>Suicide count: ",Suicide_count_in_percent,"%",sep="")))
    p<-p+labs(y="Suicides (%) ",x="Causes of suicides",title = "Percentage of suicides by suicide causes")
    p<-p + theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())
    ggplotly(p,tooltip = "text")
    
  }
  
  else  if(input$radio=="means"){
    
    shinyjs::hide("type")
    shinyjs::hide("head")
    
    p<-ggplot(data=suic_count6,aes(x=Type,y=Suicide_count_in_percent,fill=Type))
    p<-p+geom_bar(stat = "identity",aes(text=paste("Suicide method: ",Type,"<br>Suicide count: ",Suicide_count_in_percent,"%",sep="")))
    p<-p+labs(y="Suicides (%) ",x="Suicide methods",title = "Percentage of suicides by suicide methods")
    p<-p + theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())
    ggplotly(p,tooltip = "text")
    
  }
  
  else if(input$radio =="type"){
    
    shinyjs::show("type")
    shinyjs::hide("head")
    
    if(input$type=="Education_Status"){
      
      p<-ggplot(data=suic_count7,aes(x=Type,y=Suicide_count_in_percent,fill=Type))
      p<-p+geom_bar(stat = "identity",aes(text=paste("Education Status: ",Type,"<br>Suicide count: ",Suicide_count_in_percent,"%",sep="")))
      p<-p+labs(y="Suicides (%) ",x="Education status of suicide victims",title = "Suicides % based on Education status of Suicide victims")
      p<-p + theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())
      ggplotly(p,tooltip = "text")
      
    }
    
    else  if(input$type=="Social_Status"){
      
      p<-ggplot(data=suic_count8,aes(x=Type,y=Suicide_count_in_percent,fill=Type))
      p<-p+geom_bar(stat = "identity",aes(text=paste("Social Status: ",Type,"<br>Suicide count: ",Suicide_count_in_percent,"%",sep="")))
      p<-p+labs(y="Suicides (%) ",x="Social status suicide victims",title = "Suicides % based on Social status of Suicide victims")
      p<-p + theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())
      ggplotly(p,tooltip = "text")
      
    }
    
  }
  
})

#Heading for table

output$heading <- renderUI({
  
  if(input$radio =="year"){
    
    shinyjs::hide("type")
    h4("Year wise total no. of suicides")
    
  }
  
  else if(input$radio =="gender"){
    
    shinyjs::hide("type")
    h4("Gender wise percentage of suicides")
  }
  
  else if(input$radio =="agegroup"){
    
    shinyjs::hide("type")
    h4("Age group wise percentage of suicides")
  }
  
  else if(input$radio =="states"){
    
    shinyjs::hide("type")
    h4("States/UT wise no. of suicides")
  }
  
  else  if(input$radio=="profile"){
    
    shinyjs::hide("type")
    h4("Percentage of suicides by Professional Profile of suicide victims")
  }
  
  else  if(input$radio=="causes"){
    
    shinyjs::hide("type")
    h4("Percentage of suicides by Suicide causes")
  }
  
  else  if(input$radio=="means"){
    
    shinyjs::hide("type")
    h4("Percentage of suicides by methods of committing suicides")
  }
  
  else if(input$radio =="type"){
    
    shinyjs::show("type")
    
    if(input$type=="Education_Status"){
      h4("Percentage of suicides by Education status of Suicide victims")
    }
    
    else  if(input$type=="Social_Status"){
      h4("Percentage of suicides by Social status of Suicide victims")
    }
    
  }
  
})

#table code

output$table <- renderDataTable(
  
  if(input$radio =="year"){
    
    shinyjs::hide("type")
    
    suic_count
    
  }
  
  else if(input$radio =="gender"){
    
    shinyjs::hide("type")
    
    suic_count11
    
  }
  
  else if(input$radio =="agegroup"){
    
    shinyjs::hide("type")
    
    suic_count22
    
  }
  
  else if(input$radio =="states"){
    
    shinyjs::hide("type")
    
    suic_count33
    
  }
  
  else  if(input$radio=="profile"){
    
    shinyjs::hide("type")
    
    suic_count44
    
  }
  
  else  if(input$radio=="causes"){
    
    shinyjs::hide("type")
    
    suic_count55
    
  }
  
  else  if(input$radio=="means"){
    
    shinyjs::hide("type")
    
    suic_count66
    
  }
  
  else if(input$radio =="type"){
    
    shinyjs::show("type")
    
    if(input$type=="Education_Status"){
      
      suic_count77
      
    }
    
    else  if(input$type=="Social_Status"){
      
      suic_count88
      
    }
    
  }
)

#plot 2 code
options(scipen=10000)

output$distplot1 <- renderPlotly({
  
  if(input$radio =="year"){
    
    shinyjs::hide("type")
    shinyjs::show("head")
    
  }
  
  else if(input$radio =="gender"){
    
    shinyjs::hide("type")
    shinyjs::hide("head")
    
    p<-ggplot(data=suic_count11,aes(x=Year,y=Suicide_count,fill=Gender))
    p<-p+geom_bar(stat = "identity",position="dodge",aes(text=paste("Year: ",Year,"<br>Suicide count: ",Suicide_count,"<br>Gender: ",Gender)))
    p<-p+labs(y="Suicides count ",x="Years",title = "Year and Gender wise no. of suicides")
    ggplotly(p,tooltip = "text")
  }
  
  else if(input$radio =="agegroup"){
    
    shinyjs::hide("type")
    shinyjs::hide("head")
    
    p<-ggplot(data=suic_count22,aes(x=Age_group,y=Suicide_count,fill=Gender))
    p<-p+geom_bar(stat = "identity",position="dodge",aes(text=paste("Age group: ",Age_group,"<br>Suicide count: ",Suicide_count,"<br>Gender: ",Gender)))
    p<-p+labs(y="Suicides count ",x="Age group",title = "Age group and Gender wise no. of suicides")
    ggplotly(p,tooltip = "text")
  }
  
  else if(input$radio =="states"){
    
    shinyjs::hide("type")
    shinyjs::hide("head")
    
    p<-ggplot(data=suic_count33,aes(x=States_UT,y=Suicide_count,fill=Gender))
    p<-p+geom_bar(stat = "identity",aes(text=paste("State/UT: ",States_UT,"<br>Suicide count: ",Suicide_count,"<br>Gender: ",Gender)))
    p<-p+theme(axis.text.x=element_text(angle=90),axis.ticks.x = element_blank())
    p<-p+labs(y="Suicides count ",x="States/UTs",title = "States/UT and Gender wise no. of suicides")
    ggplotly(p,tooltip = "text")
  }
  
  else  if(input$radio=="profile"){
    
    shinyjs::hide("type")
    shinyjs::hide("head")
    
    p<-ggplot(data=suic_count44,aes(x=Type,y=Suicide_count,fill=Gender))
    p<-p+geom_bar(stat = "identity",aes(text=paste("Professional Profile: ",Type,"<br>Suicide count: ",Suicide_count,"<br>Gender: ",Gender)))
    p<-p+theme(axis.text.x=element_text(angle=90),axis.ticks.x = element_blank())
    p<-p+labs(y="Suicides count ",x="Professional Profile of suicide victims",title = "No. of suicides by Professional Profile and Gender of suicide victims")
    ggplotly(p,tooltip = "text")
    
  }
  
  else  if(input$radio=="causes"){
    
    shinyjs::hide("type")
    shinyjs::hide("head")
    
    p<-ggplot(data=suic_count55,aes(x=Type,y=Suicide_count,fill=Gender))
    p<-p+geom_bar(stat = "identity",aes(text=paste("Suicide cause: ",Type,"<br>Suicide count: ",Suicide_count,"<br>Gender: ",Gender)))
    p<-p+theme(axis.text.x=element_text(angle=90),axis.ticks.x = element_blank())
    p<-p+labs(y="Suicides count",x="Causes of suicides",title = "No. of suicides by Suicide causes and Gender of suicide victims")
    ggplotly(p,tooltip = "text")
    
  }
  
  else  if(input$radio=="means"){
    
    shinyjs::hide("type")
    shinyjs::hide("head")
    
    p<-ggplot(data=suic_count66,aes(x=Type,y=Suicide_count,fill=Gender))
    p<-p+geom_bar(stat = "identity",aes(text=paste("Suicide method: ",Type,"<br>Suicide count: ",Suicide_count,"<br>Gender: ",Gender)))
    p<-p+theme(axis.text.x=element_text(angle=90),axis.ticks.x = element_blank())
    p<-p+labs(y="Suicides count",x="Suicide methods",title = "No. of suicides by Suicide methods and Gender of suicide victims")
    ggplotly(p,tooltip = "text")
    
  }
  
  else if(input$radio =="type"){
    
    shinyjs::show("type")
    shinyjs::hide("head")
    
    if(input$type=="Education_Status"){
      
      p<-ggplot(data=suic_count77,aes(x=Type,y=Suicide_count,fill=Gender))
      p<-p+geom_bar(stat = "identity",aes(text=paste("Education Status: ",Type,"<br>Suicide count: ",Suicide_count,"<br>Gender: ",Gender)))
      p<-p+theme(axis.text.x=element_text(angle=90),axis.ticks.x = element_blank())
      p<-p+labs(y="Suicides count ",x="Education status of suicide victims",title = "No. of suicides by Education status and Gender of suicide victims")
      ggplotly(p,tooltip = "text")
      
    }
    
    else  if(input$type=="Social_Status"){
      
      p<-ggplot(data=suic_count88,aes(x=Type,y=Suicide_count,fill=Gender))
      p<-p+geom_bar(stat = "identity",aes(text=paste("Social Status: ",Type,"<br>Suicide count: ",Suicide_count,"<br>Gender: ",Gender)))
      p<-p+theme(axis.text.x=element_text(angle=90),axis.ticks.x = element_blank())
      p<-p+labs(y="Suicides count ",x="Social status suicide victims",title = "No. of suicides by Social status and Gender of suicide victims")
      ggplotly(p,tooltip = "text")
      
    }
    
  }
  
})

#insight code

output$insight <- renderUI(
  
  if(input$radio =="year"){
    
    shinyjs::hide("type")
    
  }
  
  else if(input$radio =="gender"){
    
    shinyjs::hide("type")
    
    div(
      h3("Insight:"),
      h4("Suicide rate is 28% higher in men than in women in India.")
    )
    
  }
  
  else if(input$radio =="agegroup"){
    
    shinyjs::hide("type")
    
    div(
      h3("Insight:"),
      h4("People in the Age group of 15-20 has the highest percentage of committing suicide followed by Age group of 30-44.")
    )
  }
  
  else if(input$radio =="states"){
    
    shinyjs::hide("type")
    
    div(
      h3("Insight:"),
      h4("Maharashtra, West Bengal, Tamilnadu, Andhra Pradesh and Karnataka has highest no. of suicide rates in India.")
    )
  }
  
  else  if(input$radio=="profile"){
    
    shinyjs::hide("type")
    div(
      h3("Insight:"),
      h4("Excluding people whose professional profile is unknown, housewives have the highest no. of suicide rates followed by people involved in the farming activities and people who are unemployed. ")
    )
    
  }
  
  else  if(input$radio=="causes"){
    
    shinyjs::hide("type")
    div(
      h3("Insight:"),
      h4("Family problems,prolonged & mental illness, love affairs, poverty and unemployment are the main reasons of committing suicides in India.")
    )
    
  }
  
  else  if(input$radio=="means"){
    
    shinyjs::hide("type")
    div(
      h3("Insight:"),
      h4("Hanging,consuming insecticides and poison are the most frequently used method of suicides in India.")
    )
  }
  
  else if(input$radio =="type"){
    
    shinyjs::show("type")
    
    if(input$type=="Education_Status"){
      div(
        h3("Insight:"),
        h4("Most of the people who have committed suicide have either no education or less education upto Primary and Secondary.")
      )
    }
    
    else  if(input$type=="Social_Status"){
      div(
        h3("Insight:"),
        h4("Married people have the highest suicide rates.")
      )
    }
    
  }
)

#download report code

output$report <- downloadHandler(
  filename = "report.pdf",
  content = function(file) {
    src <- normalizePath('suicide_report.Rmd')
    
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'suicide_report.Rmd', overwrite = TRUE)
    
    library(rmarkdown)
    out <- render('suicide_report.Rmd',pdf_document())
    file.rename(out, file)
  }
)