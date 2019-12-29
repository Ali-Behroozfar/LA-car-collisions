rm(list=ls())
library(shiny)
library(leaflet)
library(shinythemes)
library(readr)
par(mar=c(1,1,1,1))
library(lubridate)
library(ggplot2)
library(dplyr)
library(ggmap)
library(maps)

all_areas<-read_csv('all_areas.csv');
all_pred<-read_csv('all_pred.csv');
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("superhero"),
                
                # Application title
                titlePanel("Number of car collisions in different areas of Los Angeles"),
                sidebarLayout(
                    sidebarPanel(
                        br(),
                        dateInput(inputId="z", label="Pick a date from the calendar below to see predicted numbers of car collisions in different areas", value = "2019-09-02", min = "2018-09-02", max = "2019-12-10",
                                  format = "yyyy-mm-dd", startview = "month", weekstart = 0)
                        
                        #numericInput(inputId = "z", label = "day n", min = 0, 
                         #            max = 100, value = 1)
                       # sliderInput(inputId = "z", label = "day n", min = 0, 
                       #             max = 100, value = 7)

                    ),
                    
                    mainPanel(
                        
                        tabsetPanel(type = "tabs",
                                    tabPanel("Los Angeles map", leafletOutput("plot1",width = 800,height = 450),verbatimTextOutput("text1"))
                                    
                        )
                    )
                    
                )
)
                
                server <- function(input, output) {
                    
                    ######   TAB 1
                    
                    output$plot1 <- renderLeaflet({
                        f<-7
                        n<-as.integer(difftime(input$z,"2019-08-31",units = "days"))
                        
                        j<-0
 #                       for(k in 1:21){
  #                          all_areas$sum[k]<-0
   #                         for (i in 1:f){
    #                            j<-(k-1)*100+i
     #                           all_areas$sum[k]<-all_pred[j]+all_areas$sum[k]
      #                      }
       #                 }
                        for(k in 1:21){
                            j<-(k-1)*100+n
                            all_areas$n_day[k]<-all_pred[j,2]
                        }
                        all_areas$sum<-as.numeric(all_areas$sum)
                        all_areas$n_day<-as.numeric(all_areas$n_day)
                        pal<-colorNumeric("RdYlGn",domain = as.numeric(all_areas$n_day),reverse = TRUE)
                        m <- leaflet(all_areas) %>%
                            addTiles() %>% 
                            addCircleMarkers(lng = all_areas$mean_lon, lat = all_areas$mean_lat,popup = all_areas$`Area Name`,fillOpacity = 0.8,stroke = TRUE ,radius = 15,color = pal(as.numeric(all_areas$n_day)),label =as.character(ceiling(as.numeric(all_areas$n_day))))
                        
                    })
                    
                    ######   TAB 1
                    
                    output$plot1 <- renderLeaflet({
                        f<-7
                        n<-as.integer(difftime(input$z,"2019-08-31",units = "days"))
                        
                        j<-0
                        #                       for(k in 1:21){
                        #                          all_areas$sum[k]<-0
                        #                         for (i in 1:f){
                        #                            j<-(k-1)*100+i
                        #                           all_areas$sum[k]<-all_pred[j]+all_areas$sum[k]
                        #                      }
                        #                 }
                        for(k in 1:21){
                            j<-(k-1)*100+n
                            all_areas$n_day[k]<-all_pred[j,2]
                        }
                        all_areas$sum<-as.numeric(all_areas$sum)
                        all_areas$n_day<-as.numeric(all_areas$n_day)
                        pal<-colorNumeric("RdYlGn",domain = as.numeric(all_areas$n_day),reverse = TRUE)
                        m <- leaflet(all_areas) %>%
                            addTiles() %>% 
                            addCircleMarkers(lng = all_areas$mean_lon, lat = all_areas$mean_lat,popup = all_areas$`Area Name`,fillOpacity = 0.8,stroke = TRUE ,radius = 15,color = pal(as.numeric(all_areas$n_day)),label =as.character(ceiling(as.numeric(all_areas$n_day))))
                        
                    })
                    output$text1 <- renderText({
                      
                      
                      paste("- Hover the mouse over the spots (circles) to see the number of accidents
- Click the spots (circles) to see the area name")
                    })
                }

                    # Run the application 
                    shinyApp(ui = ui, server = server)
                    
                    