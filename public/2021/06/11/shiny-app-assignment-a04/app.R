#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#load libraries

library(shiny)
library(ggplot2)
library(shinyanimate)
library(shinybusy)
library(shinyjs)
library(tidyverse)
library(tidytext)
library(plotly)
library(glue)
library(rsconnect)
library(shinycustomloader)
library(aos)
library(animation)
library(shinydashboard)
library(shinyWidgets)

## Import air_quality_index_india.csv

Indian_air_quality_index <- read_csv(file = "air_quality_index_india.csv")

## Transform Data

# ui User Interface

ui <- fluidPage(
    title="A04 Shiny App",
        headerPanel(
        h1("A04 Shiny App Visualization: Air Quality Index in Indian Cities", align = "center", style = "color: black")
        
        ),
    titlePanel("Selection Panel"),
    add_busy_spinner(spin = "fading-circle"),
    sidebarLayout(
        sidebarPanel(
            align = "center",
            selectInput(
                inputId = "city",
                label = "Select City",
                choices = c("Bengaluru","Chandigarh","Chennai","Ghaziabad","Hyderabad","Kolkata","Lucknow","Mumbai","New Delhi","Thiruvananthapuram","Visakhapatnam"
                )
            ),
            dateRangeInput(
                inputId = "date_range",
                label = "Select date range",
                start = min(Indian_air_quality_index$date),
                end = max(Indian_air_quality_index$date)
            ),wellPanel(
                radioButtons("extension", "Save As:",
                             choices = c("png", "pdf", "jpeg"), inline = TRUE),
                downloadButton("download", "Save Plot"),
                downloadButton("Download_dataset", "Download Dataset from App"),
                helpText(a("Click Here to Download Dataset from Weblink", href="https://unbcloud-my.sharepoint.com/:x:/g/personal/rmohanku_unb_ca/EQoCbxDzIsVLu0xh-vraHE8BC1N594HV1SPjYlpQR2ue-g?e=kf2DPV")
                )
            ),img(src = "https://www.deq.ok.gov/wp-content/uploads/air-division/aqi_mini-768x432.png", height = 175, width = 225, align = "auto")
        ),
               mainPanel(
            plotlyOutput(outputId = "Quality_plot"),
            h2("App Description:",style = "color: white"),
            p(style="text-align: justify;","Air Quality application depicts the 
              air quality among the 11 Indian cities for the range of dates 
              from 1st January 2019 to 31st May 2021. 
              The App user can select the range of date and the city to 
              view the visualisation. The Air Quality Index is plotted in 
              Y axis with respect to the selected range of dates in X - axis 
              for the selected city. The Air quality Index is ploted in 
              Magenta Colour for the values higher than or equal to 100, 
              whereas it is plotted in Green colour for the values lesser than 100. 
              Most Significantly, user can download the visualisation plot in the 
              formats such as PDF, PNG and JPEG. 
              Furthermore, the dataset is available to download in two modes, 
              one is from the application and another is from the weblink.",
              style = "color: white"),
            br(),
            h2("Additional App Features:",style = "color: white"),
            p(" i) Save the Visualisation Plot in PDF, PNG and JPEG formats",style = "color: white"),
            p(" ii) Download Dataset from the App",style = "color: white"),
            p(" iii) Alternatively, Weblink to download the Dataset",style = "color: white"),
            p(" iv) Loader Animation while waiting for the visualisation generation.",style = "color: white"),
            p(" v) Air Quality meter based on Air Quality Index for reference.",style = "color: white")
        )
    ),setBackgroundImage(
        src = "https://assets.new.siemens.com/siemens/assets/api/uuid:5e8832d7-b28d-4cf0-9046-bfcf05ac3c28/width:1125/crop:0:0,04464:0,999:0,83705/quality:high/city-air-quality-management--cyam--software-from-siemens-uses-ar.jpg"
    )
)

# Server 

server <- function(input, output) {
    output$Quality_plot <- renderPlotly({
        dataset<- Indian_air_quality_index
        p <- Indian_air_quality_index %>%
            filter(city == input$city) %>%
            filter(between(date, input$date_range[1], input$date_range[2])) %>%
            count(date, value) %>%
            ggplot(aes(x = date, y = value)) +
            geom_line(color = "gray") +
            scale_fill_discrete(name="AQ Index", labels = c("Poor Quality","Good Quality"))+
            geom_point(aes(color = value < 100), size = 1) +
            geom_hline(yintercept = 0, linetype = "dashed", color = "lightgray") +
            labs(
                title = glue("Pollution Index in Indian Cities, {input$origin} Air quality in Indian Cities"),
                x = "Date", y = "Air Quality Index Score"
            ) +
            theme_minimal() +
            theme(legend.position = "none")
        ggplotly(p, tooltip = c("x", "y"))
    })
    
    output$download <- downloadHandler(
        filename = function() {
            paste("Shiny_plot", input$extension, sep = ".")
        },
        content = function(file){
            ggsave(file,device = input$extension)
        }
    )
    output$Download_dataset <- downloadHandler(
        filename = function(){"Dataset.csv"}, 
        content = function(fname){
            write.csv(view(air_quality_index_india), fname)
        }
    )
            }

# Shiny App
shinyApp(ui = ui, server = server)
