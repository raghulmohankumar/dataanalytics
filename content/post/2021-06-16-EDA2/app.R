#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
library(quantmod)
library(readxl)

## Import weather_dataset.csv

US_weather_data <- read_csv(file = "weather.csv")

## Transform Data

# ui User Interface

ui <- fluidPage(
    title="A05 Assignment EDA2",
    headerPanel(
        h1("A05 Shiny App Visualization: Weather Pattern in USA", align = "center", style = "color: white")
        
    ),
    titlePanel("Selection Panel"),
    add_busy_spinner(spin = "fading-circle"),
    sidebarLayout(
        sidebarPanel(
            align = "center",
            selectInput(
                inputId = "state",
                label = "Select State",
                choices = c("AR","CA","CO","CT","DE","IA","ID","IL","IN","KS","KY","MD","ME","MI","MN","MO","MT","NC","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VA","VT","WA","WI","WV","WY"
                )
            ),		 dateRangeInput(
                inputId = "date_range",
                label = "Select date range",
                start = min(US_weather_data$date),
                end = max(US_weather_data$date)
            ),
           wellPanel(
                radioButtons("extension", "Save As:",
                             choices = c("png", "pdf", "jpeg"), inline = TRUE),
                downloadButton("download", "Save Plot"),
                downloadButton("Download_dataset", "Download Dataset from App"),
                helpText(a("Click Here to Download Dataset from Weblink", href="https://unbcloud-my.sharepoint.com/:f:/g/personal/rmohanku_unb_ca/Eh_-w2Z3cKlEgNiSZkWlex0BwRTVeyK7Qq6VqNiDO2snaQ?e=BKNndp")
                )
            ),img(src = "https://w7.pngwing.com/pngs/674/852/png-transparent-temperature-cold-fahrenheit-hot-measure-scale-thermometer.png", height = 200, width = 225, align = "auto")
        ),
        mainPanel(
            plotlyOutput(outputId = "Weather_plot"),
            h2("App Description:",style = "color: white"),
            p(style="text-align: justify;","Weather application depicts the maximum temparature among the 42 states of USA for the range of dates from 1st January 2017 to 21st September 2017. The App user can select the range of date and the state to view the visualisation. The Maximum temparature from various meteorological stations are plotted in Y axis with respect to the selected range of dates in X - axis for the selected state. The Air quality Index is ploted in Magenta Colour for the temperatures higher than or equal to 80 deg Fahrenheit, 
whereas it is plotted in Green colour for the temperatures lesser than 80. Most Significantly, user can download the visualisation plot in the formats such as PDF, PNG and JPEG. Furthermore, the dataset is available to download in two modes,  one is from the application and another is from the weblink.",
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
        src = "https://images0.westend61.de/0001475098pw/aerial-view-of-new-york-city-against-orange-sky-during-sunset-new-york-usa-HOHF01427.jpg"
    )
)

# Server 

server <- function(input, output) {
    output$Weather_plot <- renderPlotly({
        dataset<- US_weather_data
        p <- US_weather_data %>%
            filter(state == input$state) %>%
            filter(between(date, input$date_range[1], input$date_range[2])) %>%
            count(date, TMAX) %>%
            ggplot(aes(x = date, y = TMAX)) +
            geom_line(color = "gray") +
            scale_fill_discrete(name="AQ Index", labels = c("Poor Quality","Good Quality"))+
            geom_point(aes(color = TMAX < 80), size = 1) +
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
