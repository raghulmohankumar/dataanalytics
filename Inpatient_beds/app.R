#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinybusy)
library(shinyjs)
library(tidyverse)
library(tidytext)
library(plotly)
library(glue)
library(rsconnect)
library(shinycustomloader)




## Import Estimated_Inpatient_Beds.csv

Indian_air_quality_index <- read_csv(file = "air_quality_index_india.csv")

## Transform Data

# ui User Interface

ui <- fluidPage(
    titlePanel("Air Quality Index in Indian Cities"),
    add_busy_spinner(spin = "fading-circle"),
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "city",
                label = "Select City",
                choices = c("Bengaluru","Bhopal","Chandigarh","Chennai","Gandhinagar","Hyderabad","Jaipur","Kolkata","Lucknow","Mumbai","Mysore",
                            "Nagpur","New Delhi","Patna","Shillong","Thiruvananthapuram","Thrissur","Visakhapatnam"
                )
            ),
            dateRangeInput(
                inputId = "date_range",
                label = "Select date range",
                start = min(Indian_air_quality_index$date),
                end = max(Indian_air_quality_index$date)
            )
        ),
        
        mainPanel(
            plotlyOutput(outputId = "Quality_plot")
        )
    )
)


# Server

server <- function(input, output) {
    output$Quality_plot <- renderPlotly({
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
    
    
    output$markdown <- renderUI({
        HTML(markdown::markdownToHTML(knit('RMarkdownFile.rmd', quiet = TRUE)))
    })
}

# Shiny App
shinyApp(ui = ui, server = server)
