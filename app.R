# 20200325 R Shiny App for COVID-19 Tracking

library(shiny)


# Adopted from https://www.r-bloggers.com/tidying-the-new-johns-hopkins-covid-19-time-series-datasets/
source("COVID-19-JHU.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("COVID-19 Cases Track"),
   
   
   # Select y variable
   selectInput("ycol",
               "Y Variable:",
               c("Confirmed cases (Cumulative)", "Deaths (Cumulative)", "Confirmed cases (Average Daily Increase)", "Deaths (Average Daily Increase)")),
   
   # Plot
   plotOutput("distPlot")
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    
    # y variable selection
    if (input$ycol == "Confirmed cases (Cumulative)") {
      jh_covid19_data[, yvar := confirmed]
    } else if (input$ycol == "Deaths (Cumulative)") {
      jh_covid19_data[, yvar := deaths]
    } else if (input$ycol == "Confirmed cases (Average Daily Increase)") {
      jh_covid19_data[, yvar := confirmed_inc_avg]
    } else {
      jh_covid19_data[, yvar := deaths_inc_avg]
    }
    
    return(jh_covid19_data)
    
  }) 
  
   output$distPlot <- renderPlot({
     # Plot
     ggplot(data=selectedData()) +
       geom_line(aes(x=DS100, y=yvar, color=country), size=1) +
       theme_minimal() +
       labs(x = "\nDays since 100 confirmed cases", y = "COVID-19 cases\n")+
       # scale_y_continuous(trans = "log10", labels = scales::comma) +
       theme(text = element_text(size=18), panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
       scale_color_manual(values = c("#FF3300", "#000000", "#E69F00", "#009E73", "#CC79A7", "#0072B2")) +
       gghighlight(TRUE,  label_key = country, use_direct_label = TRUE,
                 label_params = list(segment.color = NA, nudge_x = 1))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

