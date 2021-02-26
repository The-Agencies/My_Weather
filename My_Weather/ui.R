library(leaflet)
library(plotly)
shinyUI(fluidPage(
    fluidRow(
        column(width=6,align="center",
               br(),
               tags$h4(tags$strong("Leaflet, Plotly and Shiny: Weather Forecasts In The Northeast",
                                   style="text-align:center;color: #0000ff")),
               
               leafletOutput("leaflet",height = 500)
        ),
        
        column(width=6,align="center",
               conditionalPanel(
                   
                   condition = "output.condition1 == 0",
                   br(),
                   tags$h4("About",style='color:blue'),
                   tags$p('This Shiny App helps visualize weather forecasts in the US Northeast for the next two weeks.
                            The icons show weather conditions for tomorrow. You can click on an icon and then select any of the tabs that show up to see
                            the weather conditions for the next three, five and sixteen days. The weather forecast data includes temperature (minimum, maximum, day time, night time, morning and evening), precipitation, humidity and clouds. 
                            If you click on an icon and then click "Open in Google maps" or "Open in Wikipedia", you will be directed to Google maps or Wikipedia and you can get other information about the specific city. 
                            The app uses the JavaScript libraries Leaflet and Plotly. The data is from',span(tags$a(href="https://openweathermap.org/forecast16", "OpenWeatherMap."),'You can also make the app to update in realtime if you have a server that provides that service. All the plots are interactive.') ,style="text-align:left;color:black;font-size:140%")
               ),
               
               
               conditionalPanel(   
                   condition = "output.condition1 == 1",
                   tabsetPanel(
                       tabPanel(tags$em("Temperature",style="font-size:120%"),
                                tags$hr(style="border-color: #ffc266;"),
                                tabsetPanel(
                                    tabPanel(tags$em("Three Days"),plotlyOutput("max_min_temperature_plotly_3days")),
                                    tabPanel(tags$em("Five Days"), plotlyOutput("max_min_temperature_plotly_5days")),
                                    tabPanel(tags$em("Two Weeks"), plotlyOutput("max_min_temperature_plotly_16days"))
                                )),
                       
                       tabPanel(tags$em("Rainfall, Humidity and Clouds",style="font-size:120%"),
                                tags$hr(style="border-color:  #d27979;"),
                                tabsetPanel(
                                    tabPanel(tags$em("Three Days"),plotlyOutput("humidty_rain_cloudness_3days")),
                                    tabPanel(tags$em("Five Days"),plotlyOutput("humidty_rain_cloudness_5days")),
                                    tabPanel(tags$em("Two Weeks"),plotlyOutput("humidty_rain_cloudness_16days"))
                                ))
                   )))
    ))
)