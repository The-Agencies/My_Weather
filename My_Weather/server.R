library(shiny)
library(leaflet)
library(dplyr)
library(plotly)
library(png)
library(stringi)

load("data/collected_data.Rdata")

collected_data$new_city=gsub(" ","_",stri_trans_totitle(collected_data$city_other))

collected_data$maps=paste0("https://www.google.com/maps/place/",collected_data$new_city,"+",collected_data$state)

collected_data$wikipedia= paste0("https://en.wikipedia.org/wiki/",collected_data$new_city,",_",collected_data$state)


myicon=function(condition){
    makeIcon(
        iconUrl = paste0("data/",condition,".png"),
        iconWidth = 70, iconHeight = 70,
        iconAnchorX = 22, iconAnchorY = 94
    )}

collected_data_tomorrow_distinct_city=distinct(collected_data,city_other,.keep_all = TRUE)


shinyServer(function(input, output) {
    
    output$leaflet <- renderLeaflet({
        top = max(collected_data_tomorrow_distinct_city$latitude,na.rm = T) # north lat
        left = min(collected_data_tomorrow_distinct_city$longitude,na.rm = T) # west long
        right = max(collected_data_tomorrow_distinct_city$longitude,na.rm = T) # west long
        bottom = min(collected_data_tomorrow_distinct_city$latitude,na.rm = T) # south lat
        
        leaflet(collected_data_tomorrow_distinct_city)%>%setMaxBounds(right,bottom,left,top)%>% addTiles(options = providerTileOptions(minZoom = 6, maxZoom = 8)) %>%
            addMarkers(
                data = collected_data_tomorrow_distinct_city,
                icon = myicon(collected_data_tomorrow_distinct_city$condition),
                popup=~ sprintf(
                    'City = %s <br/> County = %s <br/> <a href=%s  target="_blank">Open in Google maps</a> <br/> <a href=%s  target="_blank"> Open in Wikipedia</a>', stri_trans_totitle(city_other), county,maps,wikipedia
                )
            )
    })
    
    
    clicked_leaflet <- reactiveValues(clickedMarker=NULL)
    observeEvent(input$leaflet_marker_click,{
        clicked_leaflet$clickedMarker <- input$leaflet_marker_click
    })
    
    
    selected_coordinates= reactive(({
        c(clicked_leaflet$clickedMarker$lng,clicked_leaflet$clickedMarker$lat)
    }))
    
    output$fish=renderTable({
        selected_data()
    })
    
    selected_data= reactive(({
        if(is.null(clicked_leaflet$clickedMarker))
            return(NULL)
        
        filter(collected_data, longitude == as.numeric(as.character(selected_coordinates()[1])),latitude==as.numeric(as.character(selected_coordinates()[2])))
    }))
    
    output$max_min_temperature_plotly_16days=renderPlotly({
        temp=selected_data()
        if(is.null(temp))
            return(NULL)
        plot_ly() %>%
            add_lines(x = temp$date, y = temp$max_temperature, name = "Maximum Temperature") %>%
            add_lines(x = temp$date, y = temp$min_temperature, name = "Minimum Temperature")%>%
            add_lines(x = temp$date, y = temp$day_temperature, name = "Day Temperature") %>%
            add_lines(x = temp$date, y = temp$night_temperature, name = "Night Temperature") %>%
            add_lines(x = temp$date, y = temp$eve_temperature, name = "Evening Temperature") %>%
            add_lines(x = temp$date, y = temp$morn_temperature, name = "Morning Temperature") %>%
            layout(title = '',
                   legend = list(orientation = 'h'),
                   xaxis = list(title = ""))
    })
    
    output$max_min_temperature_plotly_3days=renderPlotly({
        temp=selected_data()
        if(is.null(temp))
            return(NULL)
        
        temp=temp[1:3,]
        plot_ly() %>%
            add_lines(x = temp$date, y = temp$max_temperature, name = "Maximum Temperature") %>%
            add_lines(x = temp$date, y = temp$min_temperature, name = "Minimum Temperature")%>%
            add_lines(x = temp$date, y = temp$day_temperature, name = "Day Temperature") %>%
            add_lines(x = temp$date, y = temp$night_temperature, name = "Night Temperature") %>%
            add_lines(x = temp$date, y = temp$eve_temperature, name = "Evening Temperature") %>%
            add_lines(x = temp$date, y = temp$morn_temperature, name = "Morning Temperature") %>%
            layout(title = '',
                   legend = list(orientation = 'h'),
                   xaxis = list(title = ""))
    })
    
    output$max_min_temperature_plotly_5days=renderPlotly({
        temp=selected_data()
        if(is.null(temp))
            return(NULL)
        temp=temp[1:5,]
        plot_ly() %>%
            add_lines(x = temp$date, y = temp$max_temperature, name = "Maximum Temperature") %>%
            add_lines(x = temp$date, y = temp$min_temperature, name = "Minimum Temperature")%>%
            add_lines(x = temp$date, y = temp$day_temperature, name = "Day Temperature") %>%
            add_lines(x = temp$date, y = temp$night_temperature, name = "Night Temperature") %>%
            add_lines(x = temp$date, y = temp$eve_temperature, name = "Evening Temperature") %>%
            add_lines(x = temp$date, y = temp$morn_temperature, name = "Morning Temperature") %>%
            layout(title = '',
                   legend = list(orientation = 'h'),
                   xaxis = list(title = ""))  
    })
    
    
    output$humidty_rain_cloudness_16days=renderPlotly({
        temp=selected_data()
        if(is.null(temp))
            return(NULL)
        
        plot_ly(temp, x = ~date, y = ~clouds, type = 'bar', name = 'Clouds') %>%
            add_trace(y = ~humidty, name = 'Humidity') %>%
            layout(yaxis = list(title = '%'), barmode = 'group')%>%
            add_trace(x = ~date, y = ~rain, type = 'scatter', mode = 'lines', name = 'Rainfall', yaxis = 'y2',
                      line = list(color = '#45171D'),
                      hoverinfo = "text",
                      text = ~paste(rain, '°F')) %>%
            layout(title = '',
                   xaxis = list(title = ""),
                   yaxis = list(side = 'left', title = 'Humidity, clouds (%)', showgrid = FALSE, zeroline = FALSE),
                   yaxis2 = list(side = 'right', overlaying = "y", title = 'Rainfall', showgrid = FALSE, zeroline = FALSE))
    })
    
    
    output$humidty_rain_cloudness_5days=renderPlotly({
        temp=selected_data()
        if(is.null(temp))
            return(NULL)
        temp=temp[1:5,]
        plot_ly(temp, x = ~date, y = ~clouds, type = 'bar', name = 'Clouds') %>%
            add_trace(y = ~humidty, name = 'Humidity') %>%
            layout(yaxis = list(title = '%'), barmode = 'group')%>%
            add_trace(x = ~date, y = ~rain, type = 'scatter', mode = 'lines', name = 'Rainfall', yaxis = 'y2',
                      line = list(color = '#45171D'),
                      hoverinfo = "text",
                      text = ~paste(rain, '°F')) %>%
            layout(title = '',
                   xaxis = list(title = ""),
                   yaxis = list(side = 'left', title = 'Humidity, clouds (%)', showgrid = FALSE, zeroline = FALSE),
                   yaxis2 = list(side = 'right', overlaying = "y", title = 'Rainfall', showgrid = FALSE, zeroline = FALSE))      
    })
    
    
    output$humidty_rain_cloudness_3days=renderPlotly({
        temp=selected_data()
        if(is.null(temp))
            return(NULL)
        temp=temp[1:3,]
        plot_ly(temp, x = ~date, y = ~clouds, type = 'bar', name = 'Clouds') %>%
            add_trace(y = ~humidty, name = 'Humidity') %>%
            layout(yaxis = list(title = '%'), barmode = 'group')%>%
            add_trace(x = ~date, y = ~rain, type = 'scatter', mode = 'lines', name = 'Rainfall', yaxis = 'y2',
                      line = list(color = '#45171D'),
                      hoverinfo = "text",
                      text = ~paste(rain, '°F')) %>%
            layout(title = '',
                   xaxis = list(title = ""),
                   yaxis = list(side = 'left', title = 'Humidity, clouds (%)', showgrid = FALSE, zeroline = FALSE),
                   yaxis2 = list(side = 'right', overlaying = "y", title = 'Rainfall', showgrid = FALSE, zeroline = FALSE))     
    })
    
    condition1<-reactive({
        if(is.null(selected_data())){
            result=0
        }else{
            result=1
        }
        result
    })
    
    output$condition1 <- renderText({
        condition1()
    })
    
    outputOptions(output, 'condition1', suspendWhenHidden=FALSE)
    
})