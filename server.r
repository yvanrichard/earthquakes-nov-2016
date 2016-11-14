##############
## server.r ##
##############

library(data.table)
library(ggplot2)
library(shiny)
library(shinyjs)
library(raster)
library(scales)
library(mapview)
library(RColorBrewer)
library(rgl)
library(leaflet)
library(gplots)
library(lubridate)

source('helpers.r')

options(scipen = 100)

mapbox.token <- "pk.eyJ1IjoieXZhbi1yaWNoYXJkIiwiYSI6ImVhM0NlOTgifQ.LYcg6vzjrVl7zk750CH9sA"

load('data/data.rdata', verb=T)

cols <- rich.colors(100)

colby_choices <- c('Magnitude' = 'magnitude',
                  'Depth'     = 'depth',
                  'Time'      = 'datetime')

shinyServer(function(input, output, session) {
  
#################
## UI elements ##
#################

    output$mag_sel <- renderUI({
        sI <- sliderInput("mag_sel", label = 'Magnitude',
                         min = floor(min(quakedata$quakes$magnitude)*10)/10,
                         max = ceiling(max(quakedata$quakes$magnitude)*10)/10,
                         step = 0.2, value = c(floor(min(quakedata$quakes$magnitude)*10)/10, ceiling(max(quakedata$quakes$magnitude)*10)/10), round=TRUE)
        return(sI)
    })

    output$depth_sel <- renderUI({
        sI <- sliderInput("depth_sel", label = 'Depth (km)',
                         min = floor(min(quakedata$quakes$depth)*10)/10,
                         max = ceiling(max(quakedata$quakes$depth)*10)/10,
                         step = 0.2, value = c(floor(min(quakedata$quakes$depth)*10)/10,
                                               ceiling(max(quakedata$quakes$depth)*10)/10), round=TRUE)
        return(sI)
    })

    output$time_sel <- renderUI({
        sI <- sliderInput("time_sel", label = 'Time',
                         min = min(quakedata$quakes$datetime),
                         max = max(quakedata$quakes$datetime),
                         step = 0.2, value = range(quakedata$quakes$datetime), round=TRUE)
        return(sI)
    })
    
    output$map_colour <- renderUI({
        return(selectInput("map_colour", label = 'Map colour by',
                           choices = colby_choices, selected = 'magnitude'))
    })

    getquakes <- reactive({
        q <- NULL
        if (!is.null(input$mag_sel) & !is.null(input$depth_sel)) {
            q <- subset(quakedata$quakes.sp,
                       magnitude >= input$mag_sel[1] & magnitude <= input$mag_sel[2] &
                       depth >= input$depth_sel[1] & depth <= input$depth_sel[2] &
                       datetime >= input$time_sel[1] & datetime <= input$time_sel[2])
        }
        return(q)
    })
    
    ## Map
    mapdat <- reactive ({

        pts <- getquakes()
        map <- NULL
        if (!is.null(pts)) {

            urltemplate <- sprintf("https://api.mapbox.com/styles/v1/mapbox/dark-v9/tiles/256/{z}/{x}/{y}?access_token=%s", mapbox.token)

            pts$size <- rescale(pts$magnitude^2, from = c(1, 8^2), to = c(0.001, 15))

            if (input$map_colour == 'magnitude') {
                pts <- pts[order(pts$magnitude),]
                pal <- colorNumeric(
                          palette = cols, #'YlOrRd',
                          domain  = range(pts$magnitude)
                      )
                dpal <- function(v) substr(pal(v), 1, 7)

                map <- leaflet::leaflet() %>% addTiles(urlTemplate = urltemplate) %>%
                  addCircleMarkers(data      = pts,  color = ~ dpal(magnitude),
                                   fillColor = ~ pal(magnitude), weight = 1, radius = pts$size,
                                   popup     = sprintf('Date/time: %s<br>Magnitude: %f<br>Depth: %f',
                                                       pts$datetime, pts$magnitude, pts$depth),
                                   opacity   = 1, fillOpacity = 1)  %>% #clearBounds() %>%
                  addLegend("topright", pal = pal, values = pts$magnitude, title = "Magnitude")
            } else if (input$map_colour == 'depth') {
                pts <- pts[order(pts$depth),]
                pal <- colorNumeric(
                          palette = rev(cols), #'YlOrRd',
                          domain  = range(pts$depth)
                      )
                dpal <- function(v) substr(pal(v), 1, 7)

                map <- leaflet::leaflet() %>% addTiles(urlTemplate = urltemplate) %>%
                  addCircleMarkers(data      = pts,  color = ~ dpal(depth),
                                   fillColor = ~ pal(depth), weight = 1, radius = pts$size,
                                   popup     = sprintf('Date/time: %s<br>Magnitude: %f<br>Depth: %f',
                                                       pts$datetime, pts$magnitude, pts$depth),
                                   opacity   = 1, fillOpacity = 1)  %>% #clearBounds() %>%
                  addLegend("topright", pal = pal, values = pts$depth, title = "Depth (km)")
                
            } else if (input$map_colour == 'datetime') {
                pts <- pts[order(pts$datetime),]
                pts$datetime2 <- as.numeric(pts$datetime)
                pal <- colorNumeric(
                          palette = cols, #rev(brewer.pal(9, 'RdPu')),
                          domain  = range(pts$datetime2)
                      )
                dpal <- function(v) substr(pal(v), 1, 7)

                map <- leaflet::leaflet() %>% addTiles(urlTemplate = urltemplate) %>%
                  addCircleMarkers(data      = pts,  color = ~ dpal(datetime2),
                                   fillColor = ~ pal(datetime2), weight = 1, radius = pts$size,
                                   popup     = sprintf('Date/time: %s<br>Magnitude: %f<br>Depth: %f',
                                                       pts$datetime, pts$magnitude, pts$depth),
                                   opacity   = 1, fillOpacity = 1)  %>% #clearBounds() %>%
                  addLegend("topright", pal = pal, values = pts$datetime2, title = "Date/time",
                            labFormat = )
                
            }
            return(map)
        }
        
    })
    output$myMap <- renderLeaflet(mapdat())

    output$densplot_mag <- renderPlot({
        d <- as.data.frame(getquakes())
        if (!is.null(d)) {
            g <- ggplot(d, aes(x = magnitude)) +
              geom_histogram(color = "#41AB5D", fill = "#41AB5D88", bins = 30) +
            labs(x = 'Magnitude', y = 'Number of quakes') +
              theme_minimal()
            return(g)
        }
    })
    output$densplot_depth <- renderPlot({
        d <- as.data.frame(getquakes())
        if (!is.null(d)) {
            g <- ggplot(d, aes(x = depth)) +
              geom_histogram(color = "#41AB5D", fill = "#41AB5D88", bins = 30) +
            labs(x = 'Depth', y = 'Number of quakes') +
              theme_minimal()
            return(g)
        }
    })

    ## input <- list(mag_sel = c(5, 8), depth_sel = c(0, 150), map_colour = 'magnitude')
    output$timeline_mag <- renderPlot({
        d <- as.data.frame(getquakes())
        if (!is.null(d)) {
            d <- d[order(d$magnitude),]
            g <- ggplot(d, aes(x = datetime, xend = datetime,
                              y = 0, yend = magnitude, colour = magnitude^2)) +
              geom_segment() +
              geom_smooth(aes(x = datetime, y = magnitude), se=F) +
              scale_color_gradientn(colours=brewer.pal(9, 'YlOrRd'), guide = 'none') +
              coord_cartesian(xlim = range(d$datetime)) +
              labs(x = NULL, y = 'Magnitude') +
              theme_minimal() +
              theme(panel.grid.minor.x = element_blank(),
                    panel.grid.minor.y = element_blank())
            return(g)
        }
    })

    output$timeline_depth <- renderPlot({
        d <- as.data.frame(getquakes())
        if (!is.null(d)) {
            d$col <- "#08306B77"
            d <- d[order(d$depth),]
            g <- ggplot(d, aes(x = datetime, xend = datetime,
                              y = 0, yend = depth, colour = magnitude^2)) +
              geom_segment() +
              geom_smooth(aes(x = datetime, y = depth), se=F) +
              scale_color_gradientn(colours=brewer.pal(9, 'YlOrRd'), guide = 'none') +
              coord_cartesian(xlim = range(d$datetime)) +
              scale_y_log10() +
              labs(x = NULL, y = 'Depth (km)') +
              theme_minimal() +
              theme(panel.grid.minor.x = element_blank(),
                    panel.grid.minor.y = element_blank())
            return(g)
        }
    })

    output$timeline_dens <- renderPlot({
        d <- as.data.frame(getquakes())
        if (!is.null(d)) {
            d$col <- "#08306B77"
            g <- ggplot(d, aes(x = datetime)) +
              geom_histogram(color = "#41AB5D", fill = "#41AB5D88", bins = 50) +
            coord_cartesian(xlim = range(d$datetime)) +
              labs(x = 'Time', y = 'No. quakes') +
              theme_minimal() +
              theme(panel.grid.minor.x = element_blank(),
                    panel.grid.minor.y = element_blank())
            return(g)
        }
    })
    
    output$scatter3d <- renderRglwidget({
        p <- getquakes()
        if (!is.null(p)) {
            options(rgl.useNULL=TRUE)
            d <- as.data.frame(spTransform(p, CRS('+init=epsg:3994')))

            d$size <- rescale(d$magnitude^2, from = c(0, 8^2), to = c(0.001, 50000))
            if (input$map_colour == 'magnitude') {
                d$col <- cols[rescale(as.numeric(d$magnitude), to = c(1, 100))]
            } else if (input$map_colour == 'depth') {
                d$col <- cols[rescale(as.numeric(d$depth), to = c(1, 100))]
            } else if (input$map_colour == 'datetime') {
                d$col <- cols[rescale(as.numeric(d$datetime), to = c(1, 100))]
            }
            zrat <- 1
            try(rgl.close())
            aspect3d(1,1, zrat)
            spheres3d(x=d$longitude, y=d$latitude, z=-7*d$depth*1000,
                      col=d$col, radius=d$size)
            lines3d(si$long, si$lat, rep(0, nrow(si)))
            lines3d(ni$long, ni$lat, rep(0, nrow(ni)))
            ## axes3d()
            rglwidget()
        }
    })

    output$scatter3dp <- renderPlaywidget({ 
        p <- getquakes()
        if (!is.null(p)) {
            options(rgl.useNULL=TRUE)
            d <- as.data.frame(spTransform(p, CRS('+init=epsg:3994')))

            d$size <- rescale(d$magnitude^2, from = c(0, 8^2), to = c(0.001, 50000))
            if (input$map_colour == 'magnitude') {
                d$col <- cols[rescale(as.numeric(d$magnitude), to = c(1, 100))]
            } else if (input$map_colour == 'depth') {
                d$col <- cols[rescale(as.numeric(d$depth), to = c(1, 100))]
            } else if (input$map_colour == 'datetime') {
                d$col <- cols[rescale(as.numeric(d$datetime), to = c(1, 100))]
            }
            zrat <- 1
            try(rgl.close())
            aspect3d(1,1, zrat)
            s <- spheres3d(x=d$longitude, y=d$latitude, z=-10*d$depth*1000,
                          col=d$col, radius=d$size, alpha = 0.5)["data"]
            lines3d(si$long, si$lat, rep(0, nrow(si)))
            lines3d(ni$long, ni$lat, rep(0, nrow(ni)))
            ## axes3d()
            widget <- rglwidget(width=700, height=700) %>%
              playwidget(ageControl(births = as.numeric(d$datetime),
                                    ages = c(0, 1800, 3000),
                                    objids = s,
                                    alpha = c(1, 0.5, 0)),
                         start = as.numeric(min(d$datetime)), stop = as.numeric(max(d$datetime)),
                         step = 60, rate = 4)
        }
    })

    quakedata <- reactiveValues(
                    quakes = allquakes,
                    quakes.sp = allquakes.sp,
                    quakes.spt = allquakes.spt
                )

    observeEvent(input$updatedata, {
        ## Download data from GeoNet
        allquakes <- suppressWarnings(fread('http://quakesearch.geonet.org.nz/csv?startdate=2016-11-13T11:00:00&enddate=2020-11-13T11:00:00'))

        allquakes[longitude < 0, longitude := longitude + 360]
        allquakes[, datetime := as.POSIXct(origintime, tz = 'UTC')]
        allquakes[, datetime := with_tz(parse_date_time(origintime, 'ymd HMS', tz = 'UTC'), 'NZ')]

        cat(sprintf('\nDownloaded %i quakes\n', nrow(allquakes)))

        allquakes.sp <- copy(allquakes)
        coordinates(allquakes.sp) <- ~ longitude + latitude
        proj4string(allquakes.sp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

        allquakes.sp <- crop(allquakes.sp, extent(c(166.5, 179, -47, -34.5)))
        allquakes <- allquakes[publicid %in% allquakes.sp$publicid]

        allquakes.spt <- spTransform(allquakes.sp, CRS('+init=epsg:3994'))
        
        quakedata$quakes.sp <- allquakes.sp
        quakedata$quakes.spt <- allquakes.spt
        quakedata$quakes <- allquakes
    })

    
    
##########
## Text ##
##########
    
    output$caption <- renderText({
        d <- as.data.frame(getquakes())
        if (!is.null(d)) {
            sprintf('Selection of %i (out of %i) quakes that occurred between %s in %s in New Zealand',
                    nrow(d), nrow(quakedata$quakes), min(d$datetime), max(d$datetime))
        }
    })
    
    
})

