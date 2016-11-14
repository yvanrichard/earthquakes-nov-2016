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
## library(Rgraphviz)
## library(rhandsontable)
library(RColorBrewer)
## library(maptools)
## library(shinyRGL)
library(rgl)
## library(threejs)
library(leaflet)
library(gplots)
library(lubridate)

source('helpers.r')

options(scipen = 100)

mapbox.token <- "pk.eyJ1IjoieXZhbi1yaWNoYXJkIiwiYSI6ImVhM0NlOTgifQ.LYcg6vzjrVl7zk750CH9sA"

load('data/data.rdata', verb=T)
## source('shiny_funs.r', chdir=T)

## ext <- extent(c(166.5, 179, -47, -34.5))
## ext <- extent(c(172, 176, -44, -40.5))
## extp <- as(ext, 'SpatialPolygons')
## proj4string(extp) <- proj4string(nz)


## nz <- crop(nz, extent(c(166.5, 179, -47, -34.5)))
## quakes <- crop(quakes, ext)

cols <- rich.colors(100)

colby_choices <- c('Magnitude' = 'magnitude',
                  'Depth'     = 'depth',
                  'Time'      = 'datetime')

## plot(quakes, col=alpha(quakes$col, 0.4), bg=alpha(quakes$col, 0.3), pch = 21, cex = quakes$size)
## quakes

## quakes$col <- cols[rescale(as.numeric(as.POSIXct(quakes$origintime)), to = c(1, 100))]

## scatterplot3js(x=quakes$longitude, y=quakes$latitude, z=-quakes$depth,
##                color=quakes$col, size=quakes$size, renderer="canvas", stroke=0.5)


## surface3d(ni$x, ni$y, ni$z)

## plot(nzd[[2]])
## as(nz, 'data.frame')
## as.data.frame(coordinates(nz))

## prepare_app_data <- function(fields) {
##     fields[is.na(label) | label == '', label := column_name]
##     fields[is.na(category) | category == '', category := 'Other']
##     if ('Other' %in% fields$category) {
##         fields[, category := factor(as.character(category),
##                                     levels = c(setdiff(unique(category), 'Other'), 'Other'))]
##     } else {
##         fields[, category := factor(as.character(category),
##                                     levels = unique(category))]
##     }
##     cats <- sapply(levels(fields$category), function(x) {
##         y0 <- fields[category == x, .(column_name, table_schema)]
##         y <- y0$column_name
##         names(y) <- fields[category == x, sprintf('%s (%s)', label, table_schema)]
##         return(y)
##     } , simplify=F)

##     lab2var <- fields$column_name
##     names(lab2var) <- fields$label

##     var2lab <- fields$label
##     names(var2lab) <- fields$column_name

##     return(list(fields = fields, cats = cats, lab2var = lab2var, var2lab = var2lab))
## }

## ## TESTING!!!
## selfields <- rbind(selfields,
##                   data.table(table_schema = 'Custom', table_name = 'Custom', column_name = 'myVar',
##                              column_type = 'character', category = 'Custom', included = 'Y', label = 'myVar',
##                              unit = NA, year = NA))
## reachdatawide[, myVar := ifelse(uspasture > 0.5, 'Good', ifelse(segmintnorm > 10, 'Medium', 'Bad'))]
## rval <- list(reachdatawide = reachdatawide)

## d <- prepare_app_data(selfields)
## fields <- d$fields
## cats <- d$cats
## lab2var <- d$lab2var
## var2lab <- d$var2lab

## regcs <- c('All regions',
##           sort(setdiff(unique(as.character(reachdatawide$regc)), c(NA, "Area Outside Region"))))
## names(regcs) <- sub(' Region$', '', regcs)

## for (v in names(reachdatawide)) {
##     if (!is.numeric(reachdatawide[, get(v)]) & !is.factor(reachdatawide[, get(v)])) {
##         reachdatawide[, eval(v) := factor(get(v))]
##     }
## }

## input <- list(x_var='myVar', y_var='lognconcentration', c_var='order', s_var='order', propsampled=5, regc = "Manawatu-Wanganui Region", plu_stat='Percentage', plu_cat='Good', plu_var='myVar', createdvarname='myVar')

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
    
    ## output$x_var <- renderUI({
    ##     selectInput("x_var", "Select X variable", choices = rval$cats, selected = "uspasture")
    ## })

    ## output$y_var <- renderUI({
    ##     selectInput("y_var", "Select Y variable", choices = rval$cats, selected = "lognconcentration")
    ## })

    ## output$c_var <- renderUI({
    ##     selectInput("c_var", "Select colour variable", choices = rval$cats, selected = "order")
    ## })

    ## output$s_var <- renderUI({
    ##     selectInput("s_var", "Select size variable", choices = rval$cats, selected = "order")
    ## })
    
    ## output$regc <- renderUI({
    ##     selectInput("regc", 
    ##                 label = NULL,
    ##                 choices = c(regcs),
    ##                 selected = 'All regions')
    ## })

    ## output$propsampled <- renderUI({
    ##     sliderInput("propsampled", "% of reaches included in plots", min = 0, max = 100, post='%', step = 1, value = 5)
    ## })

    ## output$plu_var <- renderUI({
    ##     selectInput("plu_var", "Select variable", choices = rval$cats, selected = 'lognconcentration')
    ## })

    ## output$plu_cat <- renderUI({
    ##     if (is_plu_var_cat()) {
    ##         varcats <- levels(rval$reachdatawide[, get(input$plu_var)])
    ##         selectInput("plu_cat", "Select category of interest", choices = varcats, selected = varcats[1])
    ##     }
    ## })
    
    ## output$plu_stat <- renderUI({
    ##     if (is_plu_var_cat()) {
    ##         choices <- list("Percentage (%)" = 'Percentage')
    ##     } else {
    ##         choices <- list("Mean" = 'Mean', "Min" = 'Min', "Max" = 'Max')
    ##     }
    ##     radioButtons("plu_stat", label = "Aggregate type",
    ##                  choices = choices, 
    ##                  selected = choices[1])
    ## })

    ## output$plu_valrank <- renderUI({
    ##     choices <- list("Value" = 1, "National rank" = 2, "Regional rank" = 3)
    ##     radioButtons("plu_valrank", label = "Scale",
    ##                  choices = choices, 
    ##                  selected = choices[1])
    ## })

    getquakes <- reactive({
        q <- NULL
        if (!is.null(input$mag_sel) & !is.null(input$depth_sel)) {
            q <- subset(quakedata$quakes.sp,
                       magnitude >= input$mag_sel[1] & magnitude <= input$mag_sel[2] &
                       depth >= input$depth_sel[1] & depth <= input$depth_sel[2] &
                       datetime >= input$time_sel[1] & datetime <= input$time_sel[2])
        }
        ## dt <- data.table(datetime = seq(min(q$datetime) + minutes(15), max(q$datetime) - minutes(15), 30*60))
        ## dt[, n_quakes := sapply(1:nrow(dt), function(i) {
        ##     t <- dt$datetime[i]
        ##     nrow(subset(q, datetime >= t - minutes(15) & datetime <= t + minutes(15)))
        ## })]
        return(q)
    })
    
    ## Map
    mapdat <- reactive ({
        ## if (length(input$sp) & length(input$loc)) {

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
            
            ## d$col <- "#08306B77"
            ## ifelse(d$magnitude >= input$mag_sel[1] & d$magnitude <= input$mag_sel[2],
            ##                "#08306B", "#9ECAE1")
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
            ## d$col <- ifelse(d$depth >= input$depth_sel[1] & d$depth <= input$depth_sel[2],
            ##                "#08306B", "#9ECAE1")
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
            ## d$col <- ifelse(d$depth >= input$depth_sel[1] & d$depth <= input$depth_sel[2],
            ##                "#08306B", "#9ECAE1")
            ## d <- d[order(d$depth),]
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
            zrat <- 1 #.2 * diff(range(d$latitude)) / diff(range(d$depth))
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

    output$scatter3dp <- renderPlaywidget({ #renderRglwidget({
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
            zrat <- 1 #.2 * diff(range(d$latitude)) / diff(range(d$depth))
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
        ## download.file('http://quakesearch.geonet.org.nz/csv?startdate=2016-11-13T11:00:00&enddate=2020-11-13T11:00:00', 'data/earthquakes.csv')
        allquakes <- suppressWarnings(fread('http://quakesearch.geonet.org.nz/csv?startdate=2016-11-13T11:00:00&enddate=2020-11-13T11:00:00'))

        allquakes[longitude < 0, longitude := longitude + 360]
        allquakes[, datetime := as.POSIXct(origintime, tz = 'UTC')]
        allquakes[, datetime := with_tz(parse_date_time(origintime, 'ymd HMS', tz = 'UTC'), 'NZ')]

        cat(sprintf('\nDownloaded %i quakes\n', nrow(allquakes)))

        allquakes.sp <- copy(allquakes)
        coordinates(allquakes.sp) <- ~ longitude + latitude
        proj4string(allquakes.sp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

        ## nz <- crop(nz, extent(c(166.5, 179, -47, -34.5)))
        allquakes.sp <- crop(allquakes.sp, extent(c(166.5, 179, -47, -34.5)))
        allquakes <- allquakes[publicid %in% allquakes.sp$publicid]

        allquakes.spt <- spTransform(allquakes.sp, CRS('+init=epsg:3994'))
        
        quakedata$quakes.sp <- allquakes.sp
        quakedata$quakes.spt <- allquakes.spt
        quakedata$quakes <- allquakes
    })

    ## output$scatter3d <- renderScatterplotThree({
    ##     d <- as.data.frame(getquakes())
    ##     d$size <- rescale(d$magnitude^2, from = c(1, 8^2), to = c(0.001, 5))

    ##     if (input$map_colour == 'magnitude') {
    ##         d$col <- cols[rescale(as.numeric(d$magnitude), to = c(1, 100))]
    ##     } else if (input$map_colour == 'depth') {
    ##         d$col <- cols[rescale(as.numeric(d$depth), to = c(1, 100))]
    ##     } else if (input$map_colour == 'datetime') {
    ##         d$col <- cols[rescale(as.numeric(d$datetime), to = c(1, 100))]
    ##     }
    ##     scatterplot3js(x=d$longitude, y=d$latitude, z=-d$depth,
    ##                    color=d$col, size=d$size, renderer="canvas", stroke=0.5)
    ## })
    
    
## ##########
## ## Text ##
## ##########
    
    output$caption <- renderText({
        d <- as.data.frame(getquakes())
        if (!is.null(d)) {
            sprintf('Selection of %i (out of %i) quakes that occurred between %s in %s in New Zealand',
                    nrow(d), nrow(quakedata$quakes), min(d$datetime), max(d$datetime))
        }
    })

##     output$xvartxt <- renderText({
##         if (!is.null(input$x_var)) {
##             sprintf('%s', rval$var2lab[input$x_var])
##         }
##     })

##     output$yvartxt <- renderText({
##         if (!is.null(input$y_var)) {
##             sprintf('%s', rval$var2lab[input$y_var])
##         }
##     })

##     ###########
##     ## Plots ##
##     ###########

##     sampledata <- reactive({
##         if (!is.null(input$x_var) & !is.null(input$y_var) & !is.null(input$c_var) & !is.null(input$s_var) &
##             !is.null(input$propsampled) & !is.null(input$regc)) {
##             if (input$regc != 'All regions') {
##                 d <- rval$reachdatawide[regc == input$regc, .(x = get(input$x_var),
##                                       y = get(input$y_var),
##                                       s = get(input$s_var),
##                                       c = get(input$c_var))][
##                         !is.na(x) & !is.na(y) & !is.na(s) & !is.na(c)][
##                         sample(1:.N, round(input$propsampled * .N / 100))]
##             } else {
##                 d <- rval$reachdatawide[, .(x = get(input$x_var),
##                                       y = get(input$y_var),
##                                       s = get(input$s_var),
##                                       c = get(input$c_var))][
##                         !is.na(x) & !is.na(y) & !is.na(s) & !is.na(c)][
##                         sample(1:.N, round(input$propsampled * .N / 100))]
##             }
##             return(d)
##         } else return(NULL)
##     })
    
##     output$xyplot <- renderPlot({
##         d <- sampledata()
##         if (!is.null(d)) {
##             if (is.numeric(d[, x]) & !is.numeric(d[, y])) {
##                 h <- 0
##                 w <- NULL
##                 g <- ggplot(d, aes(x = y, y = x, size = s, colour = c, fill = c)) +
##                   labs(x = rval$var2lab[input$y_var],
##                        y = rval$var2lab[input$x_var]) +
##                   coord_flip() +
##                   geom_jitter(alpha = 0.4, height = h, width = w) +
##                   geom_violin(fill = '#FFFFFF77', size = 0.5, colour = 'black')
                
##             } else if (!is.numeric(d[, x]) & is.numeric(d[, y])) {
##                 h <- 0
##                 w <- NULL
##                 g <- ggplot(d, aes(x = x, y = y, size = s, colour = c, fill = c)) +
##                   labs(x = rval$var2lab[input$x_var],
##                        y = rval$var2lab[input$y_var]) +
##                   geom_jitter(alpha = 0.4, height = h, width = w) +
##                   geom_violin(fill = '#FFFFFF77', size = 0.5, colour = 'black')
                
##             } else {
##                 g <- ggplot(d, aes(x = x, y = y, size = s, colour = c, fill = c)) +
##                   labs(x = rval$var2lab[input$x_var], y = rval$var2lab[input$y_var]) +
##                   geom_point(alpha = 0.4) +
##                   scale_x_continuous(limits = c(lims[input$x_var, 'min'], lims[input$x_var, 'max']))
##             }
            
##             g <-  g +
##               geom_smooth(show.legend=F, colour = 'grey70', fill = 'grey90') +
##               theme_minimal() +
##               scale_colour_gradientn(name = rval$var2lab[input$c_var], colours = gplots::rich.colors(100)) +
##               scale_fill_gradientn(name = rval$var2lab[input$c_var], colours = gplots::rich.colors(100)) +
##               scale_size_continuous(name = rval$var2lab[input$s_var])

##             return(g)
##         } else return(NULL)
##     })

##     output$xdensplot <- renderPlot({
##         if (!is.null(rval$reachdatawide) & !is.null(input$x_var)) {
##             d <- rval$reachdatawide[, .(x = get(input$x_var))]
##             if (!is.null(d)) {
##                 g <- ggplot(d, aes(x = x)) +
##                   theme_minimal() +
##                   labs(x = rval$var2lab[input$x_var], y = 'Count')
##                 if (is.numeric(d[, x])) {
##                     g <- g + coord_cartesian(xlim=c(lims[input$x_var, 'min'], lims[input$x_var, 'max'])) +
##                       geom_histogram(color = "#2171B5", fill = "#2171B588", bins = 30)
##                 } else {
##                     g <- g + geom_bar(color = "#2171B5", fill = "#2171B588")
##                 }
##                 return(g)
##             } else return(NULL)
##         }
##     })
    
##     output$ydensplot <- renderPlot({
##         if (!is.null(rval$reachdatawide) & !is.null(input$y_var)) {
##             d <- rval$reachdatawide[, .(x = get(input$y_var))]
##             if (!is.null(d)) {
##                 g <- ggplot(d, aes(x = x)) +
##                   theme_minimal() +
##                   labs(x = rval$var2lab[input$y_var], y = 'Count')
##                 if (is.numeric(d[, x])) {
##                     g <- g + coord_cartesian(xlim=c(lims[input$y_var, 'min'], lims[input$y_var, 'max'])) +
##                       geom_histogram(color = "#2171B5", fill = "#2171B588", bins = 30)
##                 } else {
##                     g <- g + geom_bar(color = "#2171B5", fill = "#2171B588")
##                 }
##                 return(g)
##             } else return(NULL)
##         }
##     })

##     output$xmap <- renderImage({
##         if (length(input$x_var)) {
##             pngname <- mappng(input$x_var, rval$reachdatawide, reaches.spl, nz,
##                              rval$var2lab, input$regc, regcouncils, selfields)
##         } else pngname <- '.'
##         return( list(src = pngname,
##                      contentType = 'image/png', width = 300,
##                      alt = "Map being generated") )
##     }, deleteFile = F)

##     output$ymap <- renderImage({
##         if (length(input$y_var)) {
##             pngname <- mappng(input$y_var, rval$reachdatawide, reaches.spl, nz,
##                              rval$var2lab, input$regc, regcouncils, selfields)
##         } else pngname <- '.'
##         return( list(src = pngname,
##                      contentType = 'image/png', width = 300,
##                      alt = "Map being generated") )
##     }, deleteFile = F)

##     is_plu_var_cat <- reactive({
##         if (!is.null(input$plu_var) & !is.null(rval$reachdatawide)) {
##             if (!is.numeric(rval$reachdatawide[, get(input$plu_var)])) {
##                 return(T)
##             } else return(F)
##         } else return(F)
##     })

##     leafmapdata <- reactive({
##         if (!is.null(input$regc) & !is.null(input$plu_var) & !is.null(input$plu_stat)) {
##             f <- switch(input$plu_stat,
##                        'Mean' = function(x, ...) mean(x, ..., na.rm=T),
##                        'Min' = function(x, ...) min(x, ..., na.rm=T),
##                        'Max' = function(x, ...) max(x, ..., na.rm=T),
##                        'Percentage' = function(x, ...) 100 * mean(x %in% input$plu_cat, na.rm=T))
##             statname <- input$plu_stat
##             if (input$regc == 'All regions') {
##                 suppressWarnings(znat <- rval$reachdatawide[, .(z = f(get(input$plu_var))), regc])
##                 znat[!is.finite(z), z := NA]
##                 znat[, z.nat.rank := NA_real_]
##                 znat[!is.na(z), z.nat.rank := rank(z)]
##                 znat[, z.nat.rank := 100 * (z.nat.rank / sum(!is.na(z.nat.rank)))]
##                 znat[, z.reg.rank := z.nat.rank]

##                 p <- regwgsc
##                 p@data <- cbind(p@data, znat[match(p$name, regc), .(z, z.nat.rank, z.reg.rank)])

##                 if (is.numeric(p$z)) {
##                     p$z <- round(p$z, ifelse(input$plu_stat == 'Percentage', 1, 3))
##                 }

##                 zPalette <- colorNumeric(palette = gplots::rich.colors(10), domain=0)
##                 tryCatch({
##                     rnPalette <- colorNumeric(palette = gplots::rich.colors(10), domain=c(0, 100))
##                     rrPalette <- colorNumeric(palette = gplots::rich.colors(10), domain=c(0, 100))
##                     if (input$plu_stat == 'Percentage') {
##                         zPalette <- colorNumeric(palette = gplots::rich.colors(10), domain=c(0, 100))
##                     } else {
##                         zPalette <- colorNumeric(palette = gplots::rich.colors(10), domain=p$z)
##                     }
##                 }, error = function(e) {
##                     print('')
##                 })

##                 tiesnat <- p$z.nat.rank %in% p$z.nat.rank[duplicated(p$z.nat.rank)]
##                 tiesreg <- p$z.reg.rank %in% p$z.reg.rank[duplicated(p$z.reg.rank)]
##                 pop <- sprintf('Region: %s<br/>%s(%s%s): %s%s<br/>National rank: %s%%%s',
##                               p$name, statname, rval$var2lab[input$plu_var], 
##                               ifelse(input$plu_stat == 'Percentage', paste0(' = ', input$plu_cat), ''),
##                               as.character(p$z),
##                               ifelse(input$plu_stat == 'Percentage', '%', ''),
##                               as.character(round(p$z.nat.rank,1)), ifelse(tiesnat, ' (tied)', ''))
##                 return(list(data = p, zpal = zPalette, rnpal = rnPalette, rrpal = rrPalette, pop = pop))
##             } else {
##                 pn <- plun3
##                 ## Select units shapes
##                 p <- subset(pn, regc == input$regc)
##                 suppressWarnings(znat <- rval$reachdatawide[, .(z = f(get(input$plu_var))), plu])
##                 znat[!is.finite(z), z := NA]

##                 ## Calculate Z and ranks
##                 znat[, z.nat.rank := NA_real_]
##                 znat[!is.na(z), z.nat.rank := rank(z)]
##                 znat[, z.nat.rank := 100 * (z.nat.rank / sum(!is.na(z.nat.rank)))]
##                 zreg <- znat[plu %in% p$id]
##                 zreg[, z.reg.rank := NA_real_]
##                 zreg[!is.na(z), z.reg.rank := rank(z)]
##                 zreg[, z.reg.rank := 100 * (z.reg.rank / sum(!is.na(z.reg.rank)))]
                
##                 p@data <- cbind(p@data, zreg[match(p$id, plu), .(z, z.nat.rank, z.reg.rank)])

##                 if (is.numeric(p$z)) {
##                     p$z <- round(p$z, ifelse(input$plu_stat == 'Percentage', 1, 3))
##                 }

##                 ## Ranking
##                 zPalette <- colorNumeric(palette = gplots::rich.colors(10), domain=0)
##                 tryCatch({
##                     rnPalette <- colorNumeric(palette = gplots::rich.colors(10), domain=c(0, 100))
##                     rrPalette <- colorNumeric(palette = gplots::rich.colors(10), domain=c(0, 100))
##                     if (input$plu_stat == 'Percentage') {
##                         zPalette <- colorNumeric(palette = gplots::rich.colors(10), domain=c(0, 100))
##                     } else {
##                         zPalette <- colorNumeric(palette = gplots::rich.colors(10), domain=p$z)
##                     }
##                 }, error = function(e) {
##                     print('')
##                 })

##                 tiesnat <- p$z.nat.rank %in% p$z.nat.rank[duplicated(p$z.nat.rank)]
##                 tiesreg <- p$z.reg.rank %in% p$z.reg.rank[duplicated(p$z.reg.rank)]
##                 pop <- sprintf('%s(%s%s): %s%s<br/>National rank: %s%%%s<br/>Regional rank: %s%%%s',
##                               statname, rval$var2lab[input$plu_var], 
##                               ifelse(input$plu_stat == 'Percentage', paste0(' = ', input$plu_cat), ''),
##                               as.character(p$z),
##                               ifelse(input$plu_stat == 'Percentage', '%', ''),
##                               as.character(round(p$z.nat.rank,1)), ifelse(tiesnat, ' (tied)', ''),
##                               as.character(round(p$z.reg.rank,1)), ifelse(tiesreg, ' (tied)', ''))
##                 return(list(data = p, zpal = zPalette, rnpal = rnPalette, rrpal = rrPalette, pop = pop))
##             }} else return(NULL) })
    
##     output$plunmap2 <- renderLeaflet({
##         d <- leafmapdata()
##         if (!is.null(d) & !is.null(input$plu_var) & !is.null(input$plu_valrank)) {
##             v <- switch( input$plu_valrank,
##                        '1' = d$data$z,
##                        '2' = d$data$z.nat.rank,
##                        '3' = d$data$z.reg.rank)
##             pal <- switch( input$plu_valrank,
##                          '1' = d$zpal,
##                          '2' = d$rnpal,
##                          '3' = d$rrpal)
##             tit <- switch( input$plu_valrank,
##                          '1' = as.character(rval$var2lab[input$plu_var]),
##                          '2' = 'National rank',
##                          '3' = 'Regional rank')
##             suff <- switch( input$plu_valrank,
##                          '1' = ifelse(input$plu_stat == 'Percentage', '%', ''),
##                          '2' = '%',
##                          '3' = '%')
##             leaflet(d$data) %>%
##               addProviderTiles("Esri.WorldImagery") %>%
##               addPolygons(stroke=FALSE, smoothFactor = 0.2, fillOpacity = .8,
##                           color= ~pal(v), popup=d$pop) %>%
##               addLegend('bottomleft', pal = pal, values = ~v,
##                         title = paste0(tit, ifelse(input$plu_stat == 'Percentage',
##                                                    paste0(' = ', input$plu_cat), '')),
##                         labFormat = labelFormat(suffix = suff))
##         }
##     })


##     ###################
##     ## Index creator ##
##     ###################
    
##     quants <- function(x) {
##         paste(quantile(input$x_var2, c(0, 0.025, 0.5, 0.975, 1)), collapse = ',')
##     }
    
##     selvarModal <- function(failed = FALSE) {
##         modalDialog(
##             fluidRow(
##                 column(6,
##                        selectInput("x_var2", "Select variable", choices = rval$cats, selected = "uspasture"),
##                        textInput('condvar', "Specify threshold", value='')),
##                 column(6,
##                        helpText('Distribution among reaches', align = 'center'),
##                        imageOutput("xvar2densimg", width = 'auto', height = 'auto'))
##             ),
##             footer = tagList(
##                          modalButton("Cancel"),
##                          actionButton("ok", "OK")
##                      )
##         )
##     }

##     output$xvar2densimg <- renderImage({
##         if (length(input$x_var2)) {
##             pngname <- sprintf('densities/%s_density.png', input$x_var2)
##         } else pngname <- '.'
##         return( list(src = pngname,
##                      contentType = 'image/png', width = 250,
##                      alt = "") )
##     }, deleteFile = F)
    
##     tryagainModal <- function(failed = FALSE) {
##         modalDialog(
##             p("Please choose a decision node (True or False)"),
##             easyClose = TRUE,
##             footer = tagList(modalButton("Let's try again"))
##         )
##     }

##     ## rval <- list(tree = list(nodes = data.table(name = character(0), thresh = numeric(0)), edges = data.table(from = character(0), to = character(0)), groups = data.table(endnode = character(0), cond = character(0), n = integer(0), label = character(0))))

##     ## rval$tree <- addnode(rval$tree, "usavgtnorm", NA, 10, reachdatawide)
##     ## rval$tree <- addnode(rval$tree, "catcharea", "usavgtnorm", 2, reachdatawide)
    
##     rval <- reactiveValues(
##         tree = list(nodes = data.table(name = character(0), thresh = numeric(0)),
##                     edges = data.table(from = character(0), to = character(0)),
##                     groups = data.table(endnode = character(0), cond = character(0),
##                                         n = character(0), label = character(0))),
##         nodexy = NULL,
##         reachdatawide = reachdatawide,
##         fields = fields,
##         cats = cats,
##         lab2var = lab2var,
##         var2lab = var2lab
##     )
    
##     observeEvent(input$ok, {
##         if (!is.null(input$x_var2) & !is.null(input$condvar)) {
##             if (!is.null(rval$nodexy)) {
##                 f <- nearPoints(rval$nodexy, input$treeplot_click, 'x', 'y', threshold = 20)
##                 if (nrow(f))
##                     f <- f$name else f <- NA
##             } else f <- NA
##             if (any(is.na(rval$tree$edges$from)) & is.na(f)) {
##                 removeModal()
##                 showModal(tryagainModal())
##             } else {
##                 rval$tree <- addnode(rval$tree, input$x_var2, f, as.numeric(input$condvar), rval$reachdatawide)
##                 if (sum(is.na(rval$tree$edges$from)) > 1) {
##                     rval$tree <- removenode(rval$tree, input$x_var2)
##                 } else {
##                     removeModal()
##                 }
##             }
##         }
##     })

##     observeEvent(input$treeplot_click, {
##         good <- F
##         if (!is.null(rval$nodexy)) {
##             f <- nearPoints(rval$nodexy, input$treeplot_click, 'x', 'y', threshold = 20)
##             if (nrow(f)) {
##                 good <- T
##             }
##         } else good <- T
##         if (good) {
##             showModal(selvarModal())
##         } else showModal(tryagainModal())
##     })

##     output$treeplot <- renderPlot({
##         if (nrow(rval$tree$nodes)) {
##             ## Node names
##             nnames <- rval$tree$nodes$name
##             nlabs <- ifelse(grepl('__T$', nnames), 'True',
##                     ifelse(grepl('__F$', nnames), 'False',
##                            sub('_[0-9]+', '', nnames)))
##             ## Condition nodes
##             nlabs[nlabs %in% rval$fields$column_name] <-
##               paste0(rval$fields$label[match(nlabs[nlabs %in% rval$fields$column_name],
##                                            rval$fields$column_name)], ' > ',
##                      rval$tree$nodes$thresh[match(nlabs[nlabs %in% rval$fields$column_name],
##                                                   rval$tree$nodes$name)], ' ?')
##             ns <- rval$tree$groups$n[match(nnames, rval$tree$groups$endnode)]
##             ns <- ifelse(is.na(ns), NA, format(ns, trim=T, big.mark=' '))
##             ls <- rval$tree$groups$label[match(nnames, rval$tree$groups$endnode)]
##             ls <- ifelse(is.na(ls) | ls == '', NA, paste0(ls, ' '))
##             nlabs[!is.na(ns)] <- paste0(nlabs[!is.na(ns)], sprintf(' (%s)', ns[!is.na(ns)]))
##             nlabs[!is.na(ls)] <- sprintf('%s\\\n%s', nlabs[!is.na(ls)], ls[!is.na(ls)])
##             names(nlabs) <- nnames
##             ## Node shape
##             nshapes <- ifelse(grepl('__[TF]$', nnames), 'plaintext', 'plaintext')
##             names(nshapes) <- nnames
##             ## Node colour
##             ncols <- ifelse(grepl('__T$', nnames), 'darkgreen',
##                     ifelse(grepl('__F$', nnames), 'red', 'black'))
##             names(ncols) <- nnames
##             ## Node size
##             nsizes <- ifelse(grepl('__[TF]$', nnames), 12, 15)
##             names(nsizes) <- nnames
##             nAttrs <- list(label = nlabs, shape = nshapes, fontcolor = ncols, fontsize = nsizes)
##             ## Edges
##             ecols <- ifelse(grepl('__T$', rval$tree$edges$to), 'darkgreen',
##                     ifelse(grepl('__F$', rval$tree$edges$to), 'red', 'grey60'))
##             enames <- sprintf('%s~%s', rval$tree$edges$from, rval$tree$edges$to)
##             names(ecols) <- enames
##             earrowsizes <- rep(0.6, nrow(rval$tree$edges))
##             earrowheads <- rep('normal', nrow(rval$tree$edges))
##             names(earrowsizes) <- names(earrowheads) <- enames
##             eAttrs <- list(color = ecols, arrowsize = earrowsizes, arrowhead = earrowheads)
##             attrs <- list(node=list(shape='ellipse', fixedsize = FALSE), edge=list(penwidth = 0.1))
##             ## Create graph
##             rEG <- new("graphNEL", nodes = nnames, edgemode="directed")
##             for (i in 2:nrow(rval$tree$edges)) {
##                 if (!is.na(rval$tree$edges[i, from]) & !is.na(rval$tree$edges[i, to])) {
##                     rEG <- addEdge(rval$tree$edges[i, from], rval$tree$edges[i, to], rEG, 1)
##                 }
##             }
##             h <- plot(rEG, nodeAttrs = nAttrs, edgeAttrs = eAttrs, attrs = attrs, mar = c(0, 0, 0, 0),
##                      bg = '#EEEEEE', family = 'sans', lwd=0.5)
            
##             rval$nodexy <- rbindlist(lapply(h@AgNode, function(z) {
##                 data.table(name = z@name,
##                            x = z@center@x,
##                            y = z@center@y)
##             }))
##         }
##     }, pointsize = 10, bg = '#EEEEEE', res = 200)


##     observeEvent(input$cleartree, {
##         rval$tree <- list(nodes = data.table(name = character(0), thresh = numeric(0)),
##                          edges = data.table(from = character(0), to = character(0)),
##                          groups = data.table(endnode = character(0), cond = character(0),
##                                              n = integer(0), label = character(0)))
##         rval$nodexy <- NULL
##     })


##     output$groupstable <- renderRHandsontable({
##         if (length(rval$tree$groups$label)) {
##             d <- rval$tree$groups[, .(cond, label)]
##             setnames(d, c('cond', 'label'), c('Condition', 'Category'))
##             rhandsontable(d)
##         }
##     })

    
##     createvarModal <- function(failed = FALSE) {
##         modalDialog(
##             if (input$createdvarname %in% names(rval$reachdatawide))
##                 h5('WARNING! The new variable name already exists in the reach dataset. The previous variable will be replaced.'),
##             h5(sprintf('The variable %s will be created. Are you sure to continue?', input$createdvarname)),
##             footer = tagList(
##                          modalButton("Cancel"),
##                          actionButton("okcreate", "OK")
##                      )
##         )
##     }

##     observeEvent(input$createvar, {
##         if (nrow(rval$tree$groups)) {
##             if (length(input$groupstable$data)) {
##                 h <- hot_to_r(input$groupstable)
##                 rval$tree$groups$label <<- h$Category[match(rval$tree$groups$cond, h$Condition)]
##             }
##             ## Fill in empty category labels
##             rval$tree$groups$label[rval$tree$groups$label %in% c('',NA)] <<-
##               sprintf('Category %i', 1:sum(rval$tree$groups$label %in% c('',NA)))
##             showModal(createvarModal())
##         }
##     })
    
##     observeEvent(input$okcreate, {
##         rval$reachdatawide$zenewvar <- NA_character_
##         if (input$createdvarname %in% names(rval$reachdatawide))
##             rval$reachdatawide <- rval$reachdatawide[, -input$createdvarname, with=F]
##         for (i in 1:nrow(rval$tree$groups)) {
##             rval$reachdatawide[eval(parse(text = rval$tree$groups$cond[i])),
##                                zenewvar := rval$tree$groups$label[i]]
##         }
##         rval$reachdatawide[, zenewvar := factor(zenewvar, levels = rval$tree$groups$label)]
##         setnames(rval$reachdatawide, 'zenewvar', input$createdvarname)
##         if (!(input$createdvarname %in% rval$fields$column_name)) { 
##             rval$fields <- rbind(rval$fields, data.table(table_schema = 'Custom',
##                                                         table_name = 'Custom',
##                                                         column_name = input$createdvarname,
##                                                         column_type = 'character',
##                                                         category = 'Custom',
##                                                         included = 'Y',
##                                                         label = input$createdvarname,
##                                                         unit = NA,
##                                                         year = NA))
##         }
##         d <- prepare_app_data(rval$fields)
##         rval$fields <- d$fields
##         rval$cats <- d$cats
##         rval$lab2var <- d$lab2var
##         rval$var2lab <- d$var2lab
##         removeModal()
##     })
    
    
##     output$createdsummary <- renderRHandsontable({
##         if (input$createdvarname %in% names(rval$reachdatawide)) {
##             d <- rval$reachdatawide[, .(nzreach, x=get(input$createdvarname))]
##             d <- d[, .(`No. reaches` = .N), .(Category = x)][order(-`No. reaches`)]
##             rhandsontable(d)
##         }
##     })

##     ## output$treelog <- renderPrint({
##     ##     print(rval$tree)
##     ## })

##     ## output$rndlog <- renderPrint({
##     ##     print(input$groupstable)
##     ##     head(rval$reachdatawide,1)
##     ## })
    
    
})

