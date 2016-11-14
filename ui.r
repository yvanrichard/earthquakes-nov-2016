##########
## ui.r ##
##########
library(data.table)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(leaflet)
library(mapview)
library(threejs)
library(rgl)
addResourcePath('data', 'data')

enableBookmarking(store = "url")

function(request) {
    loadingLogo <- function(href, src, loadingsrc, height = NULL, width = NULL, alt = NULL) {
        tagList(
            tags$head(
                     tags$script(
                              "setInterval(function(){
                     if ($('html').attr('class')=='shiny-busy') {
                     $('div.busy').show();
                     $('div.notbusy').hide();
                     } else {
                     $('div.busy').hide();
                     $('div.notbusy').show();
           }
         },100)")
         ),
         div(class = "busy",  
             img(src=loadingsrc,height = height, width = width, alt = alt)),
         div(class = 'notbusy',
             div(class = 'logo', "Earthquakes of 14 Nov 2016"))
        )
    }
    dashboardPage(
        title = "Earthquakes of 14 Nov 2016",
        header = dashboardHeader(
                     title = loadingLogo('http://www.google.co.nz',
                                         'data/logo.png',
                                         'data/ajax-loader.gif'),
                     titleWidth = 300
                 ),
        
        sidebar = dashboardSidebar(
                      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
                      h6(),
                      uiOutput('mag_sel'),
                      uiOutput('depth_sel'),
                      uiOutput('time_sel'),
                      uiOutput('map_colour'),
                      hr(),
                      actionButton("updatedata", "Download updated data")

                  ),

        body = dashboardBody(
                   width = 12,
                   tags$head(tags$img(src='Roaddamage59quake.JPG', width = '100%')),
                   h5(textOutput('caption')),
                   fluidRow(column(width = 6,
                                   box(width = '100%',
                                       title = 'Interactive map of earthquakes',
                                       leafletOutput('myMap', width='100%', height=700))
                                   ),
                            column(width = 6,
                                   box(width = '100%',
                                       title = 'Interactive 3D plot of quakes (Note: depth is not to scale)',
                                       rglwidgetOutput('scatter3d', width = '100%', height=700))
                                       ## playwidgetOutput('scatter3dp', width = '100%', height=700))
                                   )),
                   fluidRow(column(width = 6,
                                   box(width = '100%',
                                       title = 'Temporal change in magnitude, depth, and density',
                                       plotOutput("timeline_mag", width = '100%', height = '175px'),
                                       plotOutput("timeline_depth", width = '100%', height = '175px'),
                                       plotOutput("timeline_dens", width = '100%', height = '175px'))
                                   ),
                            column(width = 6,
                                   box(width = '100%',
                                       title = 'Distribution of magnitude and depth',
                                       plotOutput("densplot_mag", width = '100%', height = '200px'),
                                       plotOutput("densplot_depth", width = '100%', height = '200px')
                                       ))
                            )
               )
    )
}
