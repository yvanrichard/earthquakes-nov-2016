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
                                       ## footer = 'Depth is not to scale',
                                       rglwidgetOutput('scatter3d', width = '100%', height=700))
                                       ## playwidgetOutput('scatter3dp', width = '100%', height=700))
                                       ## scatterplotThreeOutput('scatter3d', width = '100%', height=700))
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

    ##                tabsetPanel(
    ##                    id="tabvals",
    ##                    tabPanel(
    ##                        "Map",
    ##                        fluidRow(
    ##                            column(
    ##                                width = 6,
    ##                                align = 'center',
    ##                                box(width = '100%',
    ##                                    textOutput('xyplottxt'),
    ##                                    plotOutput("xyplot", width = '100%', height = '500px')
    ##                                    )
    ##                            ),
    ##                            column(
    ##                                3,
    ##                                align = 'center',
    ##                                box(width = '100%',
    ##                                    textOutput('yvartxt'),
    ##                                    imageOutput("ymap", width = '100%', height = 'auto'),
    ##                                    hr(),
    ##                                    plotOutput("ydensplot", width = '100%', height = '150px')
    ##                                    )
    ##                            ),
    ##                            column(
    ##                                3,
    ##                                align = 'center',
    ##                                box(width = '100%',
    ##                                    textOutput('xvartxt'),
    ##                                    imageOutput("xmap", width = '100%', height = 'auto'),
    ##                                    hr(),
    ##                                    plotOutput("xdensplot", width = '100%', height = '150px')
    ##                                    )
    ##                            )),
    ##                        fluidRow(
    ##                            column(
    ##                                offset=9,
    ##                                width=3,
    ##                                HTML('<div align="right"><a href="http://www.dragonfly.co.nz" align="right"><img src="data/logo.png" width="250px"></a></div>'))
    ##                        )
    ##                    ),
    ##                    tabPanel(
    ##                        "Planning units",
    ##                        leafletOutput('plunmap2', width='100%', height=650)
    ##                    ),
    ##                    tabPanel(
    ##                        "Index creator",
    ##                        fluidRow(
    ##                            column(width = 6,
    ##                                   box(width = 12,
    ##                                       title = 'Decision tree',
    ##                                       helpText('Click anywhere to add a variable and the associated decision rule. Add more variables by clicking on the centre of the desired node.'),
    ##                                       plotOutput('treeplot', height = 500,
    ##                                                  click = "treeplot_click",
    ##                                                  brush = brushOpts(id = "treeplot_brush")),
    ##                                       actionButton("cleartree", "Clear tree"))),
    ##                            column(width = 6,
    ##                                   box(width = 12,
    ##                                       title = 'Resulting categories',
    ##                                       helpText("Specify the names of the categories from the decision tree once you're done, then click the button to create the variable.  The newly created variable will appear at the bottom of the variable list in the selectors of the left panels"),
    ##                                       rHandsontableOutput("groupstable"),
    ##                                       br(),
    ##                                       actionButton("createvar", "Create/Update variable"),
    ##                                       hr(),
    ##                                       helpText('Distribution of the created variable among reaches'),
    ##                                       rHandsontableOutput('createdsummary')))
    ##                        ),
    ##                        verbatimTextOutput('treelog'),
    ##                        verbatimTextOutput('rndlog')
    ##                    )
    ##                )
    ##            )
    ## )
