#' @title Shiny App to calculate images of RLC parameters
#' @description  A Shiny app for processing and visualizing rapid light curve (RLC) datasets using Imaging-PAM data. The app calculates photosynthesis-related parameters and exports spatially resolved results.
#' @return A graphical interface for importing RLC images, running pixel-wise model fits, and exporting results either to the workspace or as external files (Tif and CSV format).
#' @keywords external
#' @importFrom magrittr %>%
#' @import sp
#' @examples
#' if (interactive()) {
#'   pamfit::pamfit()
#' }
#' @export
pamfit<-function(){

  suppressWarnings({
    #library(geosphere) #for circular polygon aspects
    #library(shiny)
    #library(tiff)
    #library(raster)
    options("sp_evolution_status" = 2) # use sf instead of rgdal and rgeos in sp
    #library(sp)
    #library(DT)
    #library(magrittr)
    #library(shinydashboard)
    #library(mapview)
    #library(leaflet)
    #library(leaflet.extras)
    #library(plotly)
    #library(parallel)
    #library(snow)
    #library(shinyFiles)
    #library(rhandsontable)
    #library(shinyWidgets)
    #library(Orcs)
    #library(terra) #added on 04.06.24 purely to save geoTIFFs in a manner than preservers layer names`
  })


  ######################## MAIN CODE
  `%>%` <- magrittr::`%>%`

  # Reactive values to store warnings
  warnings_store <- reactiveVal(character())

  # Function to globally suppress and store warnings
  suppress_all_warnings <- function(expr) {
    local_warnings <- character()
    withCallingHandlers(
      expr,
      warning = function(w) {
        local_warnings <<- c(local_warnings, conditionMessage(w))  # Store warning
        invokeRestart("muffleWarning")  # Suppress console output
      }
    )
    warnings_store(local_warnings)  # Update reactive storage
  }

  #completely disable console warnings
  options(warn = -1)

  #BrunoReview
  #BJ:temporary until I find the missing raster function
  library(raster)

  #Increase uplaod file size to 30MB
  options(shiny.maxRequestSize = 30*1024^2)

  #check number of cores available for processing
  no.cores <- parallel::detectCores()

  #define projection to apply to uploaded data for later leaflet raster plotting
  leafletProj <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +nadgrids=@null +wktext +no_defs"

  #define polygon projection
  poly.proj <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

  #create environment to store model parameters, needed to pass values around
  #in the parallel model fitting analysis
  e2 <- new.env()

  #define function to iteratively add textInput with an ID and initial value to a dataframe
  shinyInput = function(FUN, len, id, int.value, ...) {
    inputs = character(len)
    for (i in seq_len(len)) {
      inputs[i] = as.character(FUN(paste0(id, i), label = NULL,
                                   value = int.value[i], width = '100px',...))
    }
    inputs
  }


  #create a 'hidden' environment that will store previous used light levels
  #used to re-fill the light data table with previous values
  if(exists('.light_env')==TRUE && exists("tmp.light", envir = .light_env)){
    #load a light object from the .light_env if it was created in a previous analysis
    tmp.light <- local(tmp.light,env=.light_env)
  }else{
    #if the tmp.light doesn't exist create an environment to store light values
    .light_env <<- new.env()
  }

  #Two functions to create circular polygons
  create.spPolygon <- function(x){
    p = SpatialPolygons(list(Polygons(list(Polygon(x)),1)))
    projection(p) <- poly.proj
    p
  }

  create.circle.spPolygon <- function(long, lat, dist.m){
    bearing <- seq(1,360,1)
    circle <- data.frame(bearing)

    circle[,2:3] <- geosphere::destPoint(c(long, lat), circle$bearing, dist.m)
    circle$bearing <- NULL
    names(circle)[1] <- "lon"
    names(circle)[2] <- "lat"

    circle.p <- create.spPolygon(circle)
    circle.p
  }


  ################################################################################
  ################################################################################

  #beginning of user interface

  ################################################################################
  ################################################################################
  ui <- shinydashboard::dashboardPage(

    #define title
    shinydashboard::dashboardHeader(title = "Imaging PAM RLC fit"),

    #add sidebar
    shinydashboard::dashboardSidebar(

      #dynamic sidebarMenu driven by folder heirarchry
      shinydashboard::sidebarMenu(id = 'sidebarmenu',
                                  #define 4 menuItems rendered server side
                                  shinydashboard::menuItemOutput('menu1'),
                                  shinydashboard::menuItemOutput('menu2'),
                                  shinydashboard::menuItemOutput('menu3'),
                                  shinydashboard::menuItemOutput('menu4'),

                                  shiny::br(),

                                  #define all conditional panels for saving/exporting data
                                  shiny::conditionalPanel("input.sidebarmenu === 'summaryStatistics'",
                                                          shiny::uiOutput('exportData')),
                                  shiny::conditionalPanel("input.sidebarmenu === 'summaryStatistics'",
                                                          shiny::uiOutput('saveData')),
                                  shiny::conditionalPanel("input.mainPanel === 'Transect'",
                                                          shiny::actionButton('pushTransect', 'Export Transect Data', width='80%')),
                                  shiny::conditionalPanel("input.mainPanel === 'Transect'",
                                                          shiny::actionButton('exportTransect', 'Save Transect Data', width='80%')),
                                  shiny::conditionalPanel("input.mainPanel === 'ROI'",
                                                          shiny::actionButton('pushROI', 'Export ROI Data', width='80%')),
                                  shiny::conditionalPanel("input.mainPanel === 'ROI'",
                                                          shiny::actionButton('exportROI_data', 'Save ROI Data', width='80%')),
                                  shiny::br(),

                                  #define permanent button to load pre-processed app output
                                  shiny::actionButton("loadDat", label = "Load Pre-Processed GeoTIFF", width = "80%"),
                                  #define permanent button to quit the app
                                  shiny::actionButton("quit", label = "Quit PAMfit", width = '80%')
      )

    ),#end of dashboardSidebar

    #add custom formatting options
    shinydashboard::dashboardBody(
      shinyjs::useShinyjs(),

      #define CSS section
      shiny::tags$head(
        shiny::tags$style(
          # Colorize the actionButton.
          shiny::HTML(
            '.bruno{
          background-color:#7FFF00;
          }
          .bruno2{
          background-color: red;
          }
          '
          )
        )
      ),

      #define each shiny app tab
      shinydashboard::tabItems(

        #Tab 1 - image upload, PAR levels input, pixel smoothing, absorbance image, confirm.
        shinydashboard::tabItem(

          tabName = 'uploadImage',

          shiny::fluidPage(

            shiny::titlePanel(title = 'Input data and initial settings'),
            shiny::hr(),
            shiny::fluidRow(shiny::column(6,
                                          shiny::fileInput('fileIn', '1. Upload your TIFF file',
                                                           multiple = F, accept = c(".tif",".tiff")),
                                          shiny::hr(),
                                          shiny::HTML('<b>2. Input PAR levels for each light step or load file</b>'),
                                          shiny::br(),
                                          shiny::br(),
                                          shiny::fileInput('fileLight', 'Light file',
                                                           multiple = F, accept = c(".txt",".csv")),
                                          rhandsontable::rHandsontableOutput('lightSteps'),
            ),
            shiny::column(6,
                          leaflet::leafletOutput("img1"),
                          shiny::radioButtons('smooth', '3. Choose degree of pixel smoothing',
                                              choices = c(0,3,5,7,9,11,13,15), selected = 0,inline = T),
                          shiny::hr(),
                          shiny::radioButtons('abs', '4. Identify whether data contains a PAR-absorptivity image',
                                              choices = c('Yes' = 'yes', 'No' = 'no'), inline = T, selected = 'no'),
                          shiny::hr(),
                          shiny::HTML('<b>5. Confirm Settings to Proceed</b>'),
                          shiny::actionButton('confirm', 'Confirm', width='100%')
            )
            )

          )
        ),

        #Tab 2 - select region of interest for subsequent modelling, export ROI, import ROI and confirm
        shinydashboard::tabItem(tabName = 'selectROI',
                                shiny::fluidPage(
                                  tags$style(type = "text/css", "#leafmap {height: calc(80vh - 80px) !important;}"),
                                  shiny::titlePanel(title = 'Select ROI for analysis'),
                                  shiny::br(),
                                  shiny::fluidRow(
                                    shiny::uiOutput("roiActBut")
                                  ),
                                  shiny::fluidRow(
                                    shiny::br(),
                                    shiny::column(12,
                                                  leaflet::leafletOutput("leafmap", height = '400px')
                                    )),
                                  shiny::br(),
                                  shiny::fluidRow(
                                    shiny::column(4,
                                                  div(style = "background-color:yellow; text-align:center;",
                                                      shiny::actionButton('exportROI', 'Export ROI?', width = '100%'))),
                                    shiny::column(4, div(style = "background-color:lightgray; text-align:center;",
                                                         shinyFiles::shinyFilesButton('importROI', "Import ROI" ,
                                                                                      title = 'Import ROI?',
                                                                                      width = '200%', multiple = FALSE,
                                                                                      buttonType = "default", class = NULL))),
                                    shiny::column(4,
                                                  shiny::actionButton('confirm2', 'Confirm ROI', width = '100%'))
                                  )
                                )),

        #Tab 3 - select fit parameters for given pixel, select model, select number of cores, and confirm
        shinydashboard::tabItem(tabName = 'selectFitParameters',
                                shiny::fluidPage(

                                  shiny::titlePanel(title = 'Examine rETR and select starting parameters'),
                                  shiny::hr(),
                                  shiny::p(shiny::strong ("Click on a pixel and select model to fit")),
                                  shiny::fluidRow(shiny::column(4,
                                                                leaflet::leafletOutput('pixelSelector',
                                                                                       width = '100%', height = '400px')
                                  ),
                                  shiny::column(3,
                                                shiny::uiOutput('renderLayers'),
                                                shiny::radioButtons('model', 'Select model fit',
                                                                    choices = c('none','Jassby & Platt',
                                                                                'Platt et al', 'Eilers & Peeters'),
                                                                    selected = 'none'),
                                                shiny::br(),
                                                #output of potential starting parameters
                                                shiny::htmlOutput('currentParams'),
                                                shiny::actionButton('reset', 'Reset', width = '100%')
                                  ),
                                  shiny::column(5,
                                                plotly::plotlyOutput('etrPlot', width = '100%', height='400px')
                                  )
                                  ),
                                  shiny::fluidRow(shiny::column(6,
                                                                shiny::textInput('noCores',
                                                                                 label = paste('No. of cores for processing (max = ',
                                                                                               no.cores,")", sep=""),
                                                                                 value = 1, width = '100%'),
                                  ),
                                  shiny::column(6,
                                                shiny::br(),
                                                shiny::actionButton('useParams', 'Confirm & Run Model', width = '100%')))
                                )),

        #Tab 4 - summary statistics, showing raw data, model outputs tab, settings tab, conditional transect/ROI tab
        shinydashboard::tabItem(tabName = 'summaryStatistics',
                                shiny::fluidPage(
                                  shiny::titlePanel(title = 'View data and settings'),
                                  shiny::mainPanel(
                                    shiny::tabsetPanel(id = 'mainPanel',
                                                       shiny::tabPanel('Raw data',
                                                                       shiny::fluidPage(
                                                                         shiny::fluidRow(shiny::column(12,
                                                                                                       shiny::uiOutput('selectBrick', width = '50%')
                                                                         )
                                                                         ),
                                                                         shiny::fluidRow(shiny::column(10,
                                                                                                       leaflet::leafletOutput('rasterImage')
                                                                         ),
                                                                         shiny::column(2,
                                                                                       shiny::htmlOutput('selectLayer')
                                                                         )
                                                                         )
                                                                       )
                                                       ),

                                                       shiny::tabPanel('Model Outputs',
                                                                       shiny::fluidPage(
                                                                         tags$style(type = "text/css", "#modelOutputs {height: calc(60vh - 80px) !important;}"),
                                                                         shiny::fluidRow(
                                                                           shiny::uiOutput('selectParameter', width = '100%')
                                                                         ),
                                                                         shiny::fluidRow(
                                                                           leaflet::leafletOutput('modelOutputs',
                                                                                                  width = '100%')),
                                                                         shiny::fluidRow(
                                                                           column(6, shiny::uiOutput('zmin', width = '50%')),
                                                                           column(6, shiny::uiOutput('zmax', width = '50%'))
                                                                         ),
                                                                         shiny::fluidRow(
                                                                           column(5, shiny::p("Click on pixels to see their values:")),
                                                                           column(2, shiny::htmlOutput('current_val'))
                                                                         ),
                                                                         shiny::fluidRow(
                                                                           shiny::p("Select ROI or transect for data analysis. A new tab will open with the result.")
                                                                         )
                                                                       )
                                                       ),

                                                       shiny::tabPanel('Settings',
                                                                       shiny::fluidRow(
                                                                         shiny::column(6,
                                                                                       DT::dataTableOutput('input.settings'))
                                                                       ),
                                                                       shiny::br(),
                                                                       shiny::fluidRow(
                                                                         column(6,
                                                                                shiny::uiOutput('saveSettings'))
                                                                       )))))),

        #Tab 5 - data analysis tab,
        shinydashboard::tabItem(tabName = 'dataAnalysis',
                                shiny::fluidPage(
                                  shiny::titlePanel(title = 'Data Analysis Tools'),
                                  shiny::mainPanel(
                                    shiny::tabsetPanel( id = 'main5',
                                                        shiny::tabPanel(title = 'Data Analysis',
                                                                        shiny::fluidPage(
                                                                          shiny::fluidRow(
                                                                            shiny::uiOutput('selectParameter2', width = '100%')
                                                                          ),#end of row 1
                                                                          shiny::fluidRow(
                                                                            shiny::column(10,
                                                                                          leaflet::leafletOutput('modelOutputs2', width = '100%'),
                                                                                          shiny::uiOutput('zlim2', width = '100%')
                                                                            ),
                                                                            shiny::column(2,
                                                                                          shiny::uiOutput('zmin2'),
                                                                                          shiny::uiOutput('zmax2'),
                                                                                          shiny::HTML('<b>Current Value</b>'),
                                                                                          shiny::htmlOutput('current_val2')
                                                                            ))))))))
      )#dashboardBody
    )#dashboardPage
  )#end of UI




  ################################################################################
  ################################################################################

  #beginning of server function

  ################################################################################
  ################################################################################

  server <- function(input, output, session){

    #code to suppress ALL warnings (These are being captured in warnings_store())
    suppress_all_warnings({

      #establish reactive object to store all objects and variables to pass between
      #the different shiny components
      app.data <- shiny::reactiveValues(data = NULL,
                                        no.of.cores = no.cores)

      #create backup reactive values obj for later "reset" potential
      my.back <- shiny::reactiveValues(data = NULL)

      #observe for quit button press
      observeEvent(input$quit, {
        stopApp()
      })

      ################################
      #loading of pre-processed GeoTIFF (only for level 4 viewing/exporting etc)
      ###############################

      #open dialog box to load geoTIFF
      shiny::observeEvent(input$loadDat, {
        showModal(
          modalDialog(title = 'Upload Pre-Processed GeoTIF', size='m',
                      shiny::fileInput("uploadGEOTIF", "Select File",
                                       multiple = F, accept = c("tif", "tiff")))
        )
      })

      #look for file, upload and take actions
      shiny::observeEvent(input$uploadGEOTIF, {

        #read in file with terra (to keep metags table)
        r1 <- terra::rast(input$uploadGEOTIF$datapath)
        r1.met <- terra::metags(r1)

        #extract PAR attributes
        par <- r1.met[["PAR levels"]] %>% strsplit(.,split = ",", fixed = T)
        par <- par[[1]] %>% as.numeric()
        app.data$no.light.steps <- length(par)
        app.data$light <- par
        app.data$par.levels <- par
        assign('tmp.light', app.data$par.levels, envir = .light_env)

        #extract smoothing attributes
        app.data$smooth <- r1.met[["Smooth factor"]] %>% as.numeric()

        #extract polygon attributes
        px <- r1.met[["Polygon X Coords"]] %>% strsplit(., split = ",", fixed = T)
        px <- px[[1]] %>% as.numeric()
        py <- r1.met[["Polygon Y Coords"]] %>% strsplit(., split = ",", fixed = T)
        py <- py[[1]] %>% as.numeric()
        app.data$poly.mat <- data.frame(x = px, y = py)
        app.data$poly.coords <- data.frame(x = px, y = py)
        app.data$final.poly <- data.frame(x = px, y = py)

        #extract absorbance
        app.data$abs <- r1.met[["Absorbance Meas."]]
        if(app.data$abs == "yes"){
          app.data$nir <- raster::brick(r1) %>% raster::subset(., "NIR")
          app.data$red <- raster::brick(r1) %>% raster::subset(., "Red")
          app.data$abs_img <- raster::brick(r1) %>% raster::subset(., "Abs")
        }

        #produce F_img, Fm_img, Yield, ETR dataset names
        f.nams <- paste("F", 1:length(par), sep="_")
        fm.nams <- paste("Fm", 1:length(par), sep="_")
        y.nams <- paste("Yield", 1:length(par), sep="_")
        etr.nams <- paste("ETR", 1:length(par), sep="_")

        #extract data, populate to app.data and add in appropriate layer names
        app.data$f_img <- raster::subset(raster::brick(r1), f.nams)
        app.data$fm_img <- raster::subset(raster::brick(r1), fm.nams)
        app.data$yield <- raster::subset(raster::brick(r1), y.nams)
        app.data$etr <- raster::subset(raster::brick(r1), etr.nams)
        app.data$fv_fm_img <- raster::subset(raster::brick(r1), "Yield_1")

        #extract main data (including cropped data, which is the same here)
        if(app.data$abs == "yes"){
          mb.nams <- c(rep(f.nams[1],times = 2),"NIR", "Red", c(rbind(f.nams, fm.nams)))
        } else {
          mb.nams <- c(rep(f.nams[1],times = 4), c(rbind(f.nams, fm.nams)))
        }
        mb <- raster::subset(raster::brick(r1), mb.nams)

        #populate to app.data
        app.data$my.brick <- mb
        app.data$my.crop <- mb

        #extract model applied
        app.data$model <- r1.met[["Model Applied"]]
        app.data$click <- F

        #extract model outputs
        if(app.data$abs == "yes"){
          used.nams <- c("NIR", "Red", f.nams, fm.nams, y.nams, etr.nams)
        } else {
          used.nams <- c(f.nams, fm.nams, y.nams, etr.nams)
        }
        mod.nams <- setdiff(names(r1), used.nams)
        mod.out <- raster::subset(raster::brick(r1), mod.nams)
        #populate to app.data
        app.data$model.outputs <- mod.out
        for(i in 1:(raster::nlayers(mod.out))){
          app.data[[names(mod.out)[i]]] <- raster::raster(mod.out, i)
        }

        #extract start parameters
        start.p <- r1.met[["Start Parameters"]] %>% strsplit(., split = " ; ", fixed = T)
        start.p <- start.p[[1]]
        for(i in 1:length(start.p)){
          this.nam <- start.p[i] %>% strsplit(., split = ":") %>% lapply('[[',1) %>%
            unlist() %>% tolower()
          this.val <- start.p[i] %>% strsplit(., split = ":") %>% lapply('[[',2) %>%
            unlist() %>% as.numeric()

          app.data[[this.nam]] <- this.val
        }

        #define extras
        app.data$brick <- 'F'
        app.data$layer <- 0
        app.data$first_result <- T
        app.data$user_roi <- F
        app.data$file.name <- input$uploadGEOTIF$name

        #create back ups incase using reset later.
        my.back$par.levels <- app.data$par.levels
        my.back$f_img <- app.data$f_img
        my.back$fm_img <- app.data$fm_img
        my.back$yield <- app.data$yield
        my.back$etr <- app.data$etr

        #update app check points
        app.data$step_1 <- "complete"
        app.data$step_2 <- "complete"
        app.data$step_3 <- "complete"

        shinydashboard::updateTabItems(session, "sidebarmenu", 'summaryStatistics')

      })

      ####################### Render Menus ##################################
      #render Menus 1 to 4 dependent on app check points
      ####################### Render Menus ##################################

      ##Render Menu1
      output$menu1 <- shinydashboard::renderMenu({
        if(!is.null(app.data$step_1)){
          my.icon <- icon('check-square', lib = 'font-awesome', class = 'bruno')
        } else {
          my.icon <- icon('check-square', lib = 'font-awesome', class = 'bruno2')
        }
        shinydashboard::menuItem("1. Upload Image", tabName = "uploadImage",  icon = my.icon

        )
      })

      ##Render Menu2
      output$menu2 <- shinydashboard::renderMenu({
        if(!is.null(app.data$step_2)){
          my.icon <- icon('check-square', lib = 'font-awesome', class = 'bruno')
        } else {
          my.icon <- icon('check-square', lib = 'font-awesome', class = 'bruno2')
        }
        shinydashboard::menuItem("2. Select ROI", tabName = "selectROI", icon = my.icon)
      })

      ##Render Menu3
      output$menu3 <- shinydashboard::renderMenu({
        if(!is.null(app.data$step_3)){
          my.icon <- icon('check-square', lib = 'font-awesome', class = 'bruno')
        } else {
          my.icon <- icon('check-square', lib = 'font-awesome', class = 'bruno2')
        }
        shinydashboard::menuItem("3. Select Fit Parameters", tabName = "selectFitParameters", icon = my.icon)
      })

      ##Render Menu4
      output$menu4 <- shinydashboard::renderMenu({
        if(!is.null(app.data$step_3)){
          my.icon <- icon('check-square', lib = 'font-awesome', class = 'bruno')
        } else {
          my.icon <- icon('check-square', lib = 'font-awesome', class = 'bruno2')
        }
        shinydashboard::menuItem("4. View & Export Data", tabName = "summaryStatistics", icon = my.icon)
      })

      ####################### Shiny 1 code ##################################
      #server side code for file upload, PAR selection, and pixel smoothing
      ####################### Shiny 1 code ##################################


      #to activate the first tab
      shinydashboard::updateTabItems(session, 'sidebarmenu', 'uploadImage')

      #observe fileIn, load and initial data processing
      shiny::observeEvent(input$fileIn, {
        my.file <- input$fileIn
        app.data$file.name <- input$fileIn$name

        #load the data
        #suppress unknown tag warning from imaging PAM tiff
        suppressWarnings(
          dat <- tiff::readTIFF(my.file$datapath, all = T)
        )
        app.data$data <- dat
        #load data as raster stack
        my.brick <- raster::stack()
        for(i in 1:length(dat)){
          my.brick <- raster::stack(my.brick, raster::raster(as.matrix(dat[[i]])))
        }

        #establish number of light levels based on layer number
        app.data$no.light.steps <- (raster::nlayers(my.brick) - 4) / 2


        #set extent based on image properties and set our desired CRS
        raster::extent(my.brick) <- c(0,ncol(my.brick), 0, nrow(my.brick))
        raster::projection(my.brick) <- sp::CRS(leafletProj)

        #push to app.data
        app.data$my.brick <- my.brick

        #add NA population of PAR levels data table unless light object already present
        if(exists('tmp.light')){
          if(length(tmp.light) == app.data$no.light.steps){
            app.data$light <- tmp.light
          } else {
            app.data$light <- rep('NA', app.data$no.light.steps)
          }
        } else {
          app.data$light <- rep('NA', app.data$no.light.steps)
        }
      })

      #observe file Light, import and add to app.data
      shiny::observeEvent(input$fileLight, {
        if (is.null(app.data$my.brick) == TRUE){
          showModal(modalDialog(
            title = "Missing Tif file",
            "Please select a Tif file before loading light levels",
            easyClose = FALSE,
            footer = modalButton("Dismiss")
          ))
        }else{
          #input light file
          light_file <- input$fileLight
          light <- tryCatch({
            as.numeric(utils::read.csv(light_file$datapath, header = FALSE))
          },error=function(e){NA}
          )

          #check format of light file
          if (is.na(light[1]) == TRUE){
            showModal(modalDialog(
              title = "Error in the light file",
              "The light file is not formatted correctly",
              easyClose = FALSE,
              footer = modalButton("Dismiss")))
          }

          #check number of light levels matches length of tiff light curve
          if (length(light) !=1 & length(light) != (raster::nlayers(app.data$my.brick) - 4) / 2){
            showModal(modalDialog(
              title = "Error in the light file",
              "Wrong number of light levels in the input file",
              easyClose = FALSE,
              footer = modalButton("Dismiss")
            ))
          }

          #store light data
          app.data$light <- light

        }
      })

      #render leafmap plot of uploaded TIFF for pixel smoothing
      output$img1 <- leaflet::renderLeaflet({
        my.smooth <- input$smooth %>% as.numeric
        if(!is.null(app.data$my.brick)){
          my.brick <- app.data$my.brick
          my.image <- raster::raster(my.brick, 5)
          if(my.smooth > 0){
            my.mat <- matrix(1, nrow = my.smooth, ncol = my.smooth)
            my.image <- raster::focal(my.image, w = my.mat, fun = mean, pad = T,
                                      padValue = NA, na.rm = T)
          }

          #output smooth level selected to app.data
          app.data$smooth <- my.smooth

          #render plot
          my.rast <- my.image
          pal <- leaflet::colorNumeric(palette = 'magma',
                                       domain = c(min(raster::values(my.rast), na.rm = T),
                                                  max(raster::values(my.rast), na.rm = T)),
                                       na.color = 'transparent')
          leaflet::leaflet() %>%
            leaflet::addRasterImage(my.rast, project = T, colors = pal)
        }
      })


      #render output light levels data table
      output$lightSteps <- rhandsontable::renderRHandsontable({
        if(!is.null(app.data$my.brick)){

          #take most recent light data from app.data
          my.int.par <- app.data$light

          #make table for rendering
          rhandsontable::rhandsontable(
            data.frame('Light_Step' = 1:app.data$no.light.steps,
                       'PAR_level' = my.int.par),
            stretchH = "all") %>%
            rhandsontable::hot_cols(colWidths = 100) %>%
            rhandsontable::hot_col("Light_Step", readOnly = TRUE) %>%
            rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE)
        }
      })


      #actions on confirm button (save light levels; save to external env; smooth image; smooth raster stack; label raster stack)
      shiny::observeEvent(input$confirm, {

        #check if there is anything missing before moving on
        if(is.null(app.data$my.brick) == TRUE){
          showModal(modalDialog(
            title = "Input error",
            "Please select a file and set light levels",
            easyClose = FALSE,
            footer = modalButton("Dismiss")
          ))
        } else {
          #read in the FINAL par labels from the data table
          my.par <- rhandsontable::hot_to_r(input$lightSteps) %>% as.data.frame
          my.par <- suppressWarnings(
            my.par$PAR_level %>% as.numeric()
          )

          if(any(is.na(my.par))){
            showModal(modalDialog(
              title = "Input error",
              "Please make sure all light levels are provided (no NAs allowed)",
              easyClose = FALSE,
              footer = modalButton("Dismiss")
            ))
          } else {
            #populate light levels into app.data
            app.data$par.levels <- my.par
            app.data$light <- my.par

            #write out tmp.light to a 'hidden' environment so that it can be loaded automatically
            assign('tmp.light', app.data$par.levels, envir = .light_env)

            #apply smoothing to original data set
            if(app.data$smooth > 0){
              my.mat <- matrix(1, nrow = app.data$smooth, ncol = app.data$smooth)
              for(i in 1:(raster::nlayers(app.data$my.brick))){
                app.data$my.brick[[i]] <- raster::focal(raster::raster(app.data$my.brick,i),
                                                        fun = mean, w = my.mat, pad = T,
                                                        padValue = NA, na.rm = T)
              }
            }

            #apply names to each layer of the original data
            ls.nos <- 1:app.data$no.light.steps
            names(app.data$my.brick) <- c('Fo_initial', 'Fm_initial', 'NIR',
                                          'Red',c(rbind(paste('F_', ls.nos, sep=''),
                                                        paste('Fm_', ls.nos, sep=''))))

            #check to see if abs image incorporated in fileInput
            app.data$abs <- input$abs
            if(app.data$abs == "yes"){
              #push nir and red to app.data and calc initial abs img
              app.data$nir <- raster::raster(app.data$my.brick, "NIR")
              app.data$red <- raster::raster(app.data$my.brick, "Red")
              abs.rast <- 1 - app.data$red / app.data$nir
              app.data$abs_img <- abs.rast
              names(app.data$abs_img) <- "Abs"
            }

            #check to ensure step 1 (file upload is complete)
            app.data$step_1 <- 'complete'

            #add catch for user ROI input (needs to default to FALSE to start with)
            app.data$user_roi <- FALSE

            #switch to next tab
            shinydashboard::updateTabItems(session, 'sidebarmenu', 'selectROI')
          }}
      })


      ####################### Shiny 2 code ##################################
      #rendered dependent on step1 (file upload) being complete
      #tab for the ROI selection
      ####################### Shiny 2 code ##################################

      #render radioButtons
      output$roiActBut <- renderUI({
        req(app.data$step_1)
        if(app.data$abs == "yes"){
          my.choices <- c("NIR", "Red", "Abs", "Fo", "Fm")
        } else {
          my.choices <- c("Fo", "Fm")
        }

        shiny::radioButtons('roiImageSelect', label = "Select image for ROI delination",
                            choices = my.choices, selected = "Fo", inline = T)
      })

      #render leafmap plot of uploaded TIFF if it exists
      output$leafmap <- leaflet::renderLeaflet({
        if(!is.null(app.data$my.brick) && app.data$step_1 == "complete"){

          #check which image is desired
          my.select <- input$roiImageSelect

          if(!is.null(my.select)){

            #switch to appropriate layer
            my.rast <- switch(my.select,
                              "NIR" = app.data$nir,
                              "Red" = app.data$red,
                              "Abs" = app.data$abs_img,
                              "Fo" = raster::raster(app.data$my.brick, 5),
                              "Fm" = raster::raster(app.data$my.brick, 6))

            my.rast[is.infinite(my.rast)] <- NA

            pal <- leaflet::colorNumeric(palette = "magma",
                                         domain = c(min(raster::values(my.rast), na.rm = T),
                                                    max(raster::values(my.rast), na.rm = T)),
                                         na.color = 'transparent')

            #render dependent on presence of user imported ROI
            if(app.data$user_roi == TRUE){

              leaflet::leaflet() %>%
                leaflet::addRasterImage(my.rast, project = T, colors = pal) %>%
                leaflet.extras::addDrawToolbar( polylineOptions = F,
                                                circleOptions = T,
                                                rectangleOptions = T,
                                                markerOptions = F,
                                                circleMarkerOptions = F,
                                                singleFeature = T,
                                                polygonOptions = leaflet.extras::drawPolygonOptions(repeatMode = F)) %>%
                leaflet::addPolygons(data = app.data$user_poly)
            } else {

              leaflet::leaflet() %>%
                leaflet::addRasterImage(my.rast, project = T, colors = pal) %>%
                leaflet.extras::addDrawToolbar( polylineOptions = F, circleOptions = T,
                                                rectangleOptions = T,
                                                markerOptions = F,
                                                circleMarkerOptions = F,
                                                singleFeature = T,
                                                polygonOptions = leaflet.extras::drawPolygonOptions(repeatMode = F)
                )}
          }}
      })

      #capture the drawn polygon
      shiny::observeEvent(input$leafmap_draw_new_feature, {

        #in the case of circular polygon selection
        if(input$leafmap_draw_new_feature$properties$feature_type == "circle"){

          #take the x and y of the centre of the circle
          poly.coords <- input$leafmap_draw_new_feature$geometry$coordinates %>% unlist
          poly.mat <- data.frame(x = poly.coords[c(TRUE, FALSE)],
                                 y = poly.coords[c(FALSE, TRUE)])

          #transform poly mat for later push to app.data in MERC CRS
          xy <- sp::SpatialPoints(poly.mat)
          sp::proj4string(xy) <- poly.proj

          #suppress spTransform warning with no consequence
          suppressWarnings(
            xy <- as.data.frame(sp::spTransform(xy, leafletProj))
          )

          #get the radius in units of metres and capture in poly mat/xy
          poly.radius <- input$leafmap_draw_new_feature$properties$radius %>% unlist
          xy$radius <- poly.radius
          poly.mat$radius <- poly.radius
          app.data$poly.mat <- poly.mat

          #use functions at start of script to generate polygon from these data
          cc <- create.circle.spPolygon(poly.mat$x, poly.mat$y, poly.radius)
          #transform the leaflet projection for cropping
          final.poly <- sp::spTransform(cc, leafletProj)

          #crop original raster brick
          my.crop <- app.data$my.brick %>%
            raster::crop(raster::extent(final.poly)) %>%
            raster::mask(final.poly)

          #push outputs to app.data
          app.data$my.crop <- my.crop
          app.data$poly.coords <- xy #includes the radius data here
          app.data$poly.draw <- final.poly

        } else {

          #in the case of non-circular polygon
          poly.coords <- input$leafmap_draw_new_feature$geometry$coordinates %>% unlist
          poly.mat <- data.frame(x = poly.coords[c(TRUE, FALSE)],
                                 y = poly.coords[c(FALSE, TRUE)])

          app.data$poly.mat <- poly.mat #this is needed to export the ROI as csv
          xy <- sp::SpatialPoints(poly.mat)
          sp::proj4string(xy) <- poly.proj

          #suppress spTransform warning with no consequence
          suppressWarnings(
            xy <- as.data.frame(sp::spTransform(xy, leafletProj))
          )

          #take final polygon coordinates and crop
          final.poly <- Orcs::coords2Polygons(as.matrix(xy), ID='chris')
          my.crop <- app.data$my.brick %>%
            raster::crop(raster::extent(final.poly)) %>%
            raster::mask(final.poly)

          #populate all to app.data
          app.data$my.crop <- my.crop
          app.data$poly.coords <-xy
          app.data$poly.draw <- final.poly
        }

      })


      #Exporting ROIs functionality

      #observe export ROI and render modalDialog box
      observeEvent(input$exportROI,{
        req(app.data$poly.draw)
        showModal(
          modalDialog(title = 'Export ROI', size='m',
                      downloadButton("downloadROI", "Save ROI",
                                     easyClose = TRUE)
          )
        )
      })
      #establish download handler
      output$downloadROI <- downloadHandler(
        filename = function(){paste("roi", "txt", sep=".")},
        content = function(file) {
          write.table(app.data$poly.mat,file = file)
        }
      )

      #Importing ROIs

      #set root to one level back from working directory
      roots = c(wd = '../')
      #upload the ROI file
      shinyFiles::shinyFileChoose(input, 'importROI', roots=roots, filetypes=c('', 'txt'))

      #observe importROI
      observeEvent(input$importROI, {
        req(app.data$my.brick)
        req(input$importROI)
        req(as.character(shinyFiles::parseFilePaths(roots, input$importROI)[4] != "character(0)"))
        new_roi <- read.table(as.character(shinyFiles::parseFilePaths(roots, input$importROI)[4]),header = TRUE)

        #duplicate for later crop action
        new_roi2 <-new_roi #this is in wgs84

        #assess whether circular polygon was input
        if("radius" %in% names(new_roi)){

          #turn input directly into wgs84 polygon for PLOTTING
          user_poly <- create.circle.spPolygon(new_roi$x, new_roi$y, new_roi$radius)

          #transform to leaflet MERC projection for CROPPING
          new_poly <- sp::spTransform(user_poly, leafletProj)

          #take x/y data from new_roi input table.
          poly.mat <- new_roi[,1:2]

          #transform poly mat for later push to app.data in MERC CRS
          xy <- sp::SpatialPoints(poly.mat)
          sp::proj4string(xy) <- poly.proj

          #suppress spTransform warning with no consequence
          suppressWarnings(
            xy <- as.data.frame(sp::spTransform(xy, leafletProj))
          )
          #add back in the radius data
          xy$radius <- new_roi$radius

          #crop original raster brick with MERC polygon
          my.crop <- app.data$my.brick %>%
            raster::crop(raster::extent(new_poly)) %>%
            raster::mask(new_poly)

          #push outputs to app.data
          app.data$my.crop <- my.crop
          app.data$poly.mat <- new_roi
          app.data$poly.coords <- xy #includes the radius data here
          app.data$poly.draw <- new_poly #used for CROPPING
          app.data$user_roi <- TRUE
          app.data$user_poly <- user_poly #for PLOTTING

        } else {

          #catch original polygon coords in wgs84 input
          app.data$poly.mat <- new_roi

          #turn this into a polygon - needed for PLOTTING, NOT needed for CROPPING
          user_poly <- Orcs::coords2Polygons(as.matrix(new_roi), ID='wgs84')

          #convert to MERC for cropping below
          xy <- sp::SpatialPoints(new_roi)
          sp::proj4string(xy) <- poly.proj
          #suppress spTransform warning with no consequence
          suppressWarnings(
            xy <- as.data.frame(sp::spTransform(xy, leafletProj))
          )
          new_poly <- Orcs::coords2Polygons(as.matrix(xy), ID='chris')
          sp::proj4string(new_poly) <- leafletProj

          #crop original raster brick - with the MERC poly
          my.crop <- app.data$my.brick %>%
            raster::crop(raster::extent(new_poly)) %>% raster::mask(new_poly)

          #push outputs to app.data
          app.data$my.crop <- my.crop
          app.data$poly.coords <- xy
          app.data$poly.draw <- new_poly #used for CROPPING
          app.data$user_roi <- TRUE
          app.data$user_poly <- user_poly #used for PLOTTING
        }
      })


      #observe confirm ROI, process datasets for ROI
      shiny::observeEvent(input$confirm2,{

        #check that ROI exists
        if (is.null(app.data$my.crop) == TRUE){
          showModal(modalDialog(
            title = "Error ROI",
            "You need to select a ROI for analysis",
            easyClose = FALSE,
            footer = modalButton("Dismiss")
          ))
        }else{

          #process rasters to determine f, fm, yield and etr images
          this.dat <- app.data$my.crop[[-c(1:4)]]
          num_samples <-  raster::nlayers(this.dat)

          #alternate sequences to be used for subsetting Fo / Fm images, respectively
          my.seq <- 1:num_samples
          odd_sequence <- my.seq[c(TRUE, FALSE)]
          even_sequence <- my.seq[c(FALSE, TRUE)]

          #1 - extract F images
          f_img <- this.dat[[odd_sequence]]
          #very small Fo values introduce a lot of noise in the analysis
          #convert anything to NA below 0.04 to avoid this
          f_img[f_img < 0.04] <- NaN

          #2 - extract Fm images
          fm_img <- this.dat[[even_sequence]]

          #3 - calculate Yield images and trim below 0.01 and above 0.85
          fv_fm_img <- (fm_img-f_img)/fm_img
          fv_fm_img[fv_fm_img < 0.01] <- NaN
          fv_fm_img[fv_fm_img > 0.85] <- NaN

          #4 - calculate ETR images
          light <- app.data$par.levels
          abs <- app.data$abs
          if(abs == 'yes'){
            nir <- raster::subset(app.data$my.crop, 'NIR')
            red <- raster::subset(app.data$my.crop, 'Red')
            abs.rast <- 1 - red / nir
            app.data$nir <- nir
            app.data$red <- red
            app.data$abs_img <- abs.rast
            names(app.data$abs_img) <- "Abs"
          }

          #define simple ETR function
          #BrunoReview
          etr.fun.1 <- function(x) { x * light * 0.5 }

          if(abs == "no"){
            etr <- raster::calc(fv_fm_img, etr.fun.1)
          } else {
            etr <- raster::calc(fv_fm_img, etr.fun.1)
            etr <- etr * abs.rast
          }

          #push all to app.data with associated names
          app.data$f_img <- f_img
          names(app.data$f_img) <- paste('F', 1:(raster::nlayers(app.data$f_img)), sep='_')
          app.data$fm_img <- fm_img
          names(app.data$fm_img) <- paste('Fm', 1:(raster::nlayers(app.data$fm_img)), sep='_')
          app.data$yield <- fv_fm_img
          names(app.data$yield) <- paste('Yield', 1:(raster::nlayers(app.data$yield)), sep='_')
          app.data$etr <- etr
          names(app.data$etr) <- paste('ETR', 1:(raster::nlayers(app.data$etr)), sep='_')

          #confirmation that step 3 complete
          app.data$step_2 <- 'complete'

          #create back ups here
          my.back$par.levels <- app.data$par.levels
          my.back$f_img <- app.data$f_img
          my.back$fm_img <- app.data$fm_img
          my.back$yield <- app.data$yield
          my.back$etr <- app.data$etr

          #switch to next tab
          shinydashboard::updateTabItems(session, 'sidebarmenu', 'selectFitParameters')

          #set the model status to null if no pixel has been clicked
          #the goal is to present plotly with a text message asking to click on a pixel
          if(is.null(app.data$current.etr) == TRUE){
            app.data$model <- "no_pixel"
          }

          #sets the pixel selection to false
          app.data$click <- FALSE
        }
      })

      ####################### Shiny 3 code ##################################
      #select model and start values for model fitting
      ####################### Shiny 3 code ##################################

      #observe reset button
      shiny::observeEvent(input$reset,{
        if(!is.null(app.data$step_2) && app.data$step_2 == 'complete'){

          #re-populate app.data
          app.data$par.levels <- my.back$par.levels
          app.data$f_img <- my.back$f_img
          app.data$fm_img <- my.back$fm_img
          app.data$yield <- my.back$yield
          app.data$etr <- my.back$etr

          #reset layers menu
          my.layers <- raster::nlayers(app.data$etr)
          shiny::updateSelectInput(session = session, inputId = 'layers', label = 'Number of light steps to plot',
                                   choices = 4:my.layers, selected = my.layers)
        }
      })


      #render selectInput layers
      output$renderLayers <- shiny::renderUI({
        if(!is.null(app.data$step_2) && app.data$step_2 == 'complete'){
          my.layers <- raster::nlayers(app.data$etr)
          shiny::selectInput('layers', 'Number of light steps to plot',
                             choices = 4:my.layers, selected = my.layers)
        }
      })

      #observe pixel selection
      shiny::observeEvent(input$pixelSelector_click, {
        app.data$click <- input$pixelSelector_click
      })

      #observe model selection
      shiny::observeEvent(input$model, {
        app.data$model <- input$model
      })

      #observe number of light steps selection
      shiny::observeEvent(input$layers,{
        app.data$layers <- input$layers
      })

      #render pixelSelector
      output$pixelSelector <- leaflet::renderLeaflet({
        if(!is.null(app.data$step_2) && app.data$step_2 == 'complete'){
          my.rast <- app.data$yield
          pal <- leaflet::colorNumeric(palette = 'magma',
                                       domain = c(min(raster::values(my.rast), na.rm = T),
                                                  max(raster::values(my.rast), na.rm = T)),
                                       na.color = 'transparent')
          leaflet::leaflet() %>%
            leaflet::addRasterImage(raster::raster(app.data$yield, 1), project = T, colors = pal)
        }
      })

      #extract click coordinates and use to extract ETR data from raster brick
      shiny::observeEvent(input$pixelSelector_click, {
        if(!is.null(app.data$step_2) && app.data$step_2 == 'complete'){

          #get coords from click object and project back to raster CRS
          my.x.coord <- app.data$click$lng
          my.y.coord <- app.data$click$lat
          coord.mat <- data.frame(x = my.x.coord, y = my.y.coord)
          xy <- sp::SpatialPoints(coord.mat)
          sp::proj4string(xy) <- poly.proj

          #suppress spTransform warning with no consequence
          suppressWarnings(
            click.coord <- as.data.frame(sp::spTransform(xy, leafletProj))
          )
          #make available
          app.data$click.coord <- click.coord

          #extract associated data from raster brick
          app.data$current.etr <- raster::extract(app.data$etr, click.coord) %>% as.vector

          #if this is the first click then set the model to none and plot the etr
          if(app.data$model == 'no_pixel'){
            app.data$model <- 'none'
          }
          #object to store if a pixel has been clicked
          app.data$click <- TRUE
        }
      })

      #fit particular model depending on input$model (and add to plot)
      shiny::observe({
        if(!is.null(app.data$step_2) && app.data$step_2 == 'complete' && !is.null(app.data$layers)){
          #no model currently selected (default starting parameter?)
          if(app.data$model == 'none'){
            no.obs <- app.data$layers %>% as.numeric
            model.etr <- app.data$current.etr[1:no.obs]
            model.par <- app.data$par.levels[1:no.obs]
            app.data$model.etr <- model.etr
            app.data$model.par <- model.par
          }

          #Jassby & Platt model selected
          if(app.data$model == 'Jassby & Platt'){
            no.obs <- app.data$layers %>% as.numeric
            model.etr <- app.data$current.etr[1:no.obs]
            model.par <- app.data$par.levels[1:no.obs]
            my.res<-tryCatch({
              minpack.lm::nlsLM(model.etr ~ etrmax * tanh((alpha * model.par) / etrmax),
                                start=list(alpha = 0.1, etrmax = 40), algorithm="port", trace=F, lower = c(0,0),
                                control=stats::nls.control(maxiter=1024))
            },error=function(e){NaN}
            )
            #make available
            app.data$model.etr <- model.etr
            app.data$model.par <- model.par

            if(!is.na(my.res[1])){
              coefs <- stats::coef(my.res)
              my.alpha <- coefs[1] %>% as.numeric
              my.etrmax <- coefs[2] %>% as.numeric

              #run predict to get fit line
              new.dat <- data.frame(model.par = seq(0,max(model.par), by=1))
              pred <- stats::predict(my.res, new.dat)

              #make available
              app.data$alpha <- my.alpha
              app.data$etrmax <- my.etrmax
              app.data$pred.par <- new.dat$model.par
              app.data$pred <- pred
            }
          }

          #Platt model selected - currently defaults to plot1() driven by no model
          if(app.data$model == 'Platt et al'){
            no.obs <- app.data$layers %>% as.numeric
            model.etr <- app.data$current.etr[1:no.obs]
            model.par <- app.data$par.levels[1:no.obs]

            my.res <- tryCatch({
              minpack.lm::nlsLM(model.etr ~ Ps*(1-exp(-alpha*model.par/Ps))*exp(-beta*model.par/Ps),
                                start = list(alpha = 0.2, Ps = 2000, beta=150), algorithm = "port",
                                trace = F, control = stats::nls.control(maxiter=1024),
                                lower = c(0,0,0))
            }, error = function(e) {NaN} )

            #make available
            app.data$model.etr <- model.etr
            app.data$model.par <- model.par

            #extract parameters
            if(!is.na(my.res[1])){
              coefs <- stats::coef(my.res)
              my.alpha <- coefs[1] %>% as.numeric
              my.ps <- coefs[2] %>% as.numeric
              my.beta <- coefs[3] %>% as.numeric

              #run predict to get fit line
              new.dat <- data.frame(model.par = seq(0,max(model.par), by=1))
              pred <- stats::predict(my.res, new.dat)

              #make available
              app.data$alpha <- my.alpha
              app.data$ps <- my.ps
              app.data$beta <- my.beta
              app.data$pred.par <- new.dat$model.par
              app.data$pred <- pred
              app.data$etrmax <- app.data$ps*(app.data$alpha/(app.data$alpha+app.data$beta))*(app.data$beta/(app.data$alpha+app.data$beta))^(app.data$beta/app.data$alpha)
            }
          }

          #Eilers and Peeters model selected
          if(app.data$model == 'Eilers & Peeters'){
            no.obs <- app.data$layers %>% as.numeric
            model.etr <- app.data$current.etr[1:no.obs]
            model.par <- app.data$par.levels[1:no.obs]

            my.res <- tryCatch({
              minpack.lm::nlsLM(model.etr ~ model.par/(model.par^2*(1/(alpha*Eopt^2))+(model.par/etrmax)-((2*model.par)/(alpha*Eopt))+(1/alpha)),
                                start=list(alpha = 0.4, etrmax = 40, Eopt=150), algorithm="port", trace=F,
                                control=stats::nls.control(maxiter=1024),lower=c(0,0,0))
            },error=function(e){NaN}
            )

            #make available
            app.data$model.etr <- model.etr
            app.data$model.par <- model.par

            if(!is.na(my.res[1])){
              coefs <- stats::coef(my.res)
              my.alpha <- coefs[1] %>% as.numeric
              my.etrmax <- coefs[2] %>% as.numeric
              my.eopt <- coefs[3] %>% as.numeric

              #run predict to get fit line
              new.dat <- data.frame(model.par = seq(0,max(model.par), by=1))
              pred <- stats::predict(my.res, new.dat)

              #make available
              app.data$alpha <- my.alpha
              app.data$etrmax <- my.etrmax
              app.data$eopt <- my.eopt
              app.data$pred.par <- new.dat$model.par
              app.data$pred <- pred
            }
          }
        }
      })

      #reactive plot when no pixel is selected
      plot0 <- shiny::reactive({
        plotly::plot_ly(x = c(0,1), y = c(0,1), type='scatter',mode='markers',
                        'fillcolor' = 0,marker = list(size = 0,color = 'rgba(255, 255, 255, 0)')) %>%
          plotly::layout(xaxis = list(title = '', zeroline = FALSE, showticklabels=FALSE, showgrid = FALSE),
                         yaxis = list(title = '', zeroline = FALSE, showticklabels=FALSE, showgrid = FALSE),
                         annotations = list(text = "<b>Click on a pixel <br> to select starting values</b>",  x = 0.5, y = 0.5, showarrow=F)) %>%
          plotly::config(displayModeBar = FALSE) %>% plotly::style(hoverinfo = 'none')
      })

      #reactive plot when no model is selected
      plot1 <- shiny::reactive({
        plotly::plot_ly(x = app.data$model.par, y = app.data$model.etr, type='scatter', mode='line') %>%
          plotly::layout(xaxis = list(title = 'PAR'), yaxis = list(title = 'rETR')
          )
      })

      #reactive plot when a model is selected
      plot2 <- shiny::reactive({
        plotly::plot_ly(x = app.data$model.par, y = app.data$model.etr, type='scatter', mode='line', name = 'data') %>%
          plotly::layout(xaxis = list(title = 'PAR'), yaxis = list(title = 'rETR'), legend = list(x = 0.1, y = 0.95)) %>%
          plotly::add_trace(x = app.data$pred.par, y = app.data$pred, name = app.data$model) %>%
          plotly::layout(legend = list(x = 0.5, y = 0.1))
      })

      #select which plot to use
      myGraph <- shiny::reactive({
        switch(app.data$model,
               "no_pixel"= plot0(),
               'none' = plot1(),
               'Jassby & Platt' = plot2(),
               'Eilers & Peeters' = plot2(),
               'Platt et al' = plot2())
      })

      #send to UI
      output$etrPlot <- plotly::renderPlotly({
        myGraph()
      })

      #determine which model parameters to print to screen
      no.param <- reactive({
        my.info <- c('<b>Current model parameters</b>',
                     'No model selected')
        shiny::HTML(paste0(my.info, sep='',collapse='<br/>'))
      })

      jp.param <- shiny::reactive({
        if(!is.null(app.data$click[[1]])){
          if(!is.null(app.data$etrmax) & !is.null(app.data$alpha)){
            etrmax <- round(app.data$etrmax,2)
            alpha <- round(app.data$alpha,2)
          } else {
            etrmax <- NA
            alpha <- NA}
          my.info <- c('<b>Current Model Parameters</b>',
                       paste('<b>ETRmax</b>: ', round(etrmax,0), sep=''),
                       paste('<b>Alpha</b>: ', round(alpha,2), sep=''),
                       paste('<b>Ek</b>: ', round(etrmax/alpha,0), sep=''))
          shiny::HTML(paste0(my.info, sep='',collapse='<br/>'))
        }
      })

      ep.param <- shiny::reactive({
        if(!is.null(app.data$click[[1]])){
          if(!is.null(app.data$etrmax) & !is.null(app.data$alpha) & !is.null(app.data$eopt)){
            etrmax <- app.data$etrmax
            alpha <- app.data$alpha
            eopt <- app.data$eopt
          } else {
            etrmax <- NA
            alpha <- NA
            eopt <- NA
          }
          my.info <- c('<b>Current Model Parameters</b>',
                       paste('<b>ETRmax</b>: ', round(etrmax,0), sep=''),
                       paste('<b>Alpha</b>: ', round(alpha,2), sep=''),
                       paste('<b>Eopt</b>: ', round(eopt,0), sep=''),
                       paste('<b>Ek</b>: ', round(etrmax/alpha,0), sep=''))
          shiny::HTML(paste0(my.info, sep='',collapse='<br/>'))
        }
      })

      p.param <- shiny::reactive({
        if(!is.null(app.data$click[[1]])){
          if(!is.null(app.data$alpha) & !is.null(app.data$ps) & !is.null(app.data$beta) & !is.null(app.data$etrmax)){
            alpha <- round(app.data$alpha,2)
            ps <- round(app.data$ps,2)
            beta <- round(app.data$beta,2)
            etrmax <-round(app.data$etrmax,2)
          } else {
            alpha <- NA
            ps <- NA
            beta <- NA
            etrmax <- NA}
          my.info <- c('<b>Current Model Parameters</b>',
                       paste('<b>Alpha</b>: ', round(alpha,2), sep=''),
                       paste('<b>Ps</b>: ', round(ps,0), sep=''),
                       paste('<b>Beta</b>: ', round(beta,0), sep=''),
                       paste('<b>ETRmax</b>: ', round(etrmax,0), sep=''),
                       paste('<b>Ek</b>: ', round(etrmax/alpha,0), sep=''))
          shiny::HTML(paste0(my.info, sep='',collapse='<br/>'))
        }
      })

      #check input of no of cores
      shiny::observeEvent(input$noCores, {
        app.data$no.of.cores <- input$noCores %>% as.numeric()
      })

      #check with model selected
      my.params <- shiny::reactive({
        switch(app.data$model,
               'none' = no.param(),
               'Jassby & Platt' = jp.param(),
               'Eilers & Peeters' = ep.param(),
               'Platt et al' = p.param()
        )
      })

      #otuput parameters
      output$currentParams <- shiny::renderUI({
        my.params()
      })

      #observe useParams, fit model to all ROI pixels
      shiny::observeEvent(input$useParams, {
        req(app.data$step_2)

        #force the user to select a pixel and a model before attempting calculations
        if (app.data$model == 'no_pixel' | app.data$model == 'none'| is.null(app.data$etr)==TRUE| app.data$click == FALSE){
          showModal(modalDialog(
            title = "No model or no starting values",
            "Please select a pixel and a model",
            easyClose = FALSE,
            footer = modalButton("Dismiss")
          ))
        }else{

          #assign  desired objects back to my.data list
          #save model parameters depending on which model is selected
          my.model <- app.data$model
          cut.light.steps <- app.data$layers %>% as.numeric

          #run the model selected on the nlayers chosen over the entire cropped image.
          #clip down data and light levels to desired number of light steps
          clipped.etr <- app.data$etr[[1:as.numeric(app.data$layers)]]
          clipped.par <- app.data$par.levels[1:as.numeric(app.data$layers)]

          #replace the original F, Fm, Yield, PAR and rETR values by the user selection so that the
          #output files contain the right number of light levels used in the model fit
          app.data$etr <- clipped.etr
          app.data$par.levels <- clipped.par
          app.data$f_img <- app.data$f_img[[1:length(clipped.par)]]
          app.data$fm_img  <- app.data$fm_img[[1:length(clipped.par)]]
          app.data$yield <- app.data$yield[[1:length(clipped.par)]]

          ####################
          # #Define models
          ####################
          jassby.platt.mod<-function(x){

            #draw in required values from environment e2
            light <- local(light, env = e2)
            s.alpha <- local(s.alpha, env = e2)
            s.etrmax <- local(s.etrmax, env = e2)

            if(is.na(x[[1]])){
              my.res <- c(NaN,NaN,NaN,NaN,NaN,NaN)
            }else{
              my.res <- tryCatch({
                minpack.lm::nlsLM(x ~ etrmax * tanh((alpha * light) / etrmax),
                                  start=list(alpha = s.alpha, etrmax = s.etrmax), algorithm="port", trace=F,
                                  control=stats::nls.control(maxiter=1024),lower=c(0,0))
              },error=function(e){NaN}
              )
              if(is.na(my.res[1])){
                my.res <- c(NaN,NaN,NaN,NaN,NaN,NaN)
              }else{
                my.res <- c(summary(my.res)$coefficients[1:2],
                            (summary(my.res)$coefficients[2]/summary(my.res)$coefficients[1]),
                            summary(my.res)$coefficients[3:4],
                            summary(my.res)$sigma)}

              return(my.res)
            }
          }
          ######
          platt.mod <- function(x){

            #retrieve the parameters from the local environment e2
            light <- local(light, env = e2)
            s.alpha <- local(s.alpha, env = e2)
            s.ps <- local(s.ps, env = e2)
            s.beta <- local(s.beta, env = e2)

            if (is.na(x[[1]]) ){
              my.res <- c(NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN)
            }else{
              my.res <- tryCatch({
                minpack.lm::nlsLM(x ~ Ps*(1-exp(-alpha*light/Ps))*exp(-beta*light/Ps),
                                  start = list(alpha = s.alpha, Ps = s.ps, beta=s.beta), algorithm = "port",
                                  trace = F, control = stats::nls.control(maxiter=1024),
                                  lower = c(0,0,0))
              }, error = function(e) {NaN} )
              if (is.na(my.res[1])) {
                my.res <- c(NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN)
              }else{
                my.res <- c(summary(my.res)$coefficients[1:3],
                            summary(my.res)$coefficients[2]*(summary(my.res)$coefficients[1]/(summary(my.res)$coefficients[1]+summary(my.res)$coefficients[3]))*(summary(my.res)$coefficients[3]/(summary(my.res)$coefficients[1]+summary(my.res)$coefficients[3]))^(summary(my.res)$coefficients[3]/summary(my.res)$coefficients[1]),
                            summary(my.res)$coefficients[2]*(summary(my.res)$coefficients[1]/(summary(my.res)$coefficients[1]+summary(my.res)$coefficients[3]))*(summary(my.res)$coefficients[3]/(summary(my.res)$coefficients[1]+summary(my.res)$coefficients[3]))^(summary(my.res)$coefficients[3]/summary(my.res)$coefficients[1])/summary(my.res)$coefficients[1],
                            summary(my.res)$coefficients[4:6],
                            summary(my.res)$sigma)}
              return(my.res)
            }
          }
          ######
          ep.mod <- function(x){

            light <- local(light, env = e2)
            s.alpha <- local(s.alpha, env = e2)
            s.etrmax <- local(s.etrmax, env = e2)
            s.eopt <- local(s.eopt, env = e2)

            if(is.na(x[[1]])){
              my.res <- c(NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN)
            }else{
              my.res <- tryCatch({
                minpack.lm::nlsLM(x ~ light/(light^2*(1/(alpha*Eopt^2))+(light/etrmax)-((2*light)/(alpha*Eopt))+(1/alpha)),
                                  start=list(alpha = s.alpha, etrmax = s.etrmax, Eopt=s.eopt), algorithm="port", trace=F,
                                  control=stats::nls.control(maxiter=1024),lower=c(0,0,0))
              },error=function(e){NaN}
              )

              if (is.na(my.res[1])){
                my.res <- c(NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN)
              }else{
                my.res <- c(summary(my.res)$coefficients[1:3],#alpha, etrmax, eopt
                            summary(my.res)$coefficients[2]/summary(my.res)$coefficients[1],#ek
                            summary(my.res)$coefficients[4:6],summary(my.res)$sigma)#se alpha, etermax, eopt, RMSE
              }
              return(my.res)
            }
          }

          ####################
          #run desired model - with progress bar...
          shiny::withProgress(message = 'Running model.',
                              detail = 'It might take a while.' ,
                              {

                                #to remove the possibility of the user closing the progress bar
                                shinyjs::runjs('$(".shiny-notification").has(".progress").children(".shiny-notification-close").hide()')

                                #######
                                #Jassby and Platt
                                ######
                                if(my.model == 'Jassby & Platt'){
                                  #light needs to be available as object in the environment to the functions below
                                  light <- assign('light', clipped.par, envir = e2)
                                  s.etrmax <- assign('s.etrmax', app.data$etrmax, envir = e2)
                                  s.alpha <- assign('s.alpha', app.data$alpha, envir = e2)
                                  no.cores <- app.data$no.of.cores

                                  if(no.cores == 1){
                                    app.data$model.outputs <-  raster::calc(clipped.etr, jassby.platt.mod, progress='text')
                                  } else {
                                    raster::beginCluster((no.cores-1))
                                    app.data$model.outputs <- raster::clusterR(clipped.etr, raster::calc, args=list(fun=jassby.platt.mod))
                                    raster::endCluster()
                                  }
                                  names(app.data$model.outputs) <- c('alpha','etrmax', 'Ek', 'se_alpha',' se_etrmax', 'RMSE')
                                }

                                #######
                                #Platt
                                #######
                                if(my.model == 'Platt et al'){
                                  light <- assign('light', clipped.par, envir = e2)
                                  s.alpha <- assign('s.alpha', app.data$alpha, envir = e2)
                                  s.ps <- assign('s.ps', app.data$ps, envir = e2)
                                  s.beta <- assign('s.beta', app.data$beta, envir = e2)
                                  no.cores <- app.data$no.of.cores
                                  if(no.cores == 1){
                                    app.data$model.outputs <-  raster::calc(clipped.etr, platt.mod, progress='text')
                                  } else {
                                    raster::beginCluster((no.cores-1))
                                    app.data$model.outputs <- raster::clusterR(clipped.etr, raster::calc, args=list(fun=platt.mod))
                                    raster::endCluster()
                                  }
                                  names(app.data$model.outputs) <- c('alpha','ps', 'beta', 'etrmax','ek', 'se_alpha',' se_ps', 'se_beta', 'RMSE')
                                }

                                #######
                                #Eilers & Peeters
                                ######
                                if(my.model == 'Eilers & Peeters'){
                                  light <- assign('light', clipped.par, envir = e2)
                                  s.alpha <- assign('s.alpha', app.data$alpha, envir = e2)
                                  s.etrmax <- assign('s.etrmax', app.data$etrmax, envir = e2)
                                  s.eopt <- assign('s.eopt', app.data$eopt, envir = e2)
                                  no.cores <- app.data$no.of.cores
                                  if(no.cores == 1){
                                    app.data$model.outputs <-  raster::calc(clipped.etr, ep.mod, progress='text')
                                  } else {
                                    raster::beginCluster((no.cores-1))
                                    app.data$model.outputs <- raster::clusterR(clipped.etr, raster::calc, args=list(fun=ep.mod))
                                    raster::endCluster()
                                  }
                                  names(app.data$model.outputs) <- c('alpha','etrmax', 'eopt', 'Ek', 'se_alpha',' se_etrmax', 'se_eopt', 'RMSE')
                                }
                              })#end of progress checker

          #completion check for step 3
          app.data$step_3 <- 'complete'
          app.data$brick == 'F'
          app.data$layer <- 0
          app.data$first_result <- TRUE
          #switch to next tab
          shinydashboard::updateTabItems(session, 'sidebarmenu', 'summaryStatistics')
        }
      })

      ####################### Shiny 4 code ##################################
      #Results tab
      ####################### Shiny 4 code ##################################


      #####Raw data tab
      #observe brick selection
      shiny::observeEvent(input$selectBrick, {
        app.data$brick <- input$selectBrick
      })

      #observe layer when changed
      shiny::observeEvent(input$selectLayer,{
        app.data$layer <- input$selectLayer
      })

      #render selectInput for selectBrick
      output$selectBrick <- shiny::renderUI({
        if(!is.null(app.data$step_3) && app.data$step_3 == 'complete'){

          #make list of names for plotting - here is light-step data only
          brick.names <- c('F', 'Fm', 'Y[PSII]', 'ETR')

          shiny::radioButtons('selectBrick', 'Choose parameter to plot',
                              choices = brick.names, selected = brick.names[1], inline = T)
        }
      })

      #render htmlOutput for selectLayer based on brick selection
      output$selectLayer <- shiny::renderUI({
        if(!is.null(app.data$brick)){
          layer.names <- app.data$par.levels
          shiny::radioButtons('selectLayer', 'Select light step to view', choices = layer.names, selected = layer.names[1])
        }
      })

      #render raster map
      output$rasterImage <- leaflet::renderLeaflet({
        if(!is.null(app.data$step_3) && app.data$step_3 == 'complete' && !is.null(app.data$brick)){
          #pause required
          Sys.sleep(1)
          #conditionals to find right data
          if(app.data$brick == 'F'){
            my.brick <- app.data$f_img
          }
          if(app.data$brick == 'Fm'){
            my.brick <- app.data$fm_img
          }
          if(app.data$brick == 'Y[PSII]'){
            my.brick <- app.data$yield
          }
          if(app.data$brick == 'ETR'){
            my.brick <- app.data$etr
          }
          req(!is.null(my.brick))

          #draw in the appropriate data
          my.rast <- raster::raster(my.brick, which(app.data$par.levels == app.data$layer))

          #render plot
          pal <- leaflet::colorNumeric(palette = 'magma',
                                       domain = c(min(raster::values(my.rast), na.rm = T), max(raster::values(my.rast), na.rm = T)),
                                       na.color = 'transparent')
          leaflet::leaflet() %>%
            leaflet::addRasterImage(my.rast, project = T, colors = pal) %>%
            leaflet::addLegend(pal = pal,
                               values = raster::values(my.rast)
            )
        }
      })

      ################################################################################
      ########Model outputs tab
      ################################################################################

      #render selectParameter options
      output$selectParameter <- shiny::renderUI({
        req(app.data$step_3)

        #model output names
        my.nams <- names(app.data$model.outputs)

        #plus extras
        if(app.data$abs == 'yes'){
          my.nams <- c('NIR', 'Red', 'Abs.', 'Fv/Fm', 'Fo', 'Fm', my.nams)} else {
            my.nams <- c('Fv/Fm', 'Fo', 'Fm', my.nams)
          }
        shiny::radioButtons('selectParameter', 'Select Model Parameter to Plot', choices = my.nams, selected = 'Fv/Fm', inline=T)
      })

      #read from selectParameter options
      shiny::observeEvent(input$selectParameter, {
        app.data$model.param.to.plot <- input$selectParameter
      })

      #render zmin options
      output$zmin <- shiny::renderUI({
        if(!is.null(app.data$model.param.to.plot)){

          #conditional based on which parameter is selected
          if(app.data$model.param.to.plot == 'NIR'){
            my.rast <- app.data$nir
          }
          if(app.data$model.param.to.plot == 'Red'){
            my.rast <-app.data$red
          }
          if(app.data$model.param.to.plot == 'Abs.'){
            my.rast <- app.data$abs_img
          }
          if(app.data$model.param.to.plot == 'Fv/Fm'){
            my.rast <- raster::raster(app.data$yield, layer = 1)
          }
          if(app.data$model.param.to.plot == 'Fo'){
            my.rast <- raster::raster(app.data$f_img, layer = 1)
          }
          if(app.data$model.param.to.plot == 'Fm'){
            my.rast <- raster::raster(app.data$fm_img, layer = 1)
          }
          alt.nams <- c('NIR', 'Red', 'Abs.', 'Fv/Fm', 'Fo', 'Fm')
          if(!app.data$model.param.to.plot %in% alt.nams){
            my.rast <- raster::raster(app.data$model.outputs, app.data$model.param.to.plot)
          }

          #define ranges
          my.min <- range(raster::values(my.rast), na.rm = T)[1]
          my.max <- range(raster::values(my.rast), na.rm = T)[2]
          my.summary <- summary(raster::values(my.rast), na.rm = T)

          #render text
          shiny::textInput('zmin_return', label = paste("Set min. pixel value (min = ",
                                                        round(my.min,2), ")", sep=""), value = round(my.min,2))
        }
      })

      #render zmax options
      output$zmax <- shiny::renderUI({
        if(!is.null(app.data$model.param.to.plot)){

          #conditionaldepending on selected parameter from Fv/Fm, Fo, Fm, Abs, model.outputs
          if(app.data$model.param.to.plot == 'NIR'){
            my.rast <- app.data$nir
          }
          if(app.data$model.param.to.plot == 'Red'){
            my.rast <- app.data$red
          }
          if(app.data$model.param.to.plot == 'Abs.'){
            my.rast <- app.data$abs_img
          }
          if(app.data$model.param.to.plot == 'Fv/Fm'){
            my.rast <- raster::raster(app.data$yield, layer = 1)
          }
          if(app.data$model.param.to.plot == 'Fo'){
            my.rast <- raster::raster(app.data$f_img, layer = 1)
          }
          if(app.data$model.param.to.plot == 'Fm'){
            my.rast <- raster::raster(app.data$fm_img, layer = 1)
          }
          alt.nams <- c('NIR', 'Red', 'Abs.', 'Fv/Fm', 'Fo', 'Fm')
          if(!app.data$model.param.to.plot %in% alt.nams){
            my.rast <- raster::raster(app.data$model.outputs, app.data$model.param.to.plot)
          }

          #define ranges
          my.min <- range(raster::values(my.rast), na.rm = T)[1]
          my.max <- range(raster::values(my.rast), na.rm = T)[2]
          my.summary <- summary(raster::values(my.rast), na.rm = T)

          #render text
          shiny::textInput('zmax_return', label = paste("Set max. pixel value (max = ",
                                                        round(my.max,2), ")", sep=""), value = round(my.max,2)
          )
        }
      })

      #observe zmin and zmax inputs for dynamic plotting below
      shiny::observeEvent(input$zmin_return, {
        app.data$zmin <- input$zmin_return[1] %>% as.numeric
      })
      shiny::observeEvent(input$zmax_return,{
        app.data$zmax <- input$zmax_return %>% as.numeric
      })
      shiny::observeEvent(input$modelOutputs_click, {
        app.data$model_click <- input$modelOutputs_click
      })

      #render main leaflet plot
      output$modelOutputs <- leaflet::renderLeaflet({
        if(!is.null(app.data$model.param.to.plot)){

          #conditional depending on selected parameter from Fv/Fm, Fo, Fm, Abs, model.outputs
          if(app.data$model.param.to.plot == 'NIR'){
            my.rast <- app.data$nir
          }
          if(app.data$model.param.to.plot == 'Red'){
            my.rast <- app.data$red
          }
          if(app.data$model.param.to.plot == 'Abs.'){
            my.rast <- app.data$abs_img
          }
          if(app.data$model.param.to.plot == 'Fv/Fm'){
            my.rast <- raster::raster(app.data$yield, layer = 1)
          }
          if(app.data$model.param.to.plot == 'Fo'){
            my.rast <- raster::raster(app.data$f_img, layer = 1)
          }
          if(app.data$model.param.to.plot == 'Fm'){
            my.rast <- raster::raster(app.data$fm_img, layer = 1)
          }
          alt.nams <- c('NIR', 'Red', 'Abs.', 'Fv/Fm', 'Fo', 'Fm')
          if(!app.data$model.param.to.plot %in% alt.nams){
            my.rast <- raster::raster(app.data$model.outputs, app.data$model.param.to.plot)
          }

          pal <- leaflet::colorNumeric(palette = 'magma',
                                       domain = c(app.data$zmin, app.data$zmax), #make them dynamic on zlim_return
                                       na.color = 'transparent')

          #remove an error on 1st loading
          req(app.data$zmin)

          leaflet::leaflet() %>%
            leaflet::addRasterImage(my.rast, project = T, colors = pal) %>%
            leaflet::addLegend(pal = pal,
                               values = raster::values(my.rast)[which(raster::values(my.rast >= app.data$zmin) &
                                                                        raster::values(my.rast <= app.data$zmax))]
            ) %>%
            leaflet.extras::addDrawToolbar(polylineOptions = T,
                                           circleOptions = T,
                                           rectangleOptions = T,
                                           markerOptions = F,
                                           circleMarkerOptions = F,
                                           singleFeature = T,
                                           polygonOptions = leaflet.extras::drawPolygonOptions(repeatMode = F))
        }
      })

      #extract click coordinates and use to extract  data from current plot
      shiny::observeEvent(input$modelOutputs_click, {

        if(!is.null(app.data$model.param.to.plot)){

          #get coords from click object and project back to raster CRS
          my.x.coord <- app.data$model_click$lng
          my.y.coord <- app.data$model_click$lat
          coord.mat <- data.frame(x = my.x.coord, y = my.y.coord)
          xy <- sp::SpatialPoints(coord.mat)
          sp::proj4string(xy) <- poly.proj

          #suppress spTransform warning with no consequence
          suppressWarnings(
            click.coord <- as.data.frame(sp::spTransform(xy, leafletProj))
          )
          #conditionaldepending on selected parameter from Fv/Fm, Fo, Fm, Abs, model.outputs
          if(app.data$model.param.to.plot == 'NIR'){
            my.rast <- app.data$nir
          }
          if(app.data$model.param.to.plot == 'Red'){
            my.rast <- app.data$red
          }
          if(app.data$model.param.to.plot == 'Abs.'){
            my.rast <- app.data$abs_img
          }
          if(app.data$model.param.to.plot == 'Fv/Fm'){
            my.rast <- raster::raster(app.data$yield, layer = 1)
          }
          if(app.data$model.param.to.plot == 'Fo'){
            my.rast <- raster::raster(app.data$f_img, layer = 1)
          }
          if(app.data$model.param.to.plot == 'Fm'){
            my.rast <- raster::raster(app.data$fm_img, layer = 1)
          }
          alt.nams <- c('NIR', 'Red', 'Abs.', 'Fv/Fm', 'Fo', 'Fm')
          if(!app.data$model.param.to.plot %in% alt.nams){
            my.rast <- raster::raster(app.data$model.outputs, app.data$model.param.to.plot)
          }

          #extract associated data from raster brick
          app.data$current.model.param <- raster::extract(my.rast, click.coord) %>% as.vector
        }
      })

      #show value in output
      output$current_val <- shiny::renderUI({
        if(!is.null(app.data$current.model.param)){
          paste(round(app.data$current.model.param,2))
        }
      })


      ######Settings data tab

      #render input settings info
      output$input.settings <- DT::renderDataTable({
        req(app.data$step_3)
        id.column <- c('Filename', 'PAR levels', 'Smooth factor', 'Absorbance Meas.',
                       'Model Applied', 'Start Parameters', 'Polygon X Coords', 'Polygon Y Coords')
        model.info <- switch(app.data$model,
                             'Jassby & Platt' = paste('ETRmax: ',round(app.data$etrmax,2),
                                                      '; Alpha: ', round(app.data$alpha,2),
                                                      'Ek: ',round(app.data$etrmax/app.data$alpha,0) ),
                             'Eilers & Peeters' = paste('ETRmax: ', round(app.data$etrmax,2),
                                                        '; Alpha: ',round(app.data$alpha,2),
                                                        '; Eopt: ', round(app.data$eopt,2),
                                                        '; Ek: ', round(app.data$etrmax/app.data$alpha,0)),
                             'Platt et al' = paste('Alpha: ',round(app.data$alpha,2),
                                                   '; Ps: ', round(app.data$ps,2),
                                                   '; ETRmax: ', round(app.data$etrmax,2),
                                                   '; Ek: ', round(app.data$etrmax/app.data$alpha,0),
                                                   '; Beta: ', round(app.data$beta,2)))

        value.column <- c(app.data$file.name, app.data$par.levels %>% paste0(collapse=','),
                          app.data$smooth, app.data$abs, app.data$model,
                          model.info, round(app.data$poly.coords[,1],2) %>%
                            paste0(collapse=','), round(app.data$poly.coords[,2],2) %>%
                            paste0(collapse=','))

        #push to app.data for saving button below
        all.settings <- data.frame(ID = id.column, Value = value.column)
        app.data$all_settings <- all.settings

        DT::datatable(
          data.frame('Parameter' = id.column,
                     'Value' = value.column),
          selection="multiple",
          rownames = FALSE,
          escape = FALSE,
          #filter = list(position = 'top', clear = FALSE),
          #extensions = list("ColReorder" = NULL, "Buttons" = NULL),
          options = list(
            dom = 't',
            ColReorder = TRUE,
            #extensions = list("ColReorder" = NULL, "Buttons" = NULL),
            autoWidth=TRUE
            # lengthMenu = list(c(no.light.steps), c(as.character(no.light.steps))),
            # rowCallback = DT::JS("function(r,d) {$(r).attr('height', '50px')}"),
            # preDrawCallback = DT::JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
            # drawCallback = DT::JS('function() { Shiny.bindAll(this.api().table().node()); } '),
            # columnDefs = list(list(targets=c(0), visible = T, width = '100px'))
          )
        )
      })

      ###Save option for settings dataframe

      #render save button
      output$saveSettings <- shiny::renderUI({
        req(app.data$step_3)
        shiny::actionButton('save_settings', 'Save Settings', width = '80%')
      })

      #take action on save button press
      shiny::observeEvent(input$save_settings, {
        shiny::showModal(
          shiny::modalDialog(
            title = 'Save Settings', size = 'm',
            shiny::fluidRow(
              shiny::column(4,
                            shiny::HTML('<b>3. Save Settings       </b>'),
                            shiny::downloadButton("downloadData2", "Save Settings")
              )),
            easyClose = T
          ))
      })

      #download handler
      output$downloadData2 <- shiny::downloadHandler(
        filename = function() {
          my.ext <- paste('pamfit_settings', '.csv', sep='')
          my.ext
        },
        content = function(file) {
          my.settings <- app.data$all_settings
          write.csv(my.settings, file = file, row.names = F)
        })

      ##########################All data export (sidebarMenu conditional panel)
      #render export to global environment button
      output$exportData <- shiny::renderUI({
        req(app.data$step_3)
        shiny::actionButton('exportGE', 'Export All Data to R', width = '80%')
      })

      #send to global environment
      shiny::observeEvent(input$exportGE, {
        shiny::showModal(
          shiny::modalDialog(
            title = 'Export all to R Global Environment',
            size = 'm',
            shiny::textInput('export_object_name', "Name for R object:", 'pamfit_results'),
            "Data will be availale as the above named object in the R Global Environment once PAMfit is closed",
            easyClose = T,
            footer=tagList(
              actionButton('submit', 'Submit'),
              modalButton('cancel')
            )))
      })

      #export to global environment when submit is pushed
      observeEvent(input$submit, {
        removeModal()
        if(app.data$abs == "yes"){
          my.list <- list('NIR' = app.data$nir,
                          'Red' = app.data$red,
                          'abs' = app.data$abs_img,
                          'f_images' = app.data$f_img, 'fm_images' = app.data$fm_img ,
                          'Y[PSII]_images' = app.data$yield, 'ETR_images' = app.data$etr,
                          'model_outputs' = app.data$model.outputs)
        } else {
          my.list <- list('f_images' = app.data$f_img, 'fm_images' = app.data$fm_img ,
                          'Y[PSII]_images' = app.data$yield, 'ETR_images' = app.data$etr,
                          'model_outputs' = app.data$model.outputs)
        }
        assign(x = input$export_object_name, my.list, envir = .GlobalEnv)
      })

      #render save button
      output$saveData <- shiny::renderUI({
        req(app.data$step_3)
        shiny::actionButton('save_all', 'Save All Data', width = '80%')
      })

      #take action on save button press
      shiny::observeEvent(input$save_all, {
        #calculate the different choices to be rendered
        if(app.data$abs == "yes"){
          my.choices <- c('All data', names(app.data$model.outputs), 'F images'
                          ,'Fm images',"NIR", "Red", "Abs")
        } else {
          my.choices <- c('All data', names(app.data$model.outputs), 'F images'
                          ,'Fm images')
        }

        shiny::showModal(
          shiny::modalDialog(
            title = 'Save Data', size = 'l',
            shiny::fluidRow(
              shiny::column(4,
                            shiny::radioButtons('parameter.space', '1. Choose Parameters',
                                                choices = my.choices, selected = 'All data')
              ),
              shiny::column(4,
                            shiny::radioButtons('output.types', '2. Choose Output Format',
                                                choices = c('TIFF file(s)', 'CSV file(s)'), selected = 'TIFF file(s)')
              ),
              shiny::column(4,
                            shiny::HTML('<b>3. Save Data       </b>'),
                            shiny::downloadButton("downloadData", "Save Data")
              )
            ),
            easyClose = T
          ))
      })

      #download handler
      output$downloadData <- shiny::downloadHandler(
        filename = function() {
          #file type switch
          if(input$output.types == 'TIFF file(s)'){
            my.ext <- paste('pamfit_results', '.tiff', sep='')
          }
          if(input$output.types == 'CSV file(s)'){
            my.ext <- paste('pamfit_results', '.csv', sep='')
          }
          my.ext
        },
        content = function(file) {

          #save content dependent on input parameters
          #TIFF data - all data
          if(input$parameter.space == 'All data' && input$output.types == 'TIFF file(s)'){

            if(app.data$abs == "yes"){
              all.stack <- raster::brick(raster::stack(app.data$nir,
                                                       app.data$red,
                                                       app.data$abs_img,
                                                       app.data$f_img,
                                                       app.data$fm_img,
                                                       app.data$yield,
                                                       app.data$etr,
                                                       app.data$model.outputs))
            } else {
              all.stack <- raster::brick(raster::stack(app.data$f_img,
                                                       app.data$fm_img,
                                                       app.data$yield,
                                                       app.data$etr,
                                                       app.data$model.outputs))
            }

            #draw down settings data to add into saved geotiFF
            id.column <- c('Filename', 'PAR levels', 'Smooth factor', 'Absorbance Meas.',
                           'Model Applied', 'Start Parameters', 'Polygon X Coords', 'Polygon Y Coords')
            model.info <- switch(app.data$model,
                                 'Jassby & Platt' = paste('ETRmax: ',round(app.data$etrmax,2),
                                                          '; Alpha: ', round(app.data$alpha,2),
                                                          'Ek: ',round(app.data$etrmax/app.data$alpha,0) ),
                                 'Eilers & Peeters' = paste('ETRmax: ', round(app.data$etrmax,2),
                                                            '; Alpha: ',round(app.data$alpha,2),
                                                            '; Eopt: ', round(app.data$eopt,2),
                                                            '; Ek: ', round(app.data$etrmax/app.data$alpha,0)),
                                 'Platt et al' = paste('Alpha: ',round(app.data$alpha,2),
                                                       '; Ps: ', round(app.data$ps,2),
                                                       '; ETRmax: ', round(app.data$etrmax,2),
                                                       '; Ek: ', round(app.data$etrmax/app.data$alpha,0),
                                                       '; Beta: ', round(app.data$beta,2)))

            value.column <- c(app.data$file.name, app.data$par.levels %>% paste0(collapse=','),
                              app.data$smooth, app.data$abs, app.data$model,
                              model.info, round(app.data$poly.coords[,1],2) %>%
                                paste0(collapse=','), round(app.data$poly.coords[,2],2) %>%
                                paste0(collapse=','))

            all.meta <- cbind(id.column, value.column)

            #convert to terra:rast object for saving with terra to preserve layer names
            all.stack <- terra::rast(all.stack)
            terra::metags(all.stack) <- all.meta
            terra::writeRaster(all.stack, filename = file, overwrite=TRUE)
          }

          #TIFF data - not all data
          if(input$parameter.space != 'All data' && input$output.types == 'TIFF file(s)'){
            mod.param <- input$parameter.space

            if(mod.param == 'NIR'){
              all.stack <- app.data$nir
            }
            if(mod.param == 'Red'){
              all.stack <- app.data$red
            }
            if(mod.param == 'Abs.'){
              all.stack <- app.data$abs_img
            }
            if(mod.param == 'F images'){
              all.stack <- raster::stack(app.data$f_img)
            }
            if(mod.param == "Fm images"){
              all.stack <- raster::stack(app.data$fm_img)
            }
            if(mod.param != 'F images' && mod.param != 'Fm images'){
              all.stack <- raster::raster(raster::stack(app.data$model.outputs, app.data$abs_img), layer = mod.param)
            }

            #draw down settings data to add into saved geotiFF
            id.column <- c('Filename', 'PAR levels', 'Smooth factor', 'Absorbance Meas.',
                           'Model Applied', 'Start Parameters', 'Polygon X Coords', 'Polygon Y Coords')
            model.info <- switch(app.data$model,
                                 'Jassby & Platt' = paste('ETRmax: ',round(app.data$etrmax,2),
                                                          '; Alpha: ', round(app.data$alpha,2),
                                                          'Ek: ',round(app.data$etrmax/app.data$alpha,0) ),
                                 'Eilers & Peeters' = paste('ETRmax: ', round(app.data$etrmax,2),
                                                            '; Alpha: ',round(app.data$alpha,2),
                                                            '; Eopt: ', round(app.data$eopt,2),
                                                            '; Ek: ', round(app.data$etrmax/app.data$alpha,0)),
                                 'Platt et al' = paste('Alpha: ',round(app.data$alpha,2),
                                                       '; Ps: ', round(app.data$ps,2),
                                                       '; ETRmax: ', round(app.data$etrmax,2),
                                                       '; Ek: ', round(app.data$etrmax/app.data$alpha,0),
                                                       '; Beta: ', round(app.data$beta,2)))

            value.column <- c(app.data$file.name, app.data$par.levels %>% paste0(collapse=','),
                              app.data$smooth, app.data$abs, app.data$model,
                              model.info, round(app.data$poly.coords[,1],2) %>%
                                paste0(collapse=','), round(app.data$poly.coords[,2],2) %>%
                                paste0(collapse=','))

            all.meta <- cbind(id.column, value.column)
            all.stack <- terra::rast(all.stack)
            terra::metags(all.stack) <- all.meta
            terra::writeRaster(all.stack, filename = file, overwrite=TRUE)
          }

          ##CSV data
          if(input$parameter.space == 'All data' && input$output.types == 'CSV file(s)'){

            if(app.data$abs == "yes"){
              all.stack <- raster::stack(app.data$nir,
                                         app.data$red,
                                         app.data$abs_img,
                                         app.data$f_img,
                                         app.data$fm_img,
                                         app.data$yield,
                                         app.data$etr,
                                         app.data$model.outputs)
            } else {
              all.stack <- raster::stack(app.data$f_img,
                                         app.data$fm_img,
                                         app.data$yield,
                                         app.data$etr,
                                         app.data$model.outputs)
            }

            dat <- as.data.frame(all.stack, xy = T)
            write.csv(dat, file = file, row.names = F)
          }
          if(input$parameter.space != 'All data' && input$output.types == 'CSV file(s)'){
            mod.param <- input$parameter.space
            if(mod.param == 'NIR'){
              all.stack <- app.data$nir
            }
            if(mod.param == 'Red'){
              all.stack <- app.data$red
            }
            if(mod.param == 'Abs.'){
              all.stack <- app.data$abs_img
            }
            if(mod.param == 'F images'){
              all.stack <- raster::stack(app.data$f_img)
            }
            if(mod.param == "Fm images"){
              all.stack <- raster::stack(app.data$fm_img)
            }
            if(mod.param != 'F images' && mod.param != 'Fm images'){
              all.stack <- raster::raster(raster::stack(app.data$model.outputs,app.data$abs_img), layer = mod.param)
            }
            dat <- as.data.frame(all.stack, xy = T)
            write.csv(dat, file = file, row.names = F)
          }
        }
      )

      ########### Transect / ROI analysis tab

      #capture the drawn polyline / polygon and get data values from all layers
      shiny::observeEvent(input$modelOutputs_draw_new_feature, {

        #circular polygon selection
        if(input$modelOutputs_draw_new_feature$properties$feature_type == "circle"){

          #take the x and y of the centre of the circle
          poly.coords <- input$modelOutputs_draw_new_feature$geometry$coordinates %>% unlist
          poly.mat <- data.frame(x = poly.coords[c(TRUE, FALSE)],
                                 y = poly.coords[c(FALSE, TRUE)])

          #transform poly mat for later push to app.data in MERC CRS
          xy <- sp::SpatialPoints(poly.mat)
          sp::proj4string(xy) <- poly.proj

          #spTransform generated a warning with no consequence, suppressed
          suppressWarnings(
            xy <- as.data.frame(sp::spTransform(xy, leafletProj))
          )

          #get the radius - need this to be in units of metres (and capture in poly mat/xy)
          poly.radius <- input$modelOutputs_draw_new_feature$properties$radius %>% unlist
          xy$radius <- poly.radius
          poly.mat$radius <- poly.radius


          #use functions at start of script to generate polygon from these data
          cc <- create.circle.spPolygon(poly.mat$x, poly.mat$y, poly.radius)
          #transform the leaflet projection for cropping
          final.poly <- sp::spTransform(cc, leafletProj)

          #push to app.data
          app.data$analysis.coords <- xy
          app.data$analysis.type <- 'circle'
          app.data$circle.poly <- final.poly
        }


        #capture coordinates in raster CRS, extract values and push to app.data
        poly.coords <- input$modelOutputs_draw_new_feature$geometry$coordinates %>% unlist
        poly.mat <- data.frame(x = poly.coords[c(TRUE, FALSE)], y = poly.coords[c(FALSE, TRUE)])
        xy <- sp::SpatialPoints(poly.mat)
        proj4string(xy) <- poly.proj
        #suppress spTransform warning with no consequence
        suppressWarnings(
          xy <- as.data.frame(sp::spTransform(xy, leafletProj))
        )
        app.data$analysis.coords <- xy
        app.data$analysis.type <- input$modelOutputs_draw_new_feature$properties$feature_type

        #if a transect, extract data from raster
        if(input$modelOutputs_draw_new_feature$properties$feature_type == 'polyline'){
          x <- xy[,1]
          y <- xy[,2]
          my.line <- sp::SpatialLines(list(sp::Lines(sp::Line(cbind(x,y)), ID="a")))
          all.stack <- raster::stack(app.data$f_img, app.data$fm_img, app.data$yield, app.data$etr, app.data$model.outputs)
          my.vals <- raster::extract(all.stack, my.line, cellnumbers = T)[[1]] %>% as.data.frame
          my.vals[,c('x','y')] <- raster::xyFromCell(all.stack, my.vals$cell) #this is not working!!!!
          app.data$analysis.vals <- my.vals
        }

        #if a polygon, extract data from raster
        if(input$modelOutputs_draw_new_feature$properties$feature_type == 'polygon'
           | input$modelOutputs_draw_new_feature$properties$feature_type == 'rectangle'){
          final.poly <- Orcs::coords2Polygons(as.matrix(xy), ID='chris')
          all.stack <- raster::stack(app.data$f_img, app.data$fm_img, app.data$yield, app.data$etr, app.data$model.outputs)
          my.roi <- all.stack %>% raster::crop(raster::extent(final.poly)) %>% raster::mask(final.poly)
          my.vals <- as.data.frame(my.roi, xy = T)
          app.data$analysis.vals <- my.vals
        }

        #if a circle, extract data from raster
        if(input$modelOutputs_draw_new_feature$properties$feature_type == "circle"){
          final.poly <- app.data$circle.poly
          all.stack <- raster::stack(app.data$f_img, app.data$fm_img, app.data$yield, app.data$etr, app.data$model.outputs)
          my.roi <- all.stack %>% raster::crop(raster::extent(final.poly)) %>% raster::mask(final.poly)
          my.vals <- as.data.frame(my.roi, xy = T)
          app.data$analysis.vals <- my.vals
        }
      })

      #dynamic removal of tabs rendered below, in case already present.
      shiny::observeEvent(app.data$analysis.type,{
        if(app.data$analysis.type == 'polyline'){
          shiny::removeTab(inputId = 'mainPanel', target = 'ROI')
        }
        if(app.data$analysis.type == 'polygon'){
          shiny::removeTab(inputId = 'mainPanel', target = 'Transect')
          shiny::removeTab(inputId = 'mainPanel', target = 'ROI')
        }
        if(app.data$analysis.type == 'rectangle'){
          shiny::removeTab(inputId = 'mainPanel', target = 'Transect')
          shiny::removeTab(inputId = 'mainPanel', target = 'ROI')
        }
        if(app.data$analysis.type == 'circle'){
          shiny::removeTab(inputId = 'mainPanel', target = 'Transect')
          shiny::removeTab(inputId = 'mainPanel', target = 'ROI')
        }
      })

      #append a tab with correct type of display depending on transect or ROI analysis type
      shiny::observeEvent(app.data$analysis.type, {
        if(app.data$analysis.type == 'polyline'){

          shiny::appendTab('mainPanel',
                           shiny::tabPanel('Transect',
                                           shiny::fluidPage(
                                             shiny::fluidRow(
                                               plotly::plotlyOutput('transectPlot', width = '100%', height='400px')
                                             ),
                                             shiny::fluidRow(
                                               shiny::uiOutput('transectX')
                                             ),
                                             shiny::fluidRow(
                                               shiny::uiOutput('transectY')
                                             )
                                           )))
        }

        if(app.data$analysis.type == 'polygon' | app.data$analysis.type == 'rectangle' | app.data$analysis.type == 'circle'){

          shiny::appendTab('mainPanel',
                           shiny::tabPanel('ROI',
                                           shiny::fluidPage(
                                             shiny::fluidRow(
                                               shiny::column(12,
                                                             plotly::plotlyOutput('roiPlot1', width = '100%', height = '400px'),
                                                             shiny::br()
                                               )),
                                             shiny::fluidRow(
                                               shiny::column(6,
                                                             plotly::plotlyOutput('roiPlot2', width = '100%', height = '200px')
                                               ),
                                               shiny::column(6,
                                                             plotly::plotlyOutput('roiPlot3', width = '100%', height = '200px')
                                               )

                                             ),
                                             shiny::fluidRow(
                                               shiny::uiOutput('transectX2')
                                             ),
                                             shiny::fluidRow(
                                               shiny::uiOutput('transectY2')
                                             )
                                           )))
        }
      })


      #render x axis options - transect
      output$transectX <- shiny::renderUI({
        nams <- c('pixel no.', 'Fv/Fm', 'Fo', 'Fm', names(app.data$model.outputs))
        shiny::radioButtons('transectX_return', 'Choose X axis parameter', choices = nams, selected = nams[1], inline = T)
      })
      #render y axis options - transect
      output$transectY <- shiny::renderUI({
        nams <- c('pixel no.', 'Fv/Fm', 'Fo', 'Fm', names(app.data$model.outputs))
        shiny::radioButtons('transectY_return', 'Choose Y axis parameter', choices = nams, selected = nams[2], inline = T)
      })

      #render transect plot
      output$transectPlot <- plotly::renderPlotly({

        req(input$transectX_return)

        if(app.data$analysis.type == 'polyline'){
          my.x <- input$transectX_return
          my.y <- input$transectY_return

          #parameter switch for x axis
          switch(my.x,
                 'pixel no.' = {xx <- 1:nrow(app.data$analysis.vals)},
                 'Fv/Fm' = {xx <- app.data$analysis.vals[,'Yield_1']},
                 'Fo' = {xx <- app.data$analysis.vals[,'F_1']},
                 'Fm' = {xx <- app.data$analysis.vals[,'Fm_1']},
                 xx <- app.data$analysis.vals[,my.x])

          #parameter switch for y axis
          switch(my.y,
                 'pixel no.' = {yy <- 1:nrow(app.data$analysis.vals)},
                 'Fv/Fm' = {yy <- app.data$analysis.vals[,'Yield_1']},
                 'Fo' = {yy <- app.data$analysis.vals[,'F_1']},
                 'Fm' = {yy <- app.data$analysis.vals[,'Fm_1']},
                 yy <- app.data$analysis.vals[,my.y])

          #type of plot switch
          if(my.x == 'pixel no.' | my.y == 'pixel no.'){
            my.line.type <- 'lines+markers'
          } else {
            my.line.type <- 'markers'
          }

          #plot
          plotly::plot_ly(data = app.data$analysis.vals, x = xx, y = yy, type='scatter', mode = my.line.type) %>%
            plotly::layout(xaxis = list(title = my.x), yaxis = list(title = my.y))
        }
      })

      #save transect data
      shiny::observeEvent(input$exportTransect,{
        shiny::showModal(
          shiny::modalDialog(title = 'Save Transect Data', size='m',
                             shiny::downloadButton("downloadTransect", "Save Transect Data")
          ))
      })

      #download handler for transect
      output$downloadTransect <- shiny::downloadHandler(
        filename = function() {
          paste('transect_data', '.csv', sep='')
        },
        content = function(file) {
          t.dat <- app.data$analysis.vals
          names(t.dat)[1] <- 'pixel_number'
          t.dat$pixel_number <- 1:nrow(t.dat)
          write.csv(t.dat, file = file, row.names = F)
        })

      #render x axis options - roi
      output$transectX2 <- shiny::renderUI({
        nams <- c('Fv/Fm', 'Fo', 'Fm', names(app.data$model.outputs))
        shiny::radioButtons('transectX2_return', 'Choose X axis parameter and Box Plot parameter', choices = nams, selected = nams[1], inline = T)
      })
      #render y axis options - roi
      output$transectY2 <- shiny::renderUI({
        nams <- c('Fv/Fm', 'Fo', 'Fm', names(app.data$model.outputs))
        shiny::radioButtons('transectY2_return', 'Choose Y axis parameter', choices = nams, selected = nams[2], inline = T)
      })

      #render ROI plot
      output$roiPlot1 <- plotly::renderPlotly({
        req(input$transectX2_return)

        if(app.data$analysis.type == 'polygon' | app.data$analysis.type == 'rectangle' | app.data$analysis.type == 'circle'){
          #get input values
          my.x <- input$transectX2_return
          my.y <- input$transectY2_return

          #parameter switch for x axis
          switch(my.x,
                 'Fv/Fm' = {xx <- app.data$analysis.vals[,'Yield_1']},
                 'Fo' = {xx <- app.data$analysis.vals[,'F_1']},
                 'Fm' = {xx <- app.data$analysis.vals[,'Fm_1']},
                 xx <- app.data$analysis.vals[,my.x])

          #parameter switch for y axis
          switch(my.y,
                 'Fv/Fm' = {yy <- app.data$analysis.vals[,'Yield_1']},
                 'Fo' = {yy <- app.data$analysis.vals[,'F_1']},
                 'Fm' = {yy <- app.data$analysis.vals[,'Fm_1']},
                 yy <- app.data$analysis.vals[,my.y])
          #plot
          plotly::plot_ly(data = app.data$analysis.vals, x = xx, y = yy, type='scatter', mode = 'markers') %>%
            plotly::layout(xaxis = list(title = my.x), yaxis = list(title = my.y))
        }
      })

      #Boxplot for x variable
      output$roiPlot2 <- plotly::renderPlotly({
        if(app.data$analysis.type == 'polygon' | app.data$analysis.type == 'rectangle' | app.data$analysis.type == 'circle'){

          req(input$transectX2_return)

          #get input values - only x-axis parameter otherwise scale can be very off
          my.x <- input$transectX2_return

          #parameter switch for x
          switch(my.x,
                 'Fv/Fm' = {xx <- app.data$analysis.vals[,'Yield_1']},
                 'Fo' = {xx <- app.data$analysis.vals[,'F_1']},
                 'Fm' = {xx <- app.data$analysis.vals[,'Fm_1']},
                 xx <- app.data$analysis.vals[,my.x])
          #plot
          plotly::plot_ly(data = app.data$analysis.vals, y = ~xx, type = "box", name = my.x) %>%
            plotly::layout(yaxis = list(title = my.x))
        }
      })

      #Boxplot for the y variable
      output$roiPlot3 <- plotly::renderPlotly({
        if(app.data$analysis.type == 'polygon' | app.data$analysis.type == 'rectangle' | app.data$analysis.type == 'circle'){

          req(input$transectX2_return)

          #get input values - only y-axis parameter otherwise scale can be very off
          my.y <- input$transectY2_return

          #parameter switch for x
          switch(my.y,
                 'Fv/Fm' = {yy <- app.data$analysis.vals[,'Yield_1']},
                 'Fo' = {yy <- app.data$analysis.vals[,'F_1']},
                 'Fm' = {yy <- app.data$analysis.vals[,'Fm_1']},
                 yy <- app.data$analysis.vals[,my.y])
          #plot
          plotly::plot_ly(data = app.data$analysis.vals, y = ~yy, type = "box", name = my.y) %>%
            plotly::layout(yaxis = list(title = my.y))
        }
      })

      #functionality to export the analysis ROI data to the global environment.
      shiny::observeEvent(input$pushROI,{
        shiny::showModal(
          shiny::modalDialog(
            title = 'Export ROI to R Global Environment',
            size = 'm',
            shiny::textInput('export_roi_name', "Name for R object:", 'pamfit_roi'),
            "Data will be availale as the above named object in the R Global Environment once PAMfit is closed",
            easyClose = T,
            footer=tagList(
              actionButton('submit2', 'Submit'),
              modalButton('cancel')
            )))
      })

      #export to global environment when submit is pushed
      observeEvent(input$submit2, {
        removeModal()
        assign(x = input$export_roi_name, app.data$analysis.vals, envir = .GlobalEnv)
      })

      #save ROI data
      shiny::observeEvent(input$exportROI_data,{
        shiny::showModal(
          shiny::modalDialog(title = 'Save ROI Data', size='m',
                             shiny::downloadButton("downloadROI_data", "Save ROI Data")
          ))
      })

      #download handler for ROI data
      output$downloadROI_data <- shiny::downloadHandler(
        filename = function() {
          paste('roi_data', '.csv', sep='')
        },
        content = function(file) {
          write.csv(app.data$analysis.vals, file = file, row.names = F)
        })

      #####Export transect data to global environment
      #functionality to export the analysis transect data to the global environment.
      shiny::observeEvent(input$pushTransect,{
        shiny::showModal(
          shiny::modalDialog(
            title = 'Export Transrect to R Global Environment',
            size = 'm',
            shiny::textInput('export_trans_name', "Name for R object:", 'pamfit_transect'),
            "Data will be availale as the above named object in the R Global Environment once PAMfit is closed",
            easyClose = T,
            footer=tagList(
              actionButton('submit3', 'Submit'),
              modalButton('cancel')
            )))
      })

      #export transect data to global environment when submit is pushed
      observeEvent(input$submit3, {
        removeModal()
        t.dat <- app.data$analysis.vals
        names(t.dat)[1] <- 'pixel_number'
        t.dat$pixel_number <- 1:nrow(t.dat)
        assign(x = input$export_trans_name, t.dat, envir = .GlobalEnv)
      })

    }) #end of warnings suppression for whole server

  }#end of server

  #run the app!
  shiny::shinyApp(ui = ui, server = server)








} #end of pamfit function
