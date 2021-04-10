#' @title shiny app calculate images of RLC parameters
#' @description  shiny app calculate images of RLC parameters (add bigger description)
#' @return Returns a list with the images of the RLC parameters
#' @keywords external
#' @importFrom magrittr %>%
#' @import sp
#' @examples
#'
#'
#' @export
#shiny app to input a file and read in light levels
shiny_master<-function(){

  `%>%` <- magrittr::`%>%`

  #this increases uplaod file size to 30MB
  options(shiny.maxRequestSize = 30*1024^2)

  #projection to apply to uploaded data for later leaflet raster plotting
  leafletProj <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +nadgrids=@null +wktext +no_defs"

  #define polygon projection
  poly.proj <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

  #create environment
  e2 <- new.env()


  ui <- shinydashboard::dashboardPage(

    shinydashboard::dashboardHeader(title = "Imaging PAM RLC fit"),

    shinydashboard::dashboardSidebar(
      #dynamic sidebarMenu driven by folder heirachry
      shinydashboard::sidebarMenu(id = 'sidebarmenu',
                                  #menuItem("1. Upload Image", tabName = "uploadImage",  icon = icon("check-circle", lib="font-awesome"), badgeColor = 'red'),
                                  shinydashboard::menuItemOutput('menu1'),
                                  shinydashboard::menuItemOutput('menu2'),
                                  shinydashboard::menuItemOutput('menu3'),
                                  shinydashboard::menuItemOutput('menu4'),
                                  #menuItemOutput('menu5'),

                                  shiny::conditionalPanel("input.sidebarmenu === 'summaryStatistics'",
                                                          shiny::uiOutput('exportData')),
                                  shiny::conditionalPanel("input.sidebarmenu === 'summaryStatistics'",
                                                          shiny::uiOutput('saveData')),
                                  shiny::conditionalPanel("input.mainPanel === 'Transect'",
                                                          shiny::actionButton('exportTransect', 'Export Transect', width='80%')),
                                  shiny::conditionalPanel("input.mainPanel === 'ROI'",
                                                          shiny::actionButton('exportROI', 'Export ROI', width='80%'))
      )),#end of dashboardSidebar

    shinydashboard::dashboardBody(

      #define CSS section
      tags$head(
        tags$style(
          # Colorize the actionButton.
          HTML(
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


      shinydashboard::tabItems(
        shinydashboard::tabItem(
          #this is our shiny 1 tab
          tabName = 'uploadImage',

          shiny::fluidPage(

            shiny::titlePanel(title = 'Input data and initial settings'),
            shiny::hr(),
            shiny::fluidRow(shiny::column(6,
                                          shiny::fileInput('fileIn', '1. Upload your TIFF file',
                                                           multiple = F, accept = c(".tif",".tiff")),
                                          shiny::hr(),
                                          shiny::HTML('<b>2. Input light step PAR levels</b>'),
                                          shiny::br(),
                                          DT::dataTableOutput('lightSteps')
            ),
            shiny::column(6,
                          leaflet::leafletOutput("img1"),
                          shiny::radioButtons('smooth', '3. Choose degree of pixel smoothing',
                                              choices = c(0,3,5,7,9,11,13,15), selected = 0,inline = T),
                          shiny::hr(),
                          shiny::radioButtons('abs', '4. Identify whether data contains an absorbance image',
                                              choices = c('Yes' = 'yes', 'No' = 'no'), inline = T, selected = 'no'),
                          shiny::hr(),
                          shiny::HTML('<b>5. Confirm Settings to Proceed</b>'),
                          shiny::actionButton('confirm', 'Confirm', width='100%')
            )
            )

          )
        ),
        shinydashboard::tabItem(tabName = 'selectROI',
                                shiny::fluidRow(
                                  column(10,
                                         leaflet::leafletOutput("leafmap", height = '600px')
                                  ), #end of first 8 columns
                                  column(2,
                                         hr(),
                                         shiny::actionButton('exportROI', 'Export ROI?', width = '100%'), #no server side yet
                                         br(),
                                         shiny::actionButton('importROI', 'Import ROI?', width = '100%'), #no server side yet
                                         shiny::actionButton('confirm2', 'Confirm ROI', width = '100%')
                                  )#end of last 4 columns
                                )#end of fluid row
        ),
        shinydashboard::tabItem(tabName = 'selectFitParameters',
                                shiny::fluidPage(

                                  shiny::titlePanel(title = 'Examine rETR and select starting parameters'),
                                  shiny::hr(),

                                  #panel showing image to select pixel from to display ETR
                                  shiny::fluidRow(shiny::column(4,
                                                                leaflet::leafletOutput('pixelSelector', width = '100%', height = '400px')
                                  ),#end of column
                                  shiny::column(3,
                                                shiny::uiOutput('renderLayers'),
                                                shiny::radioButtons('model', 'Select model fit',
                                                                    choices = c('none','Jassby & Platt', 'Platt', 'Eilers & Peeters'),
                                                                    selected = 'none'),
                                                shiny::br(),
                                                #output of etrmax, alpha as potential starting parameters
                                                shiny::htmlOutput('currentParams'),
                                                shiny::br(),
                                                shiny::br(),
                                                shiny::actionButton('useParams', 'Confirm & Run Model', width = '100%')
                                  ),
                                  shiny::column(5,
                                                plotly::plotlyOutput('etrPlot', width = '100%', height='400px')
                                  )#end of column
                                  )#
                                )), #end of fluid page & tab items

        shinydashboard::tabItem(tabName = 'summaryStatistics',
                                shiny::fluidPage(
                                  shiny::titlePanel(title = 'View data and settings'),
                                  shiny::mainPanel(
                                    shiny::tabsetPanel(id = 'mainPanel',
                                                       shiny::tabPanel('Raw data',
                                                                       shiny::fluidPage(
                                                                         shiny::fluidRow(shiny::column(12,
                                                                                                       shiny::uiOutput('selectBrick', width = '50%')
                                                                         )#end of column1
                                                                         ),#end of row 1
                                                                         shiny::fluidRow(column(10,
                                                                                                leaflet::leafletOutput('rasterImage')
                                                                         ),
                                                                         column(2,
                                                                                shiny::htmlOutput('selectLayer')
                                                                         )
                                                                         )#end of row 2
                                                                       )#end of fluidPage
                                                       ), #end of tabPanel 1

                                                       shiny::tabPanel('Model Outputs',
                                                                       shiny::fluidPage(
                                                                         shiny::fluidRow(
                                                                           shiny::uiOutput('selectParameter', width = '100%')
                                                                         ),#end of row 1
                                                                         shiny::fluidRow(
                                                                           column(10,
                                                                                  leaflet::leafletOutput('modelOutputs', width = '100%'),
                                                                                  shiny::uiOutput('zlim', width = '100%')
                                                                           ),
                                                                           column(2,
                                                                                  shiny::uiOutput('zmin'),
                                                                                  shiny::uiOutput('zmax'),
                                                                                  shiny::HTML('<b>Current Value</b>'),
                                                                                  shiny::htmlOutput('current_val')
                                                                           )
                                                                         )#end of row 2
                                                                       )#end of fluidPage
                                                       ),#end of tabPanel 2

                                                       shiny::tabPanel('Settings',
                                                                       shiny::fluidRow(
                                                                         shiny::column(6,
                                                                                       DT::dataTableOutput('input.settings'))
                                                                       ) )
                                    )
                                  )#end of main panel
                                )#sidebarLayout
        ),#fluidPage
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
                                                                            column(10,
                                                                                   leaflet::leafletOutput('modelOutputs2', width = '100%'),
                                                                                   shiny::uiOutput('zlim2', width = '100%')
                                                                            ),
                                                                            column(2,
                                                                                   shiny::uiOutput('zmin2'),
                                                                                   shiny::uiOutput('zmax2'),
                                                                                   shiny::HTML('<b>Current Value</b>'),
                                                                                   shiny::htmlOutput('current_val2')
                                                                            )
                                                                          )#end of row 2
                                                                        )#end of fluidPage
                                                        )

                                    )#end of tabset panel
                                  )#end of main panel

                                )#end of flluid page

        )#end of tabItem
      )#dashboardBody
          )
        )#end of UI


  server <- function(input, output, session){

    app.data <<- shiny::reactiveValues(data = NULL)

    ##Render Menu1
    output$menu1 <- shinydashboard::renderMenu({
      if(!is.null(app.data$step_1)){
        my.icon <- icon('check-square', lib = 'font-awesome', class = 'bruno')
      } else {
        my.icon <- icon('check-square', lib = 'font-awesome', class = 'bruno2')
      }
      shinydashboard::menuItem("1. Upload Image", tabName = "uploadImage",  icon = my.icon)
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

    ##Render Menu5
    # output$menu5 <- renderMenu({
    #   if(!is.null(app.data$step_3)){
    #     my.icon <- icon('check-square', lib = 'font-awesome', class = 'bruno')
    #   } else {
    #     my.icon <- icon('check-square', lib = 'font-awesome', class = 'bruno2')
    #   }
    #   menuItem("5. Analyse Data", tabName = 'dataAnalysis', icon = my.icon)
    #
    # })

    ####################### Shiny 1 code ##################################
    #file upload
    shiny::observeEvent(input$fileIn, {
      #get file details - also makes this reactive on input$fileIn
      my.file <- input$fileIn

      #load the data
      dat <- tiff::readTIFF(my.file$datapath, all = T)
      app.data$data <- dat
      #load in as stack
      my.brick <- raster::stack()
      for(i in 1:length(dat)){
        my.brick <- raster::stack(my.brick, raster::raster(as.matrix(dat[[i]])))
      }
      #set extent based on image properties and set our desired CRS
      raster::extent(my.brick) <- c(0,ncol(my.brick), 0, nrow(my.brick))
      raster::projection(my.brick) <- sp::CRS(leafletProj)
      #push to app.data
      app.data$my.brick <- my.brick
    })


    #render leafmap plot of uploaded TIFF for smoothing
    output$img1 <- leaflet::renderLeaflet({
      my.smooth <- input$smooth %>% as.numeric
      if(!is.null(app.data$my.brick)){
        my.brick <- app.data$my.brick
        my.image <- raster::raster(my.brick, 5)
        if(my.smooth > 0){
          my.mat <- matrix(1, nrow = my.smooth, ncol = my.smooth)
          my.image <- raster::focal(my.image, w = my.mat, fun = mean, pad = T, padValue = NA, na.rm = T)
        }

        #output smooth level selected to app.data
        app.data$smooth <- my.smooth

        my.rast <- my.image
        pal <- leaflet::colorNumeric(palette = 'magma',
                                     domain = c(min(raster::values(my.rast), na.rm = T), max(raster::values(my.rast), na.rm = T)),
                                     na.color = 'transparent')
        leaflet::leaflet() %>%
          leaflet::addRasterImage(my.rast, project = T, colors = pal)
      }
    })



    #function to interatively add textInput with an ID and initial value to dataframe
    shinyInput = function(FUN, len, id, int.value, ...) {
      #validate(need(character(len)>0,message=paste("")))
      inputs = character(len)
      for (i in seq_len(len)) {
        inputs[i] = as.character(FUN(paste0(id, i), label = NULL,
                                     value = int.value[i], width = '100px',...))
      }
      inputs
    }

    #render output light levels data table
    output$lightSteps <- DT::renderDataTable({
      if(!is.null(app.data$my.brick)){
        my.brick <- app.data$my.brick
        #establish number of light levels to have
        no.light.steps <- (raster::nlayers(my.brick) - 4) / 2
        #check to see if light levels already loaded (object tmp.light in global environment)
        if(exists('tmp.light')){
          my.int.par <- tmp.light
          #check right number of light levels apparent
          if(length(my.int.par) != no.light.steps){
            my.int.par <- rep('NA', no.light.steps)
          }

        } else {
          my.int.par <- rep('NA', no.light.steps)
        }
        #make datatable for rendering
        DT::datatable(
          data.frame('Light_Step' = 1:no.light.steps,
                     'PAR_level' = shinyInput(shiny::textInput,no.light.steps,"ls_", my.int.par)),
          selection="multiple",
          rownames = FALSE,
          escape = FALSE,
          #filter = list(position = 'top', clear = FALSE),
          #extensions = list("ColReorder" = NULL, "Buttons" = NULL),
          options = list(
            dom = 't',
            ColReorder = TRUE,
            #extensions = list("ColReorder" = NULL, "Buttons" = NULL),
            autoWidth=TRUE,
            lengthMenu = list(c(no.light.steps), c(as.character(no.light.steps))),
            rowCallback = DT::JS("function(r,d) {$(r).attr('height', '50px')}"),
            preDrawCallback = DT::JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
            drawCallback = DT::JS('function() { Shiny.bindAll(this.api().table().node()); } '),
            columnDefs = list(list(targets=c(0), visible = T, width = '100px'))
          ))

      }#end of if is.NULL my.brick
    })

    #actions on confirm button selection (save light levels; save to external env; smooth image; smooth raster stack; label raster stack)
    shiny::observeEvent(input$confirm, {
      no.light.steps <- (raster::nlayers(app.data$my.brick) - 4) / 2
      my.par <- vector()
      for(i in 1:no.light.steps){
        my.in <- paste('ls_',i,sep='')
        my.par[i] <- input[[my.in]] %>% as.numeric
      }
      #send to app.data and send par.levels to s1.env to check on reload...
      app.data$par.levels <- my.par
      #writeout tmp.light to global environment?
      assign('tmp.light', my.par, envir = .GlobalEnv)

      #apply smoothing to original data set
      if(app.data$smooth > 0){
        my.mat <- matrix(1, nrow = app.data$smooth, ncol = app.data$smooth)
        for(i in 1:(raster::nlayers(app.data$my.brick))){
          app.data$my.brick[[i]] <- raster::focal(raster::raster(app.data$my.brick,i), fun = mean, w = my.mat, pad = T, padValue = NA, na.rm = T)
        }
      }
      #apply names to each layer of the original data
      ls.nos <- 1:length(app.data$par.levels)
      names(app.data$my.brick) <- c('Fo_initial', 'Fm_initial', 'NIR', 'Red',c(rbind(paste('F_', ls.nos, sep=''), paste('Fm_', ls.nos, sep=''))))

      #check to see if abs image incorporated in fileInput
      app.data$abs <- input$abs

      #check to ensure step 1 (file upload is compelete)
      app.data$step_1 <- 'complete'

      #switch to next tab
      shinydashboard::updateTabItems(session, 'sidebarmenu', 'selectROI')
    })


    ####################### Shiny 2 code ##################################
    #want this page to be rendered depedent on step1 (file upload) being complete

    #render leafmap plot of uploaded TIFF
    output$leafmap <- leaflet::renderLeaflet({
      if(!is.null(app.data$step_1) && app.data$step_1 == 'complete'){
        my.rast <- raster::raster(app.data$my.brick,5)
        pal <- leaflet::colorNumeric(palette = 'magma',
                                     domain = c(min(raster::values(my.rast), na.rm = T), max(raster::values(my.rast), na.rm = T)),
                                     na.color = 'transparent')
        leaflet::leaflet() %>%
          leaflet::addRasterImage(my.rast, project = T, colors = pal) %>%
          leaflet.extras::addDrawToolbar( polylineOptions = F, circleOptions = F, rectangleOptions = T,
                                          markerOptions = F, circleMarkerOptions = F, singleFeature = T,
                                          polygonOptions = leaflet.extras::drawPolygonOptions(repeatMode = F)
          )
      } else {
        'Complete step 1'
      }
    })

    #capture the drawn polygon
    shiny::observeEvent(input$leafmap_draw_new_feature, {
      input$leafmap_draw_new_feature

      poly.coords <- input$leafmap_draw_new_feature$geometry$coordinates %>% unlist
      poly.mat <- data.frame(x = poly.coords[c(TRUE, FALSE)], y = poly.coords[c(FALSE, TRUE)])
      xy <- sp::SpatialPoints(poly.mat)
      proj4string(xy) <- poly.proj
      xy <- as.data.frame(spTransform(xy, leafletProj))
      final.poly <- Orcs::coords2Polygons(as.matrix(xy), ID='chris')
      my.crop <- app.data$my.brick %>% raster::crop(raster::extent(final.poly)) %>% raster::mask(final.poly)

      app.data$my.crop <- my.crop
      app.data$poly.coords <-xy
      app.data$poly.draw <- final.poly

    })


    #data handling on confirm ROI
    shiny::observeEvent(input$confirm2,{

      #process rasters to determine f, fm, yield and etr images
      this.dat <- app.data$my.crop[[-c(1:4)]]
      #num_samples <- this.dat %>% raster::nlayers
      num_samples <-  raster::nlayers(this.dat)

      #alternate sequnces to be used for subsetting Fo / Fm images, respectively
      my.seq <- 1:num_samples
      odd_sequence <- my.seq[c(TRUE, FALSE)]
      even_sequence <- my.seq[c(FALSE, TRUE)]

      #1 - extract F images
      f_img <- this.dat[[odd_sequence]]
      #2 - extract Fm images
      fm_img <- this.dat[[even_sequence]]
      #3 - calculate Yield images (this is a brick object)
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
        app.data$abs_img <- abs.rast
      }

      etr.fun.1 <- function(x) { x * light * 0.5 }
      #etr.fun.2 <- function(x) {x * light * 0.5 * abs.rast}

      if(abs == "no"){
        etr <- raster::calc(fv_fm_img, etr.fun.1)
      } else {
        etr <- raster::calc(fv_fm_img, etr.fun.1)
        etr <- etr * abs.rast
      }

      #push data to app.data reactive values list
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

      #switch to next tab
      shinydashboard::updateTabItems(session, 'sidebarmenu', 'selectFitParameters')
    })


    ####################### Shiny 3 code ##################################

    #render selectInput layers
    output$renderLayers <- shiny::renderUI({
      if(!is.null(app.data$step_2) && app.data$step_2 == 'complete'){
        my.layers <- raster::nlayers(app.data$etr)
        shiny::selectInput('layers', 'Number of light steps to plot',
                           choices = 4:my.layers, selected = my.layers)
      }
    })

    shiny::observeEvent(input$pixelSelector_click, {
      app.data$click <- input$pixelSelector_click
    })

    shiny::observeEvent(input$model, {
      app.data$model <- input$model
    })

    shiny::observeEvent(input$layers,{
      app.data$layers <- input$layers
    })


    output$pixelSelector <- leaflet::renderLeaflet({
      if(!is.null(app.data$step_2) && app.data$step_2 == 'complete'){
        my.rast <- app.data$yield
        pal <- leaflet::colorNumeric(palette = 'magma',
                                     domain = c(min(raster::values(my.rast), na.rm = T), max(raster::values(my.rast), na.rm = T)),
                                     na.color = 'transparent')
        leaflet::leaflet() %>%
          leaflet::addRasterImage(raster::raster(app.data$yield, 1), project = T, colors = pal) %>%
          leaflet::addLegend(pal = pal,
                             values = raster::values(my.rast))

      }
    })

    #extract click coordinates and use to extract etr data from raster brick
    shiny::observeEvent(input$pixelSelector_click, {
      if(!is.null(app.data$step_2) && app.data$step_2 == 'complete'){
        #get coords from click object and project back to raster CRS
        my.x.coord <- app.data$click$lng
        my.y.coord <- app.data$click$lat
        coord.mat <- data.frame(x = my.x.coord, y = my.y.coord)
        xy <- sp::SpatialPoints(coord.mat)
        sp::proj4string(xy) <- poly.proj
        click.coord <- as.data.frame(sp::spTransform(xy, leafletProj))

        #make available
        app.data$click.coord <- click.coord

        #extract associated data from raster brick
        app.data$current.etr <- raster::extract(app.data$etr, click.coord) %>% as.vector
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
        }#end of if

        #Platt model selected - currently defaults to plot1() driven by no model
        if(app.data$model == 'Platt'){
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
          }

        }#end of platt model if

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

    #reactive plot when no model OR Platt (not made) selected
    plot1 <- shiny::reactive({
      plotly::plot_ly(x = app.data$model.par, y = app.data$model.etr, type='scatter', mode='line') %>%
        plotly::layout(xaxis = list(title = 'PAR'), yaxis = list(title = 'rETR'))
    })

    #reactive plot when Jassby & Platt OR EP model selected
    plot2 <- shiny::reactive({
      plotly::plot_ly(x = app.data$model.par, y = app.data$model.etr, type='scatter', mode='line', name = 'data') %>%
        plotly::layout(xaxis = list(title = 'PAR'), yaxis = list(title = 'rETR'), legend = list(x = 0.1, y = 0.95)) %>%
        plotly::add_trace(x = app.data$pred.par, y = app.data$pred, name = app.data$model) %>%
        plotly::layout(legend = list(x = 0.5, y = 0.1))
    })

    #select which plot to use
    myGraph <- shiny::reactive({
      switch(app.data$model,
             'none' = plot1(),
             'Jassby & Platt' = plot2(),
             'Eilers & Peeters' = plot2(),
             'Platt' = plot2())
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
                     paste('<b>ETRmax</b>: ', etrmax, sep=''),
                     paste('<b>Alpha</b>: ', alpha, sep=''))
        shiny::HTML(paste0(my.info, sep='',collapse='<br/>'))

      }
    })

    ep.param <- shiny::reactive({
      if(!is.null(app.data$click[[1]])){
        if(!is.null(app.data$etrmax) & !is.null(app.data$alpha) & !is.null(app.data$eopt)){
          etrmax <- round(app.data$etrmax,2)
          alpha <- round(app.data$alpha,2)
          eopt <- round(app.data$eopt,2)
        } else {
          etrmax <- NA
          alpha <- NA
          eopt <- NA
        }
        my.info <- c('<b>Current Model Parameters</b>',
                     paste('<b>ETRmax</b>: ', etrmax, sep=''),
                     paste('<b>Alpha</b>: ', alpha, sep=''),
                     paste('<b>Eopt</b>: ', eopt, sep=''))
        shiny::HTML(paste0(my.info, sep='',collapse='<br/>'))
      }
    })

    p.param <- shiny::reactive({
      if(!is.null(app.data$click[[1]])){
        if(!is.null(app.data$alpha) & !is.null(app.data$ps) & !is.null(app.data$beta)){
          alpha <- round(app.data$alpha,2)
          ps <- round(app.data$ps,2)
          beta <- round(app.data$beta,2)
        } else {
          alpha <- NA
          ps <- NA
          beta <- NA}
        my.info <- c('<b>Current Model Parameters</b>',
                     paste('<b>Alpha</b>: ', alpha, sep=''),
                     paste('<b>Ps</b>: ', ps, sep=''),
                     paste('<b>Beta</b>: ', beta, sep=''))
        shiny::HTML(paste0(my.info, sep='',collapse='<br/>'))
      }
    })

    #check with model selected
    my.params <- shiny::reactive({
      switch(app.data$model,
             'none' = no.param(),
             'Jassby & Platt' = jp.param(),
             'Eilers & Peeters' = ep.param(),
             'Platt' = p.param()
      )
    })

    output$currentParams <- shiny::renderUI({
      my.params()
    })


    #data processing on confirmation of model and parameter selection
    shiny::observeEvent(input$useParams, {
      #assign our desired objects back to our my.data list
      #save model parameters dependingon what model is selected
      my.model <- app.data$model
      cut.light.steps <- app.data$layers %>% as.numeric

      #run the model selected on the nlayers chosen over the entire cropped image..
      #clip down data and light levels too desirced number of light steps
      clipped.etr <- app.data$etr[[1:as.numeric(app.data$layers)]]
      clipped.par <- app.data$par.levels[1:as.numeric(app.data$layers)]


      ####################
      #Define models - should be read in from other file in final version
      jassby.platt.mod<-function(x){

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

      shiny::withProgress(message = 'Running model', { ##***this could be improved ###
        #######
        #Jassby and Platt
        ######
        if(my.model == 'Jassby & Platt'){
          #light needs to be available as object to the functions below
          light <- assign('light', clipped.par, envir = e2)
          s.etrmax <- assign('s.etrmax', app.data$etrmax, envir = e2)
          s.alpha <- assign('s.alpha', app.data$alpha, envir = e2)

          no.cores <- parallel::detectCores()
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
        if(my.model == 'Platt'){
          #light needs to be available as object to the functions below
          light <- assign('light', clipped.par, envir = e2)
          s.alpha <- assign('s.alpha', app.data$alpha, envir = e2)
          s.ps <- assign('s.ps', app.data$ps, envir = e2)
          s.beta <- assign('s.beta', app.data$beta, envir = e2)

          no.cores <- parallel::detectCores()
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
          #light needs to be available as object to the functions below
          light <- assign('light', clipped.par, envir = e2)
          s.alpha <- assign('s.alpha', app.data$alpha, envir = e2)
          s.etrmax <- assign('s.etrmax', app.data$etrmax, envir = e2)
          s.eopt <- assign('s.eopt', app.data$eopt, envir = e2)

          no.cores <- parallel::detectCores()
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

      #switch to next tab
      shinydashboard::updateTabItems(session, 'sidebarmenu', 'summaryStatistics')

    })



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
        #F, Fm, Y[PSII], ETR
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

        my.rast <- raster::raster(my.brick, which(app.data$par.levels == app.data$layer))
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


    ########Model outputs tab

    #render selectParameter options
    output$selectParameter <- shiny::renderUI({
      #model output names
      my.nams <- names(app.data$model.outputs)
      #plus extras
      if(app.data$abs == 'yes'){
        my.nams <- c('Abs.', 'Fv/Fm', 'Fo', 'Fm', my.nams)} else {
          my.nams <- c('Fv/Fm', 'Fo', 'Fm', my.nams)
        }
      shiny::radioButtons('selectParameter', 'Select Model Parameter to Plot', choices = my.nams, selected = 'Fv/Fm', inline=T)
    })

    #read from selectParameter options
    shiny::observeEvent(input$selectParameter, {
      app.data$model.param.to.plot <- input$selectParameter
    })

    #render zmin options...
    output$zmin <- shiny::renderUI({
      if(!is.null(app.data$model.param.to.plot)){
        #need conditionals here depending on selected parameter from Fv/Fm, Fo, Fm, Abs, model.outputs
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
        alt.nams <- c('Abs.', 'Fv/Fm', 'Fo', 'Fm')
        if(!app.data$model.param.to.plot %in% alt.nams){
          my.rast <- raster::raster(app.data$model.outputs, app.data$model.param.to.plot)
        }

        my.min <- range(raster::values(my.rast), na.rm = T)[1]
        shiny::textInput('zmin_return', 'zmin', value = my.min)
      }
    })
    #render zmax options
    output$zmax <- shiny::renderUI({
      if(!is.null(app.data$model.param.to.plot)){
        #need conditionals here depending on selected parameter from Fv/Fm, Fo, Fm, Abs, model.outputs
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
        alt.nams <- c('Abs.', 'Fv/Fm', 'Fo', 'Fm')
        if(!app.data$model.param.to.plot %in% alt.nams){
          my.rast <- raster::raster(app.data$model.outputs, app.data$model.param.to.plot)
        }

        my.max <- range(raster::values(my.rast), na.rm = T)[2]
        shiny::textInput('zmax_return', 'zmax', value = my.max)
      }
    })

    #observe zmin and zmax inputs for dynamic plotting below
    shiny::observeEvent(input$zmin_return, {
      app.data$zmin <- input$zmin_return %>% as.numeric
    })
    shiny::observeEvent(input$zmax_return,{
      app.data$zmax <- input$zmax_return %>% as.numeric
    })

    shiny::observeEvent(input$modelOutputs_click, {
      app.data$model_click <- input$modelOutputs_click
    })

    #render main leaflet plot; need to add in polygon / transect options for this
    output$modelOutputs <- leaflet::renderLeaflet({
      if(!is.null(app.data$model.param.to.plot)){
        #need conditionals here depending on selected parameter from Fv/Fm, Fo, Fm, Abs, model.outputs
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
        alt.nams <- c('Abs.', 'Fv/Fm', 'Fo', 'Fm')
        if(!app.data$model.param.to.plot %in% alt.nams){
          my.rast <- raster::raster(app.data$model.outputs, app.data$model.param.to.plot)
        }

        pal <- leaflet::colorNumeric(palette = 'magma',
                                     domain = c(app.data$zmin, app.data$zmax), #make them dynamic on zlim_return
                                     na.color = 'transparent')

        leaflet::leaflet() %>%
          leaflet::addRasterImage(my.rast, project = T, colors = pal)  %>%
          #leaflet::addPopups(lng=comb$x, lat=comb$y, popup = comb[,3]) %>%
          leaflet::addLegend(pal = pal,
                             values = raster::values(my.rast)[which(raster::values(my.rast >= app.data$zmin) & raster::values(my.rast <= app.data$zmax))]
          ) %>%
          leaflet.extras::addDrawToolbar(polylineOptions = T, circleOptions = F, rectangleOptions = T,
                                         markerOptions = F, circleMarkerOptions = F, singleFeature = T,
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
        click.coord <- as.data.frame(sp::spTransform(xy, leafletProj))

        #need conditionals here depending on selected parameter from Fv/Fm, Fo, Fm, Abs, model.outputs
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
        alt.nams <- c('Abs.', 'Fv/Fm', 'Fo', 'Fm')
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

    #render plotly histogram of selected pixels (not in ui yet)



    ######Settings data tab

    #render input settings info
    output$input.settings <- DT::renderDataTable({

      id.column <- c('Filename', 'PAR levels', 'Smooth factor', 'Absorbance Meas.',
                     'Model Applied', 'Start Parameters', 'Polygon X Coords', 'Polygon Y Coords')
      model.info <- switch(app.data$model,
                           'Jassby & Platt' = paste('etrmax: ',round(app.data$etrmax,2),'; alpha: ', round(app.data$alpha,2)),
                           'Eilers & Peeters' = paste('etrmax: ', round(app.data$etrmax,2), '; alpha: ',
                                                      round(app.data$alpha,2), '; eopt: ', round(app.data$eopt,2)),
                           'Platt' = paste('alpha: ',round(app.data$alpha,2), '; ps: ', round(app.data$ps,2),
                                           '; beta: ', round(app.data$beta,2)))

      value.column <- c(input$fileIn$name, app.data$par.levels %>% paste0(collapse=','), app.data$smooth, app.data$abs, app.data$model,
                        model.info, round(app.data$poly.coords$x,2) %>% paste0(collapse=','), round(app.data$poly.coords$y,2) %>% paste0(collapse=','))
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


    ##########################All data export (sidebarMenu conditional panel)
    #render export to global environment button
    output$exportData <- shiny::renderUI({
      actionButton('exportGE', 'Export to R', width = '80%')
    })

    #send to global environment
    observeEvent(input$exportGE, {
      my.list <- list('f_images' = app.data$f_img, 'fm_images' = app.data$fm_img, 'Y[PSII]_images' = app.data$yield, 'ETR_images' = app.data$etr, 'model_outputs' = app.data$model.outputs) #need to expand this to include all variables we think + settings

      assign(x = 'bruno_pam_machine', my.list, envir = .GlobalEnv)

      showModal(
        modalDialog(
          title = 'Export all to R Global Environment',
          size = 'm',
          "All data is now availale as an object called 'bruno_pam_machine' in R Global Environment",
          easyClose = T
        )
      )
    })


    #render save button
    output$saveData <- shiny::renderUI({
      actionButton('save_all', 'Save Data', width = '80%')
    })

    #take action on save button press
    observeEvent(input$save_all, {

      #code to calculate the different choices etc to be rendered
      my.choices <- c('All data', names(app.data$model.outputs))

      showModal(
        modalDialog(
          title = 'Save Data', size = 'l',
          fluidRow(
            column(4,
                   radioButtons('parameter.space', '1. Choose Parameters',
                                choices = my.choices, selected = 'All data')
            ),
            column(4,
                   radioButtons('output.types', '2. Choose Output Format',
                                choices = c('TIFF file(s)', 'CSV file(s)'), selected = 'TIFF file(s)')
            ),
            column(4,
                   HTML('<b>3. Save Data       </b>'),
                   downloadButton("downloadData", "Save Data")
            )
          ),
          easyClose = T
        )
      )
    })

    #observe save settings and construct data for saving
    # observeEvent(input$parameter.space, {
    #   #format data for download depending on parameter.space and output.types
    #   if(input$parameter.space == 'All data'){
    #     #make organised and labelled raster stack to loop through
    #     all.stack <<- stack(app.data$f_img, app.data$fm_img, app.data$yield, app.data$etr, app.data$model.outputs)
    #     #make organised dataframe of settings
    #
    #     #push to app.data
    #     app.data$to.download <- all.stack
    #   }
    #
    # })

    output$downloadData <- downloadHandler(
      filename = function() {
        #essentially this is only for the file type extension
        #file type switch
        if(input$output.types == 'TIFF file(s)'){
          my.ext <- '.tif'
        }
        if(input$output.types == 'CSV file(s)'){
          my.ext <- '.csv'
        }
        my.ext
      },
      content = function(file) {

        #save content dependent on input parameters
        #TIFF data - all data
        if(input$parameter.space == 'All data' && input$output.types == 'TIFF file(s)'){
          all.stack <- stack(app.data$f_img, app.data$fm_img, app.data$yield, app.data$etr, app.data$model.outputs)
          writeRaster(all.stack, filename = file, format = 'GTiff')

        }
        #TIFF data - not all data
        if(input$parameter.space != 'All data' && input$output.types == 'TIFF file(s)'){
          mod.param <- input$parameter.space
          all.stack <- raster(app.data$model.outputs, layer = mod.param) #only works for model params (not raw data) at the mo
          writeRaster(all.stack, filename = file, format = 'GTiff')
        }

        ##CSV data
        if(input$parameter.space == 'All data' && input$output.types == 'CSV file(s)'){
          all.stack <- stack(app.data$f_img, app.data$fm_img, app.data$yield, app.data$etr, app.data$model.outputs)
          dat <- as.data.frame(all.stack, xy = T)
          write.csv(dat, file = file, row.names = F)
        }
        if(input$parameter.space != 'All data' && input$output.types == 'CSV file(s)'){
          mod.param <- input$parameter.space
          #print(mod.param)
          all.stack <- raster(app.data$model.outputs, layer = mod.param) #only works for model params (not raw data) at the mo
          dat <- as.data.frame(all.stack, xy = T)
          write.csv(dat, file = file, row.names = F)
        }

      }
    )

    ###########adding transect / roi tab
    #capture the drawn polyline / polygon and get data values from all layers
    shiny::observeEvent(input$modelOutputs_draw_new_feature, {
      #capture coordinates in raster CRS, extract values and push to app.data
      poly.coords <- input$modelOutputs_draw_new_feature$geometry$coordinates %>% unlist
      poly.mat <- data.frame(x = poly.coords[c(TRUE, FALSE)], y = poly.coords[c(FALSE, TRUE)])
      xy <- sp::SpatialPoints(poly.mat)
      proj4string(xy) <- poly.proj
      xy <- as.data.frame(spTransform(xy, leafletProj))
      app.data$analysis.coords <- xy
      app.data$analysis.type <- input$modelOutputs_draw_new_feature$properties$feature_type

      #if a transect, extract data from raster
      if(input$modelOutputs_draw_new_feature$properties$feature_type == 'polyline'){
        x <- xy$x
        y <- xy$y
        my.line <- SpatialLines(list(Lines(Line(cbind(x,y)), ID="a")))
        all.stack <- stack(app.data$f_img, app.data$fm_img, app.data$yield, app.data$etr, app.data$model.outputs)
        my.vals <- extract(all.stack, my.line, cellnumbers = T)[[1]] %>% as.data.frame
        my.vals[,c('x','y')] <- xyFromCell(all.stack, my.vals$cell) #this is not working!!!!
        app.data$analysis.vals <- my.vals
      }

      #if a polygon, extract data from raster
      if(input$modelOutputs_draw_new_feature$properties$feature_type == 'polygon'
         | input$modelOutputs_draw_new_feature$properties$feature_type == 'rectangle'){
        final.poly <- Orcs::coords2Polygons(as.matrix(xy), ID='chris')
        all.stack <- stack(app.data$f_img, app.data$fm_img, app.data$yield, app.data$etr, app.data$model.outputs)
        my.roi <- all.stack %>% raster::crop(raster::extent(final.poly)) %>% raster::mask(final.poly)
        my.vals <- as.data.frame(my.roi, xy = T)
        app.data$analysis.vals <- my.vals

      }

    })

    #dynamic removal of tabs rendered below, incase already present.
    observeEvent(app.data$analysis.type,{
      if(app.data$analysis.type == 'polyline'){
        removeTab(inputId = 'mainPanel', target = 'ROI')
      }
      if(app.data$analysis.type == 'polygon' | app.data$analysis.type == 'rectangle'){
        removeTab(inputId = 'mainPanel', target = 'Transect')
      }
    })
    #append a tab with correct type of display depending on transcet or ROI analysis type
    observeEvent(app.data$analysis.type, {
      if(app.data$analysis.type == 'polyline'){

        appendTab('mainPanel',
                  tabPanel('Transect',
                           fluidPage(
                             fluidRow(
                               plotly::plotlyOutput('transectPlot', width = '100%', height='400px')
                             ),
                             fluidRow(
                               uiOutput('transectX')
                             ),
                             fluidRow(
                               uiOutput('transectY')
                             )
                           )))
      }

      if(app.data$analysis.type == 'polygon' | app.data$analysis.type == 'rectangle'){

        appendTab('mainPanel',
                  tabPanel('ROI',
                           fluidPage(
                             fluidRow(
                               column(6,
                                      plotly::plotlyOutput('roiPlot1', width = '100%', height = '400px')
                               ),
                               column(6,
                                      plotly::plotlyOutput('roiPlot2', width = '100%', height = '400px')
                               )
                             ),
                             fluidRow(
                               uiOutput('transectX2')
                             ),
                             fluidRow(
                               uiOutput('transectY2')
                             )
                           )))
      }
    })


    #render x axis options - transect
    output$transectX <- shiny::renderUI({
      nams <- c('pixel no.', 'Fv/Fm', 'Fo', 'Fm', names(app.data$model.outputs))
      radioButtons('transectX_return', 'Choose X axis parameter', choices = nams, selected = nams[1], inline = T)
    })
    #render y axis options - transect
    output$transectY <- shiny::renderUI({
      nams <- c('pixel no.', 'Fv/Fm', 'Fo', 'Fm', names(app.data$model.outputs))
      radioButtons('transectY_return', 'Choose Y axis parameter', choices = nams, selected = nams[2], inline = T)
    })

    #render transect plot
    output$transectPlot <- plotly::renderPlotly({
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

    #export transect data
    shiny::observeEvent(input$exportTransect,{
      shiny::showModal(
        shiny::modalDialog(title = 'Export Transect Data', size='m',
                           shiny::downloadButton("downloadTransect", "Save Transect Data")
        )
      )
    })

    #########
    output$downloadTransect <- shiny::downloadHandler(
      filename = function() {

        paste('transect_data', '.csv', sep='')
      },
      content = function(file) {

        write.csv(app.data$analysis.vals, file = file, row.names = F)

      }
    )

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
      if(app.data$analysis.type == 'polygon' | app.data$analysis.type == 'rectangle'){

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

    output$roiPlot2 <- plotly::renderPlotly({
      if(app.data$analysis.type == 'polygon' | app.data$analysis.type == 'rectangle'){

        #get input values - only x-axis parameter otherwise scale can be very off
        my.x <- input$transectX2_return
        #my.y <- input$transectY2_return

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

    #export ROI data
    shiny::observeEvent(input$exportROI,{
      shiny::showModal(
        shiny::modalDialog(title = 'Export ROI Data', size='m',
                           shiny::downloadButton("downloadROI", "Save ROI Data")
        )
      )
    })

    #########
    output$downloadROI <- shiny::downloadHandler(
      filename = function() {

        paste('roi_data', '.csv', sep='')
      },
      content = function(file) {

        write.csv(app.data$analysis.vals, file = file, row.names = F)

      }
    )

  }#end of server

  shiny::shinyApp(ui, server)


}


