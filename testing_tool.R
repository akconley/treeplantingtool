library(sf)
library(ggplot2)
library(dplyr)
library(terra)

database<-"H:\\Please_Do_Not_Delete_me\\TreePlantingTools\\TreePlantingTools.gdb"
layerin<-"NHDFlowlineVAA_Coa_Intersect"
layer_Eco<-"ny_EPA_ecoreginsIV_L4Dissolve_FULL"
layerin_forest<-"Forested_Clip"
layerin_wetland<-"GISCREATOR_FreshwaterWetlands"

polys<-st_read(dsn=database,layer=layerin)%>%
  sf::st_transform('+proj=longlat +datum=WGS84')

polys_forest<-st_read(dsn=database,layer=layerin_forest)%>%
  sf::st_transform('+proj=longlat +datum=WGS84')

# polys_wetland<-st_read(dsn=database,layer=layerin_wetland)%>%
#   sf::st_transform('+proj=longlat +datum=WGS84')
polys_full<-st_read(dsn=database,layer=layer_Eco)%>%
  sf::st_transform('+proj=longlat +datum=WGS84')

forest_tif<-"H:\\Please_Do_Not_Delete_me\\TreePlantingTools\\nlcd_forest.tif"
forests<-rast(forest_tif)
names(forests)<-"forest"

wetland_tif<-"H:\\Please_Do_Not_Delete_me\\TreePlantingTools\\wetland_0.tif"
wetland<-rast(wetland_tif)
names(wetland)<-"wetland"

elev_tif<-"W:\\GIS_Data\\DEM\\DEM_10m\\Masked_NED_Resampled_10m_DEM.tif"
elev<-rast(elev_tif)
names(elev)<-"elevation"

coastal_tif<-"H:\\Please_Do_Not_Delete_me\\TreePlantingTools\\coastal_0.tif"
coastal<-rast(coastal_tif)
names(coastal)<-"coastal"

bedrock_tif<-"H:\\Please_Do_Not_Delete_me\\TreePlantingTools\\bedrock_shallow_0.tif"
bedrock_ras<-rast(bedrock_tif)
names(bedrock_ras)<-"bedrock_depth"
####For test- add additional categories: Soil 
names(polys)
polys<-polys %>% mutate(OBJECTID=row.names(polys),
  Soil= case_when(OBJECTID <= 5 ~"Serpentine",
                  OBJECTID >5 ~"Non-Serpentine"))

6 0.739462903977551 41.2406419026948 -73.817138671875

13 0.914452804838759 41.2189528077412 -74.088191986084

eco_list<-unique(polys_full$US_L4NAME)
costal_list<-c(0,1)
wetland_list<-c(0,1)
elevation_list<-c("< 1500 ft","> 1500 ft",">3000 ft")
aspect_list<-c("North","South","East","West","Northeast","Northwest","Southeast","Southwest")
slope_list<-c(">15 degrees","<15 degrees")
forest_list<-c(0,1)
bedrock_depth<-c("<50 cm",">50 cm")


temp_df<-expand.grid(Coastal=costal_list,Wetland=wetland_list,Elevation=elevation_list,Slope=slope_list,Forest=forest_list,Bedrock=bedrock_depth)
13 0.471095860115519 41.2075889818102 -74.073600769043
test_df<-data.frame(lat1=41.2189528077412, long1=-74.088191986084)
test_df<-data.frame(x=-74.088191986084,y=41.2189528077412 )
test_pt<-st_as_sf(test_df,coords = c("long1","lat1"))%>%sf::st_set_crs(4326)

test_UTM<-data.frame(project(as.matrix(test_df[,c('x', 'y')]), 
        "+proj=longlat", "+proj=utm +zone=18 +units=m"))



test_pt<-terra::vect(test_pt)
test_poly<-terra::vect(polys)
test_forest<-terra::vect(polys_forest)
test_wetland<-terra::vect(polys_wetland)
test_spc<-svc()
test_spc<-terra::svc(test_poly,test_forest)
##Test extract the point information from a vector

terra::extract(test_poly,test_pt)
terra::extract(test_forest,test_pt)
terra::extract(test_spc[2],test_pt)$Forest
terra::extract(wetland,test_pt)
terra::extract(elev,test_pt)
terra::extract(forests,test_pt,raw=TRUE)

######Start Ap-----
library(shiny)
library(leaflet)
library(leaflet.extras)
library(DT)
#####UI-----
ui <- fluidPage(
  br(),
  fluidRow(
    column(4,
           hr(),
           selectInput("category", "Select a category:", c("Fruit"="Fruit", "Vegetable"="Vegetable") )
      
    ),
    column(4,
           hr(),
           uiOutput("items")),
    column(4,
           hr(),
           actionButton("trigger","Click after selecting"))
  ),
  
  fluidRow(
    leafletOutput(outputId = "map"),
    
  ),
  
  fluidRow(
    column(3,
           hr(),
           selectizeInput(inputId = "selected_locations",
                          label = "EcoRegion Selected",
                          choices = c(Choose="",polys_full$US_L4NAME),
                        
                          multiple = FALSE)
  ),
  column(3,hr("Is this location within 10 miles of the ocean or tidal river?"),
         selectInput(inputId = "selected_coastal",
                     label = "Coastal Status selected",
                     choices = c(Choose="","Coastal"=1,"Not Coastal"=0),
                     #selected = NULL,
                     multiple = FALSE)
    
  ),
  column(3,hr("Is this location forested?"),
         selectInput(inputId = "selected_forest",
                     label = "Forest Status Selected",
                     choices = c(Choose="","Forested"=1,"Not Forested"= 0),
                     #selected = NULL,
                     multiple = FALSE),
         ),
  column(3,hr("Is this location in a wetland?"),
         selectInput(inputId = "selected_wetland",
                     label = "Wetland Status Selected",
                     choices = c(Choose="",Wetland=1,"Upland"= 0),
                     #selected = "",
                     multiple = FALSE),
  ),
),

  fluidRow(
    textOutput("selected_var"),
    textOutput("Click_text"),
    textOutput("message_print"),
    DT::DTOutput("table")
  )
   
)

server <- function(input, output, session) {
  data<-c("Apple", "Banana", "Cherry")
  
  output$items <- renderUI({
    data <- switch(input$category,
                   "Fruit" = c("Apple", "Banana", "Cherry"),
                   "Vegetable" = c("Carrot", "Potato", "Spinach"))
    selectizeInput("items", "Select an item:", choices = unique(data)) 
  })%>%
    bindEvent(input$category,label="test")
  #####Map Setup-----
  map<-leaflet(data=polys) %>%
    addProviderTiles("OpenStreetMap") %>%
    addPolygons(data=polys_full,label =  ~US_L4NAME,layerId=polys_full$US_L4NAME,opacity = 0,fillOpacity = 0) %>%
    #addPolygons(data=polys,layerId=polys$OBJECTID,label =  ~US_L4NAME,opacity = 0,fillOpacity = 0)  
    leaflet.extras::addSearchOSM(options = searchOptions(collapsed = FALSE))
  output$map <- renderLeaflet(map)
  
  
  ######Set up reactive values------
  #for the Coastal, Ecoregion, and "Soil" types associated with the OBJECTID
  #These should automatically update when a different polygon is picked
  mod_df <- shiny::reactiveValues(x = polys_full)
  ####Set up reactive values for terra extract-----
  terra_data<-shiny::reactiveValues()
  
  ##Set up a series of actions that depend on the click of the map. 
  ##Change the selected input, the selected soils, the selected Coastal
  observe({
    updateSelectizeInput(session,
                         inputId = "selected_locations",
                         choices = c(Choose="",polys_full$US_L4NAME),
                         selected = input$map_shape_click$id)
    updateSelectizeInput(session,
                         inputId = "selected_coastal",
                         choices = c(Choose="","Coastal"=1,"Not Coastal"= 0),
                         selected =  terra_data$coastal)
    
    updateSelectizeInput(session,
                         inputId = "selected_forest",
                         choices = c(Choose="","Forested"=1,"Not Forested"= 0),
                         selected = terra_data$forest)
    
    updateSelectizeInput(session,
                         inputId = "selected_wetland",
                         choices = c(Choose="",Wetland=1,"Upland"= 0),
                         selected = terra_data$wetland)
    
    
  })
  observeEvent(input$map_shape_click, {
  
    # ##Filter the table----
    # mod_df$x <- polys %>%
    #   dplyr::filter(OBJECTID == input$map_shape_click$id)
    # 
    # mod_df$eco<-mod_df$x$US_L4NAME
    # mod_df$coastal<-mod_df$x$Coastal
    # mod_df$soil<-mod_df$x$Soil
  #   
   #Update the marker to show the location every map click
    
    leafletProxy('map') %>%
      removeMarker('my_marker') %>%
      addMarkers(layerId = 'my_marker',lng=input$map_shape_click$lng,lat = input$map_shape_click$lat )
    
    
    
   })
  
  #####Find the location anywhere not just in the shapes-----
  observe({
    click = input$map_click
    if(is.null(click))
      return()
    text<-paste("Latitude: ", click$lat, ", Longtitude: ", click$lng)
    text2<-paste("You've selected point ", text)
    
    
    ####Extract forest values at the clicked point----
    #Make a dataframe from the clicked locations Lat and Long
     temp_df<-data.frame(x=input$map_click$lng,y=input$map_click$lat)
    #Project the input point from the leaflet proj to the UTM proj of the raster data
     temp_UTM<-data.frame(project(as.matrix(temp_df[,c('x', 'y')]), 
                                  "+proj=longlat", "+proj=utm +zone=18 +units=m"))
     #extract value
     terra_data$forest<-as.character(terra::extract(forests,temp_UTM)$forest)
     terra_data$coastal<-terra::extract(coastal,temp_UTM)$coastal
     terra_data$wetland<-terra::extract(wetland,temp_UTM)$wetland
     terra_data$elev<-terra::extract(elev,temp_UTM)$elevation
     ###TEMPORARY - To see what values the rasters are giving, print out the values
     text3<-terra::extract(wetland,temp_UTM)$wetland  ####How to find wetland binary
     text4<-terra::extract(forests,temp_UTM)$forest
     text5<-terra::extract(elev,temp_UTM)$elevation
     #text6<-paste("wetland: ",text3,", forest: ",text4,", elevation ",round(text5,digits=0))
     text6<-paste("wetland: ",terra_data$wetland,", forest: ",terra_data$forest,", elevation ",round(terra_data$elev,digits=0), " costal:",terra_data$coastal)
     
    
    output$Click_text<-renderText({
      text6
      
      })
    
    
  })  
  
  output$selected_var <- renderText({
    as.character(input$map_shape_click)
  })
  
  ####Results Frame Observes the action button----
  observeEvent(input$trigger,{
    req(input$selected_locations,input$selected_coastal,input$selected_wetland,input$selected_forest)
    
    output$message_print<-renderText(
      isolate(selection_message())
      
    )
    
  })
  
  selection_message<-reactive({
    req(input$selected_locations,input$selected_coastal,input$selected_wetland,input$selected_forest)
    if (input$selected_coastal==1){
      c_text<-"less than"
    } else{
      c_text<-"more than"
    }
    if(input$selected_wetland == 1){
      w_text<-"within"
    }else{
      w_text<-"not within"
    }
    if(input$selected_forest==1){
      f_text<-"within"
    } else {
      f_text<-"not within"
    }
    selection_message<-paste("You appear to have selected a location that is ",c_text," 10 miles from the ocean, ",w_text," a wetland area, and ",f_text, " a forested area in the ",input$selected_locations," ecoregion. INSERT SUGGESTED PLANTING LIST FOR THIS GROUPING")
     
  })
  # output$table <- 
  #   DT::renderDT(polys) 
  
  
  # ###Make output table ----
  # output$table <- DT::renderDT({
  #   
  #   isolate(mod_df$x)
  # })
  # 
  # #proxy <- dataTableProxy('table')
  # proxy <- DT::dataTableProxy('table')
  # shiny::observe({
  #   
  #   DT::replaceData(proxy, mod_df$x)
  #   
  # })
  
}

shinyApp(ui, server)

