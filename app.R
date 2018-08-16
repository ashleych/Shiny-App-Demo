#loading library
library(shiny)
library(DT)
library(shinydashboard)
library(leaflet)
library(rgdal)

library(RColorBrewer)

#library(raster)
#library(ggmap)
#library(dplyr)

#loading shape file
mp<-readOGR(
  dsn="C:\\Users\\aquaya\\Desktop\\R shiny\\merge",
  layer="m1")


#Converting to dataframe
#mp<-as.data.frame(mp)

#Remove NAs from AreaType col
mp<-mp[!is.na(mp$AreaTyp),]


#loading data
nakuru<-read.csv("C:\\Users\\aquaya\\Desktop\\R shiny\\merge\\nak2.csv")
#slicing data
nakuru1<-nakuru[,c(4,5,6,7,8,9,10,11,12)]
#Areatype
type<-unique(nakuru$AreaType)

ui<-dashboardPage(
  dashboardHeader(title = "NAKURU DATA",titleWidth = 275),
  dashboardSidebar(
    width = 275,
    sidebarMenu(
      menuItem("DATA EXPLORER",tabName = "data",icon = icon("th")),
      menuItem("PLOT EXPLORER",tabName = "plot",icon =icon("bar-chart-o")),
      menuItem("INTERACTIVE MAPPING",tabName = "map",icon = icon("map"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "data",
        h3("INTERACTING WITH DATA."),
        
        fluidRow(
          
          box(title = "FILTERING",status = "primary",solidHeader = TRUE,
              #select file to upload
              #fileInput(inputId = "dataset",
              #label = "SELECT FILE:",
              #multiple = TRUE,
              #accept = c("text/csv","text/comma-separated-values,text/plain",
              # ".csv")
              # ),
              #Input: Select number of rows to display
              selectInput(inputId = "dis",
                          label = "Display",
                          choices = c(Head="head",
                                      All="all"),
                          selected = "head"),
              
              
              checkboxGroupInput(inputId = "popdensity", 
                                 label = " population:",
                                 choices = list(
                                   "< 15,000", 
                                   "15,001 - 30,000" , 
                                   "30,001 - 152,000"
                                 ), 
                                 selected = list(
                                   "< 15,000", 
                                   "15,001 - 30,000" , 
                                   "30,001 - 152,000"
                                 )),
              
              selectInput(inputId = "area", 
                          label = " AreaType:",
                          choices = c(
                            "All",
                            #"Planned",
                            #"Unplanned",
                            #"MixedArea"
                            unique(as.character(nakuru$AreaType))
                          ) 
              ),
              selectInput(inputId = "water", 
                          label = " WaterAccessType:",
                          choices = c(
                            "All",
                            "PipedWaterOnPlot",
                            "WaterSourceOnPlot"
                          ) 
              ),
              selectInput(inputId = "sanitation", 
                          label = " Sanitation Type:",
                          choices =c(
                            All="All",
                            FlushToilets="FlushToilets",
                            OtherImproved="OtherImproved",
                            Unimproved="Unimproved",
                            OpenDefecation="OpenDefecation"
                          ), 
                          selected = "All"),width = 4),
          
          
          box(title = "OUTPUT",status = "primary",solidHeader = TRUE,
              #tableoutput
              DT::dataTableOutput("table"),
              width = 8
              
          ))),
      
      tabItem(
        tabName = "plot",
        h3("PLOT INTERACTION."),
        fluidRow(
          box(
            title = "AXIS VALUES",status = "primary",solidHeader = TRUE,
            radioButtons(inputId = "status",
                         label = "SELECT X VALUE:",
                         choices = colnames(nakuru1)
                         
            ),
            radioButtons(inputId = "status2",
                         label = "SELECT Y VALUE:",
                         choices = colnames(nakuru1)
                         
                         
            ),width=4
          ),
          box(
            title = "PLOT OUTPUT",status = "primary",solidHeader = TRUE,
            plotOutput("stat"),width = 8
          )
        )
      ),
      tabItem(
        tabName = "map",
        h3("INTERACTIVE MAP DISPLAYING DATA."),
        fluidRow(
          box(
            title = "MAP",status = "primary",solidHeader = TRUE,
            leafletOutput("leaf",height = 600),width = 12
            
          ),
          
          absolutePanel( fixed = TRUE,
                         draggable = TRUE, top = 180, left = "auto", right = 10,
                         width = 330, height = "auto",style="opacity:0.8;background:FFFFEE;",
                         
                         h2("MAP EXPLORER"),
                         
                         checkboxGroupInput(inputId = "popdensity", 
                                            label = " population:",
                                            choices = list(
                                              "< 15,000", 
                                              "15,001 - 30,000" , 
                                              "30,001 - 152,000"
                                            ), 
                                            selected = list(
                                              "< 15,000", 
                                              "15,001 - 30,000" , 
                                              "30,001 - 152,000"
                                            )),
                         
                         selectInput(inputId = "area", 
                                     label = " AreaType:",
                                     choices = c(
                                       "All",
                                       #"Planned",
                                       #"Unplanned",
                                       #"MixedArea"
                                       unique(as.character(mp$AreaTyp))
                                     ) 
                         ),
                         
                         selectInput(inputId = "water", 
                                     label = " WaterAccessType:",
                                     choices = c(
                                       "All",
                                       "PipedWaterOnPlot",
                                       "WaterSourceOnPlot"
                                     ) 
                         ),
                         selectInput(inputId = "sanitation", 
                                     label = " Sanitation Type:",
                                     choices =c(
                                       All="All",
                                       FlushToilets="FlushToilets",
                                       OtherImproved="OtherImproved",
                                       Unimproved="Unimproved",
                                       OpenDefecation="OpenDefecation"
                                     ), 
                                     selected = "All"),
                         checkboxInput("legend", "Show legend", TRUE)
          )
        )
        
        
        
      )
    )
  )
)


server<-function(input,output,session){
  
  #data explorer display
  # data<-reactive({
  #switch (input$water,
  # "PipedWaterOnPlot"=nakuru$PipedWaterOnPlot,
  # "WaterSourceOnPlot"=nakuru$WaterSourceOnPlot
  #  )
  #})
  
  output$table<-renderDataTable({
    
    #filtering based on user selection
    #req(input$dataset)
    #df<-read.csv(input$dataset$datapath)
    df<-nakuru
    
    if(input$dis=="head")
    {
      return(head(df))
    }
    
    
    
    if(input$area!="All"){
      
      df<-df[df$AreaType==input$area,]
      
    }
    
    df
    
  })
  #plot explorer display
  output$stat<-renderPlot({
    # x<-nakuru[,input$status]
    #y<-nakuru[,input$status2]
    x<-nakuru[,c(input$status,input$status2)]
    plot(x,col = "#75AADB", pch = 19)
    
  })
  
  
  #INTERACTIVE MAPPING
  #colormapping
  pal<-colorFactor(rainbow(7),mp$AreaTyp)
  
  #set data based on user input
  fdata<-reactive({
    data<-mp
    if(input$area!="All"){
      #data<-subset(data,AreaTyp %in% input$area)
      data<-data[data$AreaType==input$area,]
      # mp=mp[mp$AreaTyp%in%input$area,]
    }
    return(data)
  })
  
  #Map output
  #mp<-readOGR(
  # dsn="C:\\Users\\aquaya\\Desktop\\QGIS\\GIS for Brian\\New LIA map",
  #layer="Nakuru LIA Map AQUAYA")
  #mp<-readOGR(
  # dsn="C:\\Users\\aquaya\\Desktop\\R shiny\\merge",
  #layer="m1")
  
  output$leaf<-renderLeaflet({
    #nakuru$AreaType <- factor(sample.int(5L, nrow(nakuru), TRUE))
    
    #factpal <- colorFactor(topo.colors(5),nakuru$AreaType)
    leaflet(fdata()) %>%
      
      #Initializing the map
      setView(lng=36.092245, lat=-00.292115,zoom=15)%>%
      
      #Base map
      #Add default OpenStreetMap map tiles
      addTiles(group = "default")%>%
      #addProviderTiles("Esri.NatGeoWorldMap",group = "default")%>%  
      #addProviderTiles("CartoDB.Positron",group = "custom")%>%
      
      #Overlay map
      addPolygons(
        data = fdata(),
        #fillColor = ~pal(AreaTyp),
        fillColor = "blue",
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 1.0,
        #color = ~factpal(AreaTyp),
        group = "basepoly",
        highlightOptions = highlightOptions(
          weight = 2,
          color = "red",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),label =~LIA
        #labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE,
        #opacity = 1.5 , textsize='10px')
      ) #%>%
    #addLegend(title = "AreaType", position = "topleft",
    #         pal = pal, values = ~AreaTyp, opacity = 1)
    
    #add control widget
    #addLayersControl(overlayGroups = c("basepoly"),
    #baseGroups = c("default","custom"),
    #options = layersControlOptions(collapsed = FALSE)
    #)
  })
  
  
  observe({
    pal<-colorFactor(rainbow(7),mp$AreaTyp)
    
    leafletProxy("leaf",data=fdata()) %>%
      
      clearMarkers() %>%
      clearControls() %>%
      clearShapes()%>%
      addPolygons(
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 1.0,
        data=fdata(),
        fillColor = ~pal(AreaTyp)
        #label =~LIA
        #labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE,
        #opacity = 1.5 , textsize='10px') 
      )
    
    
    
  })
  dat<-mp
  observeEvent(input$area,{
    if(input$area!="All"){
      
      dat<-dat[dat$AreaType==input$area,]
    }
    else{
      dat
    }
    leafletProxy("leaf",data =dat)%>%
      clearShapes()%>%
      addPolygons(
        data = dat,
        stroke = FALSE,
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 1.0,
        fillColor = ~pal(AreaTyp),
        label =~LIA
      )
    
    
  })
}

shinyApp(ui,server)