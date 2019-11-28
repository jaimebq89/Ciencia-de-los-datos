# Integrantes 
# Andres Felipe Gonzalez Alzate c.c: 1.053.824.691
# Jose Fernando Montoya Cardona c.c: 1.144.176.600
# Mario Ricardo CastAño Duque   c.c: 1.053.813.451
# Jaime Alberto Bedoya Quintero c.c: 1.036.622.345

# Ultima modificacion:  21:15 sabado 19 de octubre de 2019
# Tiene los filtros funcionales Comuna, Barrio, Año, Mes y Clase accidente


# Load packages ---------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)


# Load data -------------------------------------------------------------------
accidentalidad=read.csv("AccidentalidadDepurada.csv", sep = ",", header = T,encoding="UTF-8")



# 1. ui ---------------------------------------------------------------
ui <- dashboardPage(
    
    dashboardHeader(title = "Accidentalidad en Medellín", titleWidth =280),
    
    dashboardSidebar(width =280,
                     
                     selectInput(inputId = "comuna_seleccionada",
                                 label = "Comuna a analizar:",
                                 choices = sort(c("Seleccione una comuna",
                                                  unique(as.character(accidentalidad$COMUNA)))),
                                 selected = "Seleccione una comuna"),
                     
                     selectInput(inputId = "barrio_seleccionado",
                                 label = "Barrio a analizar:",
                                 choices = sort(c("Seleccione un barrio",
                                                  unique(as.character(accidentalidad$BARRIO)))),
                                 selected = "Seleccione un barrio"),
                     
                     selectInput(inputId = "anio_seleccionado",
                                 label = "Año a analizar:",
                                 choices = sort(c("Todos los Años",
                                                  unique(as.character(accidentalidad$PERIODO)))),
                                 selected = "Todos los Años"),
                     
                     selectInput(inputId = "mes_seleccionado",
                                 label = "Mes a analizar:",
                                 choices = c("Todos los meses",
                                             unique(as.character(accidentalidad$MES))),
                                 selected = "Todos los meses"),
                     
                     selectInput(inputId = "clase_seleccionada",
                                 label = "Clase de accidente analizar:",
                                 choices = sort(c("Seleccione la clase",
                                                  unique(as.character(accidentalidad$CLASE)))),
                                 selected = "Seleccione la clase"),
                     hr(),
                     helpText("Nota: Se recomienda si desea cambiar de comuna poner el menu de barrio en 
                              Seleccione un barrio, para así poder cambiar de comuna sin presentar error.")
    ),
    
    dashboardBody(
        
        # Primera columna  de la primera fila. El ancho va entre valores del 1 al 12 correspondientes
        # al 0% o el 100% de ocupaciÃ³n del ancho del dashboard. En este caso es 6 dado que se requiere dividir el ancho total
        # en dos columnas
        fluidRow(
            column(width = 6,offset = 0,
                   # A static valueBox
                   valueBoxOutput("vbox1", width = 12)
                   # Segunda fila de la primera columna
                   ,fluidRow(
                       column(width = 12,offset = 0,
                              box(width = 12, status = "info",
                                  title = NULL,
                                  leafletOutput("Mapa")))
                       
                   )
            )
            ,
            
            # Segunda columna de la primera fila del dashboard
            column(width = 6, offset = 0,
                   valueBox(value = "Info",subtitle = "El gráfico muestra los accidentes por clase", width = 12, color="green", icon = icon("calendar"))
                   ,
                   # Segunda fila de la segunda columna
                   fluidRow(
                       column(width = 12,offset = 0,
                              box(width = 12, status = "info",
                                  title = NULL,
                                  plotOutput(outputId = "cantidad")))
                       
                       
                       
                   )
                   
            )
            
        )
        
        
    )
    
    
)



# 2. Server ---------------------------------------------------------------
server <- function(input, output, session){
    
    # Filtrar 
    observe({
        
        data <- accidentalidad
        
        # Si  la comuna fue seleccionada actualizara los demas desplegables.
        if (input$comuna_seleccionada != "Seleccione una comuna") {
            
            data <- data[data$COMUNA == input$comuna_seleccionada,]
            
            # Indicar la informacion de la comuna seleccionada
            updateSelectInput(session,
                              inputId = "barrio_seleccionado",
                              label = paste("Barrio de la comuna ",input$comuna_seleccionada," a analizar"),
                              choices = sort(c("Seleccione un barrio", unique(as.character(data$BARRIO)))),
                              selected = "Seleccione un barrio")
            
            
            # Si la comuna aun no se ha seleccionado 
        } else if (input$comuna_seleccionada == "Seleccione una comuna") {
            
            if (input$barrio_seleccionado == "Seleccione un barrio") {
                
                updateSelectInput(session,
                                  inputId = "barrio_seleccionado",
                                  label = "Barrio a analizar:",
                                  choices = sort(c("Seleccione un barrio", unique(as.character(accidentalidad$BARRIO)))),
                                  selected = "Seleccione un barrio")
                
          
                
            } else {
                
                data <- data[data$BARRIO == input$barrio_seleccionado,]
                
                
            }
            
        } 
        
    })
    

    
    
    
    
    
    # Dibujar el mapa con los accidentes
    output$Mapa <- renderLeaflet({
        
        data <- accidentalidad
        
        
        # Grafico de los accidentes teniendo en cuenta el barrio seleccionado    
        if (input$comuna_seleccionada == "Seleccione una comuna"
            & input$barrio_seleccionado == "Seleccione un barrio"
            & input$anio_seleccionado == "Todos los Años"
            & input$mes_seleccionado == "Todos los meses"
            & input$clase_seleccionada == "Seleccione la clase"
            ){
           
          # Encontrar los valores extremos para graficar
            
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions = markerClusterOptions())
            map 
        }
        
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            data<-subset(data,subset = (BARRIO == input$barrio_seleccionado))
            
            # Encontrar los valores extremos para graficar
            
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions = markerClusterOptions())
            map 
        }
        # Esta presentando problemas para graficar lo referente a las comunas
        
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            data<-subset(data,subset = ( COMUNA == input$comuna_seleccionada))
            
            # Encontrar los valores extremos para graficar
            
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map,
                             lng1=lng1,
                             lat1=lat1,
                             lng2=lng2,
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions = markerClusterOptions())
            map
        }
        
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            data<-subset(accidentalidad,subset = (COMUNA == input$comuna_seleccionada
                                                  & BARRIO == input$barrio_seleccionado))
            
            # Encontrar los valores extremos para graficar
            
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions = markerClusterOptions())
            map 
        }  
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            data<-subset(accidentalidad,subset = (COMUNA == input$comuna_seleccionada
                                                  & PERIODO == input$anio_seleccionado))
            
            # Encontrar los valores extremos para graficar
            
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions = markerClusterOptions())
            map 
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            data<-subset(accidentalidad,subset = (COMUNA == input$comuna_seleccionada
                                                  & MES == input$mes_seleccionado))
            
            # Encontrar los valores extremos para graficar
            
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions = markerClusterOptions())
            map 
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            data<-subset(accidentalidad,subset = (COMUNA == input$comuna_seleccionada
                                                  & CLASE == input$clase_seleccionada))
            
            # Encontrar los valores extremos para graficar
            
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions = markerClusterOptions())
            map 
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            data<-subset(accidentalidad,subset = (COMUNA == input$comuna_seleccionada
                                                  & BARRIO == input$barrio_seleccionado
                                                  & PERIODO == input$anio_seleccionado))
            
            # Encontrar los valores extremos para graficar
            
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions = markerClusterOptions())
            map 
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            data<-subset(accidentalidad,subset = (COMUNA == input$comuna_seleccionada
                                                  & BARRIO == input$barrio_seleccionado
                                                  & MES == input$mes_seleccionado))
            
            # Encontrar los valores extremos para graficar
            
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions = markerClusterOptions())
            map 
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            data<-subset(accidentalidad,subset = (COMUNA == input$comuna_seleccionada
                                                  & BARRIO == input$barrio_seleccionado
                                                  & CLASE == input$clase_seleccionada))
            
            # Encontrar los valores extremos para graficar
            
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions = markerClusterOptions())
            map 
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            data<-subset(accidentalidad,subset = (COMUNA == input$comuna_seleccionada
                                                  & PERIODO == input$anio_seleccionado
                                                  & MES == input$mes_seleccionado))
            
            # Encontrar los valores extremos para graficar
            
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions = markerClusterOptions())
            map 
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            data<-subset(accidentalidad,subset = (COMUNA == input$comuna_seleccionada
                                                  & PERIODO == input$anio_seleccionado
                                                  & CLASE == input$clase_seleccionada))
            
            # Encontrar los valores extremos para graficar
            
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions = markerClusterOptions())
            map 
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            data<-subset(accidentalidad,subset = (COMUNA == input$comuna_seleccionada
                                                  & MES == input$mes_seleccionado
                                                  & CLASE == input$clase_seleccionada))
            
            # Encontrar los valores extremos para graficar
            
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions = markerClusterOptions())
            map 
        }
        
          
            
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            data<-subset(data,subset = (PERIODO == input$anio_seleccionado))
            
            # Encontrar los valores extremos para graficar
            
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions = markerClusterOptions())
            map 
        } 
        
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            data<-subset(data,subset = (MES == input$mes_seleccionado))
            
            # Encontrar los valores extremos para graficar
            
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions = markerClusterOptions())
            map 
        } 
        
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            data<-subset(data,subset = (CLASE == input$ clase_seleccionada))
            
            # Encontrar los valores extremos para graficar
            
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions = markerClusterOptions())
            map 
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            data<-subset(data,subset = (BARRIO == input$barrio_seleccionado
                                        & PERIODO == input$anio_seleccionado))
            
            # Encontrar los valores extremos para graficar
            
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions = markerClusterOptions())
            map 
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            data<-subset(data,subset = (BARRIO == input$barrio_seleccionado
                                        & MES == input$mes_seleccionado))
            
            # Encontrar los valores extremos para graficar
            
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions = markerClusterOptions())
            map 
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            data<-subset(data,subset = (BARRIO == input$barrio_seleccionado
                                        & CLASE == input$clase_seleccionada))
            
            # Encontrar los valores extremos para graficar
            
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions = markerClusterOptions())
            map 
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            data<-subset(data,subset = (PERIODO == input$anio_seleccionado
                                        & MES == input$mes_seleccionado))
            
            # Encontrar los valores extremos para graficar
            
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions = markerClusterOptions())
            map 
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            data<-subset(data,subset = (PERIODO == input$anio_seleccionado
                                        & CLASE == input$clase_seleccionada))
            
            # Encontrar los valores extremos para graficar
            
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions = markerClusterOptions())
            map 
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            data<-subset(data,subset = (MES == input$mes_seleccionado
                                        & CLASE == input$clase_seleccionada))
            
            # Encontrar los valores extremos para graficar
            
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions = markerClusterOptions())
            map 
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            data<-subset(data,subset = (BARRIO == input$barrio_seleccionado
                                        & MES == input$mes_seleccionado
                                        & CLASE == input$clase_seleccionada))
            
            # Encontrar los valores extremos para graficar
            
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions = markerClusterOptions())
            map 
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            data<-subset(data,subset = (BARRIO == input$barrio_seleccionado
                                        & PERIODO == input$anio_seleccionado
                                        & MES == input$mes_seleccionado))
            
            # Encontrar los valores extremos para graficar
            
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions = markerClusterOptions())
            map 
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            data<-subset(data,subset = (BARRIO == input$barrio_seleccionado
                                        & PERIODO == input$anio_seleccionado
                                        & CLASE == input$clase_seleccionada))
            
            # Encontrar los valores extremos para graficar
            
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions = markerClusterOptions())
            map 
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            data<-subset(data,subset = (PERIODO == input$anio_seleccionado
                                        & MES == input$mes_seleccionado
                                        & CLASE == input$clase_seleccionada))
            
            # Encontrar los valores extremos para graficar
            
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions = markerClusterOptions())
            map 
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            data<-subset(data,subset = ( PERIODO == input$anio_seleccionado
                                        & MES == input$mes_seleccionado
                                        & CLASE == input$clase_seleccionada
                                        & BARRIO == input$barrio_seleccionado))
            
            # Encontrar los valores extremos para graficar
            
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions = markerClusterOptions())
            map 
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            data<-subset(data,subset = ( PERIODO == input$anio_seleccionado
                                         & MES == input$mes_seleccionado
                                         & CLASE == input$clase_seleccionada
                                         & BARRIO == input$barrio_seleccionado
                                         & COMUNA == input$comuna_seleccionada))

            # Encontrar los valores extremos para graficar

            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)

            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map,
                             lng1=lng1,
                             lat1=lat1,
                             lng2=lng2,
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions = markerClusterOptions())
            map
        }
        
        
        
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            data<-subset(data,subset = ( PERIODO == input$anio_seleccionado
                                         & MES == input$mes_seleccionado
                                         & BARRIO == input$barrio_seleccionado
                                         & COMUNA == input$comuna_seleccionada))
            
            # Encontrar los valores extremos para graficar
            
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions = markerClusterOptions())
            map 
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            data<-subset(data,subset = ( PERIODO == input$anio_seleccionado
                                         & CLASE == input$clase_seleccionada
                                         & BARRIO == input$barrio_seleccionado
                                         & COMUNA == input$comuna_seleccionada))
            
            # Encontrar los valores extremos para graficar
            
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions = markerClusterOptions())
            map 
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            data<-subset(data,subset = ( MES == input$mes_seleccionado
                                         & CLASE == input$clase_seleccionada
                                         & BARRIO == input$barrio_seleccionado
                                         & COMUNA == input$comuna_seleccionada))
            
            # Encontrar los valores extremos para graficar
            
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions = markerClusterOptions())
            map 
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            data<-subset(data,subset = ( PERIODO == input$anio_seleccionado
                                         & MES == input$mes_seleccionado
                                         & CLASE == input$clase_seleccionada
                                         & COMUNA == input$comuna_seleccionada))
            
            # Encontrar los valores extremos para graficar
            
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions = markerClusterOptions())
            map 
        }
        
        
        
    })
    
    
    #Segunda salida 
    output$cantidad <- renderPlot({
        
        if (input$comuna_seleccionada == "Seleccione una comuna"
            & input$barrio_seleccionado == "Seleccione un barrio"
            & input$anio_seleccionado == "Todos los Años"
            & input$mes_seleccionado == "Todos los meses"
            & input$clase_seleccionada == "Seleccione la clase"){

            
            choques_barrio<-subset(accidentalidad)
            
            # choques_barrio$f_accidente<-substr(choques_barrio$Date,1,10)
            choques_barrio$f_accidente <- as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            orden_acc<-sort(accidentes_dia$CLASE,decreasing = TRUE,index.return=TRUE)
            
            plot(accidentes_dia$f_accidente,accidentes_dia$CLASE, type="h",
                 xlab="Fecha accidente",ylab="Cantidad accidentes",yaxt="n",
                 main =paste("Accidentes en todo Medellín"))
            axis(1,at=c(0,1),las=1)
            axis(2,at=c(1:1000),labels=c(1:1000),las=1)
            
    
        }
        
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"){
            
            
            choques_barrio <- subset(accidentalidad, subset = (BARRIO == input$barrio_seleccionado))
            
            
            choques_barrio$f_accidente <- as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            orden_acc<-sort(accidentes_dia$CLASE,decreasing = TRUE,index.return=TRUE)
            
            plot(accidentes_dia$f_accidente,accidentes_dia$CLASE, type="h",
                 xlab="Fecha accidente",ylab="Cantidad accidentes",yaxt="n",
                 main =paste("Accidentes en Medellín en el barrio ",input$barrio_seleccionado))
            axis(1,at=c(0,1),las=1)
            axis(2,at=c(1:1000),labels=c(1:1000),las=1)
            
            
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"){


            choques_barrio <- subset(accidentalidad, subset = (COMUNA == input$comuna_seleccionada))

           
            choques_barrio$f_accidente <- as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio,
                                      FUN=length)
            orden_acc<-sort(accidentes_dia$CLASE,decreasing = TRUE,index.return=TRUE)

            plot(accidentes_dia$f_accidente,accidentes_dia$CLASE, type="h",
                 xlab="Fecha accidente",ylab="Cantidad accidentes",yaxt="n",
                 main =paste("Accidentes en Medellín en la comuna ",input$comuna_seleccionada))
            axis(1,at=c(0,1),las=1)
            axis(2,at=c(1:1000),labels=c(1:1000),las=1)


        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"){
            
            
            choques_barrio <- subset(accidentalidad, subset = (PERIODO == input$anio_seleccionado))
            
            # choques_barrio$f_accidente<-substr(choques_barrio$Date,1,10)
            choques_barrio$f_accidente <- as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            orden_acc<-sort(accidentes_dia$CLASE,decreasing = TRUE,index.return=TRUE)
            
            plot(accidentes_dia$f_accidente,accidentes_dia$CLASE, type="h",
                 xlab="Fecha accidente",ylab="Cantidad accidentes",yaxt="n",
                 main =paste("Accidentes en todo Medellín en el año ",input$anio_seleccionado))
            axis(1,at=c(0,1),las=1)
            axis(2,at=c(1:1000),labels=c(1:1000),las=1)
            
            
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (MES == input$mes_seleccionado))
            
            # choques_barrio$f_accidente<-substr(choques_barrio$Date,1,10)
            
            choques_barrio$f_accidente <- as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            orden_acc<-sort(accidentes_dia$CLASE,decreasing = TRUE,index.return=TRUE)
            
            plot(accidentes_dia$f_accidente,accidentes_dia$CLASE, type="h",
                 xlab="Fecha accidente",ylab="Cantidad accidentes",yaxt="n",
                 main =paste("Accidentes en todo Medellín en el mes", input$mes_seleccionado))
            axis(1,at=c(0,1),las=1)
            axis(2,at=c(1:1000),labels=c(1:1000),las=1)
            
            
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (CLASE == input$clase_seleccionada))
            
            
            
            choques_barrio$f_accidente <- as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            orden_acc<-sort(accidentes_dia$CLASE,decreasing = TRUE,index.return=TRUE)
            
            plot(accidentes_dia$f_accidente,accidentes_dia$CLASE, type="h",
                 xlab="Fecha accidente",ylab="Cantidad accidentes",yaxt="n",
                 main =paste("Accidentes en todo Medellín de acuerdo a la clase:", input$clase_seleccionada))
            axis(1,at=c(0,1),las=1)
            axis(2,at=c(1:1000),labels=c(1:1000),las=1)
            
            
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (COMUNA == input$comuna_seleccionada
                                                               & BARRIO == input$barrio_seleccionado))
            
           
            
            choques_barrio$f_accidente <- as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            orden_acc<-sort(accidentes_dia$CLASE,decreasing = TRUE,index.return=TRUE)
            
            plot(accidentes_dia$f_accidente,accidentes_dia$CLASE, type="h",
                 xlab="Fecha accidente",ylab="Cantidad accidentes",yaxt="n",
                 main =paste("Accidentes en la comuna",input$comuna_seleccionada, "y el barrio",
                             input$barrio_seleccionado))
            axis(1,at=c(0,1),las=1)
            axis(2,at=c(1:1000),labels=c(1:1000),las=1)
            
            
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (COMUNA == input$comuna_seleccionada
                                                               & PERIODO == input$anio_seleccionado))
            
            # choques_barrio$f_accidente<-substr(choques_barrio$Date,1,10)
            
            choques_barrio$f_accidente <- as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            orden_acc<-sort(accidentes_dia$CLASE,decreasing = TRUE,index.return=TRUE)
            
            plot(accidentes_dia$f_accidente,accidentes_dia$CLASE, type="h",
                 xlab="Fecha accidente",ylab="Cantidad accidentes",yaxt="n",
                 main =paste("Accidentes en la comuna",input$comuna_seleccionada, "y el año",
                             input$anio_seleccionado))
            axis(1,at=c(0,1),las=1)
            axis(2,at=c(1:1000),labels=c(1:1000),las=1)
            
            
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (COMUNA == input$comuna_seleccionada
                                                               & MES == input$input$mes_seleccionado))
            
            # choques_barrio$f_accidente<-substr(choques_barrio$Date,1,10)
            
            choques_barrio$f_accidente <- as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            orden_acc<-sort(accidentes_dia$CLASE,decreasing = TRUE,index.return=TRUE)
            
            plot(accidentes_dia$f_accidente,accidentes_dia$CLASE, type="h",
                 xlab="Fecha accidente",ylab="Cantidad accidentes",yaxt="n",
                 main =paste("Accidentes en la comuna",input$comuna_seleccionada, "y el mes",
                             input$mes_seleccionado))
            axis(1,at=c(0,1),las=1)
            axis(2,at=c(1:1000),labels=c(1:1000),las=1)
            
            
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            
            choques_barrio <- subset(accidentalidad, subset = (COMUNA == input$comuna_seleccionada
                                                               & CLASE == input$clase_seleccionada))
            
            choques_barrio$f_accidente <- as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            orden_acc<-sort(accidentes_dia$CLASE,decreasing = TRUE,index.return=TRUE)
            
            plot(accidentes_dia$f_accidente,accidentes_dia$CLASE, type="h",
                 xlab="Fecha accidente",ylab="Cantidad accidentes",yaxt="n",
                 main =paste("Accidentes en la comuna",input$comuna_seleccionada, "y la clase:",
                             input$clase_seleccionada))
            axis(1,at=c(0,1),las=1)
            axis(2,at=c(1:1000),labels=c(1:1000),las=1)
            
            
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (COMUNA == input$comuna_seleccionada
                                                               & BARRIO == input$barrio_seleccionado
                                                               & PERIODO == input$anio_seleccionado))
            
            choques_barrio$f_accidente <- as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            orden_acc<-sort(accidentes_dia$CLASE,decreasing = TRUE,index.return=TRUE)
            
            plot(accidentes_dia$f_accidente,accidentes_dia$CLASE, type="h",
                 xlab="Fecha accidente",ylab="Cantidad accidentes",yaxt="n",
                 main =paste("Accidentes en la comuna",input$comuna_seleccionada, ", el barrio",
                             input$barrio_seleccionado, ", en el año ",input$anio_seleccionado))
            axis(1,at=c(0,1),las=1)
            axis(2,at=c(1:1000),labels=c(1:1000),las=1)
            
            
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (COMUNA == input$comuna_seleccionada
                                                               & BARRIO == input$barrio_seleccionado
                                                               & MES == input$mes_seleccionado))
            
            choques_barrio$f_accidente <- as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            orden_acc<-sort(accidentes_dia$CLASE,decreasing = TRUE,index.return=TRUE)
            
            plot(accidentes_dia$f_accidente,accidentes_dia$CLASE, type="h",
                 xlab="Fecha accidente",ylab="Cantidad accidentes",yaxt="n",
                 main =paste("Accidentes en la comuna",input$comuna_seleccionada, ", el barrio",
                             input$barrio_seleccionado, ", en el mes ",input$mes_seleccionado))
            axis(1,at=c(0,1),las=1)
            axis(2,at=c(1:1000),labels=c(1:1000),las=1)
            
            
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (COMUNA == input$comuna_seleccionada
                                                               & BARRIO == input$barrio_seleccionado
                                                               & CLASE == input$clase_seleccionada))
            
            choques_barrio$f_accidente <- as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            orden_acc<-sort(accidentes_dia$CLASE,decreasing = TRUE,index.return=TRUE)
            
            plot(accidentes_dia$f_accidente,accidentes_dia$CLASE, type="h",
                 xlab="Fecha accidente",ylab="Cantidad accidentes",yaxt="n",
                 main =paste("Accidentes en la comuna",input$comuna_seleccionada, ", el barrio",
                             input$barrio_seleccionado, ", del tipo de clase: ",input$clase_seleccionada))
            axis(1,at=c(0,1),las=1)
            axis(2,at=c(1:1000),labels=c(1:1000),las=1)
            
            
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (COMUNA == input$comuna_seleccionada
                                                               & PERIODO == input$anio_seleccionado
                                                               & MES == input$mes_seleccionado ))
            
            choques_barrio$f_accidente <- as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            orden_acc<-sort(accidentes_dia$CLASE,decreasing = TRUE,index.return=TRUE)
            
            plot(accidentes_dia$f_accidente,accidentes_dia$CLASE, type="h",
                 xlab="Fecha accidente",ylab="Cantidad accidentes",yaxt="n",
                 main =paste("Accidentes en la comuna",input$comuna_seleccionada, ", en el año",
                             input$anio_seleccionado, ", en el mes de ",input$mes_seleccionado))
            axis(1,at=c(0,1),las=1)
            axis(2,at=c(1:1000),labels=c(1:1000),las=1)
            
        }  
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (COMUNA == input$comuna_seleccionada
                                                               & PERIODO == input$anio_seleccionado
                                                               & CLASE == input$clase_seleccionada ))
            
            choques_barrio$f_accidente <- as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            orden_acc<-sort(accidentes_dia$CLASE,decreasing = TRUE,index.return=TRUE)
            
            plot(accidentes_dia$f_accidente,accidentes_dia$CLASE, type="h",
                 xlab="Fecha accidente",ylab="Cantidad accidentes",yaxt="n",
                 main =paste("Comuna",input$comuna_seleccionada,"Año",
                             input$anio_seleccionado, "Tipo de clase: ",input$clase_seleccionada))
            axis(1,at=c(0,1),las=1)
            axis(2,at=c(1:1000),labels=c(1:1000),las=1)
            
        } 
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (COMUNA == input$comuna_seleccionada
                                                               & MES == input$mes_seleccionado
                                                               & CLASE == input$clase_seleccionada ))
            
            choques_barrio$f_accidente <- as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            orden_acc<-sort(accidentes_dia$CLASE,decreasing = TRUE,index.return=TRUE)
            
            plot(accidentes_dia$f_accidente,accidentes_dia$CLASE, type="h",
                 xlab="Fecha accidente",ylab="Cantidad accidentes",yaxt="n",
                 main =paste("Accidentes de acuerdo a lo seleccionado:",
                             "Comuna:",input$comuna_seleccionada,
                             "Mes:",input$mes_seleccionado,
                             "Clase:",input$clase_seleccionada))
            axis(1,at=c(0,1),las=1)
            axis(2,at=c(1:1000),labels=c(1:1000),las=1)
            
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (BARRIO == input$barrio_seleccionado
                                                               & PERIODO == input$anio_seleccionado))
            
            choques_barrio$f_accidente <- as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            orden_acc<-sort(accidentes_dia$CLASE,decreasing = TRUE,index.return=TRUE)
            
            plot(accidentes_dia$f_accidente,accidentes_dia$CLASE, type="h",
                 xlab="Fecha accidente",ylab="Cantidad accidentes",yaxt="n",
                 main =paste("Accidentes de acuerdo a lo seleccionado:",
                             "Barrio:",input$barrio_seleccionado,
                             "Año:",input$anio_seleccionado))
            axis(1,at=c(0,1),las=1)
            axis(2,at=c(1:1000),labels=c(1:1000),las=1)
            
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (BARRIO == input$barrio_seleccionado
                                                               & MES == input$mes_seleccionado))
            
            choques_barrio$f_accidente <- as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            orden_acc<-sort(accidentes_dia$CLASE,decreasing = TRUE,index.return=TRUE)
            
            plot(accidentes_dia$f_accidente,accidentes_dia$CLASE, type="h",
                 xlab="Fecha accidente",ylab="Cantidad accidentes",yaxt="n",
                 main =paste("Accidentes de acuerdo a lo seleccionado:",
                             "Barrio:",input$barrio_seleccionado,
                             "Mes:",input$mes_seleccionado))
            axis(1,at=c(0,1),las=1)
            axis(2,at=c(1:1000),labels=c(1:1000),las=1)
            
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (BARRIO == input$barrio_seleccionado
                                                               & CLASE == input$clase_seleccionada))
            
            choques_barrio$f_accidente <- as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            orden_acc<-sort(accidentes_dia$CLASE,decreasing = TRUE,index.return=TRUE)
            
            plot(accidentes_dia$f_accidente,accidentes_dia$CLASE, type="h",
                 xlab="Fecha accidente",ylab="Cantidad accidentes",yaxt="n",
                 main =paste("Accidentes de acuerdo a lo seleccionado:",
                             "Barrio:",input$barrio_seleccionado,
                             "Tipo de clase:",input$clase_seleccionada))
            axis(1,at=c(0,1),las=1)
            axis(2,at=c(1:1000),labels=c(1:1000),las=1)
            
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (PERIODO == input$anio_seleccionado
                                                               & MES == input$mes_seleccionado))
            
            choques_barrio$f_accidente <- as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            orden_acc<-sort(accidentes_dia$CLASE,decreasing = TRUE,index.return=TRUE)
            
            plot(accidentes_dia$f_accidente,accidentes_dia$CLASE, type="h",
                 xlab="Fecha accidente",ylab="Cantidad accidentes",yaxt="n",
                 main =paste("Accidentes de acuerdo a lo seleccionado:",
                             "Año:",input$anio_seleccionado,
                             "Mes:",input$mes_seleccionado))
            axis(1,at=c(0,1),las=1)
            axis(2,at=c(1:1000),labels=c(1:1000),las=1)
            
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (PERIODO == input$anio_seleccionado
                                                               & CLASE == input$clase_seleccionada))
            
            choques_barrio$f_accidente <- as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            orden_acc<-sort(accidentes_dia$CLASE,decreasing = TRUE,index.return=TRUE)
            
            plot(accidentes_dia$f_accidente,accidentes_dia$CLASE, type="h",
                 xlab="Fecha accidente",ylab="Cantidad accidentes",yaxt="n",
                 main =paste("Accidentes de acuerdo a lo seleccionado:",
                             "Año:",input$anio_seleccionado,
                             "Tipo de clase:",input$clase_seleccionada))
            axis(1,at=c(0,1),las=1)
            axis(2,at=c(1:1000),labels=c(1:1000),las=1)
            
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (MES == input$mes_seleccionado
                                                               & CLASE == input$clase_seleccionada))
            
            choques_barrio$f_accidente <- as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            orden_acc<-sort(accidentes_dia$CLASE,decreasing = TRUE,index.return=TRUE)
            
            plot(accidentes_dia$f_accidente,accidentes_dia$CLASE, type="h",
                 xlab="Fecha accidente",ylab="Cantidad accidentes",yaxt="n",
                 main =paste("Accidentes de acuerdo a lo seleccionado:",
                             "Mes:",input$mes_seleccionado,
                             "Tipo de clase:",input$clase_seleccionada))
            axis(1,at=c(0,1),las=1)
            axis(2,at=c(1:1000),labels=c(1:1000),las=1)
            
        }
        
       
        
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (BARRIO == input$barrio_seleccionado
                                                               & MES == input$mes_seleccionado
                                                               & CLASE == input$clase_seleccionada ))
            
            choques_barrio$f_accidente <- as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            orden_acc<-sort(accidentes_dia$CLASE,decreasing = TRUE,index.return=TRUE)
            
            plot(accidentes_dia$f_accidente,accidentes_dia$CLASE, type="h",
                 xlab="Fecha accidente",ylab="Cantidad accidentes",yaxt="n",
                 main =paste("Accidentes de acuerdo a lo seleccionado"))
            axis(1,at=c(0,1),las=1)
            axis(2,at=c(1:1000),labels=c(1:1000),las=1)
            
        }
        
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (BARRIO == input$barrio_seleccionado
                                                               & PERIODO == input$anio_seleccionado
                                                               & MES == input$mes_seleccionado))
            
            choques_barrio$f_accidente <- as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            orden_acc<-sort(accidentes_dia$CLASE,decreasing = TRUE,index.return=TRUE)
            
            plot(accidentes_dia$f_accidente,accidentes_dia$CLASE, type="h",
                 xlab="Fecha accidente",ylab="Cantidad accidentes",yaxt="n",
                 main =paste("Accidentes de acuerdo a su selección"))
            axis(1,at=c(0,1),las=1)
            axis(2,at=c(1:1000),labels=c(1:1000),las=1)
            
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (BARRIO == input$barrio_seleccionado
                                                               & PERIODO == input$anio_seleccionado
                                                               & CLASE == input$clase_seleccionada))
            
            choques_barrio$f_accidente <- as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            orden_acc<-sort(accidentes_dia$CLASE,decreasing = TRUE,index.return=TRUE)
            
            plot(accidentes_dia$f_accidente,accidentes_dia$CLASE, type="h",
                 xlab="Fecha accidente",ylab="Cantidad accidentes",yaxt="n",
                 main =paste("Accidentes de acuerdo a su selección"))
            axis(1,at=c(0,1),las=1)
            axis(2,at=c(1:1000),labels=c(1:1000),las=1)
            
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (PERIODO == input$anio_seleccionado
                                                               & MES == input$mes_seleccionado
                                                               & CLASE == input$clase_seleccionada))
            
            choques_barrio$f_accidente <- as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            orden_acc<-sort(accidentes_dia$CLASE,decreasing = TRUE,index.return=TRUE)
            
            plot(accidentes_dia$f_accidente,accidentes_dia$CLASE, type="h",
                 xlab="Fecha accidente",ylab="Cantidad accidentes",yaxt="n",
                 main =paste("Accidentes de acuerdo a su selección"))
            axis(1,at=c(0,1),las=1)
            axis(2,at=c(1:1000),labels=c(1:1000),las=1)
            
        }
        
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (BARRIO == input$barrio_seleccionado
                                                               & PERIODO == input$anio_seleccionado
                                                               & MES == input$mes_seleccionado
                                                               & CLASE == input$clase_seleccionada))
            
            choques_barrio$f_accidente <- as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            orden_acc<-sort(accidentes_dia$CLASE,decreasing = TRUE,index.return=TRUE)
            
            plot(accidentes_dia$f_accidente,accidentes_dia$CLASE, type="h",
                 xlab="Fecha accidente",ylab="Cantidad accidentes",yaxt="n",
                 main =paste("Accidentes de acuerdo a su selección"))
            axis(1,at=c(0,1),las=1)
            axis(2,at=c(1:1000),labels=c(1:1000),las=1)
            
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (COMUNA == input$comuna_seleccionada
                                                                & BARRIO == input$barrio_seleccionado
                                                               & PERIODO == input$anio_seleccionado
                                                               & MES == input$mes_seleccionado
                                                                & CLASE == input$clase_seleccionada))
            
            choques_barrio$f_accidente <- as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            orden_acc<-sort(accidentes_dia$CLASE,decreasing = TRUE,index.return=TRUE)
            
            plot(accidentes_dia$f_accidente,accidentes_dia$CLASE, type="h",
                 xlab="Fecha accidente",ylab="Cantidad accidentes",yaxt="n",
                 main =paste("Accidentes de acuerdo a su selección"))
            axis(1,at=c(0,1),las=1)
            axis(2,at=c(1:1000),labels=c(1:1000),las=1)
            
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (COMUNA == input$comuna_seleccionada
                                                               & BARRIO == input$barrio_seleccionado
                                                               & PERIODO == input$anio_seleccionado
                                                               & MES == input$mes_seleccionado
                                                               ))
            
            choques_barrio$f_accidente <- as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            orden_acc<-sort(accidentes_dia$CLASE,decreasing = TRUE,index.return=TRUE)
            
            plot(accidentes_dia$f_accidente,accidentes_dia$CLASE, type="h",
                 xlab="Fecha accidente",ylab="Cantidad accidentes",yaxt="n",
                 main =paste("Accidentes de acuerdo a su selección"))
            axis(1,at=c(0,1),las=1)
            axis(2,at=c(1:1000),labels=c(1:1000),las=1)
            
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (COMUNA == input$comuna_seleccionada
                                                               & BARRIO == input$barrio_seleccionado
                                                               & PERIODO == input$anio_seleccionado
                                                               & CLASE == input$clase_seleccionada))
            
            choques_barrio$f_accidente <- as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            orden_acc<-sort(accidentes_dia$CLASE,decreasing = TRUE,index.return=TRUE)
            
            plot(accidentes_dia$f_accidente,accidentes_dia$CLASE, type="h",
                 xlab="Fecha accidente",ylab="Cantidad accidentes",yaxt="n",
                 main =paste("Accidentes de acuerdo a su selección"))
            axis(1,at=c(0,1),las=1)
            axis(2,at=c(1:1000),labels=c(1:1000),las=1)
            
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (COMUNA == input$comuna_seleccionada
                                                               & BARRIO == input$barrio_seleccionado
                                                               & MES == input$mes_seleccionado
                                                               & CLASE == input$clase_seleccionada))
            
            choques_barrio$f_accidente <- as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            orden_acc<-sort(accidentes_dia$CLASE,decreasing = TRUE,index.return=TRUE)
            
            plot(accidentes_dia$f_accidente,accidentes_dia$CLASE, type="h",
                 xlab="Fecha accidente",ylab="Cantidad accidentes",yaxt="n",
                 main =paste("Accidentes de acuerdo a su selección"))
            axis(1,at=c(0,1),las=1)
            axis(2,at=c(1:1000),labels=c(1:1000),las=1)
            
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (COMUNA == input$comuna_seleccionada
                                                               & PERIODO == input$anio_seleccionado
                                                               & MES == input$mes_seleccionado
                                                               & CLASE == input$clase_seleccionada))
            
            choques_barrio$f_accidente <- as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            orden_acc<-sort(accidentes_dia$CLASE,decreasing = TRUE,index.return=TRUE)
            
            plot(accidentes_dia$f_accidente,accidentes_dia$CLASE, type="h",
                 xlab="Fecha accidente",ylab="Cantidad accidentes",yaxt="n",
                 main =paste("Accidentes de acuerdo a su selección"))
            axis(1,at=c(0,1),las=1)
            axis(2,at=c(1:1000),labels=c(1:1000),las=1)
            
        }
        
        
        
    })   
    
    #valuebox1 para visualizar el número de accidentes
    output$vbox1 <- renderValueBox({
        # d <- 10
        
        
        if (input$comuna_seleccionada == "Seleccione una comuna"
            & input$barrio_seleccionado == "Seleccione un barrio"
            & input$anio_seleccionado == "Todos los Años"
            & input$mes_seleccionado == "Todos los meses"
            & input$clase_seleccionada == "Seleccione la clase"){
            
            
            choques_barrio<-subset(accidentalidad)
            
            choques_barrio$f_accidente<-as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            
            
            no_accidentes <- sum(accidentes_dia[, 'CLASE'])
            
        }
        
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"){
            
            
            choques_barrio <- subset(accidentalidad, subset = (BARRIO == input$barrio_seleccionado))
            
            choques_barrio$f_accidente<-as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
             
            
            no_accidentes<-sum(accidentes_dia[, 'CLASE'])
            
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"){
            
            
            choques_barrio <- subset(accidentalidad, subset = (COMUNA == input$comuna_seleccionada))
            
            
            choques_barrio$f_accidente<-as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            
            
            no_accidentes<-sum(accidentes_dia[, 'CLASE'])
            
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"){
            
            
            choques_barrio <- subset(accidentalidad, subset = (PERIODO == input$anio_seleccionado))
            
            
            choques_barrio$f_accidente<-as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            
            
            no_accidentes<-sum(accidentes_dia[, 'CLASE'])
            
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (MES == input$mes_seleccionado))
            
            
            
            choques_barrio$f_accidente<-as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            
            
            no_accidentes<-sum(accidentes_dia[, 'CLASE'])
            
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (CLASE == input$clase_seleccionada))
            
            
            choques_barrio$f_accidente<-as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            
            
            no_accidentes<-sum(accidentes_dia[, 'CLASE'])
            
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (COMUNA == input$comuna_seleccionada
                                                               & BARRIO == input$barrio_seleccionado))
            
            
            
            choques_barrio$f_accidente<-as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            
            
            no_accidentes<-sum(accidentes_dia[, 'CLASE'])
            
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (COMUNA == input$comuna_seleccionada
                                                               & PERIODO == input$anio_seleccionado))
            
            
            choques_barrio$f_accidente<-as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            
            
            no_accidentes<-sum(accidentes_dia[, 'CLASE'])
            
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (COMUNA == input$comuna_seleccionada
                                                               & MES == input$input$mes_seleccionado))
            
            
            
            choques_barrio$f_accidente<-as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            
            
            no_accidentes<-sum(accidentes_dia[, 'CLASE'])
            
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            
            choques_barrio <- subset(accidentalidad, subset = (COMUNA == input$comuna_seleccionada
                                                               & CLASE == input$clase_seleccionada))
            
            
            
            choques_barrio$f_accidente<-as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            
            
            no_accidentes<-sum(accidentes_dia[, 'CLASE'])
            
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (COMUNA == input$comuna_seleccionada
                                                               & BARRIO == input$barrio_seleccionado
                                                               & PERIODO == input$anio_seleccionado))
            
            
            choques_barrio$f_accidente<-as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            
            
            no_accidentes<-sum(accidentes_dia[, 'CLASE'])
            
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (COMUNA == input$comuna_seleccionada
                                                               & BARRIO == input$barrio_seleccionado
                                                               & MES == input$mes_seleccionado))
            
            
            
            choques_barrio$f_accidente<-as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            
            
            no_accidentes<-sum(accidentes_dia[, 'CLASE'])
            
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (COMUNA == input$comuna_seleccionada
                                                               & BARRIO == input$barrio_seleccionado
                                                               & CLASE == input$clase_seleccionada))
            
            
            choques_barrio$f_accidente<-as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            
            
            no_accidentes<-sum(accidentes_dia[, 'CLASE'])
            
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (COMUNA == input$comuna_seleccionada
                                                               & PERIODO == input$anio_seleccionado
                                                               & MES == input$mes_seleccionado ))
            
            
            choques_barrio$f_accidente<-as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            
            
            no_accidentes<-sum(accidentes_dia[, 'CLASE'])
            
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (COMUNA == input$comuna_seleccionada
                                                               & PERIODO == input$anio_seleccionado
                                                               & CLASE == input$clase_seleccionada ))
            
            
            
            choques_barrio$f_accidente<-as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            
            
            no_accidentes<-sum(accidentes_dia[, 'CLASE'])
            
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (COMUNA == input$comuna_seleccionada
                                                               & MES == input$mes_seleccionado
                                                               & CLASE == input$clase_seleccionada ))
            
            
            choques_barrio$f_accidente<-as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            
            
            no_accidentes<-sum(accidentes_dia[, 'CLASE'])
            
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (BARRIO == input$barrio_seleccionado
                                                               & PERIODO == input$anio_seleccionado))
            
            
            choques_barrio$f_accidente<-as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            
            
            no_accidentes<-sum(accidentes_dia[, 'CLASE'])
            
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (BARRIO == input$barrio_seleccionado
                                                               & MES == input$mes_seleccionado))
            
            
            choques_barrio$f_accidente<-as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            
            
            no_accidentes<-sum(accidentes_dia[, 'CLASE'])
            
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (BARRIO == input$barrio_seleccionado
                                                               & CLASE == input$clase_seleccionada))
            
            
            choques_barrio$f_accidente<-as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            
            
            no_accidentes<-sum(accidentes_dia[, 'CLASE'])
            
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (PERIODO == input$anio_seleccionado
                                                               & MES == input$mes_seleccionado))
            
            
            choques_barrio$f_accidente<-as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            
            
            no_accidentes<-sum(accidentes_dia[, 'CLASE'])
            
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (PERIODO == input$anio_seleccionado
                                                               & CLASE == input$clase_seleccionada))
            
            
            
            choques_barrio$f_accidente<-as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            
            
            no_accidentes<-sum(accidentes_dia[, 'CLASE'])
            
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (MES == input$mes_seleccionado
                                                               & CLASE == input$clase_seleccionada))
            
            
            choques_barrio$f_accidente<-as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            
            
            no_accidentes<-sum(accidentes_dia[, 'CLASE'])
            
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (BARRIO == input$barrio_seleccionado
                                                               & MES == input$mes_seleccionado
                                                               & CLASE == input$clase_seleccionada ))
            
            
            choques_barrio$f_accidente<-as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            
            
            no_accidentes<-sum(accidentes_dia[, 'CLASE'])
            
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (BARRIO == input$barrio_seleccionado
                                                               & PERIODO == input$anio_seleccionado
                                                               & MES == input$mes_seleccionado))
            
            
            choques_barrio$f_accidente<-as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            
            
            no_accidentes<-sum(accidentes_dia[, 'CLASE'])
            
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (BARRIO == input$barrio_seleccionado
                                                               & PERIODO == input$anio_seleccionado
                                                               & CLASE == input$clase_seleccionada))
            
            
            choques_barrio$f_accidente<-as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            
            
            no_accidentes<-sum(accidentes_dia[, 'CLASE'])
            
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (PERIODO == input$anio_seleccionado
                                                               & MES == input$mes_seleccionado
                                                               & CLASE == input$clase_seleccionada))
            
            
            choques_barrio$f_accidente<-as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            
            
            no_accidentes<-sum(accidentes_dia[, 'CLASE'])
            
        }
        else if (input$comuna_seleccionada == "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (BARRIO == input$barrio_seleccionado
                                                               & PERIODO == input$anio_seleccionado
                                                               & MES == input$mes_seleccionado
                                                               & CLASE == input$clase_seleccionada))
            
            
            choques_barrio$f_accidente<-as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            
            
            no_accidentes<-sum(accidentes_dia[, 'CLASE'])
            
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (COMUNA == input$comuna_seleccionada
                                                               & BARRIO == input$barrio_seleccionado
                                                               & PERIODO == input$anio_seleccionado
                                                               & MES == input$mes_seleccionado
                                                               & CLASE == input$clase_seleccionada))
            
            
            choques_barrio$f_accidente<-as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            
            
            no_accidentes<-sum(accidentes_dia[, 'CLASE'])
            
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada == "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (COMUNA == input$comuna_seleccionada
                                                               & BARRIO == input$barrio_seleccionado
                                                               & PERIODO == input$anio_seleccionado
                                                               & MES == input$mes_seleccionado))
            
            
            
            choques_barrio$f_accidente<-as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            
            
            no_accidentes<-sum(accidentes_dia[, 'CLASE'])
            
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado == "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (COMUNA == input$comuna_seleccionada
                                                               & BARRIO == input$barrio_seleccionado
                                                               & PERIODO == input$anio_seleccionado
                                                               & CLASE == input$clase_seleccionada))
            
            
            choques_barrio$f_accidente<-as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            
            
            no_accidentes<-sum(accidentes_dia[, 'CLASE'])
            
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado != "Seleccione un barrio"
                 & input$anio_seleccionado == "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (COMUNA == input$comuna_seleccionada
                                                               & BARRIO == input$barrio_seleccionado
                                                               & MES == input$mes_seleccionado
                                                               & CLASE == input$clase_seleccionada))
            
            
            choques_barrio$f_accidente<-as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            
            
            no_accidentes<-sum(accidentes_dia[, 'CLASE'])
            
        }
        else if (input$comuna_seleccionada != "Seleccione una comuna"
                 & input$barrio_seleccionado == "Seleccione un barrio"
                 & input$anio_seleccionado != "Todos los Años"
                 & input$mes_seleccionado != "Todos los meses"
                 & input$clase_seleccionada != "Seleccione la clase"
        ){
            
            
            choques_barrio <- subset(accidentalidad, subset = (COMUNA == input$comuna_seleccionada
                                                               & PERIODO == input$anio_seleccionado
                                                               & MES == input$mes_seleccionado
                                                               & CLASE == input$clase_seleccionada))
            
            
            choques_barrio$f_accidente<-as.Date(choques_barrio$Date,format="%Y-%m-%d")
            accidentes_dia<-aggregate(CLASE~f_accidente,data=choques_barrio, 
                                      FUN=length)
            
            
            no_accidentes<-sum(accidentes_dia[, 'CLASE'])
            
        }
        
        
        
        

        
        valueBox(value = no_accidentes,
                 width = 12,
                 subtitle =  "No. accidentes",
                 icon = icon("credit-card"),
                 color="yellow")
    })
    
    
    output$vbox2 <- renderValueBox({ 
        d <- c("Alta")
        valueBox(value = d,
                 subtitle =  "Clasificacion",
                 icon = icon("credit-card"),
                 color="red")
        
    })
    
    
    
    
    
    
}

# 3. App ------------------------------------------------------------------
shinyApp(ui = ui, server = server)

