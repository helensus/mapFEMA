library(shiny)
library(magrittr)
library(dplyr)

library(tigris)
library(rgdal)
library(geojsonio)
library(leaflet)

# use data "public.new2" as it is cleaned and reorganized.
public.new<-read.csv("public.new2.csv")
#summarize with each fips, delete the dubricate lines of states and counties
display<- public.new %>% group_by(fips,declarationYear) %>% summarise(state=unique(state),county=unique(county),sumObligated=sum(totalObligated)) %>% rename(GEO_ID = fips)
#round the amount with thousands
display$sumObligated<- display$sumObligated/1000

##get ready for mapping 
#xy:us layer
xy <- geojsonio::geojson_read("gz_2010_us_050_00_5m.json", what = "sp")
xy$GEO_ID %<>% substr(start = 10, stop = 14)
# join the US map data with funding data by fips/GEO-ID
leafmap <- geo_join(xy, display, by = "GEO_ID", how = "inner")
#pal function:maps data values "sumObligated" to colors according to a given palette
pal <- leaflet::colorNumeric(palette = "PuRd", domain = leafmap$sumObligated)
#add labels
labels <- sprintf(
    "%s<br/>%s<br/>%s",
    leafmap$state,leafmap$county, leafmap$sumObligated) 
#add popups
popup<- paste0("<strong>state:</strong>", leafmap$state,"<br>",
               "<strong>county:</strong>", leafmap$county,"<br>",
               "<strong>Sum of Obligated Amount:</strong>",round(leafmap$sumObligated,2),"thousand")




ui <- fluidPage(
    title = "Hurricane Fund",
    sidebarLayout(
        tabsetPanel(
            conditionalPanel(
                'input.dataset === "display"'),
            conditionalPanel(
                'input.dataset === "leafmap"',
            )
        ),
        
        mainPanel(
            tabsetPanel(
                id = 'dataset',
                tabPanel("The fundation amount of the year",
                         
                         fluidRow(
                             
                             column(4,
                                    selectInput("declarationYear",
                                                "Year:",
                                                c("All",
                                                  unique(display$declarationYear)))
                                    ),
                             column(4,
                                    selectInput("county",
                                                "County:",
                                                c("All",
                                                  unique(display$county)))
                             )
                         ),
 
                         DT::dataTableOutput("table1")),
                
                tabPanel("Map of fundation for hurricanes",
                         
                         fluidRow(
                             column(4,
                                    selectInput("declarationYear2",
                                                "Year:",
                                                c("All",
                                                  unique(leafmap$declarationYear)))),

                         ),
                         leafletOutput("map")))
                
            )
        )
    )




server <- function(input, output) {
    
    output$table1 <- DT::renderDataTable(DT::datatable({
        data <- display
        if (input$declarationYear != "All") {
            data <- data[data$declarationYear== input$declarationYear,]
        }
        if (input$county != "All") {
            data <- data[data$county== input$county,]
        }
        
        data
    }))
    
    output$map <-  renderLeaflet({
        data<-leafmap
        if (input$declarationYear2 != "All") {
            data <- data[data$declarationYear== input$declarationYear2,]
        }

        #mapping by leaflet
        leaflet() %>%
            #titles
            addTiles() %>%
            addProviderTiles("CartoDB.Positron") %>%
            addProviderTiles(providers$Stamen.TonerLines,
                             options = providerTileOptions(opacity = 0.75)) %>%
            
            setView(-89.275673, 37.098, zoom = 4) %>%
            addPolygons(data = data, 
                        fillColor = ~pal(sumObligated), 
                        color = "#BDBDC3",
                        fillOpacity = 1, 
                        weight = 1, 
                        smoothFactor = 0.2,
                        
                        popup= popup,
                        popupOptions = popupOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto")
                        
            ) %>%
            addLegend(pal = pal, 
                      values = data$sumObligated, 
                      position = "bottomright", 
                      title = paste0("Sum obligated amount(in thousands)<br>"))
        
    
    
    })
}


# Run the application 
shinyApp(ui = ui, server = server)



