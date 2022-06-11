# UKRAINE WAR Shiny App
library(shiny)
library(shinythemes)
library(ggmap)
library(acled.api)
library(rgdal)
library(shinyWidgets)
library(dplyr)
library(readr)
library(sp)
library(broom)
library(sf)
library(stringr)
library(scales)
library(tmap)


#-------------------------------------------------------------------------------
# PULL THE DATA
#-------------------------------------------------------------------------------
# #load in the data
# df <- acled.api(
#     email.address = Sys.getenv("ACLED_EMAIL")
#     , access.key = Sys.getenv("ACLED_API_KEY")
#     , country = "Ukraine"
#     , start.date = "2022-02-24"
#     , end.date = Sys.Date()
#     , all.variables = T
# )
# 
# df %>% write_csv("df.csv")

# copy the data so multiple calls don't have to be made
war <- read_csv("df.csv")

# read in the Ukraine Shape file
ukraine <- readOGR("ukraine", "ukr_admbnda_adm1_sspe_20220114")
#roads <- readOGR('roads', 'roads')


# =============================
# points for tmap 
# =============================
# points <- war %>% select(longitude, latitude) %>%
#   mutate_if(is.character, as.numeric) %>%
#   as.matrix()
# 
# # coordinates
# points <- coordinates(points)
# 
# # create a spatial df
# war_df <- SpatialPointsDataFrame(coords = points
#                                  , data = war %>% select(-c(longitude, latitude))
#                                  , proj4string = CRS(proj4string(ukraine)))
# =============================
# Roads and Rails
# =============================
# # import the roads shapefile
# roads <- readOGR("roads.shp")
# #roads@proj4string # used Dep. Proj.4 rep in CRS below
# 
# # turn the roads data into a df
# roads_df <- spTransform(roads,CRS("+proj=longlat +datum=WGS84 +no_defs")) %>%
#     broom::tidy()

# # import the railways shapefile
# rail <- readOGR("railways.shp")
# #rail@proj4string # used Dep. Proj.4 rep in CRS below
# 
# # turn the rail data into a df
# rail_df <- spTransform(rail, CRS("+proj=longlat +datum=WGS84 +no_defs")) %>%
#     broom::tidy()
# =============================
# Waterways
# =============================
water <- readOGR('waterways', 'waterways')
water_df <- spTransform(water, CRS(proj4string(ukraine))) %>%
  broom::tidy()
# =============================
# Blank Map
# =============================
register_google(key = Sys.getenv("ACLED_API_KEY"))
# create the boundries for the entire state of Ukraine
kyiv <- get_map(location = c(left=22, bottom=44.25, right = 40.5, top=52.5))



#-------------------------------------------------------------------------------
# CLEAN THE DATA
#-------------------------------------------------------------------------------
# make event_date a date object
# make longitude/latitude numeric
war <- war %>% dplyr::mutate(
    # change event_date from char to date
    event_date = as.Date(event_date)
    # long/lat as numeric
    , longitude = as.numeric(longitude)
    , latitude = as.numeric(latitude)
    # create the color vector
    , "color" = case_when(str_detect(war$actor1
                                     , "Russia") == TRUE ~ '#DA291C'
                          , str_detect(war$actor1, "Ukraine") == TRUE ~ '#0057B8'
                          , TRUE ~ "orange")
    # create the size vector by calculating area of the circle
    , "size" = sqrt(fatalities/pi))



#-------------------------------------------------------------------------------
# UI CODE
#-------------------------------------------------------------------------------
# Define UI for application that draws a histogram
ui <- navbarPage("Ukraine's War Against Russian Agression"
                 # set the theme
                 ,theme = shinytheme("darkly")
                 
                 ,tabPanel("Map of Event Types"
                           ,sidebarPanel(
                               pickerInput("mapevent", "Choose an Event Type From the List"
                                           , choices = unique(war$event_type)
                                           , options = list(`actions-box` = T)
                                           , multiple = T
                                           , selected = unique(war$event_type)))
                           ,sliderInput('zoom' ,"Select Zoom Level"
                                        , min = 5, max = 8, value = 6) 
                           ,mainPanel(plotOutput("map1")))
                 
                 , tabPanel("Map of Kyiv"
                            ,sidebarPanel(
                                pickerInput("mapevent2", "Choose an Event Type From the List"
                                            , choices = unique(war$event_type)
                                            , options = list(`actions-box` = T)
                                            , multiple = T
                                            , selected = unique(war$event_type)))
                            ,mainPanel(plotOutput("map2")))
                 
                 ,tabPanel("Graph of Event Types"
                           ,sidebarPanel(
                               pickerInput("event", "Choose an Event Type From the List"
                                           , choices = unique(war$event_type)
                                           , options = list(`actions-box` = T)
                                           , multiple = T
                                           , selected = unique(war$event_type)))
                           ,mainPanel(plotOutput("col1")))
                 
                 
                 ,tabPanel("Graph of Sub-Event Types"
                           , sidebarPanel(
                               pickerInput("subevent", "Choose an Sub-Event Type From the List"
                                           , choices = unique(war$sub_event_type)
                                           , options = list(`actions-box` = T)
                                           , multiple = T
                                           , selected = unique(war$sub_event_type)))
                           ,mainPanel(plotOutput("col2")))
)


#-------------------------------------------------------------------------------
# SERVER CODE
#-------------------------------------------------------------------------------
# add the code for the MAPS
server <- function(input, output) {
    
     # render the first map!
     output$map1 <- renderPlot({
         
         # plot the map
         get_stamenmap(bb2bbox(attr(kyiv, "bb"))
                       , maptype = "toner-background"
                       , zoom = input$zoom) %>% ggmap() +
             theme(axis.title = element_blank()
                   , axis.text = element_blank()
                   , axis.ticks = element_blank()) + 
             
              # add the waterways onto the map
              geom_path(data = water_df, mapping = aes(long, lat, group = group)
                        , color = '#005477') + 
             
             # plot the points onto the map
             geom_point(data = war %>% filter(event_type %in% input$mapevent)
                        , mapping = aes(as.numeric(longitude)
                                        , as.numeric(latitude))
                        
                        # add size of bubbles w/ filter
                        , size = rescale(war %>% filter(event_type %in% input$mapevent) %>%
                                             pull(size), c(1,8))
                        
                        # add color of bubbles w/ filter
                        , color = alpha(war %>% filter(event_type %in% input$mapevent) %>%
                                            pull(color), 0.5)) +
         labs(caption = "Source: Armed Conflict Location & Event Data Project (ACLED); acleddata.com")
     })

   # # renter the tmap
   # output$my_tmap <- renderPlot({
   #   tm_shape(ukraine) +
   #     tm_borders() +
   #     tm_shape(roads) +
   #     tm_lines(col = "grey60", alpha = 0.5) +
   #     tm_shape(water) +
   #     tm_lines(col = "blue", alpha = 0.25) +
   #     tm_shape(war_df) +
   #     tm_dots(col = "color", size = "size", alpha = 0.5, scale = 1)
   # })
   #   
    
    # MAP 2
    output$map2 <- renderPlot({
        get_map(location = c(30.5238, 50.454666)
                , source = "stamen"
                , maptype = "toner"
                , zoom = 10) %>% 
            
            ggmap() +
           
             theme(axis.title = element_blank()
                  , axis.text = element_blank()
                  , axis.ticks = element_blank()) + 
            
            geom_jitter(data = war %>% filter(event_type %in% input$mapevent2)
                       , mapping = aes(as.numeric(longitude)
                                       , as.numeric(latitude))
                       
                       # add size of bubbles w/ filter
                       , size = rescale(war %>% filter(event_type %in% input$mapevent2) %>%
                                            pull(size), c(2,10))
                       
                       # add color of bubbles w/ filter
                       , color = alpha(war %>% filter(event_type %in% input$mapevent2) %>%
                                           pull(color), 0.7)
                       
                       , width = 0.0125
                       
                       , height = 0.0125)
            
    })
    
    # render the first column graph
    output$col1 <- renderPlot({
        # create the column graph for EVENT TYPE
        war %>% filter(event_type %in% input$event) %>%
            group_by(event_type) %>% 
            summarise(n=n()) %>%
            arrange(n %>% desc) %>% 
            ungroup() %>%
            ggplot() +
            geom_col(aes(reorder(event_type, n), n)) + 
            coord_flip() +
            ggtitle("Number of Event Types"
                    , subtitle = paste0("data from: ", min(war$event_date), " to ", max(war$event_date))) +
            theme(axis.title = element_blank())
    })
    
    # create the column graph for SUB-Event Type
    output$col2 <- renderPlot({
        # filter the data
        war %>% filter(sub_event_type %in% input$subevent) %>%
            group_by(sub_event_type) %>% 
            summarise(n=n()) %>%
            arrange(n %>% desc) %>% 
            ungroup() %>%
            ggplot() +
            geom_col(aes(reorder(sub_event_type, n), n)) + 
            coord_flip() +
            ggtitle("Number of Sub-Event Types"
                    , subtitle = paste0("data from: ", min(war$event_date), " to ", max(war$event_date))) +
            theme(axis.title = element_blank())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
