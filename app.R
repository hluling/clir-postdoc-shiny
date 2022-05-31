#2022/05/31
# Trying to set up automatic auth for google sheet

library(googledrive)
library(googlesheets4)
library(dplyr)
library(tidyr)
library(leaflet)
library(shiny)
library(shinyWidgets)
library(stringr)
library(shinyBS)


# auto authorization to google account: 
# Step 1: do this once
#options(gargle_oauth_cache = ".secrets")
#drive_auth()
#list.files(".secrets/")

# Step 2: Keep this in script
gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE)

# Load and prepare data --------------------------------------------


# Load from google sheet
postdoc_df <- read_sheet("https://docs.google.com/spreadsheets/d/1tHvVsDcg-CwLtdZDBD4maHzuE0ZsYMWA1f9FR3jjjOE/edit#gid=0")

postdoc_type <- read_sheet("https://docs.google.com/spreadsheets/d/1OcJgprLMUTlFHjITngch4BO3qTaxnPjHehNyK98NmqE/edit#gid=0")



# Change to easier column names to work with
colnames(postdoc_df)[2] <- "latlong"
colnames(postdoc_df)[5] <- "year"
colnames(postdoc_df)[7] <- "institution"

# Change to easier column names to work with
colnames(postdoc_type)[1] <- "type"
colnames(postdoc_type)[2] <- "def"

postdoc_type <- postdoc_type %>% add_row(type="DC-ESS (Sloan)", def="Fellows in Data Curation for Energy Social Sciences")
postdoc_type <- postdoc_type %>% 
  mutate(type = recode(type, "PDOC"="PDOCS"))

# Separate the coordinate column by ','
# Set 3 columns as numbers (rather than texts)
postdoc_df <- postdoc_df %>% 
  separate(latlong, c("lat", "long"), sep=',') %>% 
  mutate_at(c("lat","long", 'year'), as.numeric)

# Create a column 'region'
postdoc_df <- postdoc_df %>%
  mutate(region = case_when(institution == "Chinese University of Hong Kong" ~ "as", # asia
                            institution != "Chinese University of Hong Kong" ~ "na")) # north america

# Crate a column that has the full names of FIELD (DEGREE AREA)
postdoc_df <- postdoc_df %>%
  mutate(area_full = case_when(`FIELD (DEGREE AREA)` == "AHUM" ~ "Arts and Humanities",
                               `FIELD (DEGREE AREA)` == "INFO" ~ "Information Science",
                               `FIELD (DEGREE AREA)` == "SOSC" ~ "Social Sciences",
                               `FIELD (DEGREE AREA)` == "STEM" ~ "Sciences, Technology, Engineering, and Mathematics")) %>% 
  mutate(type_full = case_when(`POSTDOC TYPE (FUNDER)` == "DC-AAAS (Mellon)" ~ "Fellows in Data Curation for African American and African Studies",
                               `POSTDOC TYPE (FUNDER)` == "DC-EE (Sloan)" ~ "Fellows in Data Curation for Energy Economics",
                               `POSTDOC TYPE (FUNDER)` == "DC-EMS (Mellon)" ~ "Fellows in Data Curation for Early Modern Studies",
                               `POSTDOC TYPE (FUNDER)` == "DC-ESS (Sloan)" ~ "Fellows in Data Curation for Energy Social Sciences",
                               `POSTDOC TYPE (FUNDER)` == "DC-LACS (Mellon)" ~ "Fellows in Data Curation for Latin American and Caribbean Studies",
                               `POSTDOC TYPE (FUNDER)` == "DC-MS (Mellon)" ~ "Fellows in Data Curation for Medieval Studies",
                               `POSTDOC TYPE (FUNDER)` == "DC-S/SS (Sloan)" ~ "Fellows in Data Curation for the Sciences and Social Sciences",
                               `POSTDOC TYPE (FUNDER)` == "DC-SC (Sloan)" ~ "Fellows in Software Curation for the Sciences and Social Sciences",
                               `POSTDOC TYPE (FUNDER)` == "DC-VS (Mellon)" ~ "Fellows in Data Curation for Visual Studies",
                               `POSTDOC TYPE (FUNDER)` == "PDOCS" ~ "Fellows in Academic Libraries / Scholarly and Information Resources")) %>% 
  mutate(funder = case_when(`POSTDOC TYPE (FUNDER)` == "DC-AAAS (Mellon)" ~ "The Andrew W. Mellon Foundation",
                               `POSTDOC TYPE (FUNDER)` == "DC-EE (Sloan)" ~ "The Alfred P. Sloan Foundation",
                               `POSTDOC TYPE (FUNDER)` == "DC-EMS (Mellon)" ~ "The Andrew W. Mellon Foundation",
                               `POSTDOC TYPE (FUNDER)` == "DC-ESS (Sloan)" ~ "The Alfred P. Sloan Foundation",
                               `POSTDOC TYPE (FUNDER)` == "DC-LACS (Mellon)" ~ "The Andrew W. Mellon Foundation",
                               `POSTDOC TYPE (FUNDER)` == "DC-MS (Mellon)" ~ "The Andrew W. Mellon Foundation",
                               `POSTDOC TYPE (FUNDER)` == "DC-S/SS (Sloan)" ~ "The Alfred P. Sloan Foundation",
                               `POSTDOC TYPE (FUNDER)` == "DC-SC (Sloan)" ~ "The Alfred P. Sloan Foundation",
                               `POSTDOC TYPE (FUNDER)` == "DC-VS (Mellon)" ~ "The Andrew W. Mellon Foundation",
                               `POSTDOC TYPE (FUNDER)` == "PDOCS" ~ "Host Institution"))

# Jitter coordinates a little bit for different fellows at the same institution
postdoc_df$lat_jt <- jitter(postdoc_df$lat, factor = 0.5)
postdoc_df$long_jt <- jitter(postdoc_df$long, factor = 0.5)

# Make one part of the textual information more formal
postdoc_type <- postdoc_type %>%
  mutate_at("def", str_replace, ", aka Fellows in", " /")

# This is a list converted from the dataframe postdoc_type
type_def <- split(postdoc_type$def, postdoc_type$type)

# Crate a column that has the year for fellow's url
# See how CLIR's website point to different group of fellows by year
# https://postdoc.clir.org/meet/
# For Current Fellows:
# 2020 fellows: https://postdoc.clir.org/meet/#20
# 2019 fellows: https://postdoc.clir.org/meet/#19
# For Former Fellows:
# It's all four-digit year after #
# The pointer doesn't not work for #2018 or #2019, as of March 25, 2022
postdoc_df <- postdoc_df %>%
  mutate(year_url = case_when(year == 2020 ~ 20,
                              year == 2019 ~ 19,
                              year == 2021 ~ 20,
                               TRUE ~ year)) # keep original values for all other years

postdoc_df <- postdoc_df %>% 
  mutate(funder = case_when(
    funder == "The Andrew W. Mellon Foundation" ~ "The Mellon Foundation",
    funder == "Host Institution" ~ "Partner Organization",
    TRUE ~ funder
  ))

# Create a list of popup texts used for the map (as labels for the circles)
pops <- lapply(seq(nrow(postdoc_df)), function(i) {
  paste0('<p>', 
         '<b>Partner Organization: </b>', postdoc_df[i, "institution"], '</p><p>', 
         '<b>Year: </b>', postdoc_df[i, "year"], '</p><p>', 
         '<b>Postdoc Type: </b>', postdoc_type[which(postdoc_type$type == postdoc_df[i,]$'POSTDOC TYPE (FUNDER)'),]$def,'</p><p>',
         '<b>PhD Degree: </b>', postdoc_df[i, "FIELD (PHD DEGREE)"], '</p><p>',
         '<b>Funder: </b>', postdoc_df[i, "funder"], '</p><p>',
    '<a href="https://postdoc.clir.org/meet/#', postdoc_df[i, "year_url"], '">Go to CLIR\'s fellows webpage by year</a></p>')
})

# Create a list of degree areas (full name: acronym)
degree_areas <- list("Arts and Humanities" = "AHUM",
                     "Sciences, Technology, Engineering, and Mathematics" = "STEM",
                     "Social Sciences" = "SOSC",
                     "Information Science" = "INFO")

# A list that maps color to phd area categories
# Used in the reactive map so that the color-category relationship stays constant regardless of user selection
color_area_full = c("#b30000", "#7eb0d5", "#ffee65", "#b36200")
names(color_area_full) = levels(factor(postdoc_df$area_full))


# Fixes based on Jodi's requests
postdoc_df <- postdoc_df %>% 
  mutate(icon = case_when(
    funder == "The Mellon Foundation" ~ "circle",
    funder == "Partner Organization" ~ "square",
    funder == "The Alfred P. Sloan Foundation" ~ "triangle"
  ))

# Create a color column for phd degree areas
postdoc_df <- postdoc_df %>% 
  mutate(color = case_when(
    area_full == "Arts and Humanities" ~ "#b30000",
    area_full == "Information Science" ~ "#7eb0d5",
    area_full == "Sciences, Technology, Engineering, and Mathematics" ~ "#ffee65",
    area_full == "Social Sciences" ~ "#b36200"
  ))

# Create a shape_color column for the 11 color-shape combinations
postdoc_df <- postdoc_df %>% 
  mutate(color_b = gsub("#", "", color)) %>% 
  mutate(
    shape_color = as.factor(paste(icon, color_b, sep="_"))
  ) 

# Map each of the 11 color-shape combinations to the corresponding png
icons2 <- iconList(
  circle_7eb0d5 <- makeIcon(
                            iconUrl = "www/vista-circle-7eb0d5.png",
                            iconWidth = 18, iconHeight = 18),
  circle_b30000 <- makeIcon(
                            iconUrl =  "www/vista-circle-b30000.png",
                            iconWidth = 18, iconHeight = 18),
  circle_b36200 <- makeIcon(
                            iconUrl = "www/vista-circle-b36200.png",
                            iconWidth = 18, iconHeight = 18),
  square_7eb0d5 <- makeIcon(iconUrl = "www/vista-square-7eb0d5.png",
                            iconWidth = 18, iconHeight = 18),
  square_b30000 <- makeIcon(iconUrl = "www/vista-square-b30000.png",
                            iconWidth = 18, iconHeight = 18),
  square_b36200 <- makeIcon(iconUrl = "www/vista-square-b36200.png",
                            iconWidth = 18, iconHeight = 18),
  square_ffee65 <- makeIcon(iconUrl = "www/vista-square-ffee65.png",
                            iconWidth = 18, iconHeight = 18),
  triangle_7eb0d5 <- makeIcon(iconUrl = "www/vista-triangle-7eb0d5.png",
                            iconWidth = 20, iconHeight = 20),
  triangle_b30000 <- makeIcon(iconUrl = "www/vista-triangle-b30000.png",
                              iconWidth = 20, iconHeight = 20),
  triangle_b36200 <- makeIcon(iconUrl = "www/vista-triangle-b36200.png",
                              iconWidth = 20, iconHeight = 20),
  triangle_ffee65 <- makeIcon(iconUrl = "www/vista-triangle-ffee65.png",
                              iconWidth = 20, iconHeight = 20)
)

# set legend features
# Basically, manually set up 9 lines in a legend
# Line 1 and Line 6 are the title lines
colors <- c("white", "#7eb0d5", "#b30000", "#b36200", "#ffee65", "white", "white", "white", "white")
labels <- c("Color Keys: PhD Degree Areas",
            "Information Science", "Arts and Humanities", "Social Sciences", "Sciences, Technology, Engineering, and Mathematics",
            "Shape Keys: Funder Types",
            "The Mellon Foundation", "Partner Organization", "The Alfred P. Sloan Foundation")
sizes <- c(20, 20, 20, 20, 20, 20, 20, 20, 20)
sizes_inline <- c(40, 20, 20, 20, 20, 40, 20, 20, 20)
shapes <- c("square", "square", "square", "square", "square", "square", "circle", "square", "diamond")
borders <- c("white", "white", "white", "white", "white", "white", "black", "black", "black")

addLegendCustom <- function(map, colors, labels, sizes, sizes_inline, shapes, borders, opacity = 0.5){
  
  make_shapes <- function(colors, sizes, borders, shapes) {
    shapes <- gsub("circle", "50%", shapes)
    shapes <- gsub("square", "0%", shapes)
    #shapes <- gsub("triangle-down", "0%", shapes)
    paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border:1px solid ", borders, "; border-radius:", shapes)
  }
  make_labels <- function(sizes, sizes_inline, labels) {
    paste0("<div style='display: inline-block;height: ", 
           sizes, "px;margin-top: 2px;line-height: ", 
           sizes_inline, "px;'>", labels, "</div>")
  }
  
  legend_colors <- make_shapes(colors, sizes, borders, shapes)
  
  # Modify the last line for a triangle
  # See my own answer to this post:
  # https://stackoverflow.com/questions/72296705/how-to-add-triangles-when-customizing-a-leaflet-legend-in-r/72308995#72308995
  legend_colors[length(legend_colors)] <- "url('data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABYAAAAWCAYAAADEtGw7AAAAAXNSR0IArs4c6QAAAudJREFUSEu1lU9IFFEcx79vZqfBsBUijYU6yB5ni0pJl10U3CCErEX3ICIae+jQKajQkygelA5dsqKDIIiJmAexNowwV1odvbWwKxEU7h4MQihjx6ZpZuJN82La9o8FPhhmGN585vv7vc+8ITigQQ6Ii/2C6TzTDuG8LpprP2AegA7gNoBvAO4BYPf+G0xfTI+jtbW1b3Rd/57JZM4C2LUrYFX89YJyiVmyscXFxdN7e3uucDicsNOXTF0KzNmpfE1NTc/j8fi8YRic3+9v29jYaAHwzq7GKNSPUmArESFkYX19vaKuri7NcZyZSCTOBIPBTwAipXpdDMzKvNTb2/twYmLiiaZplbQCQRCUzs7OyMzMzFUAL4vBC4HZgrncbvdaOp3+4PF4PpqmeYiWTAhRt7a2Tvp8vuOKogTtNlBr/hiFwC4APwDcGBkZud7f37+gqqpbEASrl5qmcaIo7g4MDISHh4fvAHhUKHU+mKU95vV615LJ5IooitRdjpBfU03TMkxXFKVSkqTGbDbbAOBLvn754N96zc7ONkYikRVVVY+wtKxWlnpycjLU09ND+3wrP7UTzPQ61dzcHFteXp7Xdd3Fkub3kCYnhBiBQKBNluULAN469XOCrbQ8zz+TZVmsr69PaZpWwfN8wa9L13VCDVldXaX67Zim2eFMzcCsBZej0ej98fFxSy+e5wvKz9Lrus4JgpDr6uqKTE9PRwG8YHC2WPTsqqqqklOp1HumFyGk6F5gLySh+mWz2ROSJHlyuRzVjz5jWEBbr5ujo6PX+vr6njr1Krdf26l3h4aGrgwODt4F8IAyKZguWrXX65WTyeQrURRVp17lwLZ+hqIoh30+XyCTyZwH8Jn1eGxubs7f3t4eZ3rZD5TjUjPYR/N1amqqpbu729KPghtaW1sfx2KxGADB8acoCy0wwQiFQheXlpY6KDguSdK5mpqaHcMwBI6jnfn3YZom4ThO3d7ert7c3Hz9E0/lMRSF+q9aAAAAAElFTkSuQmCC')"
  
  legend_colors[length(legend_colors)] <- gsub("')", 
                                               "'); width:22px; height:22px", 
                                               legend_colors[length(legend_colors)])
  legend_colors[1] <- "white; width:0px; height:0px; border:0px solid white; border-radius:0%"
  legend_colors[6] <- "white; width:0px; height:0px; border:0px solid white; border-radius:0%"
  legend_colors[7] <- "white; width:22px; height:22px; border:1px solid black; border-radius:50%; margin-top: 2px"
  legend_colors[8] <- "white; width:22px; height:22px; border:1px solid black; border-radius:0%; margin-top: 2px"
  
  legend_labels <- make_labels(sizes, sizes_inline, labels)
  legend_labels[1] <- "<div style='display: inline-block;height: 30px;margin-top: 0px;line-height: 30px;'><strong>Color Keys: PhD Degree Areas</strong></div>"
  legend_labels[6] <- "<div style='display: inline-block;height: 30px;margin-top: 0px;line-height: 30px;'><strong>Shape Keys: Funder Types</strong></div>"
  
  return(addLegend(map, colors = legend_colors, labels = legend_labels, opacity = opacity,"bottomright"))
}

# ui ----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel(div(class="header", 
                 a(href="https://www.clir.org/fellowships/postdoc/", 
                   img(src='clir.png', align = "right", width="32", height="32")
                 ),
                 h1("  CLIR Postdoctoral Fellowship Over the Years",
                    style='padding-left: 20px'
                 ), 
                 style='padding-right: 5px'
                 ),
             windowTitle = "CLIR Postdocs"
            
  ),

  tags$style(type = "text/css", "#livemap {height:calc(100vh - 80px)!important;}"),
  tags$style(HTML("#input_pane {background-color: rgba(192,192,192,0.3);}")),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "radiobutton_color.css")
  ),
  
  leafletOutput("livemap"),
  
  absolutePanel(top = 300, left = 80,
                width=500, height = 425,
                shinyjs::useShinyjs(),
                id = "input_pane",
                draggable=TRUE,
                chooseSliderSkin("Shiny", color = "#9a1c46"),
                setSliderColor(c("#9a1c46"), c(1)),
                div(style = "margin: auto; width: 90%",
                    prettyRadioButtons(
                      inputId = "region",
                      label = "Region:",
                      choices = c("North America" = "na",
                                  "Asia" = "as"),
                      shape = "square",
                      bigger =  TRUE,
                      inline = TRUE,
                      width = '100%',
                      fill = 'TRUE'),
                    
                    pickerInput("field", "PhD Degree Area",
                                choices = levels(factor(postdoc_df$area_full)),
                                multiple = TRUE,
                                options = list(`actions-box` = TRUE),
                                selected = levels(factor(postdoc_df$area_full)),
                                width = '100%'),
                    
                    pickerInput("type", "Postdoc Type",
                                choices = levels(factor(postdoc_df$type_full)),
                                multiple = TRUE,
                                options = list(`actions-box` = TRUE),
                                selected = levels(factor(postdoc_df$type_full)),
                                width = '100%'),
                    
                    pickerInput("funder", "Funder",
                                choices = levels(factor(postdoc_df$funder)),
                                multiple = TRUE,
                                options = list(`actions-box` = TRUE),
                                selected = levels(factor(postdoc_df$funder)),
                                width = '100%'),
                    
                    sliderInput("yslider", "Initial Award Year",
                                min(postdoc_df$year),
                                max(postdoc_df$year),
                                value = range(postdoc_df$year),
                                step = 1,
                                sep = "",
                                width='100%'),
                    
                    actionButton(
                      inputId = "help",
                      label = "Help"
                    ),
                    actionButton(
                      inputId = "reset",
                      label = "Reset"
                    )
                )
  )
)

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # When clicked, show message
  observeEvent(input$help, {
    sendSweetAlert(
      session = session,
      title = "How to go back to select all choices?",
      text = "Click 'Deselect All' in one of the dropdowns, then click 'Select All.' Or, click the Reset button",
      type = "info"
    )
  })
  
  # When clicked, reset all choices
  observeEvent(input$reset, {
    shinyjs::reset("input_pane")
  })
  
  # A reactive dataframe listening to the year slider
  reactive_data_chrono <- reactive({
    postdoc_df %>%
      filter(year >= input$yslider[1] & year <= input$yslider[2]) %>% 
      filter(area_full %in% input$field) %>% 
      filter(type_full %in% input$type) %>% 
      filter(funder %in% input$funder)
  })
  
  # A reactive list of popup texts listening to reactive_data_chrono()
  reactive_pops <- reactive({
    filtered_df = reactive_data_chrono() 
    pops <- lapply(seq(nrow(filtered_df)), function(i) {
      paste0('<p>', 
             '<b>Partner Organization: </b>', filtered_df[i, "institution"], '</p><p>', 
             '<b>Year: </b>', filtered_df[i, "year"], '</p><p>', 
             '<b>Postdoc Type: </b>', postdoc_type[which(postdoc_type$type == filtered_df[i,]$'POSTDOC TYPE (FUNDER)'),]$def,'</p><p>',
             '<b>PhD Degree: </b>', filtered_df[i, "FIELD (PHD DEGREE)"], '</p><p>',
             '<b>Funder: </b>', filtered_df[i, "funder"], '</p><p>',
             '<a href="https://postdoc.clir.org/meet/#', filtered_df[i, "year_url"], '">CLIR\'s fellows webpage by year</a></p>')
    })
  })
  
  # Base map
  output$livemap <- renderLeaflet({
    
    leaflet(postdoc_df) %>%
      
      addProviderTiles("CartoDB.Positron") %>%
      
      addMarkers(
        lng = postdoc_df$long_jt,
        lat =postdoc_df$lat_jt,
        popup = lapply(pops, htmltools::HTML),
        icon = ~ icons2[as.numeric(shape_color)],
        clusterOptions = markerClusterOptions(showCoverageOnHover = F) # toggle clustering on/off here
      ) %>% 
      addLegendCustom(colors, labels, sizes,sizes_inline, shapes, borders) %>% 
      setView(lat=41.11829615928769, lng=-103.00494833061417, zoom = 4)
  })
  
  # Show corresponding maps depending on region
  observeEvent(c(input$region), {
    if (input$region == "as") { # Asia
      
      leafletProxy("livemap", data = reactive_data_chrono()) %>%
        clearMarkers() %>%
        clearMarkerClusters %>% 
        addMarkers(
          lng = reactive_data_chrono()$long_jt,
          lat = reactive_data_chrono()$lat_jt,
          popup = lapply(reactive_pops(), htmltools::HTML),
          icon = ~ icons2[as.numeric(reactive_data_chrono()$shape_color)],
          clusterOptions = markerClusterOptions(showCoverageOnHover = F)
        ) %>%
        setView(lat=22.09852712321511, lng=114.11865077053847, zoom = 5)
    }
    
    
    if (input$region == "na") { # North America
      leafletProxy("livemap", data = reactive_data_chrono()) %>%
        clearMarkers() %>%
        clearMarkerClusters() %>% 
        addMarkers(
          lng = reactive_data_chrono()$long_jt,
          lat = reactive_data_chrono()$lat_jt,
          popup = lapply(reactive_pops(), htmltools::HTML),
          icon = ~ icons2[as.numeric(reactive_data_chrono()$shape_color)],
          clusterOptions = markerClusterOptions(showCoverageOnHover = F)
        ) %>% 
        setView(lat=41.11829615928769, lng=-103.00494833061417, zoom = 4)
    }
  })
  
  # Reactive map listening to both reactive_data_chrono() and reactive_pops()
  observe({
    leafletProxy("livemap", data = reactive_data_chrono()) %>%
      clearMarkers() %>%
      clearMarkerClusters() %>% 
      addMarkers(
        lng = reactive_data_chrono()$long_jt,
        lat = reactive_data_chrono()$lat_jt,
        popup = lapply(reactive_pops(), htmltools::HTML),
        icon = ~ icons2[as.numeric(reactive_data_chrono()$shape_color)],
        clusterOptions = markerClusterOptions(showCoverageOnHover = F) #showCoverageOnHover = F
      )
  })
  
  # Update selected choices in dropdowns based on user selection 
  observeEvent(c(input$field,
                 input$type,
                 input$funder),{
                   filtered_df <-
                     if (!is.null(input$field) &
                         !is.null(input$type) &
                         !is.null(input$funder)) {
                       postdoc_df %>%
                         filter(year >= input$yslider[1] & year <= input$yslider[2]) %>% 
                         filter(area_full %in% input$field) %>%
                         filter(type_full %in% input$type) %>%
                         filter(funder %in% input$funder)
                     }
                   else {
                     postdoc_df
                   }
                   # for field
                   if (!is.null(input$field)) {
                     updatePickerInput(
                       session,
                       "field",
                       choices = levels(factor(postdoc_df$area_full)),
                       selected = levels(factor(filtered_df$area_full)))
                   } else{
                   }
                   # for type
                   if (!is.null(input$type)) {
                     updatePickerInput(
                       session,
                       "type",
                       choices = levels(factor(postdoc_df$type_full)),
                       selected = levels(factor(filtered_df$type_full))
                     )
                   }
                   # for category
                   if (!is.null(input$funder)) {
                     updatePickerInput(
                       session,
                       "funder",
                       choices = levels(factor(postdoc_df$funder)),
                       selected  = levels(factor(filtered_df$funder))
                     )
                   }
                 },
               ignoreInit = T,
               ignoreNULL = F
  )
}

shinyApp(ui, server)
