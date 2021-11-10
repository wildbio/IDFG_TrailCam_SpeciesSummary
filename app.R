# Remove existing objects from global environment
objs <- ls(pos = ".GlobalEnv")
rm(list = objs, pos = ".GlobalEnv")

# Install/load required packages
dependencies<-c("shiny","shinyBS","shinydashboard","shinycssloaders","shinyjs",
                "tidyverse","readr","DT","stringr",
                "V8","htmltools","tippy", "slickR", "plotly", "ggpubr",
                "dplyr","ggplot2","scales","chron","lubridate","forcats",
                "rmarkdown", "markdown", "knitr", "broom", "leaflet", "rgdal", "rgeos")
# removed sf from dependencies list

for(i in 1:length(dependencies)){
         if(dependencies[i] %in% installed.packages()==FALSE){
                 install.packages(dependencies[i])
                 require(dependencies[i],character.only=TRUE)
         } else{
                require(dependencies[i],character.only=TRUE)
         }
}

# Show App Loading ####
load_data <- function() {
  Sys.sleep(2)
  shinyjs::hide("loading_page")
  shinyjs::show("main_content")
}

mycss <- "
#plot-container {
  position: relative;
}
#plot.recalculating {
  z-index: -2;
}
.radio label {
font-size: 18px;
}

.checkbox label {
font-size: 18px;
}
.shiny-split-layout > div {
overflow: visible;
}

.shiny-notification {
              height: 100px;
              width: 600px;
              position:fixed;
              top: calc(50% - 50px);;
              left: calc(50% - 400px);;
              font-size: 20px;
              opacity: 1;
            }
"

modal_download <- function() {
  div(id = "mod_dwnld",
      modalDialog(splitLayout(downloadButton("download_csv","CSV Data"),
                              downloadButton("download_report","PDF Report")),
                  easyClose = TRUE, title = "Download Species Data")
  )
}

# Prepare Rmarkdown report
if(exists("params")) rm(params)

# Spatial data
sp_dir <- "data/spatialdata"
gmu <- rgdal::readOGR(file.path(sp_dir, "Hunt_GameUnit.shp"))
gmu <- spTransform(gmu, CRS("+proj=longlat +datum=WGS84"))

reg <- rgdal::readOGR(file.path(sp_dir, "Region.shp"))
reg <- spTransform(reg, CRS("+proj=longlat +datum=WGS84"))


# Species data
species_list <- c("elk", "moose", "muledeer","whitetaileddeer", "pronghorn", "sheep (bighorn)",
                  "wolf", "coyote", "fox",
                  "mountain lion", "bobcat", "lynx",
                  "black bear", "grizzly bear",
                  "badger", "fisher", "marten", "porcupine", "wolverine")
species_names <- data.frame(species_list) %>%
  mutate(species_singular = ifelse(species_list == "muledeer", "mule deer",
                                   ifelse(species_list == "whitetaileddeer", "white-tailed deer",
                                          ifelse(species_list == "sheep (bighorn)", "bighorn sheep",
                                                 as.character(species_list))))) %>%
  mutate(species_plural = ifelse(species_singular %in% c("elk", "lynx", "moose", "pronghorn",
                                                         "mule deer", "bighorn sheep",
                                                         "white-tailed deer"), species_singular,
                                 ifelse(species_singular == "fox", "foxes",
                                        ifelse(species_singular == "wolf", "wolves",
                                               paste0(species_singular,"s"))))) %>%
  mutate(species_title = str_to_title(species_singular)) %>%
  mutate(species_code = str_replace_all(species_list, " ", "")) %>%
  mutate(species_code = str_replace_all(species_code, "\\(", "")) %>%
  mutate(species_code = str_replace_all(species_code, "\\)", "")) %>%
  arrange(species_list)

# UI
ui <- dashboardPage(title = "Species Summary Generator",
                    dashboardHeader(
                            tags$li(class = "dropdown",
                                    tags$style(".main-header {max-height: 80px}"),
                                    tags$style(".main-header .logo {
                       height: 80px; 
                       line-height: 75px !important;
                       padding: 0 0px;}")
                            ),
                            # Use image in title
                            titleWidth='100%',
                            title = span(
                                    tags$img(src="bobcat.jpg", width = '100%'),
                                    column(12, class="title-box")
                            ),
                            tags$li(a(href = 'http://idfg.idaho.gov',
                                      img(src = "idfglogo.png",
                                          title = "Idaho Fish and Game", height = "60px"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown")
                    ),
                    dashboardSidebar(disable=TRUE),
                    dashboardBody(
                            fluidPage(
                                    tags$head(tags$style(HTML(mycss))),
                                    useShinyjs(),
                                    div(
                                      id = "loading_page",
                                      hr(),
                                      icon("spinner",class = "fa-spin fa-3x"),
                                      h3("Website loading...")
                                    ),
                                    shinyjs::hidden(
                                      div(
                                        id = "main_content",
                                        column(width = 12,
                                               h1(HTML("<font color=\"#3c8dbc\" style = \"text-shadow: 1px 1px #4d3a7d\"><b>Camera Species Summary App</b></font>"))
                                        ),
                                        column(width = 12,
                                               sidebarLayout(
                                                 sidebarPanel(width = 6,
                                                              wellPanel(style = "background:#d0e2f2",
                                                                        selectInput("deployment",HTML("<font size = 4>Camera Deployment</font>"),
                                                                                    choices = c("SWWLF2019 (Statewide Wolf 2019)" = "SWWLF2019")),
                                                                        selectInput("species",HTML("<font size = 4>Species</font>"),
                                                                                    choices = c(as.character(species_names$species_list))),
                                                                        selectInput("region", HTML("<font size = 4>Geographic Area</font>"),
                                                                                    choices = c("Statewide" = "statewide",
                                                                                                "Region 1" = 1,
                                                                                                "Region 2" = 2,
                                                                                                "Region 3" = 3,
                                                                                                "Region 4" = 4,
                                                                                                "Region 5" = 5,
                                                                                                "Region 6" = 6,
                                                                                                "Region 7" = 7)),
                                                                        br(),
                                                                        HTML("<font size = 4><b>1. Submit Data Query</b></font>"),
                                                                        br(),
                                                                        HTML("<font size = 2>Preparing data may take several minutes</font>"),
                                                                        br(),
                                                                        actionButton('submit_query',HTML('<font size = 5><b>Submit</b></font>'), 
                                                                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 100%"),
                                                                        br(),br(),
                                                                        HTML("<font size = 4><b>2. Download Data</b></font>"),
                                                                        br(),
                                                                        HTML("<font size = 2>Pop-up option to download CSV Data File or PDF Summary Report</font>"),
                                                                        br(),
                                                                        actionButton('download_data',HTML('<font size = 5><b>Download</b></font>'), 
                                                                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 100%")
                                                              )
                                                              
                                                              
                                                 ),
                                                 mainPanel(width = 6,
                                                           h3(HTML("<b>Data Preview</b>")),
                                                           conditionalPanel(condition = "input.submit_query != '0'",
                                                                            leafletOutput(outputId = "sp_map"),
                                                                            br(),br(),
                                                                            dataTableOutput(outputId = "sp_table"))
                                                 )
                                               )
                                               
                                        )
                                      ) # div (under shinyjs::hidden)
                                    ) # shinyjs::hidden
                                    ) # fluidpage
                    ) # dashboard body
) # dashboard page



server <- function(input, output, session) {
  load_data()
  
  deploy_sp <- eventReactive(input$submit_query, {
    withProgress(message = "Loading deployment data...", {
      deploy_sp<-readRDS("data/deploy.Rdata")
      return(deploy_sp)
    })
  })
  
  deploy_dat <- eventReactive(input$submit_query, {
    deploy_dat<-deploy_sp()@data
  })
  
  sp_df <- eventReactive(input$submit_query, {
    withProgress(message = "Loading and filtering picture data...", 
                 detail = "This may take a few minutes", {
      ## All picture data
                   incProgress(1/10)
                   pic_dat<-readRDS(paste0("data/speciesdata/",input$species,".rds"))
                   incProgress(5/10)
                   # MERGED DATA
                   incProgress(2/10)
                   dat <- left_join(pic_dat, deploy_dat(), by = "Pic_CamID")
                   incProgress(2/10)
                   if(input$region == "statewide"){
                     sp_df <- dat %>%
                       select(-X, -Processor) %>%
                       as.data.frame()
                   } else {
                     sp_df <- dat %>%
                       filter(region == input$region)%>%
                       select(-X, -Processor) %>%
                       as.data.frame()
                   }
                   return(sp_df)
      })
    })
  
  sp_names <- eventReactive(input$submit_query, {
    sp_names <- species_names %>%
      filter(species_list == input$species)
    return(sp_names)
  })
  
  reg_code <- eventReactive(input$submit_query, {
    if(input$region == "statewide"){
      reg_code <- "statewide"
    } else {
      reg_code <- paste0("R", input$region)
    }
    return(reg_code)
  })
  
  reg_title <- eventReactive(input$submit_query, {
    if(input$region == "statewide"){
      reg_title <- "Statewide"
    } else {
      reg_title <- paste0("Region ", input$region)
    }
    return(reg_title)
  })
  
  reg_text <- eventReactive(input$submit_query, {
    if(input$region == "statewide"){
      reg_text <- "statewide"
    } else {
      reg_text <- paste0("region ", input$region)
    }
    return(reg_text)
  })
  
  reg_shp <- eventReactive(input$submit_query, {
    reg_shp <- reg[reg$ID == input$region,]
    return(reg_shp)
  })
  
  gmu_df <- eventReactive(input$submit_query, {
    if(input$region == "statewide"){
      # Convert to data frames
      gmu$id <- rownames(gmu@data)
      gmu_df <- tidy(gmu, region = "id")
    } else {
      reg_select <- reg[reg$ID == input$region,]
      clip <- gIntersection(reg_select, gmu, byid = T, drop_lower_td = T)
      gmu_df <- fortify(clip, region = "id")
    }
    return(gmu_df)
  })
  
  output$sp_table <- DT::renderDataTable({
    DT::datatable(sp_df(),
                  options = list(lengthMenu = c(5,10,20,50), 
                                 pageLength = 10,
                                 scrollX = T),
                  rownames = FALSE
                  )
    })
  
  output$sp_map <- renderLeaflet({
    withProgress(message = "Drawing map...", {
      sp_map_dat <- sp_df() %>%
        group_by(Pic_CamID) %>% 
        tally(NearSp1Count)
      
      sp_map_dat <- sp::merge(deploy_sp(), sp_map_dat, by.x="Pic_CamID", by.y="Pic_CamID", all.x=TRUE, all.y=FALSE) 
      sp_map_dat$n[is.na(sp_map_dat$n)]<-0
      
      # COLORS
      # if camera has 0 of an animal, make color medium grey, otherwise, blackish
      sp_map_dat@data$color_symb<-"#202020"
      sp_map_dat@data$color_symb[sp_map_dat@data$n==0]<-"#808080"
      # c(0.5, 3, 4, 5, 6, 7, 8, 10, 15)
      # c( 0, 2, 4, 10, 20, 40, 100, 200, 201)
      # MARKER SIZES
      # now rescale the map values from 0.5 to 10
      # range(sp_map_dat$n)
      sp_map_dat@data$radius<-0.5
      sp_map_dat@data$radius[sp_map_dat@data$n>0 & sp_map_dat@data$n<=2]<-3
      sp_map_dat@data$radius[sp_map_dat@data$n>2 & sp_map_dat@data$n<=4]<-4
      sp_map_dat@data$radius[sp_map_dat@data$n>4 & sp_map_dat@data$n<=10]<-5
      sp_map_dat@data$radius[sp_map_dat@data$n>10 & sp_map_dat@data$n<=20]<-6
      sp_map_dat@data$radius[sp_map_dat@data$n>20 & sp_map_dat@data$n<=40]<-7
      sp_map_dat@data$radius[sp_map_dat@data$n>40 & sp_map_dat@data$n<=100]<-8
      sp_map_dat@data$radius[sp_map_dat@data$n>100]<-10
      sp_map_dat@data$radius[sp_map_dat@data$n>200]<-15
      popup <- paste0("Camera: ", sp_map_dat$Pic_CamID, " <br> ", "Total ", input$species, " = " , sp_map_dat$n)
      
      # title <-paste("species =", selSpecies)
      leaflet() %>%
        addPolygons(data = reg, fillOpacity = 0.1, color="grey", weight=2) %>%
        # start with a simple base map
        addProviderTiles("Esri.WorldGrayCanvas",
                         options = providerTileOptions(attribution = NA))%>%
        
        # add polygons of strata, color by high med low
        # addPolygons(data = strata, fillOpacity = 0.3, 
        #      color = "grey", weight = 1, fillColor=pal(strata$strata)) %>%
        addPolygons(data = gmu, fillOpacity = 0.3, weight=.5) %>%
        
        # add circles for cams, sized by # of indivs
        addCircleMarkers(data = sp_map_dat, weight = 3,
                         radius = sp_map_dat@data$radius, popup = popup, color=sp_map_dat@data$color_symb)
      })
    })
  
  observeEvent(input$download_data, {
    showModal(modal_download())
  })
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste(sp_names()$species_code,"_",reg_code(), "_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      readr::write_csv(sp_df(), file)
    }
  )
  
  output$download_report <- downloadHandler(
    #withProgress(message = "Filtering Collar Data", {
    filename = function(){
      paste(sp_names()$species_code,"_",reg_code(), "_summary_", Sys.Date(),".pdf", sep="")
    },
    content = function(file) {
      withProgress(message = "Generating report...",{
        report <- reactive({
          if(input$region == "statewide"){
            "source/species_app_report.Rmd"
          } else {
            "source/species_app_report_regional.Rmd"
          }
        })
        tempReport <- file.path(tempdir(),"report.Rmd")
        file.copy(report(), tempReport, overwrite = TRUE)
        tempLogo <- file.path(tempdir(),"idfglogo.png")
        file.copy("source/idfglogo.png", tempLogo, overwrite = T)
        
        params <- list(species_title = sp_names()$species_title,
                       species_data = sp_df(),
                       species_s = sp_names()$species_singular,
                       species_p = sp_names()$species_plural,
                       deploy_dat = deploy_dat(),
                       reg_shp = reg_shp(),
                       gmu_df = gmu_df(),
                       reg_title = reg_title(),
                       reg_text = reg_text()
        )
        
        rmarkdown::render(tempReport, 
                          output_format = "pdf_document",
                          output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      })
    },
    contentType = "application/pdf"
    #}
  )
  #)
  
}




# Create a Shiny app object
shinyApp(ui = ui, server = server)

