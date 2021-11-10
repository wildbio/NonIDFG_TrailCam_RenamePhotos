# Remove existing objects from global environment
objs <- ls(pos = ".GlobalEnv")
rm(list = objs, pos = ".GlobalEnv")

# Install/load required packages
dependencies<-c("shiny","shinyBS","shinydashboard","shinycssloaders","shinyjs",
                "tidyverse","readr","exifr","DT","stringr",
                "V8","htmltools","tippy", "slickR", "plotly", "ggpubr")

for(i in 1:length(dependencies)){
        if(dependencies[i] %in% installed.packages()==FALSE){
                install.packages(dependencies[i])
                require(dependencies[i],character.only=TRUE)
        } else{
                require(dependencies[i],character.only=TRUE)
        }
}

# Specify path to 'camprocessR' source file
source("source/renameR_mult.R")


mycss <- "
#plot-container {
  position: relative;
}
#loading-spinner {
  position: absolute;
  left: 50%;
  top: 50%;
  z-index: -1;
  margin-top: -33px;  /* half of the spinner's height */
  margin-left: -33px; /* half of the spinner's width */
}
#plot.recalculating {
  z-index: -2;
}
"

# UI
ui <- dashboardPage(title = "Photo Renamer App",
                    dashboardHeader(
                            tags$li(class = "dropdown",
                                    tags$style(".main-header {max-height: 80px}"),
                                    tags$style(".main-header .logo {
                       height: 80px; 
                       line-height: 75px !important;
                       padding: 0 0px;}")
                            ),
                            # Use image in title
                            title = "Photo Renamer App",
                            titleWidth = "300px",
                            tags$li(a(href = 'http://idfg.idaho.gov',
                                      img(src = 'idfglogo.png',
                                          title = "Idaho Fish and Game", height = "50px"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown")
                    ),
                    dashboardSidebar(disable=TRUE),
                    dashboardBody(
                            fluidPage(
                                    tags$head(tags$style(HTML(mycss))),
                                    tabsetPanel(id = "tabs",
                                                tabPanel(title = HTML("<font color=\"#3c8dbc\" size = 5 style = \"text-shadow: 1px 1px #4d3a7d\"><b>RENAME</b></font>"), value = "rename_mult",
                                                         sidebarLayout(
                                                                 sidebarPanel(width = 6,
                                                                              h2(HTML("<font color=\"#3c8dbc\" style = \"text-shadow: 1px 1px #4d3a7d\"><b>STEP ONE</b></font>")),
                                                                              h3("Rename photos from multiple cameras at one time and move to new folder (originals are not retained)"),
                                                                              br(),
                                                                              wellPanel(style = "background:#d0e2f2",
                                                                                        h3(tags$b('Choose folder with photos to rename')),
                                                                                        actionButton(inputId = "chooseOrigPhotos_m", label = HTML("<font size = 4>Click to Choose Folder</font>"), 
                                                                                                     style="color: #fff; background-color: #6faade; border-color: #5491c7; width: 100%"),
                                                                                        uiOutput("selectedOrigPhotos_m"),
                                                                                        br()
                                                                                        ,
                                                                                        
                                                                                        selectInput(inputId = "folderother_m", 
                                                                                                    label = HTML("<font size=5>How do you want to organize your renamed photos?</font>"),
                                                                                                    choices = c(" " = " ",
                                                                                                                "Location ID" = "LocID",
                                                                                                                "Camera ID" = "CamID",
                                                                                                                "Year/Year_Mo" = "Year/Year_Mo"
                                                                                                    )
                                                                                        ),
                                                                                        conditionalPanel(
                                                                                                condition = "input.folderother_m == 'LocID'",
                                                                                                h4("Organizing by location ID requires location information 
                                                    in the original folder(s). The example below shows
                                                    a situation where each original image has two 
                                                    parent folders with location information"),
                                                                                                img(src = "rename_by_directory.png",
                                                                                                    title = "RenameExample", width = "100%"),
                                                                                                br(),br(),
                                                                                                selectInput(inputId = "location_length", label = HTML("<font size=5>How many folders have location
                                                                                                information?</font>"),
                                                                                                            choices = c(" " = " ",
                                                                                                                        "One" = "one",
                                                                                                                        "Two" = "two"
                                                                                                            )
                                                                                                )
                                                                                        ),
                                                                                        
                                                                                        conditionalPanel(
                                                                                                condition = "input.location_length == 'one'",
                                                                                                textInput(inputId = "loc_m", 
                                                                                                             label = HTML("<font size=5>Which folder position has the location information?</font>
                                                                      <br><h4>(e.g., GMU 32A is in position 2 above)</h4>"))
                                                                                        ),
                                                                                        
                                                                                        conditionalPanel(
                                                                                                condition = "input.location_length == 'two'",
                                                                                                textInput(inputId = "loc1_m", 
                                                                                                             label = HTML("<font size=5>Which folder position has the large-scale location information?</font>
                                                                      <br><h4>(e.g., GMU 32A is in position 2 above)</h4>")),
                                                                                                textInput(inputId = "loc2_m", 
                                                                                                             label = HTML("<font size=5>Which folder position has the small-scale location information?</font>
                                                                      <br><h4>(e.g., cell 1779 is in position 3 above)</h4>"))
                                                                                        ),
                                                                                        
                                                                                        h3(tags$b('Choose destination folder for renamed photos')),
                                                                                        conditionalPanel(
                                                                                                condition = "(input.folderother_m != ' ' & input.folderother_m != 'LocID') || input.location_length != ' '",
                                                                                                wellPanel(style = "background:#ebf0f5",
                                                                                                          uiOutput("folderother_m")
                                                                                                )
                                                                                        ),
                                                                                        actionButton(inputId = "chooseRenameLoc_m", label = HTML("<font size = 4>Click to Choose Folder</font>"), 
                                                                                                     style="color: #fff; background-color: #6faade; border-color: #5491c7; width: 100%"),
                                                                                        uiOutput("selectedRenameLoc_m"),
                                                                                        br()
                                                                                        ,
                                                                                        
                                                                                        h3(tags$b('Ready to rename photos?')),
                                                                                        actionButton('fileRenameExecute1_m',HTML('<font size = 5><b>Rename Photos</b></font>'), 
                                                                                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 100%")
                                                                              )
                                                                 ),
                                                                 mainPanel(width = 6,
                                                                           conditionalPanel(
                                                                                   condition = "input.fileRenameExecute1_m >= 1",
                                                                                   br(),
                                                                                   htmlOutput(outputId = "renameerrmessage_m"),
                                                                                   htmlOutput(outputId = "renamemessage1_m")%>% withSpinner(color="#3c8dbc"))
                                                                           
                                                                           
                                                                 )
                                                                 
                                                         )
                                                        
                                                ),
                                                tabPanel(title = HTML("<font color=\"#3c8dbc\" size = 5 style = \"text-shadow: 1px 1px #4d3a7d\"><b>ENJOY</b></font>"), value = "enjoy_mult",
                                                         sidebarLayout(
                                                                 sidebarPanel(width = 6,
                                                                              h2(HTML("<font color=\"#3c8dbc\" style = \"text-shadow: 1px 1px #4d3a7d\"><b>STEP TWO</b></font>")),
                                                                              h3("Enjoy probable animal photos!"),
                                                                              conditionalPanel(
                                                                                      condition = "input.fileRenameExecute1_m >= 1",
                                                                                      br(),
                                                                                      wellPanel(style = "background:rgba(219, 230, 240, 0.25)",
                                                                                                plotlyOutput(outputId = "triggerplot_m")
                                                                                      )
                                                                              )
                                                                 ),
                                                                 mainPanel(width = 6,
                                                                           conditionalPanel(
                                                                                   condition = "input.fileRenameExecute1_m >= 1",
                                                                                   br(),
                                                                                   uiOutput(outputId = "animalphotos_m")%>% withSpinner(color="#3c8dbc"))
                                                                           
                                                                           
                                                                 )
                                                                 
                                                         )
                                                )
                                    )
                                    
                            )
                    )
)


server <- function(input, output, session) {
        output$folderother_m <- renderUI({
                if(req(input$folderother_m) != " " & req(input$folderother_m) != "LocID"){
                        list(h4(HTML("App will create the following folder and file name structure within the destination folder:")),
                             h4(HTML(paste0("<b>Folder structure:</b> RenamedPhotos/",input$folderother_m,
                                            "<br><br><b>File name structure:</b> CamID_Date_Time_TM_Seq#"))))
                } else if(req(input$location_length) == "one"){
                        list(h4(HTML("App will create the following folder and file name structure within the destination folder:")),
                             h4(HTML(paste0("<b>Folder structure:</b> RenamedPhotos/LocID
                     <br><br><b>File name structure:</b> LocID_Date_Time_TM_Seq#"))))
                } else if(req(input$location_length) == "two"){
                        list(h4(HTML("App will create the following folder and file name structure within the destination folder:")),
                             h4(HTML(paste0("<b>Folder structure:</b> RenamedPhotos/LocA/LocB
                     <br><br><b>File name structure:</b> LocA_LocB_Date_Time_TM_Seq#"))))
                }
                
                
        })
        
        
        renameR_mult(input, output, session)
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
