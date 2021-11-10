renameR_mult <- function(input, output, session){
  
  # Choose folder with original photos
  observeEvent(input$chooseOrigPhotos_m, {
    orig_folder <- choose.dir("")
    if(!is.na(orig_folder)){
      output$selectedOrigPhotos_m <- renderUI({
        checkboxGroupInput(inputId = "selectedOrigPhotos_m", label = "",
                           choices = orig_folder, selected = orig_folder)
      })
    } else {"Folder selection cancelled."}
  })
  
  
  # Choose folder for renamed photos
  observeEvent(input$chooseRenameLoc_m, {
    rename_folder <- choose.dir("")
    if(!is.na(rename_folder)){
      output$selectedRenameLoc_m <- renderUI({
        checkboxGroupInput(inputId = "selectedRenameLoc_m", label = "",
                           choices = rename_folder, selected = rename_folder)
      })
    } else {"Folder selection cancelled."}
  })
  
  output$renamemessage1_m <- renderUI({
    
    if (input$fileRenameExecute1_m == 0){
      return()
      
    }
    
    else{
      isolate({
        shiny::withProgress(
          value = 0, {
            incProgress(2/10, message = "Finding photos to rename...")
            files <- list.files(input$selectedOrigPhotos_m, pattern = "*.JPG$", full.names = TRUE, recursive = TRUE, include.dirs = FALSE)
            n <- length(files)
            if(n == 0){
              list(h1(HTML("<center><font color=\"#3c8dbc\" style = \"text-shadow: 1.5px 1.5px #4d3a7d\"><b>OOPS!</b></font></center>")),
                   br(),
                   h4(HTML("There are no photos to rename. Please check the folder path for your photos")),
                   br(),br(),
                   h4(HTML("<b>NOTE:</b> Output above <b>will not change</b> until you click <b>Rename Photos</b> again"))
              )
            } else {
              incProgress(3/10, message = "Renaming photos...")
              if(str_sub(input$selectedRenameLoc_m,start=-1)=="\\"){
                backup<-gsub("\\\\","",input$selectedRenameLoc_m)}
              else{backup <- gsub("\\\\","/",input$selectedRenameLoc_m)}
              locfolder <- file.path(backup, "RenamedPhotos")
              if(!dir.exists(locfolder)){
                dir.create(locfolder)}
              
              errfile_m <- file.path(backup,"RenameProcessLog.txt")
              lapply(errfile_m, function(x) if(!file.exists(x)) file.create(x))
              
              if(input$folderother_m=="Year/Year_Mo"){
                rename_cmnd <- paste("exiftool -r \"-Directory<",
                                     locfolder,
                                     "/${DateTimeOriginal#;DateFmt(\'%Y\')}/${DateTimeOriginal#;DateFmt(\'%Y_%m\')}\" \"-filename<${userlabel;s/ //g}_${DateTimeOriginal#;DateFmt(\'%Y%m%d_%H%M%S\')}_${triggermode;s/(.)(.*\\s)(.)(.*)/$1$3/}${sequence;s/(\\d)(.*)/_$1/}.%e\" ",
                                     input$selectedOrigPhotos_m, sep = "")
                sysout <- file(errfile_m, "a")
                sink(sysout,append = T)
                rcd<-system(rename_cmnd, intern = T)
                print(paste("~~~~~~~~~~~~~~~~~~~~~~Renaming Log ",Sys.time()," Round One~~~~~~~~~~~~~~~~~~~~~~"))
                print(rcd)
                sink()
                close(sysout)
              }      
              else if(input$folderother_m=="CamID"){
                rename_cmnd <- paste("exiftool -r \"-Directory<",
                                     locfolder,
                                     "/${userlabel;s/ //g}\" -d %Y%m%d_%H%M%S \"-filename<${userlabel;s/ //g}_${DateTimeOriginal}_${triggermode;s/(.)(.*\\s)(.)(.*)/$1$3/}${sequence;s/(\\d)(.*)/_$1/}.%e\" ",
                                     input$selectedOrigPhotos_m, sep = "")
                sysout <- file(errfile_m, "a")
                sink(sysout,append = T)
                rcd<-system(rename_cmnd, intern = T)
                print(paste("~~~~~~~~~~~~~~~~~~~~~~Renaming Log ",Sys.time()," Round One~~~~~~~~~~~~~~~~~~~~~~"))
                print(rcd)
                sink()
                close(sysout)
              }    
              else if(input$location_length=="one"){
                i <- input$loc_m
                rename_cmnd <- paste("exiftool -r -d %Y%m%d_%H%M%S \"-filename<",locfolder,
                                     "/${Directory;$_=(split '\\/',$_)[",i,"]}/${Directory;$_=(split '\\/',$_)[",i,"]}_${DateTimeOriginal}_${triggermode;s/(.)(.*\\s)(.)(.*)/$1$3/}${sequence;s/(\\d)(.*)/_$1/}.%e\" ",
                                     input$selectedOrigPhotos_m, sep = "")
                sysout <- file(errfile_m, "a")
                sink(sysout,append = T)
                rcd<-system(rename_cmnd, intern = T)
                print(paste("~~~~~~~~~~~~~~~~~~~~~~Renaming Log ",Sys.time()," Round One~~~~~~~~~~~~~~~~~~~~~~"))
                print(rcd)
                sink()
                close(sysout)
              }
              
              else if(input$location_length=="two"){
                i <- input$loc1_m
                j <- input$loc2_m
                rename_cmnd <- paste("exiftool -r -d %Y%m%d_%H%M%S \"-filename<",locfolder,
                                     "/${Directory;$_=(split '\\/',$_)[",i,"]}/${Directory;$_=(split '\\/',$_)[",j,"]}/${Directory;$_=(split '\\/',$_)[",i,"]}_${Directory;$_=(split '\\/',$_)[",j,"]}_${DateTimeOriginal}_${triggermode;s/(.)(.*\\s)(.)(.*)/$1$3/}${sequence;s/(\\d)(.*)/_$1/}.%e\" ",
                                     input$selectedOrigPhotos_m, sep = "")
                sysout <- file(errfile_m, "a")
                sink(sysout,append = T)
                rcd<-system(rename_cmnd, intern = T)
                print(paste("~~~~~~~~~~~~~~~~~~~~~~Renaming Log ",Sys.time()," Round One~~~~~~~~~~~~~~~~~~~~~~"))
                print(rcd)
                sink()
                close(sysout)
              }
              
              incProgress(4/10, message = "Checking for errors...")
              left.files <- list.files(input$selectedOrigPhotos_m, pattern = "*.JPG$", full.names = TRUE, recursive = TRUE)
              lf <- length(left.files)
              
              left.dirs <- list.dirs(input$selectedOrigPhotos_m)
              if(lf == 0){
                unlink(left.dirs, recursive = T)
                new.dirs <- list.dirs(locfolder)
                new.dirs <- new.dirs[new.dirs != locfolder]
                todays.dirs <- file.info(new.dirs) %>% 
                  tidyr::separate(col = mtime, into = c("Date","Time"), sep = " ", remove = F) %>% 
                  tibble::rownames_to_column(var = "folder") %>% 
                  dplyr::filter(Date == Sys.Date())
                new.files <- list.files(todays.dirs$folder[1], pattern = "*.JPG$", full.names = TRUE, recursive = F, include.dirs = FALSE)
                motionphotos <- new.files[grepl("MD",new.files)]
                if(length(motionphotos)==0){
                  output$animalphotos_m <- renderUI({
                    list(h1(HTML("<center><font color=\"#3c8dbc\" style = \"text-shadow: 1.5px 1.5px #4d3a7d\"><b>OH WELL...</b></font></center>")),
                         br(),
                         h4("There are no motion-triggered photos in this image set. Fingers crossed the time-triggered photos caught something!"),
                         br(),br(),
                         actionButton(inputId = "enjoyagain_m",label = HTML("<font size = 4><b>Process More Cameras</b></font>"), 
                                      style="color: #fff; background-color: #6faade; border-color: #5491c7; width: 100%"),
                         br(),br(),
                         h4(HTML("<b>NOTE FOR SUBSEQUENT CAMERAS:</b> 
                         <br>Output above <b>will not change</b> until you click <b>Rename Photos</b> again"))
                    )
                  })
                } else{
                  motiondata <- read_exif(motionphotos, args = "-SourceFile -UserLabel -DateTimeOriginal")
                  subdata <- motiondata %>%
                    tidyr::separate(col = DateTimeOriginal, into = c("Date","Time"), sep = " ", remove = F) %>%
                    mutate(Date = stringr::str_replace_all(Date,pattern = ":", replacement = "/")) %>%
                    mutate(Date.graph = as.Date(Date))
                  
                  output$triggerplot_m <- renderPlotly({
                    triggerplot<-suppressWarnings(ggplot(subdata, aes(x = Date.graph)) +
                                                    ggtitle("Frequency of Motion-Triggered Photos through Time") +
                                                    geom_histogram(aes(text = Date),
                                                                   alpha = 0.3, stat = "count", position = "identity",
                                                                   color = "#00AFBB", fill = "#00AFBB") +
                                                    scale_y_continuous(expand = c(0,0))+
                                                    labs(x = "Date", y = "Frequency") +
                                                    theme_classic() +
                                                    theme(plot.title = element_text(hjust = 0.5)))
                    ggplotly(triggerplot, tooltip = c("text", "count"))
                    
                  })
                  
                  output$slickr_m <- renderSlickR({
                    if(length(motionphotos)<25){
                      slickR::slickR(motionphotos, height = "400px") +
                        settings(dots = T)
                    } else {
                      motion <- sample(motionphotos, 25)
                      slickR::slickR(motion, height = "400px") +
                        settings(dots = T)
                    }
                  })
                  output$animalphotos_m <- renderUI({
                    list(h1(HTML("<center><font color=\"#3c8dbc\" style = \"text-shadow: 1.5px 1.5px #4d3a7d\"><b>SMILE!</b></font></center>")),
                         br(),
                         h4(HTML("You probably caught some animals! Scroll through randomly selected motion photos in the slideshow below. 
                              When you're ready to move on, hit the <b>Process More Cameras</b> button below the slideshow (if relevant)")),
                         br(),
                         slickROutput("slickr_m"),
                         br(),br(),
                         actionButton(inputId = "enjoyagain_m",label = HTML("<font size = 4><b>Process More Cameras</b></font>"), 
                                      style="color: #fff; background-color: #6faade; border-color: #5491c7; width: 100%"),
                         br(),br(),
                         h4(HTML("<b>NOTE FOR SUBSEQUENT CAMERAS:</b> 
                         <br>Output above <b>will not change</b> until you click <b>Rename Photos</b> again"))
                    )
                  })
                }
                nf <- length(new.files)
                if(nf <= 5){
                  rdf <- read_exif(new.files, args = "-SourceFile -UserLabel -DateTimeOriginal -TriggerMode")
                  output$renametable1_m <- DT::renderDataTable({
                    (rdf)},
                    options = list(pageLength = 1,
                                   lengthMenu = c(1,3,5),
                                   scrollX = TRUE,
                                   autoWidth = TRUE
                    )
                  )
                } else {
                  rdf<- read_exif(new.files[(nf-4):nf], args = "-SourceFile -UserLabel -DateTimeOriginal -TriggerMode")
                  output$renametable1_m <- DT::renderDataTable({
                    (rdf)},
                    options = list(pageLength = 1,
                                   lengthMenu = c(1,3,5),
                                   scrollX = TRUE,
                                   autoWidth = TRUE
                    )
                  ) 
                }
                
                list(h1(HTML("<center><font color=\"#3c8dbc\" style = \"text-shadow: 1.5px 1.5px #4d3a7d\"><b>ALL DONE!</b></font></center>")),
                     br(),
                     h4(HTML(paste0("You finished renaming and moving photos from <font face=\"Courier New\">", input$selectedOrigPhotos_m,
                                    "</font> to <font face=\"Courier New\">", locfolder, "</font>"))),
                     br(),
                     h4(HTML("There are <b>", lf, " photos left</b> to rename")),
                     br(),
                     actionButton(inputId = "enjoyphotos_m",label = HTML("<font size = 4><b>ENJOY Probable Animal Photos</b></font>"), 
                                  style="color: #fff; background-color: #6faade; border-color: #5491c7; width: 100%"),
                     br(),br(),
                     dataTableOutput(outputId = "renametable1_m"),
                     br(),br(),
                     h4(HTML("<b>NOTE FOR SUBSEQUENT CAMERAS:</b> 
                         <br>Output above <b>will not change</b> until you click <b>Rename Photos</b> again"))
                )
                
              } else {
                
                files <- list.files(input$selectedOrigPhotos_m, pattern = "*.JPG$", full.names = TRUE, recursive = TRUE, include.dirs = FALSE)
                n <- length(files)
                if(n == 0){
                  
                  list(h1(HTML("<font color=\"#3c8dbc\" style = \"text-shadow: 1.5px 1.5px #4d3a7d\"><b>OOPS!</b></font>")),
                       br(),
                       h4(HTML("There are no photos to rename. Please check the folder path for your photos")))
                  
                } else {
                  
                  if(str_sub(input$selectedRenameLoc_m,start=-1)=="\\"){
                    backup<-gsub("\\\\","",input$selectedRenameLoc_m)}
                  else{backup <- gsub("\\\\","/",input$selectedRenameLoc_m)}
                  
                  locfolder <- file.path(backup, "RenamedPhotos")
                  if(!dir.exists(locfolder)){
                    dir.create(locfolder)}
                  
                  errfile_m <- file.path(backup,"RenameProcessLog.txt")
                  lapply(errfile_m, function(x) if(!file.exists(x)) file.create(x))
                  
                  if(input$folderother_m=="Year/Year_Mo"){
                    rename_cmnd <- paste("exiftool -r \"-Directory<",
                                         locfolder,
                                         "/${DateTimeOriginal#;DateFmt(\'%Y\')}/${DateTimeOriginal#;DateFmt(\'%Y_%m\')}\" \"-filename<${userlabel;s/ //g}_${DateTimeOriginal#;DateFmt(\'%Y%m%d_%H%M%S\')}_${triggermode;s/(.)(.*\\s)(.)(.*)/$1$3/}${sequence;s/(\\d)(.*)/_$1b/}.%e\" ",
                                         input$selectedOrigPhotos_m, sep = "")
                    sysout <- file(errfile_m, "a")
                    sink(sysout,append = T)
                    rcd<-system(rename_cmnd, intern = T)
                    print(paste("~~~~~~~~~~~~~~~~~~~~~~Renaming Log ",Sys.time()," Round Two~~~~~~~~~~~~~~~~~~~~~~"))
                    print(rcd)
                    sink()
                    close(sysout)
                  }      
                  else if(input$folderother_m=="CamID"){
                    rename_cmnd <- paste("exiftool -r \"-Directory<",
                                         locfolder,
                                         "/${userlabel;s/ //g}\" -d %Y%m%d_%H%M%S \"-filename<${userlabel;s/ //g}_${DateTimeOriginal}_${triggermode;s/(.)(.*\\s)(.)(.*)/$1$3/}${sequence;s/(\\d)(.*)/_$1b/}.%e\" ",
                                         input$selectedOrigPhotos_m, sep = "")
                    sysout <- file(errfile_m, "a")
                    sink(sysout,append = T)
                    rcd<-system(rename_cmnd, intern = T)
                    print(paste("~~~~~~~~~~~~~~~~~~~~~~Renaming Log ",Sys.time()," Round Two~~~~~~~~~~~~~~~~~~~~~~"))
                    print(rcd)
                    sink()
                    close(sysout)
                  }    
                  else if(input$location_length=="one"){
                    i <- input$loc_m
                    rename_cmnd <- paste("exiftool -r -d %Y%m%d_%H%M%S \"-filename<",locfolder,
                                         "/${Directory;$_=(split '\\/',$_)[",i,"]}/${Directory;$_=(split '\\/',$_)[",i,"]}_${DateTimeOriginal}_${triggermode;s/(.)(.*\\s)(.)(.*)/$1$3/}${sequence;s/(\\d)(.*)/_$1b/}.%e\" ",
                                         input$selectedOrigPhotos_m, sep = "")
                    sysout <- file(errfile_m, "a")
                    sink(sysout,append = T)
                    rcd<-system(rename_cmnd, intern = T)
                    print(paste("~~~~~~~~~~~~~~~~~~~~~~Renaming Log ",Sys.time()," Round One~~~~~~~~~~~~~~~~~~~~~~"))
                    print(rcd)
                    sink()
                    close(sysout)
                  }
                  
                  else if(input$location_length=="two"){
                    i <- input$loc1_m
                    j <- input$loc2_m
                    rename_cmnd <- paste("exiftool -r -d %Y%m%d_%H%M%S \"-filename<",locfolder,
                                         "/${Directory;$_=(split '\\/',$_)[",i,"]}/${Directory;$_=(split '\\/',$_)[",j,"]}/${Directory;$_=(split '\\/',$_)[",i,"]}_${Directory;$_=(split '\\/',$_)[",j,"]}_${DateTimeOriginal}_${triggermode;s/(.)(.*\\s)(.)(.*)/$1$3/}${sequence;s/(\\d)(.*)/_$1b/}.%e\" ",
                                         input$selectedOrigPhotos_m, sep = "")
                    sysout <- file(errfile_m, "a")
                    sink(sysout,append = T)
                    rcd<-system(rename_cmnd, intern = T)
                    print(paste("~~~~~~~~~~~~~~~~~~~~~~Renaming Log ",Sys.time()," Round One~~~~~~~~~~~~~~~~~~~~~~"))
                    print(rcd)
                    sink()
                    close(sysout)
                  }
                  
                  left.files <- list.files(input$selectedOrigPhotos_m, pattern = "*.JPG$", full.names = TRUE, recursive = TRUE)
                  lf <- length(left.files)
                  
                  left.dirs_2 <- list.dirs(input$selectedBatch)
                  if(lf == 0){
                    unlink(left.dirs_2, recursive = T)
                    new.dirs <- list.dirs(locfolder)
                    new.dirs <- new.dirs[new.dirs != locfolder]
                    todays.dirs <- file.info(new.dirs) %>% 
                      tidyr::separate(col = mtime, into = c("Date","Time"), sep = " ", remove = F) %>% 
                      tibble::rownames_to_column(var = "folder") %>% 
                      dplyr::filter(Date == Sys.Date())
                    new.files <- list.files(todays.dirs$folder[1], pattern = "*.JPG$", full.names = TRUE, recursive = F, include.dirs = FALSE)
                    motionphotos <- new.files[grepl("MD",new.files)]
                    if(length(motionphotos)==0){
                      output$animalphotos_m <- renderUI({
                        list(h1(HTML("<center><font color=\"#3c8dbc\" style = \"text-shadow: 1.5px 1.5px #4d3a7d\"><b>OH WELL...</b></font></center>")),
                             br(),
                             h4("There are no motion-triggered photos in this image set. Fingers crossed the time-triggered photos caught something!"),
                             br(),br(),
                             actionButton(inputId = "enjoyagain",label = HTML("<font size = 4><b>Select New Camera</b></font>"), 
                                          style="color: #fff; background-color: #6faade; border-color: #5491c7; width: 100%"),
                             br(),br(),
                             h4(HTML("<b>NOTE FOR SUBSEQUENT CAMERAS:</b> 
                         <br>Output above <b>will not change</b> until you click <b>Rename Photos</b> again"))
                        )
                      })
                    } else{
                      motiondata <- read_exif(motionphotos, args = "-SourceFile -UserLabel -DateTimeOriginal")
                      subdata <- motiondata %>%
                        tidyr::separate(col = DateTimeOriginal, into = c("Date","Time"), sep = " ", remove = F) %>%
                        mutate(Date = stringr::str_replace_all(Date,pattern = ":", replacement = "/")) %>%
                        mutate(Date.graph = as.Date(Date))
                      
                      output$triggerplot_m <- renderPlotly({
                        triggerplot<-suppressWarnings(ggplot(subdata, aes(x = Date.graph)) +
                                                        ggtitle("Frequency of Motion-Triggered Photos through Time") +
                                                        geom_histogram(aes(text = Date),
                                                                       alpha = 0.3, stat = "count", position = "identity",
                                                                       color = "#00AFBB", fill = "#00AFBB") +
                                                        scale_y_continuous(expand = c(0,0))+
                                                        labs(x = "Date", y = "Frequency") +
                                                        theme_classic() +
                                                        theme(plot.title = element_text(hjust = 0.5)))
                        ggplotly(triggerplot, tooltip = c("text", "count"))
                        
                      })
                      
                      output$slickr_m <- renderSlickR({
                        if(length(motionphotos)<25){
                          slickR::slickR(motionphotos, height = "400px") +
                            settings(dots = T)
                        } else {
                          motion <- sample(motionphotos, 25)
                          slickR::slickR(motion, height = "400px") +
                            settings(dots = T)
                        }
                      })
                      output$animalphotos_m <- renderUI({
                        list(h1(HTML("<center><font color=\"#3c8dbc\" style = \"text-shadow: 1.5px 1.5px #4d3a7d\"><b>SMILE!</b></font></center>")),
                             br(),
                             h4(HTML("You probably caught some animals! Scroll through randomly selected motion photos in the slideshow below. 
                                 When you're ready to move on, hit the <b>Process More Cameras</b> button below the slideshow (if relevant)")),
                             br(),
                             slickROutput("slickr_m"),
                             br(),br(),
                             actionButton(inputId = "enjoyagain_m",label = HTML("<font size = 4><b>Process More Cameras</b></font>"), 
                                          style="color: #fff; background-color: #6faade; border-color: #5491c7; width: 100%"),
                             br(),br(),
                             h4(HTML("<b>NOTE FOR SUBSEQUENT CAMERAS:</b> 
                         <br>Output above <b>will not change</b> until you click <b>Rename Photos</b> again"))
                        )
                      })
                    }
                    nf <- length(new.files)
                    if(nf <= 5){
                      rdf <- read_exif(new.files, args = "-SourceFile -UserLabel -DateTimeOriginal -TriggerMode")
                      output$renametable1_m <- DT::renderDataTable({
                        (rdf)},
                        options = list(pageLength = 1,
                                       lengthMenu = c(1,3,5),
                                       scrollX = TRUE,
                                       autoWidth = TRUE
                        )
                      )
                    } else {
                      rdf<- read_exif(new.files[(nf-4):nf], args = "-SourceFile -UserLabel -DateTimeOriginal -TriggerMode")
                      output$renametable1_m <- DT::renderDataTable({
                        (rdf)},
                        options = list(pageLength = 1,
                                       lengthMenu = c(1,3,5),
                                       scrollX = TRUE,
                                       autoWidth = TRUE
                        )
                      ) 
                    }
                    
                    list(h1(HTML("<center><font color=\"#3c8dbc\" style = \"text-shadow: 1.5px 1.5px #4d3a7d\"><b>ALL DONE!</b></font></center>")),
                         br(),
                         h4(HTML(paste0("You finished renaming and moving photos from <font face=\"Courier New\">", input$selectedOrigPhotos_m,
                                        "</font> to <font face=\"Courier New\">", locfolder, "</font>"))),
                         br(),
                         h4(HTML("There are <b>", lf, " photos left</b> to rename")),
                         br(),
                         actionButton(inputId = "enjoyphotos_m",label = HTML("<font size = 4><b>ENJOY Probable Animal Photos</b></font>"), 
                                      style="color: #fff; background-color: #6faade; border-color: #5491c7; width: 100%"),
                         br(),br(),
                         dataTableOutput(outputId = "renametable1_m"),
                         br(),br(),
                         h4(HTML("<b>NOTE FOR SUBSEQUENT CAMERAS:</b> 
                             <br>Output above <b>will not change</b> until you click <b>Rename Photos</b> again"))
                    )
                    
                  } else {
                    
                    invisible(lapply(left.dirs_2, function(x) {
                      fi <- file.info(x)
                      if(is.na(fi$isdir) | fi$isdir == T) {
                        f <- list.files(x, all.files=TRUE, recursive=TRUE, full.names=TRUE)
                        sz <- sum(file.info(f)$size)
                        if (sz==0L) unlink(x, recursive = T)
                      }
                    }))
                    new.dirs <- list.dirs(locfolder)
                    new.dirs <- new.dirs[new.dirs != locfolder]
                    todays.dirs <- file.info(new.dirs) %>% 
                      tidyr::separate(col = mtime, into = c("Date","Time"), sep = " ", remove = F) %>% 
                      tibble::rownames_to_column(var = "folder") %>% 
                      dplyr::filter(Date == Sys.Date())
                    new.files <- list.files(todays.dirs$folder[1], pattern = "*.JPG$", full.names = TRUE, recursive = F, include.dirs = FALSE)
                    motionphotos <- new.files[grepl("MD",new.files)]
                    if(length(motionphotos)==0){
                      output$animalphotos_m <- renderUI({
                        list(h1(HTML("<center><font color=\"#3c8dbc\" style = \"text-shadow: 1.5px 1.5px #4d3a7d\"><b>OH WELL...</b></font></center>")),
                             br(),
                             h4("There are no motion-triggered photos in this image set. Fingers crossed the time-triggered photos caught something!"),
                             br(),br(),
                             actionButton(inputId = "enjoyagain_m",label = HTML("<font size = 4><b>Process More Cameras</b></font>"), 
                                          style="color: #fff; background-color: #6faade; border-color: #5491c7; width: 100%"),
                             br(),br(),
                             h4(HTML("<b>NOTE FOR SUBSEQUENT CAMERAS:</b> 
                         <br>Output above <b>will not change</b> until you click <b>Rename Photos</b> again"))
                        )
                      })
                    } else{
                      motiondata <- read_exif(motionphotos, args = "-SourceFile -UserLabel -DateTimeOriginal")
                      subdata <- motiondata %>%
                        tidyr::separate(col = DateTimeOriginal, into = c("Date","Time"), sep = " ", remove = F) %>%
                        mutate(Date = stringr::str_replace_all(Date,pattern = ":", replacement = "/")) %>%
                        mutate(Date.graph = as.Date(Date))
                      
                      output$triggerplot_m <- renderPlotly({
                        triggerplot<-suppressWarnings(ggplot(subdata, aes(x = Date.graph)) +
                                                        ggtitle("Frequency of Motion-Triggered Photos through Time") +
                                                        geom_histogram(aes(text = Date),
                                                                       alpha = 0.3, stat = "count", position = "identity",
                                                                       color = "#00AFBB", fill = "#00AFBB") +
                                                        scale_y_continuous(expand = c(0,0))+
                                                        labs(x = "Date", y = "Frequency") +
                                                        theme_classic() +
                                                        theme(plot.title = element_text(hjust = 0.5)))
                        ggplotly(triggerplot, tooltip = c("text", "count"))
                        
                      })
                      
                      output$slickr_m <- renderSlickR({
                        if(length(motionphotos)<25){
                          slickR::slickR(motionphotos, height = "400px") +
                            settings(dots = T)
                        } else {
                          motion <- sample(motionphotos, 25)
                          slickR::slickR(motion, height = "400px") +
                            settings(dots = T)
                        }
                      })
                      output$animalphotos_m <- renderUI({
                        list(h1(HTML("<center><font color=\"#3c8dbc\" style = \"text-shadow: 1.5px 1.5px #4d3a7d\"><b>SMILE!</b></font></center>")),
                             br(),
                             h4(HTML("You probably caught some animals! Scroll through randomly selected motion photos in the slideshow below. 
                                 When you're ready to move on, hit the <b>Process More Cameras</b> button below the slideshow (if relevant)")),
                             br(),
                             slickROutput("slickr_m"),
                             br(),br(),
                             actionButton(inputId = "enjoyagain_m",label = HTML("<font size = 4><b>Process More Cameras</b></font>"), 
                                          style="color: #fff; background-color: #6faade; border-color: #5491c7; width: 100%"),
                             br(),br(),
                             h4(HTML("<b>NOTE FOR SUBSEQUENT CAMERAS:</b> 
                         <br>Output above <b>will not change</b> until you click <b>Rename Photos</b> again"))
                        )
                      })
                    }
                    nf <- length(new.files)
                    if(nf <= 5){
                      rdf <- read_exif(new.files, args = "-SourceFile -UserLabel -DateTimeOriginal -TriggerMode")
                      output$renametable1_m <- DT::renderDataTable({
                        (rdf)},
                        options = list(pageLength = 1,
                                       lengthMenu = c(1,3,5),
                                       scrollX = TRUE,
                                       autoWidth = TRUE
                        )
                      )
                    } else {
                      rdf<- read_exif(new.files[(nf-4):nf], args = "-SourceFile -UserLabel -DateTimeOriginal -TriggerMode")
                      output$renametable1_m <- DT::renderDataTable({
                        (rdf)},
                        options = list(pageLength = 1,
                                       lengthMenu = c(1,3,5),
                                       scrollX = TRUE,
                                       autoWidth = TRUE
                        )
                      ) 
                    }
                    
                    
                    list(h1(HTML("<center><font color=\"#3c8dbc\" style = \"text-shadow: 1.5px 1.5px #4d3a7d\"><b>HMM...</b></font></center>")),
                         br(),
                         h4(HTML(paste0("You finished renaming and moving photos from <font face=\"Courier New\">", 
                                        input$selectedOrigPhotos_m, "</font> to <font face=\"Courier New\">", locfolder, 
                                        "</font>, but there are still <b>", lf, 
                                        " photos left</b> in the original folder."))),
                         br(),
                         h4(HTML(paste0("</b>Please check ",errfile_m, " for information regarding this error,
                                                      and/or <a href=\"mailto:amanda.carr@idfg.idaho.gov\">send us the file</a> for troubleshooting"))),
                         br(),
                         actionButton(inputId = "enjoyphotos_m",label = HTML("<font size = 4><b>ENJOY Probable Animal Photos</b></font>"), 
                                      style="color: #fff; background-color: #6faade; border-color: #5491c7; width: 100%"),
                         br(),br(),
                         dataTableOutput(outputId = "renametable1_m")
                    )
                    
                  }
                }
                
              }
            }
          } # shiny progress
        ) # shiny progress
      }) # isolate
    }
    
  })
  
  
  
  
observeEvent(input$enjoyphotos_m, {
    updateTabsetPanel(session=session, "tabs",
                      selected = 'enjoy_mult'
    )
  })
  
  observeEvent(input$enjoyagain_m, {
    updateTabsetPanel(session=session, "tabs",
                      selected = 'rename_mult'
    )
  })
  
}
