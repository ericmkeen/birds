library(shiny)
library(reshape2)

# Define server logic for random distribution application
shinyServer(function(input, output) {

#######################################################################################################################################################################
###########     STUDY DESK    #########################################################################################################################################
#######################################################################################################################################################################
#### Set up values for the session
    values <- reactiveValues()
    values$pos1 <- 1
    values$pics1 <- "None"
    values$numpics1 <- 0
    values$pos2 <- 1
    values$pics2 <- "None"
    values$numpics1 <- 0

    ########################################################
    #############   FUNCTIONS  #############################
    ########################################################
    ########################################################
    ##======================================================
    ##### Function to find bird image based on search inputs
    load.db <- function(){
      # Choose Working Directory First!
      #wd <- "C:/Users/RV Bangarang/" # "Samsung 1" 
      #wd <-  "D:/"                   # "Samsung 2" 
      wd <- "/Users/eric.keen/"       # "Mac"  
      
      setwd(paste(wd,"Dropbox/Bangarang Docs/Catalogs/Seabirds/ggbirds",sep=""))
      sib <- read.csv("sibdb.csv",stringsAsFactors=FALSE)
      return(sib)
    }
    ##======================================================
    #
    code2sp <- function(code){
      sib <- load.db()
      codedex <- which(sib$code == code)
      sp <- sib$species[codedex]
      return(sp)
    }
    #
    sp2code <- function(sp){
      sib <- load.db()
      codedex <- which(sib$species == sp)
      code <- sib$code[codedex]
      return(code)
    }
    #
    ##======================================================
    ##### Function to find bird image based on search inputs
    findbird <- function(group="All",sp="All",plum="All",sex="All",motion="All"){
      # Choose Working Directory First!
      #wd <- "C:/Users/RV Bangarang/" # "Samsung 1" 
      #wd <-  "D:/"                   # "Samsung 2" 
      wd <- "/Users/eric.keen/"       # "Mac"  
      
      setwd(paste(wd,"Dropbox/Bangarang Docs/Catalogs/Seabirds/ggbirds",sep=""))
      filename <- list.files()
      jpgs <- which(substr(filename,nchar(filename)-2,nchar(filename))=="jpg" | substr(filename,nchar(filename)-2,nchar(filename))=="JPG")
      filename <- filename[jpgs]
      db <- data.frame(cbind(as.character(filename)))
      names(db)[1] <- "filename"
      db$filename <- as.character(filename)
      
      # Build new columns for the databse
      db$spp <- substring(db$filename,1,4)
      db$plum <- substring(db$filename,6,7)
      db$sex <- substring(db$filename,9,9)
      db$motion <- substring(db$filename,11,13)
      db$index <- seq(1,nrow(db),by=1)
      
      sibcsv <- load.db()
      vgroup <- vector()
      for(i in 1:nrow(db)){
        index <- which(db$spp[i] == sibcsv$code)
        if(length(index)==0){
          vgroup[i] <- "Other"
        }else{
        vgroup[i] <- sibcsv$taxon[index]
      }
      }
      db$group <- vgroup
      
      search <- db
      if(group!="All"){search<-search[search$group==group,]}
      
      if(is.null(sp)==FALSE){
        if(sp!="All"){
          code <- sp2code(sp)
          search<-search[search$sp==code,]
        }
      }
      
      if(is.null(plum)==FALSE){
        if(plum!="All"){search<-search[search$plum==plum,]}
      }
      
      if(motion!="All"){search<-search[search$motion==motion,]}
      
      if(is.null(sex)==FALSE){
      if(sex!="All"){search<-search[search$sex==sex,]}
      }
      
      pics <- search$filename
      #output <- list(pics)      
      return(pics)
    }
    ##======================================================
    #
    birdnotes <- function(code){
      sib <- load.db()
    }
    #
    ##======================================================
    #
    callplum <- function(pic){
      plum <- substr(pic,6,7)
      if(is.na(plum)==FALSE){
      if(plum=="AB"){plum <- "Adult breeding"}
      if(plum=="AN"){plum <- "Adult non-breeding"}
      if(plum=="NB"){plum <- "Non-breeding"}
      if(plum=="FL"){plum <- "Fledgling"}
      if(plum=="JV"){plum <- "Juvenile"}
      if(plum=="SA"){plum <- "Subadult"}
      if(plum=="1W"){plum <- "1st winter"}
      if(plum=="2W"){plum <- "2nd winter"}
      if(plum=="3W"){plum <- "3rd winter"}
      if(plum=="1S"){plum <- "1st summer"}
      if(plum=="AL"){plum <- "All molts"}
      if(plum=="AA"){plum <- "Adult"}
      if(plum=="MX"){plum <- "Mix of molts"}
      }
      return(plum)
    }
    #
    ##======================================================
    
    callmotion <- function(pic){
      motion <- substr(pic,11,13)
      if(is.na(motion)==FALSE){
      if(motion=="FLY"){motion <- "Flying"}
      if(motion=="SIT"){motion <- "Sitting"}
      if(motion=="RFT"){motion <- "Standing"}
      }
      return(motion)
    }
    
    ##======================================================
    
    callsex <- function(pic){
      sex <- substr(pic,9,9)
      if(is.na(sex)==FALSE){
      if(sex=="M"){sex <- "Male"}
      if(sex=="F"){sex <- "Female"}
      if(sex=="E"){sex <- "Either sex"}
      if(sex=="B"){sex <- "Both sexes"}
      }
      return(sex)
    }#
    
    ########################################################
    ########################################################
    #############   OBSERVES  ##############################
    ########################################################
    ########################################################
    #
    ##======================================================
    ##### Next Button 1: Advance to next image when "Next" button is pushed
    observe({
      if(input$next1!=0){
        isolate(values$pos1 <- values$pos1 + 1)
        isolate(if(values$pos1 > values$numpics1){values$pos1 <- 1})
      }
    })
    ##======================================================
    ##### Next Button 2: Advance to next image when "Next" button is pushed
    observe({
      if(input$next2!=0){
        isolate(values$pos2 <- values$pos2 + 1)
        isolate(if(values$pos2 > values$numpics2){values$pos2 <- 1})
      }
    })
    ##======================================================
    ##### Back Button 1: Go back to previous image when "Back" button is pushed
    observe({
    if(input$back1!=0){
       isolate(values$pos1 <- values$pos1 - 1)
       isolate(if(values$pos1 < 1){values$pos1 <- values$numpics1})
    }
    })
    ##======================================================
    ##### Back Button 2: Go back to previous image when "Back" button is pushed
    observe({
      if(input$back2!=0){
         isolate(values$pos2 <- values$pos2 - 1)
         isolate(if(values$pos2 < 1){values$pos2 <- values$numpics2})
      }
    })
    #
    ##======================================================
    ##### Search 1:  If any of the search inputs are changed, do a new search of the bird image DB, 
    ##### and reset the "position" variable for the next button.
    observe({
        values$pics1 <- findbird(group=input$grp1,sp=input$sp1,plum=input$plum1,sex="All",motion=input$motion1)
        values$numpics1 <- length(values$pics1)
        if(length(values$pics1)>=1){
          values$pos1 <- sample(seq(1,length(values$pics1)),size=1)
        }else{
          values$pos1 <- 1
        }
      })
    ##======================================================
    ###### Search 2: If any of the search inputs are changed, do a new search of the bird image DB, 
    ##### and reset the "position" variable for the next button.
    observe({
      values$pics2 <- findbird(group=input$grp2,sp=input$sp2,plum=input$plum2,sex="All",motion=input$motion2)
      values$numpics2 <- length(values$pics2)
      if(length(values$pics2)>=1){
        values$pos2 <- sample(seq(1,length(values$pics2)),size=1)
      }else{
        values$pos2 <- 1
      }
    })
    ##======================================================    #

    ########################################################
    ########################################################
    #############   OUTPUTS   ##############################
    ########################################################
    ########################################################
    
    ########################################################
    ########  DYNAMIC SELECT INPUTS
    output$sp1choices <- renderUI({
      taxon <- input$grp1
      sib <- load.db()
      sp1ops <- sib$species
      if(taxon!="All"){
        sp1ops <- subset(sib$species,sib$taxon==taxon)
      }
      
      if(length(sp1ops)==0){sp1ops <- "All"}
      sp1ops <- c("All",sort(sp1ops)) # Sort options to be alphabetical
      selectInput("sp1",label="",choices=sp1ops,multiple=FALSE)
    })
    ##======================================================
    output$sp2choices <- renderUI({
      taxon <- input$grp2
      sib <- load.db()
      sp2ops <- sib$species
      if(taxon!="All"){
        sp2ops <- subset(sib$species,sib$taxon==taxon)
      }
      
      sp2ops <- c("All",sort(sp2ops)) # Sort options to be alphabetical
      selectInput("sp2",label="",choices=sp2ops,multiple=FALSE)
    })
    ##======================================================
    ### Toggle options for Molt Species 1
    output$plumops1 <- renderUI({
      moltops1 <- c("All")
      if(input$grp1=="Loons"){moltops1 <- c(moltops1,"AB","AN","JV","1S")}
      if(input$grp1=="Grebes"){moltops1 <- c(moltops1,"AB","AN","FL","JV","1W")}
      if(input$grp1=="Shearwaters"){moltops1 <- c(moltops1)}
      if(input$grp1=="Storm petrels"){moltops1 <- c(moltops1)}
      if(input$grp1=="Cormorants"){moltops1 <- c(moltops1,"AB","AN","JV")}
      if(input$grp1=="Fowl"){moltops1 <- c(moltops1,"AB","AN","JV","1W","1S","1Y","AA")}
      if(input$grp1=="Phalaropes"){moltops1 <- c(moltops1,"AB","AN","JV")}
      if(input$grp1=="Jaegers"){moltops1 <- c(moltops1,"AB","AN","JV")}
      if(input$grp1=="Gulls"){moltops1 <- c(moltops1,"AB","AN","JV","1W","1S","2W","3W")}
      if(input$grp1=="Terns"){moltops1 <- c(moltops1,"AB","AN","JV","1W","1Y")}
      if(input$grp1=="Alcids"){moltops1 <- c(moltops1,"AB","AN","FL","JV","1W","1Y","AA","AL")}
      if(input$grp1=="Other"){moltops1 <- c(moltops1)}
      moltops1 <- c(moltops1,"MX")
      selectInput("plum1",label="",choices=moltops1,multiple=FALSE)
    })
    ##======================================================
    ### Toggle options for Molt Species 2
    output$plumops2 <- renderUI({
      moltops2 <- c("All")
      if(input$grp2=="Loons"){moltops2 <- c(moltops2,"AB","AN","JV","1S")}
      if(input$grp2=="Grebes"){moltops2 <- c(moltops2,"AB","AN","FL","JV","1W")}
      if(input$grp2=="Shearwaters"){moltops2 <- c(moltops2)}
      if(input$grp2=="Storm petrels"){moltops2 <- c(moltops2)}
      if(input$grp2=="Cormorants"){moltops2 <- c(moltops2,"AB","AN","JV")}
      if(input$grp2=="Fowl"){moltops2 <- c(moltops2,"AB","AN","JV","1W","1S","1Y","AA")}
      if(input$grp2=="Phalaropes"){moltops2 <- c(moltops2,"AB","AN","JV")}
      if(input$grp2=="Jaegers"){moltops2 <- c(moltops2,"AB","AN","JV")}
      if(input$grp2=="Gulls"){moltops2 <- c(moltops2,"AB","AN","JV","1W","1S","2W","3W")}
      if(input$grp2=="Terns"){moltops2 <- c(moltops2,"AB","AN","JV","1W","1Y")}
      if(input$grp2=="Alcids"){moltops2 <- c(moltops2,"AB","AN","FL","JV","1W","1Y","AA","AL")}
      if(input$grp2=="Other"){moltops2 <- c(moltops2)}
      moltops2 <- c(moltops2,"MX")
      selectInput("plum2",label="",choices=moltops2,multiple=FALSE)
    })

    ########################################################
    ########  IMAGES
    ##======================================================
    ##### Output bird for FRAME 1
    output$search1 <- renderImage({
    list(src=values$pics1[values$pos1],alt="No bird for you!",filetype="image/jpeg")
    },deleteFile=FALSE)
    ##======================================================
    ##### Output bird for FRAME 2
    output$search2 <- renderImage({
      list(src=values$pics2[values$pos2],alt="No bird for you!",filetype="image/jpeg")
    },deleteFile=FALSE)
    #
    ########################################################
    ########  POSITION TEXT
    ##======================================================
    ##### Picture position and pics available for FRAME 1
    output$pos1 <- renderText({
    if(length(values$pics1)==0){paste("0 search results.")}
    if(length(values$pics1)>0)paste(values$pos1," of ",values$numpics1,sep="")
    })
    ##======================================================
    ##### Picture position and pics available for FRAME 2
    output$pos2 <- renderText({
      if(length(values$pics2)==0){paste("0 search results.")}
      if(length(values$pics2)>0)paste(values$pos2," of ",values$numpics2,sep="")
    })
    #
    ########################################################
    ########  SPECIES TITLES
    #
    ####### Species names for FRAME 1
    output$sp1 <- renderText({
      if(length(values$pics1)==0){paste("Species 1")}
      if(length(values$pics1)>0){
        picfile <- values$pics1[values$pos1]
        sp1 <- substr(picfile,1,4)
        sp1 <- code2sp(sp1)
        paste(sp1,sep="")
      }
    })
    ##======================================================
    ####### Species names for FRAME 2
    output$sp2 <- renderText({
      if(length(values$pics2)==0){paste("Species 2")}
      if(length(values$pics2)>0){
        picfile <- values$pics2[values$pos2]
        sp2 <- substr(picfile,1,4)
        sp2 <- code2sp(sp2)
        paste(sp2,sep="")
      }
    })
    #
    ########################################################
    ########  SPP INFO
    output$info1 <- renderText({
      sex <- callsex(values$pics1[values$pos1])
      motion <- callmotion(values$pics1[values$pos1])
      molt <- callplum(values$pics1[values$pos1])
      paste(molt,sex,motion,sep="  |  ")
    })
    ##======================================================
    output$info2 <- renderText({
      sex <- callsex(values$pics2[values$pos2])
      motion <- callmotion(values$pics2[values$pos2])
      molt <- callplum(values$pics2[values$pos2])
      paste(molt,sex,motion,sep="  |  ")
    })
    #
    ########################################################
    ########  SPP ID NOTES
    output$notes1 <- renderUI({
      if(is.null(input$grp1)==FALSE | is.null(input$sp1)==FALSE){
      sp <- substr(values$pics1[values$pos1],1,4)
      sib <- load.db()
      dbi <- which(sib$code==sp)
      isolate({
      
      plum <- substr(values$pics1[values$pos1],6,7)
      ga <- sib$ga.gen[dbi] 
      if(plum=="AB"){ga <- paste(ga,sib$ga.ab[dbi],sep="<br/>")}
      if(plum=="AN"){ga <- paste(ga,sib$ga.an[dbi],sep="<br/>")}
      if(plum=="NB"){ga <- paste(ga,sib$ga.nb[dbi],sep="<br/>")}
      if(plum=="JV"){ga <- paste(ga,sib$ga.jv[dbi],sep="<br/>")}
      if(plum=="SA"){ga <- paste(ga,sib$ga.sa[dbi],sep="<br/>")}
      if(plum=="1W"){ga <- paste(ga,sib$ga.1w[dbi],sep="<br/>")}
      if(plum=="2W"){ga <- paste(ga,sib$ga.2w[dbi],sep="<br/>")}
      if(plum=="3W"){ga <- paste(ga,sib$ga.3w[dbi],sep="<br/>")}
      if(plum=="1S"){ga <- paste(ga,sib$ga.1s[dbi],sep="<br/>")}
      if(plum=="FL"){ga <- paste(ga,sib$ga.fl[dbi],sep="<br/>")}
      if(plum=="AA"){ga <- paste(ga,sib$ga.aa[dbi],sep="<br/>")}
      if(plum=="1Y"){ga <- paste(ga,sib$ga.1y[dbi],sep="<br/>")}
      diags <- paste("<strong>Give-a-ways:</strong> ",ga,"<br/>",sep="<br/>")
      
      tips <- paste("<strong>Tips:</strong> ",sib$tips[dbi],sep="")
      person <- paste("<strong>Personality:</strong> ",sib$personality[dbi],sep="")
      similar <- paste("<strong>Similar species:</strong> ",sib$similar[dbi],sep="")
      id2 <- paste("<strong>Treated as identical to:</strong> ",sib$identical[dbi],sep="")
      local <- paste("<strong>Local status:</strong> ",sib$Local.status[dbi],sep="")
      sciname <- paste("<strong>Latin name:</strong> ",sib$Scientific.name[dbi],sep="")
      family <- paste("<strong>Family:</strong> ",sib$family[dbi],sep="")
      picname <- paste("<strong>Picture file:</strong> ",values$pics1[values$pos1],sep="")
            
      HTML(paste("<br/>",diags,tips,person,similar,id2,local,"<br/>",sciname,family,picname,sep="<br/>"))
      })
      }
    })
    ##======================================================
    output$notes2 <- renderUI({
    if(is.null(input$grp2)==FALSE | is.null(input$sp2)==FALSE){
    sp <- substr(values$pics2[values$pos2],1,4)
    sib <- load.db()
    dbi <- which(sib$code==sp)
    isolate({
     
      plum <- substr(values$pics2[values$pos2],6,7)
      ga <- sib$ga.gen[dbi] 
      if(plum=="AB"){ga <- paste(ga,sib$ga.ab[dbi],sep="<br/>")}
      if(plum=="AN"){ga <- paste(ga,sib$ga.an[dbi],sep="<br/>")}
      if(plum=="NB"){ga <- paste(ga,sib$ga.nb[dbi],sep="<br/>")}
      if(plum=="JV"){ga <- paste(ga,sib$ga.jv[dbi],sep="<br/>")}
      if(plum=="SA"){ga <- paste(ga,sib$ga.sa[dbi],sep="<br/>")}
      if(plum=="1W"){ga <- paste(ga,sib$ga.1w[dbi],sep="<br/>")}
      if(plum=="2W"){ga <- paste(ga,sib$ga.2w[dbi],sep="<br/>")}
      if(plum=="3W"){ga <- paste(ga,sib$ga.3w[dbi],sep="<br/>")}
      if(plum=="1S"){ga <- paste(ga,sib$ga.1s[dbi],sep="<br/>")}
      if(plum=="FL"){ga <- paste(ga,sib$ga.fl[dbi],sep="<br/>")}
      if(plum=="AA"){ga <- paste(ga,sib$ga.aa[dbi],sep="<br/>")}
      if(plum=="1Y"){ga <- paste(ga,sib$ga.1y[dbi],sep="<br/>")}
      diags <- paste("<strong>Give-a-ways:</strong> ",ga,"<br/>",sep="<br/>")
      
      tips <- paste("<strong>Tips:</strong> ",sib$tips[dbi],sep="")
      person <- paste("<strong>Personality:</strong> ",sib$personality[dbi],sep="")
      similar <- paste("<strong>Similar species:</strong> ",sib$similar[dbi],sep="")
      id2 <- paste("<strong>Treated as identical to:</strong> ",sib$identical[dbi],sep="")
      local <- paste("<strong>Local status:</strong> ",sib$Local.status[dbi],sep="")
      sciname <- paste("<strong>Latin name:</strong> ",sib$Scientific.name[dbi],sep="")
      family <- paste("<strong>Family:</strong> ",sib$family[dbi],sep="")
      picname <- paste("<strong>Picture file:</strong> ",values$pics1[values$pos1],sep="")
      
      HTML(paste("<br/>",diags,tips,person,similar,id2,local,"<br/>",sciname,family,picname,sep="<br/>"))
    })
  }
})
    
#######################################################################################################################################################################
###########     QUIZ    #########################################################################################################################################
#######################################################################################################################################################################
values$posq <- 1
values$picsq <- "None"
values$quizorder <- c(1,1,1)
values$quiztell <- FALSE

#
### Toggle options for Molt in Quiz
output$plumquiz <- renderUI({
  moltopsq <- c("All")
  if(input$quizgrp=="Loons"){moltopsq <- c(moltopsq,"AB","AN","JV","1S")}
  if(input$quizgrp=="Grebes"){moltopsq <- c(moltopsq,"AB","AN","FL","JV","1W")}
  if(input$quizgrp=="Shearwaters"){moltopsq <- c(moltopsq)}
  if(input$quizgrp=="Storm petrels"){moltopsq <- c(moltopsq)}
  if(input$quizgrp=="Cormorants"){moltopsq <- c(moltopsq,"AB","AN","JV")}
  if(input$quizgrp=="Fowl"){moltopsq <- c(moltopsq,"AB","AN","JV","1W","1S","1Y","AA")}
  if(input$quizgrp=="Phalaropes"){moltopsq <- c(moltopsq,"AB","AN","JV")}
  if(input$quizgrp=="Jaegers"){moltopsq <- c(moltopsq,"AB","AN","JV")}
  if(input$quizgrp=="Gulls"){moltopsq <- c(moltopsq,"AB","AN","JV","1W","1S","2W","3W")}
  if(input$quizgrp=="Terns"){moltopsq <- c(moltopsq,"AB","AN","JV","1W","1Y")}
  if(input$quizgrp=="Alcids"){moltopsq <- c(moltopsq,"AB","AN","FL","JV","1W","1Y","AA","AL")}
  if(input$quizgrp=="Other"){moltopsq <- c(moltopsq)}
  moltopsq <- c(moltopsq,"MX")
  selectInput("plumq",label=h5("Age / Plumage"),choices=moltopsq,multiple=FALSE)
})
#
##### Search QUIZ:  If any of the search inputs are changed, do a new search of the bird image DB, 
##### and reset the "position" variable for the next button.
observe({
  values$picsq <- findbird(group=input$quizgrp,sp="All",plum=input$plumq,sex="All",motion=input$motionq)
  values$numpicsq <- length(values$picsq)
  values$quizorder <- sample(seq(1,length(values$picsq)),size=values$numpicsq,replace=FALSE)
  values$posq <- 1
 
})

##### Quiz Me Button: Advance to next image in quizorder sequence 
observe({
  if(input$quizgo!=0){
    isolate(values$posq <- values$posq + 1)
    isolate(if(values$posq > values$numpicsq){values$posq <- 1})
    isolate(values$quiztell <- FALSE)
  }
})

##### Tell Me Button: Display species info for image 
observe({
  if(input$tellme!=0){
    isolate(values$quiztell <- TRUE)
  }
})

##### Remove Card Button: Remove card from stack and advance to next 
observe({
  if(input$gotit!=0){
    isolate({
      values$posq <- values$posq + 1
      if(values$posq > length(values$quizorder)){values$posq <- 1}
      values$quiztell <- FALSE

      values$quizorder <- values$quizorder[-(values$posq-1)] 
    })
}
})

########################################################
########  IMAGES
##======================================================
##### Output bird for QUIZ
output$quizpic <- renderImage({
  isolate(values$quiztell <- FALSE)
  
  list(src=values$picsq[values$quizorder[values$posq]],alt="No bird for you!",filetype="image/jpeg")
},deleteFile=FALSE)

########################################################
########  INFO
#
####### Species name for QUIZ
output$spq <- renderUI({
  if(values$quiztell){
  if(length(values$picsq)==0){paste("<strong>Species:</strong> ")}
  if(length(values$picsq)>0){
    picfile <- values$picsq[values$quizorder[values$posq]]
    spq <- substr(picfile,1,4)
    spq <- code2sp(spq)
    HTML(paste("<strong>",spq,"</strong>",sep=""))
  }
  }
})
#
########  SPP INFO
output$infoq <- renderUI({
  if(values$quiztell){
  sexq <- callsex(values$picsq[values$quizorder[values$posq]])
  moltq <- callplum(values$picsq[values$quizorder[values$posq]])
  HTML(paste(moltq,sexq,sep="<br/>"))
  }
})
#
########  SPP ID NOTES
output$notesq <- renderUI({
  if(values$quiztell){
    if(is.null(input$quizgrp)==FALSE){
    sp <- substr(values$picsq[values$quizorder[values$posq]],1,4)
    sib <- load.db()
    dbi <- which(sib$code==sp)
    isolate({

      plum <- substr(values$picsq[values$quizorder[values$posq]],6,7)
      ga <- sib$ga.gen[dbi] 
      if(plum=="AB"){ga <- paste(ga,sib$ga.ab[dbi],sep="<br/>")}
      if(plum=="AN"){ga <- paste(ga,sib$ga.an[dbi],sep="<br/>")}
      if(plum=="NB"){ga <- paste(ga,sib$ga.nb[dbi],sep="<br/>")}
      if(plum=="JV"){ga <- paste(ga,sib$ga.jv[dbi],sep="<br/>")}
      if(plum=="SA"){ga <- paste(ga,sib$ga.sa[dbi],sep="<br/>")}
      if(plum=="1W"){ga <- paste(ga,sib$ga.1w[dbi],sep="<br/>")}
      if(plum=="2W"){ga <- paste(ga,sib$ga.2w[dbi],sep="<br/>")}
      if(plum=="3W"){ga <- paste(ga,sib$ga.3w[dbi],sep="<br/>")}
      if(plum=="1S"){ga <- paste(ga,sib$ga.1s[dbi],sep="<br/>")}
      if(plum=="FL"){ga <- paste(ga,sib$ga.fl[dbi],sep="<br/>")}
      if(plum=="AA"){ga <- paste(ga,sib$ga.aa[dbi],sep="<br/>")}
      if(plum=="1Y"){ga <- paste(ga,sib$ga.1y[dbi],sep="<br/>")}
      diags <- paste("<strong>Give-a-ways:</strong> ",ga,"<br/>",sep="<br/>")
      
      tips <- paste("<strong>Tips:</strong> ",sib$tips[dbi],sep="")
      person <- paste("<strong>Personality:</strong> ",sib$personality[dbi],sep="")
      similar <- paste("<strong>Similar species:</strong> ",sib$similar[dbi],sep="")
      id2 <- paste("<strong>Treated as identical to:</strong> ",sib$identical[dbi],sep="")
      local <- paste("<strong>Local status:</strong> ",sib$Local.status[dbi],sep="")
      sciname <- paste("<strong>Latin name:</strong> ",sib$Scientific.name[dbi],sep="")
      family <- paste("<strong>Family:</strong> ",sib$family[dbi],sep="")
      picname <- paste("<strong>Picture file:</strong> ",values$pics1[values$pos1],sep="")
      
      HTML(paste("<br/>",diags,tips,person,similar,id2,local,"<br/>",sciname,family,picname,sep="<br/>"))
    })
  }
  }
})
#
##### Picture position and pics available for FRAME 1
output$posq <- renderUI({
  if(length(values$picsq)==0){paste("0 search results.")}
  if(length(values$picsq)>0){
    HTML(paste("Stack has <strong> ",values$numpicsq," cards. </strong> <br/> You've got <strong>",length(values$quizorder),"</strong> more to learn.",sep=""))
  }
})

#######################################################################################################################################################################
###########     ABOUT    #########################################################################################################################################
#######################################################################################################################################################################
output$about <- renderUI({
  ab1 <- paste("All photographs were taken by the crew of the RV Bangarang within Gitga'at Waters during summer fieldwork 2013-2015.","<br/><strong>Photographers:</strong>",sep="<br/>")
  ab2 <- paste("Keri Bryan","Will Watson","Katie Qualls","Dylan Padgett","Matt Irwin","Nelle Pierson","Richard Candler","Kelly Beach","Jonathan Carpenter","Eric Keen","<br/>",sep="<br/>")
  ab3 <- paste("Identification information was compiled from many sources but primarily Harrison (1984), Sibley (2014) and allaboutbirds.org <br/>")
  ab4 <- paste("Seabird research and photography by the Bangarang Project are in collaboration with the Gitga'at First Nation and North Coast Cetacean Society.",
"For more information, see www.rvbangarang.wordpress.com",
"Special thanks to Jonathan Carpenter for launching this database.",
"Administered by Eric Keen, ekeen at ucsd dot edu",sep="<br/><br/>")
    
  HTML(paste(ab1,ab2,ab3,ab4,sep="<br/>"))
})


})