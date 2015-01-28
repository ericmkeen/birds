# test ui

library(shiny)
library(markdown)

shinyUI(
  navbarPage("Seabirds of the Gitga'at Territory_____________",
             ###################################             
             ###################################
             ############################################################################################################################################
             ##########   STUDY DESK    #################################################################################################################
             ############################################################################################################################################
             tabPanel("Study Desk",
                      fluidPage(
                        fluidRow(
                          column(6,h4(textOutput("sp1"))),
                          column(6,h4(textOutput("sp2")))
                        ),
                        fluidRow(
                          column(2,selectInput("grp1",label="",choices=list("Taxon" = "All", 
                                                                            "Alcids" = "Alcids", 
                                                                            "Cormorants" = "Cormorants",
                                                                            "Fowl"="Fowl",
                                                                            "Grebes"="Grebes",
                                                                            "Gulls" = "Gulls",
                                                                            "Jaegers"="Jaegers",
                                                                            "Loons"="Loons",
                                                                            "Other"="Other",
                                                                            "Phalaropes"="Phalaropes",
                                                                            "Shearwaters"="Shearwaters",
                                                                            "Storm petrels"="Storm petrels",
                                                                            "Terns"="Terns"
                                                                            ),width="100%")),
                          column(2,uiOutput("sp1choices")),
                          column(1,uiOutput("plumops1")),
                          column(1,selectInput("motion1",label="",choices=list("Motion" = "All", "Flying" = "FLY", "Sitting" = "SIT","Standing"="RFT"),width="100%")),
                          column(2,selectInput("grp2",label="",choices=list("Taxon" = "All", 
                                                                            "Alcids" = "Alcids", 
                                                                            "Cormorants" = "Cormorants",
                                                                            "Fowl"="Fowl",
                                                                            "Grebes"="Grebes",
                                                                            "Gulls" = "Gulls",
                                                                            "Jaegers"="Jaegers",
                                                                            "Loons"="Loons",
                                                                            "Other"="Other",
                                                                            "Phalaropes"="Phalaropes",
                                                                            "Shearwaters"="Shearwaters",
                                                                            "Storm petrels"="Storm petrels",
                                                                            "Terns"="Terns"
                          ),width="100%")),
                          column(2,uiOutput("sp2choices")),
                          column(1,uiOutput("plumops2")),
                          column(1,selectInput("motion2",label="",choices=list("Motion" = "All", "Flying" = "FLY", "Sitting" = "SIT","Standing"="RFT"),width="100%"))
                        ),
                        
                        fluidRow(
                          column(6,imageOutput("search1",inline=TRUE)),
                          column(6,imageOutput("search2",inline=TRUE))
                        ),
                        fluidRow(
                          column(6,textOutput("info1")),
                          column(6,textOutput("info2"))
                        ),
                        fluidRow(
                          column(2,actionButton("back1",label="Back")),
                          column(2,textOutput("pos1")),
                          column(2,actionButton("next1",label="Next")),
                          column(2,actionButton("back2",label="Back")),
                          column(2,textOutput("pos2")),
                          column(2,actionButton("next2",label="Next"))
                        ),
                        fluidRow(
                          column(6, htmlOutput("notes1")),
                          column(6,htmlOutput("notes2"))
                          
                        )     
                      )
                      
                      
             ),

############################################################################################################################################
##########      QUIZ      #################################################################################################################
############################################################################################################################################
             tabPanel("Quiz",
                      sidebarLayout(
                      sidebarPanel(
                        htmlOutput("posq"),
                        br(),
                        actionButton("quizgo",label=h3("Hit me!")),
                        actionButton("tellme",label=h3("Tell me")),
                        br(),
                        br(),
                        htmlOutput("spq"),
                        htmlOutput("infoq"),  
                        hr(),
                        
                        selectInput("quizgrp",label=h5("Taxon"),choices=list("All" = "All", 
                                                                 "Alcids" = "Alcids", 
                                                                 "Cormorants" = "Cormorants",
                                                                 "Fowl"="Fowl",
                                                                 "Grebes"="Grebes",
                                                                 "Gulls" = "Gulls",
                                                                 "Jaegers"="Jaegers",
                                                                 "Loons"="Loons",
                                                                 "Other"="Other",
                                                                 "Phalaropes"="Phalaropes",
                                                                 "Shearwaters"="Shearwaters",
                                                                 "Storm petrels"="Storm petrels",
                                                                 "Terns"="Terns"
                        ),width="100%"),
                        
                        uiOutput("plumquiz"),
                        
                        selectInput("motionq",label=h5("Motion"),choices=list("All" = "All", "Flying" = "FLY", "Sitting" = "SIT","Standing"="RFT"),width="100%"),                 
                        hr(),  
                        actionButton("gotit",label=h5("Got it!  Remove card from stack")),
                        hr(),
                        htmlOutput("notesq")
                        
                      ),
                      mainPanel(
                      fluidRow(imageOutput("quizpic",inline=TRUE))
                      )
                      )
            ),

############################################################################################################################################
##########       ABOUT      #################################################################################################################
############################################################################################################################################
tabPanel("About",
         fluidRow(
           htmlOutput("about")
           )
         )
            
  )
)
