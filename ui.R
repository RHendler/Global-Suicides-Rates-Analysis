# ====================  UI ========================================================================


ui <- fluidPage(
                 
  includeCSS("www/styles.css"),
  
# ==================== Header =================================================================
                 
  fluidRow( div(id="titlePanel",br(),
                
                titlePanel(h2("Global Suicide Rates Analysis")),br())),
  
 column(11,subset = 1,tabsetPanel(
   
# ====================  Tab World view  ========================================================================
   
tabPanel("World View",
         fluidRow(column(10,offset=1, 
         h3("Welcome to the app for global suicide rates analysis. You can observe
            and analyze global suicide rates information for the time period 1985-2016, 
            and explore its relation to various factors. Click on the factor button or on
            a country's circle for additional information."))),
         hr(),
 fluidRow(
  column(4,offset=1,br(),
          DT::dataTableOutput("countries.table",width="100%")),
 
  column(6,br(), 
          selectInput(inputId = "Analyze",
                      label = "Select Factor",
                      choices = list("Average Suicide Rate" = "cases per 100k.pop",
                                     "Number of cases" = "total number of suicide cases")),
          leafletOutput("World_Map",width="120%", height = 450))),br(),
          fluidRow(h5("Created by Roi Hendler © 2019"))
 ),

# ====================  Tab Country view  ========================================================================
   
tabPanel("Country View", fluid=TRUE,
         
    fluidRow(column(10,offset=1, 
    h3("The following graphic presents detailed informaion 
        regarding suicide rates in a specific country. 
        Use the control panel on the left to filter the data."))),hr(),
         
         sidebarLayout(
  
  sidebarPanel(id="sidebarPanel", width=2,br(),
              
    sliderInput(
      inputId = "Years_Slider2",
      label = strong("Years"), 
      min = 1985, 
      max = 2016,
      value = list(1985,2016),
      round = TRUE,
      ticks = FALSE,
      dragRange=TRUE,
      sep="",
      width = "100%"),
      fluidRow(br(),
                            
    radioButtons(inputId ="Age",label = strong("Ages:"), 
                     choices=c("All"="All_Ages","5-14"="5-14 years",
                   "15-24"="15-24 years","25-34"="25-34 years",
                   "35-54"="35-54 years","55-74"="55-74 years",
                   "75+"="75+ years")),
    radioButtons(inputId ="Sex",label = strong("Sex:"), 
                     choices=c("Both"="Both_Sexes","Females"="female",
                               "Males"="male","Compare"="Compare_Sexes"))
      )),

 mainPanel(
   fluidRow(column(11,offset = 1,br(),
    selectInput(
       inputId = "Country",
       label = "Select Country",
       width = "35%",
       choices = unique(data$country),
       selected = "Germany"),br(),
             
    plotOutput("Plot", width = "70%",height=430),
    fluidRow(br(),h5("Created by Roi Hendler © 2019")))))

 )),

# ====================  Tab Comparison view  ========================================================================

tabPanel("Comparison View", fluid=TRUE,
         
 fluidRow(column(10,offset=1, 
        h3("The following graphics compare the the suicide rate in different
        countries. Tukey-Anova test results are available in the table on the right."))),hr(),
         
 sidebarLayout(
  
  sidebarPanel(id="sidebarPanel",width=2,br(),
     
    sliderInput(
       inputId = "Years_Slider3",
       label = strong("Years"), 
       min = 1985, 
       max = 2016,
       round = TRUE,
       ticks = FALSE,
       value = list(1985,2016),
       dragRange=TRUE,
       sep="",
       width = "100%"),
    
      fluidRow(br(),
                 
      radioButtons(inputId ="Age_c",label = strong("Ages:"), 
                            choices=c("All"="All_Ages","5-14"="5-14 years",
                                      "15-24"="15-24 years","25-34"="25-34 years",
                                      "35-54"="35-54 years","55-74"="55-74 years",
                                      "75+"="75+ years")),              
      radioButtons(inputId ="Sex_c",label = strong("Sex:"), 
                            choices=c("Both"="Both_Sexes","Females"="female","Males"="male"))
 )),

  mainPanel(br(),fluidRow(column(7,offset=1,
                                 
          selectizeInput("checked_countries","Select Countries",width = "75%",
                         choices = unique(data$country),
                         selected=c("Germany","France","United States","United Kingdom"), multiple=TRUE,
                         options = list(maxItems = 15,highlight=TRUE,closeAfterSelect=TRUE)),br(),
          plotOutput("Comparison_ALL.l.Plot", height=300, width = 550),hr(),
          plotOutput("Comparison_ALL.Plot", height=250, width = 550)),
          
          column(2,offset = 1,br(),
          DT::dataTableOutput("tukey.table",width="200%"),br(),br(),
          fluidRow(h5("Created by Roi Hendler © 2019")))
          
          ))
 )),

# ====================  Tab correlation ========================================================================

tabPanel("Correlations", fluid=TRUE, 
         fluidRow(column(10,offset=1, 
        h3("Here you can explore global correlations between suicide rates to
        several financial and climatic factors. Every dot represents a different country. The red line 
        indicates the direction and the strength of the correlation. Significance is shown by a 
           p-value < 0.05"))),hr(),
         
         
 sidebarLayout(
  
    sidebarPanel(id="sidebarPanel",width=2,br(),
               
     sliderInput(
       inputId = "Years_Slider4",
       label = strong("Years"), 
       min = 1985, 
       max = 2016,
       value = list(1985,2016),
       round = TRUE,
       ticks = FALSE,
       dragRange=TRUE,
       sep="",
       width = "100%"),
               
       fluidRow(br(),
                        
      radioButtons(inputId ="Age_cor",label = strong("Ages:"), 
                   choices=c("All"="All_Ages","5-14"="5-14 years",
                             "15-24"="15-24 years","25-34"="25-34 years",
                             "35-54"="35-54 years","55-74"="55-74 years",
                             "75+"="75+ years")),              
      radioButtons(inputId ="Sex_cor",label = strong("Sex:"), 
                   choices=c("Both"="Both_Sexes","Females"="female","Males"="male"))
      )),
  
  mainPanel(br(),column(11,offset = 1,
    fluidRow(
      column(4,
       selectInput(label = "Select Factor",inputId = "factor",
                   choices = c("Gross Domestic Product",
                               "Human Developement Index",
                               "Gini Index",
                               "Temperature")),
       plotOutput("cor.plots",width = 500, height = 400)),
        
      column(4,offset = 3,
           br(),br(),br(),br(),
           wellPanel(textOutput("Text")))
      ),
        fluidRow(h5("Created by Roi Hendler © 2019")))
))),

# ====================  Tab About ========================================================================

tabPanel("About & Contact",br(),
         
      fluidRow(column(4,offset =2,h2("About Me"),
             HTML(paste(readLines("texts/about_me.txt"), collapse = ""))
             
             # fluidRow(column(12, actionLink(
             #   inputId = "contact", 
             #   label = tagList(icon("envelope"), " Contact / Hire Me")
             # ),
             
              ),
             

             column(4,offset =1,fluidRow(column(9,offset=3,
              br(),br(),br(),
              img(height = 320,width = 320,src="me.png"))))

              ),
              
      hr(),
      fluidRow(column(4,offset = 2, br(), br(),plotOutput("Plot.small", height = 320,width = 370)),
               column(5,offset = 1,br(),h2("About the App"),
               HTML(paste(readLines("texts/about_app.txt"), collapse = "")),
               actionLink(inputId = "sources",label = tagList(icon("file"), " List of Sources"))),hr(),
               br(),br(),br())
             
  )

# ============================================================================================

)))


