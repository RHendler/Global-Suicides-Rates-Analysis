server <- function(input, output){
  
# ====================  Tab World view  ===================================================
  
output$World_Map <- renderLeaflet(pre_map(df = Final_Label_Info))
    
 observe({
  
   Show <- NULL
   ScaleNum <- NULL
   Rank<-input$Analyze
  
  if(input$Analyze=="cases per 100k.pop"){
    Rank <- Final_Label_Info$rank_rate
    ScaleNum <- Final_Label_Info$X8
    Show <- paste(tags$b("Suicide Rate: "),
                  format(round(as.numeric(Final_Label_Info$X8,1), 1), 
                                 nsmall=1, big.mark=","),"per 100k")
    
    colors_palette <- colorNumeric(
      palette = c("#5FBF00", "#CD5C33"),
      domain = Final_Label_Info$X8)   
    }

  else if(input$Analyze=="total number of suicide cases"){
    Rank <- Final_Label_Info$rank_total.cases
    ScaleNum <- Final_Label_Info$X4
    Show <- paste(tags$b("Suicide Cases: "),
                  format(round(as.numeric(Final_Label_Info$X4,1), 1), 
                                            nsmall=0, big.mark=","))
    
    colors_palette <- colorNumeric(
      palette = c("#5FBF00", "#CD5C33"),
      domain = Final_Label_Info$X4)}

   leafletProxy(mapId="World_Map") %>% 
     clearMarkers() %>%
     clearControls() %>%
    addCircleMarkers(
       Final_Label_Info$Coo1,
       Final_Label_Info$Coo2, 
       fillColor = colors_palette(ScaleNum),color="black", fillOpacity  = 0.9,
       radius = scales::rescale(ScaleNum,to=c(4,17)),stroke = TRUE,
       popup = paste0(  "<h4> <b>",Final_Label_Info$X1,"</h4>",
               "<h4>","Rank: ","</b>",Rank,"/",length(Final_Label_Info$X1),
                br(),Show, br(),"(",Final_Label_Info$X2,")","</h4>"),
                popupOptions = popupOptions(minWidth = 240, maxWidth = 270, maxHeight = 120)) %>%
       addLegend(
         position = "bottomleft",
         pal = colors_palette,
         values = ScaleNum[!is.na(ScaleNum)],
        # title = paste()
         
         opacity = 1
       )})

 output$countries.table = DT::renderDataTable(
   Final_Label_Info %>% select(X1,X4,X8),colnames = c('Country','Total cases','Suicide rate (per 100k)'),
                        options = list(searching = FALSE, paging = FALSE, autoWidth = TRUE,
                        scrollX=FALSE, sScrollY = '150vh',scrollCollapse = TRUE),rownames= FALSE,
                        extensions = list("Scroller"))
 

# ====================  Tab Country view  ========================================================================
  
PlotList <- reactive({
  
  Sex_input<- input$Sex
  if(input$Sex=="Compare_Sexes"){Sex_input<-c("female","male")}
  else if(input$Sex=="Both_Sexes"){Sex_input<-c("female","male","x")}
  
  Age_input<-input$Age
  if(input$Age=="All_Ages"){Age_input<-c("5-14 years","15-24 years","25-34 years","35-54 years","55-74 years","75+ years")}
  
  list(input$Country,
       as.numeric(input$Years_Slider2[1]),
       as.numeric(input$Years_Slider2[2]),
       (as.list(Sex_input)),
       (as.list(Age_input)))
})
  
output$Plot<- renderPlot(P.data(PlotList()))
  
# ====================  Tab Comparison view  ========================================================================
  
PlotList_ALL <- reactive({
  
  Country_input_c <- input$checked_countries
  
  Sex_input_c <- input$Sex_c
  if(input$Sex_c=="Both_Sexes"){Sex_input_c <- c("female","male","x")}
  
  Age_input_c <- input$Age_c
  if(input$Age_c=="All_Ages"){Age_input_c<-c("5-14 years","15-24 years","25-34 years","35-54 years","55-74 years","75+ years")}
  
  PlotList_ALL <- list(Country_input_c,
       as.numeric(input$Years_Slider3[1]),
       as.numeric(input$Years_Slider3[2]),
       (as.list(Sex_input_c)),
       (as.list(Age_input_c)))
})
  
  
  output$Selectd_Countries <- renderPrint({ list(input$checked_countries) })
  
  output$Comparison_ALL.Plot <- renderPlot({PP.compare(PlotList_ALL())})
  
  output$Comparison_ALL.l.Plot <- renderPlot({LL.compare(PlotList_ALL())})
  
  output$tukey.table = DT::renderDataTable(Anova.compare.f(PlotList_ALL()),options = list(lengthChange = FALSE))
  
# ====================  Tab correlations =====================================================================
  
  PlotList_COR <- reactive({
    
    Sex_input_cor <- input$Sex_cor
    if(input$Sex_cor=="Both_Sexes"){Sex_input_cor <- c("female","male")}
    
    Age_input_cor <- input$Age_cor
    if(input$Age_cor=="All_Ages"){Age_input_cor<-c("5-14 years","15-24 years","25-34 years","35-54 years","55-74 years","75+ years")}
    
    PlotList_ALL <- list(as.numeric(input$Years_Slider4[1]),
                         as.numeric(input$Years_Slider4[2]),
                         (as.list(Sex_input_cor)),
                         (as.list(Age_input_cor)),
                         input$factor)
  })
  
  PlotList_SIG <- reactive({
    
    Sex_input_cor <- input$Sex_cor
    if(input$Sex_cor=="Both_Sexes"){Sex_input_cor <- c("female","male")}
    
    Age_input_cor <- input$Age_cor
    if(input$Age_cor=="All_Ages"){Age_input_cor<-c("5-14 years","15-24 years","25-34 years","35-54 years","55-74 years","75+ years")}
    
    PlotList_ALL <- list(as.numeric(input$Years_Slider4[1]),
                         as.numeric(input$Years_Slider4[2]),
                         (as.list(Sex_input_cor)),
                         (as.list(Age_input_cor)),
                         input$factor)
  })
  

  output$cor.plots <- renderPlot({Cor.plots(PlotList_COR())})
  
  txt <- reactive({
    
    if(input$factor == "Gross Domestic Product"){
      HTML(paste(readLines("texts/GDP_cap.txt"), collapse = ""))}
    else if(input$factor == "Human Developement Index"){
      HTML(paste(readLines("texts/HDI.txt"), collapse = ""))}
    else if(input$factor == "Gini Index"){
      HTML(paste(readLines("texts/Gini.txt"), collapse = ""))}
    else if(input$factor == "Temperature"){
      HTML(paste(readLines("texts/Temperature.txt"), collapse = ""))}
  })
  
  output$Text <- renderText(txt())
  
  # ====================  Tab About ========================================================================
  
  output$Plot.small <- renderPlot(P.data(PlotList()))
 
  observeEvent(input$sources, {
    output$status <- renderUI(NULL)
    showModal(sources_modal)
  })
  
   
}
