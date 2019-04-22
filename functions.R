#=================================  Map  =================================

pre_map <- function(df) {
  leaflet(data = Final_Label_Info) %>%
    addProviderTiles("Stamen.Terrain",options = providerTileOptions(minZoom = 1)) %>%
    setView(lat = 26, lng = 10, zoom = 1) %>% 
    setMaxBounds(lng1 = 260,lng2 = -181,lat1 =90,lat2 = -90)
  }


#=================================  Plot country  =================================


P.data <- function(List){
  
  Country<-List[1]
  start<-List[2]
  end<-List[3]
  Sex<-List[[4]]
  Age<-List[[5]]
  
 Out.f.data <- data %>% 
   filter(
   country == Country&
   year >= start &
   year <= end &
   sex %in% Sex &
   age %in% Age)
 
 data$year <- as.factor(data$year)
  
  if(length(Sex)==2 & length(Age)>1){
    return(ggplot(Out.f.data,aes(x=Out.f.data$year,y=suicides.100k.pop,fill=age))+
           geom_col(width = 0.7)+
           facet_wrap(~sex,nrow = 2)+
           ggtitle("Suicide rates per year")+
           theme_economist() + 
           theme(panel.spacing = unit(2, "lines"))+
           ylab("cases/100k")+
           xlab("Year")+
           theme(legend.position="bottom",legend.title=element_blank(),legend.spacing.x = unit(0.5, 'cm'))+
           labs(fill="Age Groups")+
           scale_x_continuous(limits = c(round(min(Out.f.data$year),0), round(max(Out.f.data$year),0))))
  }
            
    else if(length(Sex)==3 & length(Age)==1){
       return(ggplot(Out.f.data,aes(x=year,y=suicides.100k.pop))+
              geom_col(fill="#4279E8",width = 0.7)+
              ggtitle("Suicide rates per year")+
                theme_economist() + 
                ylab("cases/100k")+
              xlab("Year")+
                scale_x_continuous(limits = c(round(min(Out.f.data$year),0), round(max(Out.f.data$year),0))))
    } 
 
       else if(length(Sex)==3 & length(Age)>1){
         return(ggplot(Out.f.data,aes(x=year,y=suicides.100k.pop,fill=age))+
                  geom_col(width = 0.7)+
                  ggtitle("Suicide rates per year")+
                  theme_economist() + 
                  ylab("cases/100k")+
                  xlab("Year")+
                  theme(legend.position="bottom",legend.title=element_blank(),legend.spacing.x = unit(0.5, 'cm'))+
                  labs(fill="Age Groups")+
                  scale_x_continuous(limits = c(round(min(Out.f.data$year),0), round(max(Out.f.data$year),0))))
       } 
       
         else if(length(Sex)==2 & length(Age)==1){
         return(ggplot(Out.f.data,aes(x=year,y=suicides.100k.pop))+
                geom_col(fill="#4279E8",width = 0.7)+
                facet_wrap(~sex,nrow = 2)+
                theme_economist() + 
                theme(panel.spacing = unit(2, "lines"))+
                ggtitle("Suicide rates per year")+
                ylab("cases/100k")+
                xlab("Year")+
                labs(fill="Age Groups")+
                scale_x_continuous(limits = c(round(min(Out.f.data$year),0), round(max(Out.f.data$year),0))))
         }
      
           else if(length(Sex)==1 & length(Age)>1){
             return(ggplot(Out.f.data,aes(x=year,y=suicides.100k.pop,fill=age))+
                      geom_col(width = 0.7)+
                      ggtitle("Suicide rates per year")+
                      theme_economist() + 
                      ylab("cases/100k")+
                      xlab("Year")+
                      theme(legend.position="bottom",legend.title=element_blank(),legend.spacing.x = unit(0.5, 'cm'))+
                      labs(fill="Age Groups")+     
                      scale_x_continuous(limits = c(round(min(Out.f.data$year),0), round(max(Out.f.data$year),0))))
             
             }
                        
             else if(length(Sex)==1 & length(Age)==1){
                    return(ggplot(Out.f.data,aes(x=year,y=suicides.100k.pop))+
                           geom_col(fill="#4279E8",width = 0.7)+
                           ggtitle("Suicide rates per year")+
                           theme_economist() + 
                           ylab("cases/100k")+
                           xlab("Year")+
                           scale_x_continuous(limits = c(round(min(Out.f.data$year),0), round(max(Out.f.data$year),0))))
               }
                           else{return("Error")}
          }


#=================================  Plots comparison  =================================


#LIST1 <- list("Germany",1985,2016,list("female","male","x"),list("5-14 years","15-24 years","25-34 years","35-54 years","55-74 years","75+ years"))
# Var <- P.data(LIST)
# table(Var)
# L.compare(LIST)
# P.compare(LIST)
# LIST_3 <- list(list("Israel","Germany","Albania"), 1992,1995,list("female","male"),list("15-24 years","75+ years"))
# test <- Anova.compare.f(LIST_3)

 
PP.compare <- function(List){
  countries<-List[[1]]
  start<-List[2]
  end<-List[3]
  Sex<-List[[4]]
  Age<-List[[5]]
  data_compared <- data %>%
    select(country,year,sex,age,suicides.100k.pop) %>%
    filter(country %in% countries,
           year >= start,
           year <= end,
           sex %in% Sex,
           age %in% Age) %>%
    group_by(country,year) %>%
    summarise(total_suicides.100k.pop = sum(suicides.100k.pop,na.rm = TRUE))
  
  Box.all <- ggplot(data = data_compared,
                    aes(x=country,y=total_suicides.100k.pop,color=country))+
                    geom_boxplot(width=0.6,size=1)+
                    ggtitle("Average suicide rates by country")+
                    ylab("Average Suicide Rate")+
                    theme_economist() + 
                    theme(legend.position="none")+
                    scale_fill_economist()
  return(Box.all)
}


  LL.compare <- function(List){
    countries<-List[[1]]
    start<-List[2]
    end<-List[3]
    Sex<-List[[4]]
    Age<-List[[5]]
    
    data_compared <- data %>%
      select(country,year,sex,age,suicides.100k.pop) %>%
      filter(country %in% countries,
             year >= start,
             year <= end,
             sex %in% Sex,
             age %in% Age) %>%
      group_by(country,year) %>%
      summarise(total_suicides.100k.pop = sum(suicides.100k.pop,na.rm = TRUE))
    
    Line.lc <- ggplot(data = data_compared,
                mapping=aes(x=year,y=total_suicides.100k.pop,group=country,color=country))+
                        geom_line(size=1.3)+
                        geom_point(size=2)+
                        theme_linedraw()+
                        ggtitle("Suicide rates by country")+
                        ylab("Suicide rate")+
                        theme_economist() + 
                        theme(legend.position="bottom",legend.title=element_blank(),legend.spacing.x = unit(0.5, 'cm'))+
                         scale_fill_economist()+
                        scale_x_continuous(limits = c(min(data_compared$year), 
                                                      max(data_compared$year)))
          return(Line.lc)
  }
  
  
#=================================  ANOVA test =========================================================
  
  Anova.compare.f <- function(List){
    countries<-List[[1]]
    start<-List[2]
    end<-List[3]
    Sex<-List[[4]]
    Age<-List[[5]]
    
    data_compared <- data %>%
      select(country,year,sex,age,suicides.100k.pop) %>%
      filter(country %in% countries,
             year >= start,
             year <= end,
             sex %in% Sex,
             age %in% Age) %>%
      group_by(country,year) %>%
      summarise(total_suicides.100k.pop = sum(suicides.100k.pop,na.rm = TRUE))
    
    Anova.compare<- aov(total_suicides.100k.pop ~ country,data_compared)
    
    summary(Anova.compare) # result ANOVA test
    
    TukeyTable <- TukeyHSD(Anova.compare)$country%>%  # result Tukey test
                  as.data.frame() %>%
                  tibble::rownames_to_column(var = "Pair") %>%
                  transmute(
                  Pair, 
                 `P-Value` = round(`p adj`, 5), 
                  Significance = `p adj` < 0.05      )
                
    return(TukeyTable)
    
  }


#=================================  Plots correlations  =================================
  
  Cor.plots <- function(List){
    start<-List[1]
    end<-List[2]
    Sex<-List[[3]]
    Age<-List[[4]]
    factor<-List[5]
    
    data_cor <- data %>%
      select(country,year,sex,age,suicides.100k.pop,gdp_per_capita....,HDI.for.year) %>%
      filter(year >= start,
             year <= end,
             sex %in% Sex,
             age %in% Age) %>%
      group_by(country,year) %>%
      summarise(total_suicides.100k.pop = sum(suicides.100k.pop,na.rm = TRUE),
                GDP_capita = mean(gdp_per_capita....,na.rm = TRUE),
                Avg.HDI = mean(HDI.for.year,na.rm = TRUE)) %>% 
      group_by(country) %>% 
      summarise(Avg.rate = mean(total_suicides.100k.pop,na.rm = TRUE),
                GDP_capita = mean(GDP_capita,na.rm = TRUE),
                Avg.HDI = mean(Avg.HDI,na.rm = TRUE))
  
         if (factor =="Gini Index"){data.cor.Gini <- merge(data_cor,Gini_data.cor,by="country")
         
     ggplot(data = data.cor.Gini,aes(x=avg.Gini,y=Avg.rate))+
     geom_point(size = 2,aes(x=avg.Gini,y=Avg.rate),color="black")+
     geom_smooth(method='lm',color="red",na.rm=TRUE,se=FALSE)+
     ggtitle("Suicide rates Vs. Average Gini Index")+
     ylab("Average Suicide Rate (per 100.k)")+
     xlab("Average Gini Index")+
     theme(axis.text.x = element_text(angle=65, vjust=0.7))+
     theme_economist()+
     stat_cor(method = "pearson",label.x.npc = 0.7)
   }
    else if (factor =="Temperature"){data.cor.temp <- merge(data_cor,average_temp.cor,by="country")
 
      ggplot(data=data.cor.temp,aes(x=Average,y=Avg.rate))+
      geom_point(size = 2,aes(x=Average,y=Avg.rate),color="black")+
      geom_smooth(method='lm',color="red",na.rm=TRUE,se=FALSE)+
      ggtitle("Suicide rates Vs. Average Anual Temperature (Celsius)")+
      ylab("Average Suicide Rate (per 100.k)")+
      xlab("Average Temperature")+
      theme(axis.text.x = element_text(angle=65, vjust=0.7))+
      theme_economist()+
      stat_cor(method = "pearson",label.x.npc = 0.7)
    }
    else if (factor =="Human Developement Index"){
      ggplot(data=data_cor,aes(x=Avg.HDI,y=Avg.rate))+
        geom_point(size = 2,aes(x=Avg.HDI,y=Avg.rate),color="black")+
        geom_smooth(method='lm',color="red",show.legend = TRUE,na.rm=TRUE,se=FALSE)+
        ggtitle("Suicide rates Vs. Human Developement Index")+
        ylab("Average Suicide Rate (per 100.k)")+
        xlab("Human Developement Index")+
        theme(axis.text.x = element_text(angle=65, vjust=0.7))+
        theme_economist()+
        stat_cor(method = "pearson",label.x.npc = 0.7)
    }
    else if (factor =="Gross Domestic Product"){
      ggplot(data=data_cor,aes(x=GDP_capita,y=Avg.rate))+
        geom_point(size = 2,aes(x=GDP_capita,y=Avg.rate),color="black")+
        geom_smooth(method='lm',color="red",na.rm=TRUE,se=FALSE)+
        ggtitle("Suicide rates Vs. GDP per capita")+
        ylab("Average Suicide Rate (per 100.k)")+
        xlab("GDP per person ($)")+
        theme(axis.text.x = element_text(angle=65, vjust=0.7))+
        theme_economist()+
        stat_cor(method = "pearson",label.x.npc = 0.7)
    }
    
  }
 
