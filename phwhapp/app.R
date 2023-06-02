library(shiny)
library(raster)
library(sp)
library(ggplot2)
library(RColorBrewer)
library(rgdal)
library(broom)

Census<-readRDS("idwrasters_occupancy_census.Rdata")
First<-readRDS("idwrasters_occupancy_first.Rdata")
Second<-readRDS("idwrasters_occupancy_second.Rdata")

cmpboundaries<-shapefile("CMPboundaries.shp")

cmpboundaries_fortified <- tidy(cmpboundaries)

coefs_census<-read.csv("coefs_census.csv",header=T)

coefs_census_noint<-na.omit(coefs_census[coefs_census$Covariate!="Intercept",])

sppnames_census<-unique(coefs_census_noint$Taxon)


coefs_first<-read.csv("coefs_first.csv",header=T)

coefs_first_noint<-na.omit(coefs_first[coefs_first$Covariate!="Intercept",])

sppnames_first<-unique(coefs_first_noint$Taxon)

coefs_second<-read.csv("coefs_second.csv",header=T)

coefs_second_noint<-na.omit(coefs_second[coefs_second$Covariate!="Intercept",])

sppnames_second<-unique(coefs_second_noint$Taxon)

ui <- shinyUI(
  fluidPage(
    titlePanel("Occupancy Models for stream-dwelling taxa"),
    sidebarLayout(position = "left",
                  sidebarPanel("select your options",
                               selectInput("Taxon", "Select taxon name", 
                                         choices = unique(c(sppnames_census,sppnames_first,sppnames_second)))),
                  mainPanel("plots",
                            fluidRow(
                              splitLayout(style="border: 1px solid silver:",cellwidths=400,
                                          plotOutput("census"),
                                          plotOutput("first"),
                                          plotOutput("second"))),
                            fluidRow(
                              splitLayout(style="border: 1px solid silver:",cellwidths=400,
                                          plotOutput("census_effects",width = 600),
                                          plotOutput("first_effects",width = 600),
                                          plotOutput("second_effects",width = 600)))))))


server <- shinyServer(function(input, output){
  datasetInput <- reactive({as.data.frame(Census[[which(sppnames_census == input$Taxon)]],xy=TRUE)
  })
  datasetInput2 <- reactive({as.data.frame(First[[which(sppnames_first == input$Taxon)]],xy=TRUE)
  })
  datasetInput3 <- reactive({as.data.frame(Second[[which(sppnames_second == input$Taxon)]],xy=TRUE)
  }) 
  datasetInput4<-reactive({
    as.data.frame(coefs_census_noint[which(coefs_census_noint$Taxon == input$Taxon),])
  })
  datasetInput5<-reactive({
    as.data.frame(coefs_first_noint[which(coefs_first_noint$Taxon == input$Taxon),])
  })
  datasetInput6<-reactive({
    as.data.frame(coefs_second_noint[which(coefs_second_noint$Taxon == input$Taxon),])
  })
  output$census <- renderPlot({
    dataset <- datasetInput()
    ggplot()+geom_raster(data=dataset,aes(x=x,y=y,fill=var1.pred))+
      scale_fill_gradient(low="red",high="green",limits=c(0,1))+
      geom_polygon(data=cmpboundaries_fortified,aes( x = long, y = lat,group=group),fill=NA, color="black")+
      theme_minimal(base_size=16)+ggtitle("Census survey interpolations ")
  })
  output$first <- renderPlot({
    dataset2 <- datasetInput2()
    ggplot()+geom_raster(data=dataset2,aes(x=x,y=y,fill=var1.pred))+
      scale_fill_gradient(low="red",high="green",limits=c(0,1))+
      geom_polygon(data=cmpboundaries_fortified,aes( x = long, y = lat,group=group),fill=NA, color="black")+
      theme_minimal(base_size=16)+ggtitle("First survey interpolations")
  })
  output$second <- renderPlot({
    dataset3 <- datasetInput3()
    ggplot()+geom_raster(data=dataset3,aes(x=x,y=y,fill=var1.pred))+
      scale_fill_gradient(low="red",high="green",limits=c(0,1))+
      geom_polygon(data=cmpboundaries_fortified,aes( x = long, y = lat,group=group),fill=NA, color="black")+
      theme_minimal(base_size=16)+ggtitle("Second survey interpolations")
  })
  output$census_effects<-renderPlot({
    dataset4<-datasetInput4()
    ggplot(aes(x=Covariate,y=Mean),data=dataset4)+geom_point()+
      geom_errorbar(aes(x=Covariate,ymin=X5.,ymax=X95.))+
      theme_minimal(base_size=16)+ggtitle("Logit-scale census coefficients (+/- 90% CI)")
  })
  output$first_effects<-renderPlot({
    dataset5<-datasetInput5()
    ggplot(aes(x=Covariate,y=Mean),data=dataset5)+geom_point()+
      geom_errorbar(aes(x=Covariate,ymin=X5.,ymax=X95.))+
      theme_minimal(base_size=16)+ggtitle("Logit-scale first coefficients (+/- 90% CI)")
  })
  output$second_effects<-renderPlot({
    dataset6<-datasetInput6()
    ggplot(aes(x=Covariate,y=Mean),data=dataset6)+geom_point()+
      geom_errorbar(aes(x=Covariate,ymin=X5.,ymax=X95.))+
      theme_minimal(base_size=16)+ggtitle("Logit-scale second coefficients (+/- 90% CI)")
  })
})

shinyApp(ui = ui, server = server)


#code graveyard
#scale_fill_gradient(low="red",high="green")
