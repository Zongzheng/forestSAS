library(shiny)
library(ggplot2)
library(ggimage)

forestSAS_simapp<-function(){

# Define UI for dataset viewer app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Forest spatial structure analysis system"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Specify the number of observations to view ----
      numericInput("size", "Pool of species:", 100),
      # Include clarifying text ----
      helpText("Note: while the data view will show only the specified",
               "number of observations, the summary will still be based",
               "on the full dataset."),
      # Input: Specify the number of observations to view ----
      numericInput("nspe", "Number of species:", 5),
      numericInput("minX", "minimum of X range:", 0),
      numericInput("maxX", "maximum of X range:", 100),
      numericInput("minY", "minimum of Y range:", 0),
      numericInput("maxY", "maximum of Y range:", 100),
      numericInput("minDBH", "minimum of DBH:", 5),
      numericInput("maxDBH", "maximum of DBH:", 50),
      sliderInput("lambda", "random to clump:",min = 0, max = 10,
                  value = 1,step = 0.1),
      # Input: actionButton() to defer the rendering of output ----
      # until the user explicitly clicks the button (rather than
      # doing it immediately when inputs change). This is useful if
      # the computations required to render output are inordinately
      # time-consuming.
      actionButton("update", "Update View")
    ),

    mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Data",
                           fluidRow(
                             column(width = 8, class = "well",
                                    h4("Summary"),
                                    verbatimTextOutput("summary")),
                             column(width = 8, class = "well",
                                    h4("Observations"),
                                    tableOutput("view")),
                             downloadButton("download", "Observations.csv"),
                           )
                  ),
                  tabPanel("Plot",
                           fluidRow(
                             column(width = 4, class = "well",
                                    h4("Distribution"),
                                    plotOutput("plot1", height = 400)
                             ),
                             column(width = 8, class = "well",
                                    h4("Visualization"),
                                    plotOutput("plot2", height = 700),
                                    sliderInput("n", "Size:", min = 0, max = 1
                                                , value = 0.5,
                                                step = 0.1)),
                             )
                  ),
                  tabPanel("Spatial structure analysis",
                           fluidRow(
                             column(width = 8, class = "well",
                                    h4("Frequency"),
                                    plotOutput("frequency", height = 600)),
                             column(width = 8, class = "well",
                                    h4("Evaluation"),
                                    plotOutput("evaluation", height = 500)),
                             column(width = 8, class = "well",
                                    h4("Result"),
                                    verbatimTextOutput("result")),
                           )
                  ),
                  tabPanel("Assigned cutting",
                           column(width = 12, class = "well",
                                  numericInput("inter", "Inter:", 10),
                                  sliderInput("pop", "Cutting intensity:",
                                              min = 0, max = 1
                                              , value = 0.1,
                                              step = 0.05),
                                  actionButton("submit", "Update View"),
                           ),
                           column(width = 6, class = "well",
                                  h4("Before cutting"),
                                  plotOutput("plot3", height = 400)
                           ),
                           column(width = 6, class = "well",
                                  h4("After cutting"),
                                  plotOutput("plot4", height = 400),

                           ),
                           column(width = 6, class = "well",
                                  h4("Evaluation"),
                                  plotOutput("before_eva.plot", height = 400)
                           ),
                           column(width = 6, class = "well",
                                  h4("Evaluation"),
                                  plotOutput("after_eva.plot", height = 400)
                           ),
                           column(width = 12, class = "well",
                                  column(width = 4, class = "well",
                                         h4("Before cutting"),
                                         plotOutput("bfassplot1", height = 400)
                                  ),
                                  column(width = 8, class = "well",
                                         h4("Visualization"),
                                         plotOutput("bfassplot2", height = 700),
                                         sliderInput("n", "Size:", min = 0, max = 1
                                                     , value = 0.5,
                                                     step = 0.1)),
                           ),
                           column(width = 12, class = "well",
                                  column(width = 4, class = "well",
                                         h4("After cutting"),
                                         plotOutput("afassplot1", height = 400)
                                  ),
                                  column(width = 8, class = "well",
                                         h4("Visualization"),
                                         plotOutput("afassplot2", height = 700),
                                         sliderInput("n", "Size:", min = 0, max = 1
                                                     , value = 0.5,
                                                     step = 0.1)),
                           column(width = 6, class = "well",
                                  h4("Result"),
                                  verbatimTextOutput("result2")),
                           column(width = 6, class = "well",
                                  h4("Result"),
                                  verbatimTextOutput("result3")),
                  )),
                  tabPanel("Simulated cutting",
                           fluidRow(
                             column(width = 12, class = "well",
                                    numericInput("inter1", "Inter:", 10),
                                    actionButton("submit1", "Update View"),
                             ),
                             column(width = 12, class = "well",
                                    h4("Simulation process"),
                                    plotOutput("simulate", height = 400)
                             ),
                             column(width = 6, class = "well",
                                    h4("Before cutting"),
                                    plotOutput("before", height = 400)
                             ),
                             column(width = 6, class = "well",
                                    h4("After cutting"),
                                    plotOutput("after", height = 400),
                             ),
                             column(width = 6, class = "well",
                                    h4("Evaluation"),
                                    plotOutput("before_simeva.plot", height = 400)
                             ),
                             column(width = 6, class = "well",
                                    h4("Evaluation"),
                                    plotOutput("after_simeva.plot", height = 400)
                             ),
                             column(width = 12, class = "well",
                                    column(width = 4, class = "well",
                                           h4("Before cutting"),
                                           plotOutput("bfsimplot1", height = 400)
                                    ),
                                    column(width = 8, class = "well",
                                           h4("Visualization"),
                                           plotOutput("bfsimplot2", height = 700),
                                           sliderInput("n", "Size:", min = 0, max = 1
                                                       , value = 0.5,
                                                       step = 0.1)),
                             ),
                             column(width = 12, class = "well",
                                    column(width = 4, class = "well",
                                           h4("After cutting"),
                                           plotOutput("afsimplot1", height = 400)
                                    ),
                                    column(width = 8, class = "well",
                                           h4("Visualization"),
                                           plotOutput("afsimplot2", height = 700),
                                           sliderInput("n", "Size:", min = 0, max = 1
                                                       , value = 0.5,
                                                       step = 0.1)),
                             ),
                             column(width = 6, class = "well",
                                    h4("Result"),
                                    verbatimTextOutput("result4")),
                             column(width = 6, class = "well",
                                    h4("Result"),
                                    verbatimTextOutput("result5")),
                           )
                  ),
      )
    ))
)


server <- function(input, output) {
  datasetInput <- eventReactive(input$update, {
    sim_com <- simtreecom(size=input$size,nspe=input$nspe,
                          dbhrange=c(input$minDBH,input$maxDBH),
                          xrange = c(input$minX,input$maxX),
                          yrange = c(input$minY, input$maxY),
                          type="com",lambda=input$lambda)
    sim_com$size<-sim_com$volume/max(sim_com$volume)
    sim_com$storey<-storeydvd(sim_com$ht,storeynum=6)$heightdata$storey
    data<-sim_com
    data})

  output$summary<- renderPrint({
    dataset<-datasetInput()
    summary(dataset)
  })

  output$view <- renderTable({
    head(datasetInput(), n = 10)
  })

  output$download <- downloadHandler(
    filename = function() {
      paste0(input$dataset, ".csv")
    },
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  )

  output$plot1<- renderPlot({
    dataset<-datasetInput()
    treepoint.p<-ggplot() + geom_point(aes(x=x, y=y,
                                           color =spe,size=size),
                                       data=dataset)+
      xlim(c(-10,110))+ylim(c(-10,110))+coord_fixed()+labs(color="Species")
    treepoint.p
  })

  output$plot2<- renderPlot({
    dataset<-datasetInput()
    img<-list()
    for(i in 1:nrow(dataset)){
      img[[i]]<-system.file("img",paste(dataset$spe[i],".png",sep=""),package="forestSAS")
    }
    dataset$img<-unlist(img)
    library(ggplot2)
    treecom.p<-ggplot() + geom_image(aes(x=x, y=y,
                                         image =img),
                                     size=dataset$size*input$n,
                                     data=dataset)+
      xlim(c(-20,120))+ylim(c(-20,120))+coord_fixed()
    treecom.p
  })


  datasetInput_spastr<- eventReactive(datasetInput(), {
    dataset<-datasetInput()
    dataset2<-spastr(X_df=dataset,buf.xwid =10,
                     buf.ywid = 10,xrange=c(0,100),yrange=c(0,100),
                     smark=c("spe","storey","dbh","cw","x","y"))
    dataset2})


  output$frequency<- renderPlot({
    dataset2<-datasetInput_spastr()
    fretable<-table(reshape2::melt(dataset2$Coredata[c("M","H","W","C","U")]))
    fredf<-as.data.frame(fretable/rowSums((fretable)))
    fre.p<-ggplot()+geom_bar(aes(x=as.character(value),y=Freq),
                             data=fredf,stat = "identity",fill="red")+
      geom_text(aes(x=as.character(value),y=Freq,label=round(Freq,2)),
                    data=fredf,vjust=-0.2)+
      facet_wrap(~ variable)+xlab(" ")+ylab("Frequency")
    fre.p
  })

  output$evaluation<- renderPlot({
    dataset2<-datasetInput_spastr()
    pvdata<-data.frame(angle=seq(36, 360-36, by=72),
                       pv=dataset2$Index_pv,index=c("M","H","W","C","U"))
    pv.p<-ggplot(aes(x=angle,y=pv),data=pvdata)+
      geom_col(
        aes(x=angle,y=pv,fill=index),width=72)+theme_bw()+ylim(0,1)+
      scale_x_continuous(limits=c(0,360), breaks=seq(36, 324, by=72),
                         minor_breaks=seq(0, 360, by=72),
                         labels=pvdata$index)+coord_polar()+
      theme(axis.text.x = element_text(color="blue",
                                       size=rel(2)))+
      guides(fill="none")+xlab("")+ylab("")+
      annotate("text", x=-Inf, y=Inf,color="red", parse=TRUE,
               hjust=-0.8, vjust=-0.5,
               label=paste("italic(ω)==",round(dataset2$Omega,3)),size=7)
    pv.p
  })


  output$result<- renderPrint({
    dataset2<-datasetInput_spastr()
    dataset2[1:3]
  })


  output$plot3<- renderPlot({
    dataset2<-datasetInput_spastr()
    fretable<-table(reshape2::melt(dataset2$Coredata[c("M","H","W","C","U")]))
    fredf<-as.data.frame(fretable/rowSums((fretable)))
    fre.p<-ggplot()+geom_bar(aes(x=as.character(value),y=Freq),
                             data=fredf,stat = "identity",fill="red")+
      geom_text(aes(x=as.character(value),y=Freq,label=round(Freq,2)),
                data=fredf,vjust=-0.2)+
      facet_wrap(~ variable)+xlab(" ")+ylab("Frequency")
    fre.p
  })

  output$before_eva.plot<-renderPlot({
    dataset2<-datasetInput_spastr()
    pvdata<-data.frame(angle=seq(36, 360-36, by=72),
                       pv=dataset2$Index_pv,index=c("M","H","W","C","U"))
    pv.p<-ggplot(aes(x=angle,y=pv),data=pvdata)+
      geom_col(
        aes(x=angle,y=pv,fill=index),width=72)+theme_bw()+ylim(0,1)+
      scale_x_continuous(limits=c(0,360), breaks=seq(36, 324, by=72),
                         minor_breaks=seq(0, 360, by=72),
                         labels=pvdata$index)+coord_polar()+
      theme(axis.text.x = element_text(color="blue",
                                       size=rel(2)))+
      guides(fill="none")+xlab("")+ylab("")+
      annotate("text", x=-Inf, y=Inf,color="red", parse=TRUE,
               hjust=-0.8, vjust=-0.5,
               label=paste("italic(ω)==",round(dataset2$Omega,3)),size=7)
    pv.p
  })

  output$result2<- renderPrint({
    dataset2<-datasetInput_spastr()
    dataset2[1:3]
  })

  output$result4<- renderPrint({
    dataset2<-datasetInput_spastr()
    dataset2[1:3]
  })

  datasetInput_optstr<- eventReactive(input$submit, {
    dataset<-datasetInput()
    dataset2<-opt_spastr(X_df=dataset,buf.xwid =10, inter=input$inter,
                         pop=input$pop,
                         buf.ywid = 10,xrange=c(0,100),yrange=c(0,100))
    dataset2})



  output$plot4<- renderPlot({
    dataset2<- datasetInput_optstr()
    fretable<-table(reshape2::melt(dataset2$Coredata[c("M","H","W","C","U")]))
    fredf<-as.data.frame(fretable/rowSums((fretable)))
    fre.p<-ggplot()+geom_bar(aes(x=as.character(value),y=Freq),
                             data=fredf,stat = "identity",fill="red")+
      geom_text(aes(x=as.character(value),y=Freq,label=round(Freq,2)),
                data=fredf,vjust=-0.2)+
      facet_wrap(~ variable)+xlab(" ")+ylab("Frequency")
    fre.p
  })

  output$after_eva.plot<-renderPlot({
    dataset2<- datasetInput_optstr()
    pvdata<-data.frame(angle=seq(36, 360-36, by=72),
                       pv=dataset2$Index_pv,index=c("M","H","W","C","U"))
    pv.p<-ggplot(aes(x=angle,y=pv),data=pvdata)+
      geom_col(
        aes(x=angle,y=pv,fill=index),width=72)+theme_bw()+ylim(0,1)+
      scale_x_continuous(limits=c(0,360), breaks=seq(36, 324, by=72),
                         minor_breaks=seq(0, 360, by=72),
                         labels=pvdata$index)+coord_polar()+
      theme(axis.text.x = element_text(color="blue",
                                       size=rel(2)))+
      guides(fill="none")+xlab("")+ylab("")+
      annotate("text", x=-Inf, y=Inf,color="red", parse=TRUE,
               hjust=-0.8, vjust=-0.5,
               label=paste("italic(ω)==",round(dataset2$Omega,3)),size=7)
    pv.p
  })

  output$result3<- renderPrint({
    dataset2<- datasetInput_optstr()
    dataset2[1:4]
  })

  datasetInput2 <- eventReactive(input$submit1, {
    dataset<-datasetInput()
    intensity<-seq(0,0.5,0.05)
    process<-list()
    for(i in 2:length(intensity)){
      process[[1]]<-datasetInput_spastr()
      process[[i]]<-opt_spastr(X_df=dataset,inter=input$inter1,
                               pop=intensity[i],
                               xrange=c(0,100),yrange=c(0,100),
                               xwidth=10,ywidth=10,
                               buf.xwid =10, buf.ywid = 10,
                               smark=c("spe","storey","dbh","cw","x","y"))
    }
    process})


  output$simulate<- renderPlot( {
    process=datasetInput2()
    intensity<-seq(0,0.5,0.05)
    order=1:length(intensity)
    dataset2<-data.frame(order,intensity)
    dataset2$Omega<-unlist(lapply(process, function(x) x$Omega))
    first<-order(dataset2$Omega,decreasing=TRUE)[1]
    sim.p<-ggplot()+geom_line(aes(x=intensity,y=Omega),data=dataset2)+
      geom_point(aes(x=intensity,y=Omega),data=dataset2)+
      geom_text(aes(x=intensity,y=Omega,label=order),color="blue",size=7,
                data=dataset2)+xlab("Cutting intensity")+
      geom_text(aes(x=intensity[first],y=Omega[first],
                    label=order[first]),color="red",size=7,
                data=dataset2)
    ylab("Omega")
    sim.p
  })

  output$before<- renderPlot({
    dataset<-datasetInput()
    dataset2<-opt_spastr(X_df=dataset,buf.xwid =10,
                         buf.ywid = 10,xrange=c(0,100),yrange=c(0,100),
                         smark=c("spe","storey","dbh","cw","x","y"))
    fretable<-table(reshape2::melt(dataset2$Coredata[c("M","H","W","C","U")]))
    fredf<-as.data.frame(fretable/rowSums((fretable)))
    fre.p<-ggplot()+geom_bar(aes(x=as.character(value),y=Freq),
                             data=fredf,stat = "identity",fill="red")+
      geom_text(aes(x=as.character(value),y=Freq,label=round(Freq,2)),
                data=fredf,vjust=-0.2)+
      facet_wrap(~ variable)+xlab(" ")+ylab("Frequency")
    fre.p
  })

  output$after<- renderPlot({
    process=datasetInput2()
    intensity<-seq(0,0.5,0.05)
    order=1:length(intensity)
    dataset<-data.frame(order,intensity)
    dataset$Omega<-unlist(lapply(process, function(x) x$Omega))
    first<-order(dataset$Omega,decreasing=TRUE)[1]
    dataset2<-process[[first]]
    fretable<-table(reshape2::melt(dataset2$Coredata[c("M","H","W","C","U")]))
    fredf<-as.data.frame(fretable/rowSums((fretable)))
    fre.p<-ggplot()+geom_bar(aes(x=as.character(value),y=Freq),
                             data=fredf,stat = "identity",fill="red")+
      geom_text(aes(x=as.character(value),y=Freq,label=round(Freq,2)),
                data=fredf,vjust=-0.2)+
      facet_wrap(~ variable)+xlab(" ")+ylab("Frequency")
    fre.p
  })

  output$before_simeva.plot<-renderPlot({
    dataset2<-datasetInput_spastr()
    pvdata<-data.frame(angle=seq(36, 360-36, by=72),
                       pv=dataset2$Index_pv,index=c("M","H","W","C","U"))
    pv.p<-ggplot(aes(x=angle,y=pv),data=pvdata)+
      geom_col(
        aes(x=angle,y=pv,fill=index),width=72)+theme_bw()+ylim(0,1)+
      scale_x_continuous(limits=c(0,360), breaks=seq(36, 324, by=72),
                         minor_breaks=seq(0, 360, by=72),
                         labels=pvdata$index)+coord_polar()+
      theme(axis.text.x = element_text(color="blue",
                                       size=rel(2)))+
      guides(fill="none")+xlab("")+ylab("")+
      annotate("text", x=-Inf, y=Inf,color="red", parse=TRUE,
               hjust=-0.8, vjust=-0.5,
               label=paste("italic(ω)==",round(dataset2$Omega,3)),size=7)
    pv.p
  })

  output$after_simeva.plot<-renderPlot({
    process=datasetInput2()
    intensity<-seq(0,0.5,0.05)
    order=1:length(intensity)
    dataset<-data.frame(order,intensity)
    dataset$Omega<-unlist(lapply(process, function(x) x$Omega))
    first<-order(dataset$Omega,decreasing=TRUE)[1]
    dataset2<-process[[first]]
    pvdata<-data.frame(angle=seq(36, 360-36, by=72),
                       pv=dataset2$Index_pv,index=c("M","H","W","C","U"))
    pv.p<-ggplot(aes(x=angle,y=pv),data=pvdata)+
      geom_col(
        aes(x=angle,y=pv,fill=index),width=72)+theme_bw()+ylim(0,1)+
      scale_x_continuous(limits=c(0,360), breaks=seq(36, 324, by=72),
                         minor_breaks=seq(0, 360, by=72),
                         labels=pvdata$index)+coord_polar()+
      theme(axis.text.x = element_text(color="blue",
                                       size=rel(2)))+
      guides(fill="none")+xlab("")+ylab("")+
      annotate("text", x=-Inf, y=Inf,color="red", parse=TRUE,
               hjust=-0.8, vjust=-0.5,
               label=paste("italic(ω)==",round(dataset2$Omega,3)),size=7)
    pv.p
  })

  output$result5<- renderPrint({
    process=datasetInput2()
    intensity<-seq(0,0.5,0.05)
    order=1:length(intensity)
    dataset<-data.frame(order,intensity)
    dataset$Omega<-unlist(lapply(process, function(x) x$Omega))
    first<-order(dataset$Omega,decreasing=TRUE)[1]
    dataset2<-process[[first]]
    dataset2[1:4]
  })

  output$bfassplot1<- renderPlot({
    dataset<-datasetInput()
    treepoint.p<-ggplot() + geom_point(aes(x=x, y=y,
                                           color =spe,size=size),
                                       data=dataset)+
      xlim(c(-10,110))+ylim(c(-10,110))+coord_fixed()+labs(color="Species")
    treepoint.p
  })

  output$bfassplot2<- renderPlot({
    dataset<-datasetInput()
    img<-list()
    for(i in 1:nrow(dataset)){
      img[[i]]<-system.file("img",paste(dataset$spe[i],".png",sep=""),package="forestSAS")
    }
    dataset$img<-unlist(img)
    library(ggplot2)
    treecom.p<-ggplot() + geom_image(aes(x=x, y=y,
                                         image =img),
                                     size=dataset$size*input$n,
                                     data=dataset)+
      xlim(c(-20,120))+ylim(c(-20,120))+coord_fixed()+labs(color="Species")
    treecom.p
  })

  output$bfsimplot1<- renderPlot({
    dataset<-datasetInput()
    treepoint.p<-ggplot() + geom_point(aes(x=x, y=y,
                                           color =spe,size=size),
                                       data=dataset)+
      xlim(c(-10,110))+ylim(c(-10,110))+coord_fixed()+labs(color="Species")
    treepoint.p
  })

  output$bfsimplot2<- renderPlot({
    dataset<-datasetInput()
    img<-list()
    for(i in 1:nrow(dataset)){
      img[[i]]<-system.file("img",paste(dataset$spe[i],".png",sep=""),package="forestSAS")
    }
    dataset$img<-unlist(img)
    library(ggplot2)
    treecom.p<-ggplot() + geom_image(aes(x=x, y=y,
                                         image =img),
                                     size=dataset$size*input$n,
                                     data=dataset)+
      xlim(c(-20,120))+ylim(c(-20,120))+coord_fixed()
    treecom.p
  })

  output$afassplot1<- renderPlot({
    dataset2<- datasetInput_optstr()$Coredata
    treepoint.p<-ggplot() + geom_point(aes(x=x, y=y,
                                           color =spe,size=size),
                                       data=dataset2)+
      xlim(c(-10,110))+ylim(c(-10,110))+coord_fixed()+labs(color="Species")
    treepoint.p
  })

  output$afassplot2<- renderPlot({
    dataset2<- datasetInput_optstr()$Coredata
    img<-list()
    for(i in 1:nrow(dataset2)){
      img[[i]]<-system.file("img",paste(dataset2$spe[i],".png",sep=""),package="forestSAS")
    }
    dataset2$img<-unlist(img)
    library(ggplot2)
    treecom.p<-ggplot() + geom_image(aes(x=x, y=y,
                                         image =img),
                                     size=dataset2$size*input$n,
                                     data=dataset2)+
      xlim(c(-20,120))+ylim(c(-20,120))+coord_fixed()
    treecom.p
  })



  output$afsimplot1<- renderPlot({
    process=datasetInput2()
    intensity<-seq(0,0.5,0.05)
    order=1:length(intensity)
    dataset<-data.frame(order,intensity)
    dataset$Omega<-unlist(lapply(process, function(x) x$Omega))
    first<-order(dataset$Omega,decreasing=TRUE)[1]
    dataset<-process[[first]]$Coredata
    treepoint.p<-ggplot() + geom_point(aes(x=x, y=y,
                                           color =spe,size=size),
                                       data=dataset)+
      xlim(c(-10,110))+ylim(c(-10,110))+coord_fixed()+labs(color="Species")
    treepoint.p
  })

  output$afsimplot2<- renderPlot({
    process=datasetInput2()
    intensity<-seq(0,0.5,0.05)
    order=1:length(intensity)
    dataset<-data.frame(order,intensity)
    dataset$Omega<-unlist(lapply(process, function(x) x$Omega))
    first<-order(dataset$Omega,decreasing=TRUE)[1]
    dataset<-process[[first]]$Coredata
    img<-list()
    for(i in 1:nrow(dataset)){
      img[[i]]<-system.file("img",paste(dataset$spe[i],".png",sep=""),package="forestSAS")
    }
    dataset$img<-unlist(img)
    library(ggplot2)
    treecom.p<-ggplot() + geom_image(aes(x=x, y=y,
                                         image =img),
                                     size=dataset$size*input$n,
                                     data=dataset)+
      xlim(c(-20,120))+ylim(c(-20,120))+coord_fixed()
    treecom.p
  })
}



# Create Shiny app ----
shinyApp(ui, server)

}





