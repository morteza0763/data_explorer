#-------------A "shiny" UI ------------------------------
library(shiny)
library(rhandsontable)
library(ggiraph)

ui <- fluidPage(
  # App title ----
  titlePanel("Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      # import data
      fileInput("file1", "Choose excel File",
                multiple = FALSE,
                accept = c(".xls",".xlsx"))
      ,
      # Input: Selector for sheet names ----
      uiOutput("variableI")
      ,
      # Time selector----------------------
      sliderInput(inputId = "time",
                  label = "Year",
                  min = 2000,
                  max = 2019,
                  value = 2015)
      ,
      br(),br(),br(),
      actionButton("fit", "Fit Change Point Model"
                   ,icon = icon("bar-chart-o"),col="blue"
                   ,style="color: #fff; background-color: #337ab7; border-color: #2e6da4")

    ),
    # Main panel for displaying outputs ----
    mainPanel("main panel"
              ,rHandsontableOutput("data")
              ,br()
              ,ggiraphOutput("mapplot",width = 900,height = 900)
              ,plotOutput("trendplot",width = 900,height = 900)
              ,br()
              ,plotOutput("fitbut",width = 800,height = 700)
    )
  )
)


server <- function(input, output) {



  {
    library(readxl)

    library(ggplot2);library(plotly)
    library(viridis)
    library(maps)
    library(sf)

    library(reshape2)
    library(tidyverse)
    library(stringi)
  }

  #---- Get,set file path----------------------------------
  output$variableI<-renderUI({
    file<-input$file1
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "xlsx", "Please upload a .xlsx/xls file"))

    selectInput("variable", "Select variable:",
                choices= excel_sheets(file$datapath)
                ,multiple = F
    )
  })

  sanction_data_1<-reactive({
    file<-input$file1
    if(length(file$datapath)>0)
      read_excel(file$datapath, sheet = input$variable)
  })
  ##--- render data table ----------------------------------
  output$data <- renderRHandsontable({
    rhandsontable(sanction_data_1(),height = 200,width = 900)
  })

  #-------- import map of Iran-----------------------------------
  iran_map<-read_sf("iran_map//irn_admbnda_adm1_unhcr_20190514.shp")
  colnames(iran_map)[3]<-"Province"
  iran_map$Province[c(4,5,21,24,26,27,29)]<-c("Chahar Mahaal and Bakhtiari","Azerbaijan, East"
                                              ,"Khorasan, North","Khorasan, Razavi"
                                              ,"Sistan and Baluchistan","Khorasan, South"
                                              ,"Azerbaijan, West")
  observe({
    if(!is.null(sanction_data_1())){
      #-------- import sanction indexes data------------------------
      #-------- clean sanction data ---------------------------------
      ##-- remove empty province-------------------------------------
      sanction_data_1=sanction_data_1()%>%
        filter(!is.na(sanction_data_1()%>%select(1)%>%pull))%>%as_tibble()

      ##-- remove "Source:"------------------------------------------
      sourc_ind=stringi::stri_detect_regex(sanction_data_1[[1]],"Source:",case_insensitive=F)
      sanction_data_1=sanction_data_1[!sourc_ind,]
      ##-- remove gender-----------------------------------------------
      fi=(sanction_data_1%>%select(1)%>%pull) %in% c("female","Female")
      mi=(sanction_data_1%>%select(1)%>%pull) %in% c("Male","male")
      bothi=(sanction_data_1%>%select(1)%>%pull) %in% "both"

      if(any(fi)&any(mi)&any(bothi)){
        sanction_data_1=sanction_data_1[!(fi|mi|bothi),]
        sanction_data_1=sanction_data_1%>%
          add_column(Gender=rep(c("Female","Male","Both"),c(32,32,32))
                     ,.after=1)
      }

      if(any(fi)&any(mi)==F&any(bothi)==F){
        sanction_data_1=sanction_data_1[!(fi),]
        sanction_data_1=sanction_data_1%>%
          add_column(Gender=rep(c("Female"),c(32)),.after=1)
      }
      if(any(mi)&any(fi)==F&any(bothi)==F){
        sanction_data_1=sanction_data_1[!(mi),]
        sanction_data_1=sanction_data_1%>%
          add_column(Gender=rep(c("Male"),c(32)),.after=1)
      }
      # View(sanction_data_1)
      #--------Long sanction data ----------------------------------
      cnames=colnames(sanction_data_1)

      if(!any(c("Tehran","tehran")%in%sanction_data_1[[1]])){

        sanction_data_1_L=sanction_data_1%>%
          melt(na.rm = FALSE
               ,id.vars=1
               # ,measure.vars=2:21
               ,variable.name="Time"
               ,value.name =paste(cnames[1],"_index"))%>%
          as_tibble()

        sanction_data_1_L[paste(cnames[1],"_index")]<-
          as.numeric(sanction_data_1_L[[paste(cnames[1],"_index")]])

        #---trend plot----------------------------
        sanction_time=paste("1-jan-"
                            ,as.character(sanction_data_1_L$Time)
                            ,sep="")
        sanction_time=as.Date(sanction_time
                              ,format = "%d-%b-%Y")

        sanction_preiods=character(length(sanction_time))
        sanction_preiods[
          sanction_time<=as.Date("31-dec-2009","%d-%b-%Y")
        ]<-"Pre sanction"

        sanction_preiods[
          sanction_time>=as.Date("01-jan-2010","%d-%b-%Y")&
            sanction_time<=as.Date("31-dec-2013","%d-%b-%Y")
        ]<-"Sanction"

        sanction_preiods[
          sanction_time>=as.Date("01-jan-2014","%d-%b-%Y")&
            sanction_time<=as.Date("31-dec-2017","%d-%b-%Y")
        ]<-"Between sanction"

        sanction_preiods[
          sanction_time>=as.Date("01-jan-2018","%d-%b-%Y")
        ]<-"Sanction"
        sanction_data_1_L=add_column(sanction_data_1_L,.after = "Time"
                                     ,Date=sanction_time
                                     ,`Sanction periods`=sanction_preiods)
        #----------------------
        ccnames=colnames(sanction_data_1_L)
        fecet_ncol=sanction_data_1_L%>%count(!!sym(ccnames[1]))%>%nrow
        if(fecet_ncol>2 & fecet_ncol %%2==0){
          fecet_ncol=2
        }else{
          fecet_ncol=1
        }


        output$mapplot<-renderPlot({

        })
        output$trendplot<-renderPlot({

          sanction_data_1_L%>%
            ggplot(aes(x=Date
                       ,y=!!sym(ccnames[length(ccnames)])
            ))+
            geom_line()+
            geom_point(aes(colour=`Sanction periods`
                           ,shape=`Sanction periods`),cex=3)+
            scale_y_continuous(trans="identity")+
            scale_x_date(name = "\nTime(Year)"
                         ,date_labels = "%b-%Y"
                         ,date_breaks = "1 year")+
            facet_wrap(
              as.formula(
                paste(".~","`",ccnames[1],"`",sep=""))
              ,ncol = fecet_ncol,scales = "free"
            )+
            labs(title = paste(ccnames[1]))+
            theme_light()+
            theme(legend.position = "right"
                  ,axis.text.x = element_text(angle = 90))
        },res = 100
        ,width = if(fecet_ncol==2) {800 }else{800}
        ,height = if(fecet_ncol==2) {700}else{800})

        observeEvent(eventExpr = input$fit
                     ,ignoreNULL = FALSE,ignoreInit = TRUE
                     ,handlerExpr = {
          output$fitbut<-renderPlot({
            trend_data=sanction_data_1_L%>%
              filter(sanction_data_1_L[,1]==ccnames[1])
            # filter(Province=="Iran"|Province=="iran")

            trend_data1=trend_data%>%
              select(ccnames[length(ccnames)],Date,`Sanction periods`)%>%
              mutate(response=!!sym(ccnames[length(ccnames)]),
                     date=2000:2019)%>%
              drop_na()

            library(mcp)
            model=list(
              response~1,
              response~1~0+date,
              ~1+date
            )
            mcp_fit=mcp(model,data =trend_data1,sample = "both"
                        ,iter = 20000,adapt = 10000,chains = 2 )
            options(digits = 2)
            mcp_summary=summary(mcp_fit)
            mcp_summary[,-1]=round(mcp_summary[,-1],2)
            # mcp_summary%>%datatable(caption = paste("response:",ccnames[1]))
            library(patchwork)
            san=data.frame(xmin0=c(2000,2010,2013,2018)
                           ,xmax0=c(2010,2013,2018,2020)
                           ,ymin0=0,ymax0=max(trend_data1$response)
                           ,Periods=c("Pre sanction","Sanction"
                                      ,"Between sanction","Sanction"))
            san$width0=san$xmax0-san$xmin0

            mcp_fit_plot=plot(mcp_fit,lines=1) +
              geom_line()+
              geom_segment( show.legend = T,data = san
                            ,aes(x=xmin0,y=ymax0,xend=xmax0,yend=ymax0
                                 ,group=Periods)
                            ,arrow = arrow(angle =20,type = "closed"
                                           ,ends = "both",length = unit(0.4, "cm"))
                            ,colour=c(2,3,4,3)
                            ,size=.01,alpha=.5
              )+
              geom_text(inherit.aes = F,data=san
                        ,aes(x=(xmin0+xmax0)/2,y=ymax0+1,label=Periods
                             ,vjust=0,hjust=.5)
                        ,colour=c(2,3,4,3),size=3.5
              )+
              # scale_colour_manual(name = 'the colour',
              #                     values =c('black'='black','red'='red'), labels = c('c2','c1'))
              geom_vline(xintercept = c(2010,2013,2018)
                         ,lty=2,lwd=.2,col="gray60")+
              coord_cartesian(xlim = c(2000, 2020))+
              scale_x_continuous(breaks = 2000:2019)+
              scale_y_continuous(name = ccnames[1])+
              theme_minimal()+
              theme(axis.text.x = element_text(angle = 90))
            mcp_fit_plot
          },res = 120
          ,pointsize = 50)

        })

      }else{###=== panel data========================
        colnames(sanction_data_1)[1]<-"Province"
        ifelse("Gender"%in%cnames,id.v<-1:2,id.v<-1)
        sanction_data_1_L=sanction_data_1%>%
          melt(na.rm = FALSE
               ,id.vars=id.v
               # ,measure.vars=3:22
               ,variable.name="Time"
               ,value.name =cnames[1])%>%
          as_tibble()


        #-------- joining sanction indexes with map data-----------------
        iran_map_with_index=left_join(iran_map,sanction_data_1_L)
        #-------- create map plot with sanction indexes-----------------
        if("Gender"%in%cnames){
          facett<-function(){
            facet_wrap(.~Gender,nrow = 1
                       ,ncol = sanction_data_1_L%>%
                         count(Gender)%>%nrow
            )
          }}else{
            facett<-function(){}
          }

        output$mapplot <- renderggiraph( {
          mp=iran_map_with_index%>%
            filter(Time==input$time)%>%
            ggplot()+
            geom_sf_interactive(aes(group=Province
                                    ,fill=!!sym(cnames[1])
                                    ,tooltip=paste(Province,!!sym(cnames[1]),sep="\n")
                                    ,data_id=Province)
                                    )+
            # geom_sf(aes(group=Province
            #             ,fill=!!sym(cnames[1]) )#`Gross domestic product (GDP) ,million IRR`)
            # )+
            # geom_sf_text(aes(label=Province),size=2,col="white")+
            scale_fill_viridis(discrete = FALSE,option = "plasma")+
            facett()+
            labs(title =paste(cnames[1],"in year",input$time))+
            theme_light()+
            theme(legend.position = "bottom"
                  ,legend.text = element_text(angle = 90,vjust = 0.3)
                  ,axis.text.x = element_text(angle = 0))
          # mpi=mp+geom_point_interactive(aes(x=Shape_Leng,y=Shape_Area
          #                                   ,tooltip=Province))
          girafe(ggobj = mp)%>%
            girafe_options(opts_hover(css = "fill:wheat;stroke:white;r:5pt;")
                           ,opts_zoom(min = .7, max = 2) )

        })#, width = 700,height=600,res = 105)

        #-------- trend plot for total Iran-----------------------------
        if("Gender"%in%cnames){
          facett.tr<-function(){
            facet_wrap(.~Gender,ncol = 1
                       ,nrow = sanction_data_1_L%>%
                         count(Gender)%>%nrow)
          }}else{
            facett.tr<-function(){}
          }

        sanction_time=paste("1-jan-"
                            ,as.character(sanction_data_1_L$Time)
                            ,sep="")
        sanction_time=as.Date(sanction_time
                              ,format = "%d-%b-%Y")

        sanction_preiods=character(length(sanction_time))
        sanction_preiods[
          sanction_time<=as.Date("31-dec-2009","%d-%b-%Y")
        ]<-"Pre sanction"

        sanction_preiods[
          sanction_time>=as.Date("01-jan-2010","%d-%b-%Y")&
            sanction_time<=as.Date("31-dec-2013","%d-%b-%Y")
        ]<-"Sanction"

        sanction_preiods[
          sanction_time>=as.Date("01-jan-2014","%d-%b-%Y")&
            sanction_time<=as.Date("31-dec-2017","%d-%b-%Y")
        ]<-"Between sanction"

        sanction_preiods[
          sanction_time>=as.Date("01-jan-2018","%d-%b-%Y")
        ]<-"Sanction"
        sanction_data_1_L=add_column(sanction_data_1_L,.after = "Time"
                                     ,Date=sanction_time
                                     ,`Sanction periods`=sanction_preiods)
        #------------trend plot-------------------------
        output$trendplot <- renderPlot({
          sanction_data_1_L%>%
            filter(Province=="Iran"|Province=="iran")%>%
            ggplot(aes(x=Date
                       ,y=!!sym(cnames[1])
            ))+
            geom_line()+
            geom_point(aes(colour=`Sanction periods`
                           ,shape=`Sanction periods`),cex=3)+
            scale_y_continuous(trans="identity")+
            scale_x_date(name = "\nTime(Year)"
                         ,date_labels = "%b-%Y"
                         ,date_breaks = "1 year")+
            facett.tr()+
            labs(title = cnames[1])+
            theme_light()+
            theme(legend.position = "right"
                  ,axis.text.x = element_text(angle = 90))

        }, width = 700,height=500,res = 100)
        #---- trend analysis-------------------
        observeEvent(input$fit,{
          output$fitbut<-renderPlot({

            filter_info=if("Gender"%in%cnames){
              last_gender_cat=sanction_data_1_L%>%
                count(Gender)%>%slice_head()%>%pull(Gender)

              function(...)filter(...,Gender==last_gender_cat)
            }else{
              function(...) filter(...)
            }
            trend_data=sanction_data_1_L%>%
              filter(Province=="Iran"|Province=="iran")

            trend_data1=trend_data%>%
              filter_info()%>%
              select(cnames[1],Date,`Sanction periods`)%>%
              mutate(response=!!sym(cnames[1]),
                     date=2000:2019)%>%
              drop_na()

            library(mcp)
            model=list(
              response~1,
              response~1~0+date,
              ~1+date
            )
            mcp_fit=mcp(model,data =trend_data1,sample = "both"
                        ,iter = 20000,chains = 2,adapt = 10000 )
            options(digits = 2)
            mcp_summary=summary(mcp_fit)
            mcp_summary[,-1]=round(mcp_summary[,-1],2)
            # mcp_summary%>%datatable(caption = paste("response:",ccnames[1]))
            # flextable()%>% fontsize(part = "all", size = 14)
               library(patchwork)
            san=data.frame(xmin0=c(2000,2010,2013,2018)
                           ,xmax0=c(2010,2013,2018,2020)
                           ,ymin0=0,ymax0=max(trend_data1$response)
                           ,Periods=c("Pre sanction","Sanction"
                                      ,"Between sanction","Sanction"))
            san$width0=san$xmax0-san$xmin0

            mcp_fit_plot=plot(mcp_fit,lines=1) +
              geom_line()+
              geom_segment( show.legend = T,data = san
                            ,aes(x=xmin0,y=ymax0,xend=xmax0,yend=ymax0
                                 ,group=Periods)
                            ,arrow = arrow(angle =20,type = "closed"
                                           ,ends = "both",length = unit(0.4, "cm"))
                            ,colour=c(2,3,4,3)
                            ,size=.01,alpha=.5
              )+
              geom_text(inherit.aes = F,data=san
                        ,aes(x=(xmin0+xmax0)/2,y=ymax0+1,label=Periods
                             ,vjust=0,hjust=.5)
                        ,colour=c(2,3,4,3),size=3.5
              )+
              # scale_colour_manual(name = 'the colour',
              #                     values =c('black'='black','red'='red'), labels = c('c2','c1'))
              geom_vline(xintercept = c(2010,2013,2018)
                         ,lty=2,lwd=.2,col="gray60")+
              coord_cartesian(xlim = c(2000, 2020))+
              scale_x_continuous(name="\nTime (year)",breaks = 2000:2020)+
              scale_y_continuous(name = cnames[1])+
              theme_classic()+
              theme(axis.text.x = element_text(angle = 90))
            mcp_fit_plot
             },res=120)

        },ignoreNULL = FALSE,ignoreInit = TRUE)



      }


    }### end if !is.null


  })

}
shinyApp(ui,server)
