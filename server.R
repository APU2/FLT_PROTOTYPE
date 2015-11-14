library(shiny)

shinyServer(function(input, output) {
    
              #get data for run bar plot
              run_brk_df <- reactive({
                        run_brk<- RUNSTATS[STRT>ymd(input$sel_dat[1]) & STRT <ymd(input$sel_dat[2]) & FILTER_ID %in% input$sel_filt_id,length(RUN), by = list(FILTER_ID, RUN_FAIL) ]
                        run_brk[,RUN_FAIL := factor(RUN_FAIL, labels = c("PASS", "FAIL"))]
                        return(run_brk)
              })
              
              # plot run failure bar plot
              output$run_bar_plot <- renderChart2({
                        run_bar <- nPlot(V1 ~ FILTER_ID, group = 'RUN_FAIL', data = run_brk_df(), type = 'multiBarChart')
                        run_bar$chart(color = c('green', 'red'))
                        run_bar$chart(reduceXTicks = FALSE)
                        run_bar$chart(margin = list(left = 90, bottom = 120))
                        run_bar$xAxis(rotateLabels=-90, axisLabel = "Filter")
                        run_bar$yAxis( axisLabel = "Count of filter runs")
                        return(run_bar)
              })
              
              #prep data for mttf plot
              mtbf_df<- reactive({
                  MTBF<- FAILURES[STRT>ymd(input$sel_dat[1]) & STRT <ymd(input$sel_dat[2]) & FILTER_ID %in% input$sel_filt_id]
                        MTBF<-ddply(MTBF, .(FILTER_ID,STREAM),summarise,
                                    mtbf = round(sum(as.numeric(c(STRT, ymd(input$sel_dat[2])))-as.numeric(c(ymd(input$sel_dat[1]), END)))/length(RUN)/(60*60*24),1))
                        MTBF<-MTBF[order(MTBF$mtbf, decreasing = TRUE),]
                        return(MTBF)
              })
              
              # plot mtbf plot
              output$mtbf_plt <- renderChart2({
                  mtbfplot <- nPlot(mtbf ~FILTER_ID ,  data = mtbf_df(), type = 'discreteBarChart')
                  #mtbfplot$chart(reduceXTicks = FALSE)
                  mtbfplot$chart(margin = list(left = 90, bottom = 120), color =list(dput(mtbf_df()$COL)))
                  mtbfplot$chart(
                      color = "#! function(d){
    var ourColorScale = d3.scale.ordinal().domain(['1','2','3','4']).range(['red','blue','orange','purple']);
    return ourColorScale(d.STREAM);
    }!#")
                  mtbfplot$xAxis(rotateLabels=-90, axisLabel = "Filter")
                  mtbfplot$yAxis( axisLabel = "Mean time between failures (days)")
                  return(mtbfplot)
              })
              
              
              # prep data for control charts
              
              qcc_df <- reactive({
                        qcc_dat<- RUNSTATS_MEL[RUNSTATS_MEL$STRT>ymd(input$sel_dat[1]) & 
                                                         RUNSTATS_MEL$STRT <ymd(input$sel_dat[2]) &
                                                         RUNSTATS_MEL$FILTER_ID %in% input$sel_filt_id & 
                                                         RUNSTATS_MEL$variable %in% input$sel_sig_id ]
                        #qcc_dat<- qcc_dat[,cusum_filt := cumsum(value - mean(value)), by = FILTER_ID]
                        qcc_dat<- qcc_dat[,cusum_day_mean :=  mean(value), by = list(year(STRT),yday(STRT))]
                        qcc_dat<- qcc_dat[,cusum_day :=  cumsum(value-cusum_day_mean), by = FILTER_ID]
                        
                        return(qcc_dat)
              })
              
#               # plot distributions comparions between filters
              output$boxplot<- renderPlot({
                                       bwplt<- ggplot(qcc_df(), aes(x=FILTER_ID, y=value, group = FILTER_ID))+
                                           geom_boxplot()+
                                           theme_classic()+
                                           theme(axis.text.x = element_text(angle = 90, hjust = -9))
                                return(bwplt)
              })
    
              #get data for trend chart
              trend_zoos_df<- reactive({
                        trend_zoos<- dlply(qcc_df(), .(FILTER_ID), .fun = zoowrap)
                        trend_zoos<- do.call(merge, trend_zoos)
                        return(trend_zoos)
              })
    
              #get data for cusum trend chart
              trend_zoos_cusum_df<- reactive({
                        trend_zoos_cusum<- dlply(qcc_df(), .(FILTER_ID), .fun = zoowrap_cusum)
                        trend_zoos_cusum<- do.call(merge, trend_zoos_cusum)
                        return(trend_zoos_cusum)
              })
              
              # get data for sankey diagram
              
              sankey_df<- reactive({
                  fail_brk<- FAILURES[STRT>ymd(input$sel_dat[1]) & STRT <ymd(input$sel_dat[2]) & FILTER_ID %in% input$sel_filt_id]
          
                  fail_brka<- ddply(fail_brk, .( HIGH_PERIOD,FILTER_ID),summarise,
                                        count = length(RUN))
                  colnames(fail_brka)<- c("from","to","count")
                  fail_brkb<- ddply(fail_brk, .( FILTER_ID,FILTERS_IMPACTED),summarise,
                                    count = length(RUN))
                  colnames(fail_brkb)<- c("from","to","count")
                  fail_brk<-rbind(fail_brka,fail_brkb)
                  
                        return(fail_brk)
              })
              
              # plot sankey diagram
              output$sanky<- renderGvis({
                  sank<-gvisSankey(sankey_df(), from="from", 
                             to="to", weight="count",
                             options=list(
                                 height=500,
                                 width = 600,
                                 sankey="{link:{color:{fill:'lightblue'}}}"
                             ))
                  return(sank)
              })

              
              # get data for timeline
              timeline_df<- reactive({ timeline_dat<- RUNSTATS[STRT>ymd(input$sel_dat[1]) & STRT <ymd(input$sel_dat[2]) & FILTER_ID %in% input$sel_filt_id & RUN_FAIL == 1, ]
              return(timeline_dat)
              })
              
              
              
              # plot timeline 
              output$Timeline <- renderGvis({
                        tl<-gvisTimeline(data=timeline_df(), 
                                       rowlabel="FILTER_ID",
                                       barlabel="FAILTYPEA",
                                       start="STRT", 
                                       end="END",
                                       options=list(title = "Run failure timeline",
                                                 height=1200,
                                                 width = 1000,
                                            colors="['#0000FF', '#00FFFF', '#0099FF']",
                                            timeline="{colorByBarLabel:true}"))
                        return(tl)
              })
              
              # plot dygraphs trend plot
              
              output$dygraph_trend <- renderDygraph({
                        dygraph(trend_zoos_df(), main = "Aggregated signal trends") %>%
                                  dyOptions(drawPoints = TRUE, pointSize = 2, strokeWidth = 0)%>%
                                  dyRangeSelector(height = 20)
              })
              
              # plot dygraphs cusum trend plot
              
              output$dygraph_trend_cusum <- renderDygraph({
                        dygraph(trend_zoos_cusum_df(), main = print(input$sel_sig_id)) %>%
                                  dyOptions(drawPoints = TRUE, pointSize = 2, strokeWidth = 0)%>%
                                  dyRangeSelector(height = 20)
              })
              
              #get data for box and whisker plot
              whisker_dat <- reactive({ 
                        whisk<- RUNSTATS_MEL[RUNSTATS_MEL$STRT>ymd(input$sel_dat[1]) & 
                                                       RUNSTATS_MEL$STRT <ymd(input$sel_dat[2]) &
                                                       RUNSTATS_MEL$FILTER_ID %in% input$sel_filt_id, ]
                        whisk[,TURB_FAIL := factor(RUN_FAIL, labels = c("OK", "HIGH"))]
                        return(whisk)
                        
              })
    #           
    #           # plot data for potential predictor variables
    #           output$sig_whisker <- renderPlot({
    #                     ggplot(whisker_dat(), aes(x= RUN_FAIL, y = value, fill = RUN_FAIL))+
    #                               geom_boxplot()+
    #                               facet_wrap(~variable, scales = "free")
    #           })
              
              
              # gather data for classification tree
              
              run_ctree <- reactive({
                        tree_dat<-data.table(RUNSTATS %>%
                                  filter(RUNSTATS$STRT>ymd(input$sel_dat[1]) & 
                                                   RUNSTATS$STRT <ymd(input$sel_dat[2]) &
                                                   RUNSTATS$FILTER_ID %in% input$sel_filt_id)  %>%
                                      dplyr::select(c(match("FAILTYPEA",names(RUNSTATS) ),match(input$sel_sig_id_mod, names(RUNSTATS) )) ))
                        tree<-rpart(FAILTYPEA~., data = tree_dat, method = "class")
                        return(tree)
              })
              
              
              # create and plot tree
              output$class_tree<- renderPlot({fancyRpartPlot(run_ctree())})
              
              # create reactive dataframes for random forest modelling
              ml_df<- reactive({
                  
                  # split data into training and test sets
                  TRY_FILTER<- data.table(RUNSTATS[STRT>ymd(input$sel_dat[1]) & STRT <ymd(input$sel_dat[2]) & FILTER_ID %in% input$sel_filt_id,])
                  
                  # remove unwated variables from training and test data
                  
                  #TRY_FILTER[,YEAR:= factor(year(STRT))]
                  #TRY_FILTER[,MONTH:= factor(month(STRT))]
                  TRY_FILTER[,STRT:=NULL]
                  TRY_FILTER[,END:=NULL]
                  #TRY_FILTER[,TURB_MEAN:=NULL]
                  #TRY_FILTER[,T95:=NULL]
                  TRY_FILTER[,T99:=NULL]
                  TRY_FILTER[,RUN:=NULL]
                  TRY_FILTER[,TERM_HL_NORM:=NULL]
                  TRY_FILTER[,TURB_LOAD:=NULL]
                  #TRY_FILTER[,SPECIFIC_HL:=NULL]
                  #TRY_FILTER[,HIGH_LIMIT:=NULL]
                  #TRY_FILTER[,RIPEN_MAX:=NULL]
                  #TRY_FILTER[,MAX_TURB_FLOW:=NULL]
                  #TRY_FILTER[,TURB_HIGH_RATE:=NULL]
                  TRY_FILTER[,RIPEN_HIGH:=NULL]
                  #TRY_FILTER[,TURB_DIFF:=NULL]
                  TRY_FILTER[,MAX_TURB_TIME:=NULL]
                  #TRY_FILTER[,STREAM_TURB_SD:=NULL]
                  #TRY_FILTER[,STREAM_TURB_MEAN:=NULL]
                  TRY_FILTER[,FAILTYPEA:=NULL]
                  TRY_FILTER[,HIGH_PERIOD:=NULL]
                  TRY_FILTER[,FILTERS_IMPACTED:=NULL]
                  TRY_FILTER[,FAILTYPEB:=NULL]
                  
                  TRY_FILTER[,CONCURRENT_FAILS:=NULL]
                  TRY_FILTER[,CONCURRENT_FAILS_STREAM:=NULL]
                  
                  return(TRY_FILTER)
              })
              
              # get index for training data
              train_ind<- reactive({
                  train = sample(1: nrow ( ml_df() ) , nrow ( ml_df() ) * .8)
                  return(train)
              })
              
              # get training data
              
              train_df<- reactive({
                  TRY_FILTER_TRAIN<-ml_df()[train_ind(),]
                  return(na.omit(TRY_FILTER_TRAIN)) # aware NA omit used
              })
              
              # get test data
              
              test_df<- reactive({
                  TRY_FILTER_TEST<-ml_df()[-train_ind(),]
                  return(na.omit(TRY_FILTER_TEST))# aware NA omit used
                  
              })
              
              # create random forest model
              
              rf_mod<- reactive({
                  rf_class<- randomForest(formula = factor(RUN_FAIL)~., data = train_df(), mtry = 7, importance = FALSE , ntree = 80, nodesize= 1)
#                   trained<- train(formula = factor(RUN_FAIL)~., data = RUNSTATS , method = "rf",
#                                    trControl = trainControl(method = "cv", number="5") , allowParallel = TRUE)
#                    # rf_class<- ctree(formula = factor(RUN_FAIL)~., data = train_df(), mtry = 7, importance = FALSE , ntree = 1000, nodesize= 1)
#                    rf_class<- trained$
                  return(rf_class)
              })
              
              
              # plot random forest model
              
              output$rf_plot<- renderPlot({
                  model<-rf_mod()
                  layout(matrix(c(1,2),nrow=1),
                         width=c(4,1)) 
                  par(mar=c(5,4,4,0)) #No margin on the right side
                  plot(model, log="y")
                  par(mar=c(5,0,4,2)) #No margin on the left side
                  plot(c(0,1),type="n", axes=F, xlab="", ylab="")
                  legend("top", colnames(model$err.rate),col=1:2,cex=0.8,fill=1:4)
                  
              })
              
              # predict using random forest model
              
              rf_test<- reactive({
                  RF_PRED<-predict(rf_mod(), newdata = test_df())
                  return(RF_PRED)
              })

              # create contingency table for rf predictions
              
              rf_cont<- reactive({
                  cont_tab<- confusionMatrix(data =  rf_test(), reference = test_df()$RUN_FAIL, positive = 1)
                  return(cont_tab)
              })
              
              # random forest variable importance table
              
              var_import<- reactive({
                  vimp<- data.frame(randomForest::importance(rf_mod(),  scale = FALSE))
                  vimp$variable<- row.names(vimp)
                  return(vimp)
              })
              
              # random forest variable importance plot
              
              output$var_imp<- renderPlot({ varImpPlot(rf_mod())})
              
              # inference engine
              
              likely_cause<- reactive({
                  diag_tab<- ddply(INF_TAB, .(variable),mutate,
                                   score = value * var_import()[match(VARIABLE, row.names(var_import())), "MeanDecreaseGini"])
                  diag_tab<-ddply(diag_tab, .(variable), summarise,
                                  score = mean(score, na.rm = TRUE))
                  diag_tab<- diag_tab[order(diag_tab$score, decreasing = TRUE),]
                  diag_tab$cause<- FAULT_LAB[match(diag_tab$variable, FAULT_LAB$VARIABLE),"label"]
                  diag_tab$colour<- FAULT_LAB[match(diag_tab$variable, FAULT_LAB$VARIABLE),"colour"]
                  #diag_tab$cause<- gsub("[.]", " [.]", diag_tab$cause)
                  return(diag_tab)
              })
              
              output$cause_plot<- renderChart2({
                  causeplot <- nPlot(score ~cause ,  data = likely_cause(), type = 'discreteBarChart')
#                   causeplot$chart(reduceXTicks = FALSE)
                  causeplot$chart(margin = list(left = 90, bottom = 200), color = likely_cause()$colour)
                  causeplot$xAxis(rotateLabels=-90, axisLabel = "Potential cause")
                  causeplot$yAxis( axisLabel = "Score")
                  return(causeplot)
              })
              
              
              # output data table
              
              output$table_conf <- renderPrint({ rf_cont()})
              output$table_runstats <- renderDataTable({ likely_cause()})
              output$text1 <- renderText({ 
                  paste("You have selected", input$sel_sig_id)
              })
})


