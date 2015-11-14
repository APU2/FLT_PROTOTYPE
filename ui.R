library(shiny)

shinyUI(fluidPage(
    headerPanel('Filter performance and diagnostics at Case study WTW'),
    #h2(list.files(all.files = TRUE, recursive = TRUE))

              sidebarLayout(
                        sidebarPanel(
                                  h2("Select data"),
                                  tabsetPanel( id = "dat_sel_tab",
                                               tabPanel(
                                                         title = "Date and filter selection",
                                                         value = "date_filt_tab_sel",
                                                         dateRangeInput("sel_dat",
                                                                        label = "Period of operation:",
                                                                        start = ymd("13/02/01"),
                                                                        end = ymd("14/10/05")),
                                                         checkboxGroupInput("sel_filt_id", 
                                                                            label = "Select filters of interest",
                                                                            choices = levels(RUNSTATS$FILTER_ID),
                                                                            selected = levels(RUNSTATS$FILTER_ID))
                                               ),
                                               tabPanel(
                                                         title = "QCC signal selection",
                                                         value = "sig_tab_sel",
                                                         radioButtons("sel_sig_id", 
                                                                      label = "Select signal to plot",
                                                                      choices = levels(RUNSTATS_MEL$variable))
                                                         
                                               )#,
#                                                tabPanel(
#                                                          title = "Model parameter selection",
#                                                          value = "sig_tab_sel_mod",
#                                                          checkboxGroupInput("sel_sig_id_mod", 
#                                                                       label = "Select signals to include in model",
#                                                                       choices = colnames(RUNSTATS)[-c(grep("FAIL",colnames(RUNSTATS)),
#                                                                                                              grep("TURB_MEAN",colnames(RUNSTATS)),
#                                                                                                              grep("T95",colnames(RUNSTATS)),
#                                                                                                              grep("T99",colnames(RUNSTATS)))],
#                                                                       selected = colnames(RUNSTATS)[c(15:20)])
#                                                          
#                                                )
                                  )
                                  
                                  
                        ),
                        mainPanel(
                                  tabsetPanel( id = 'view',
                                               tabPanel("Run acceptability by filter",
                                                        h1("High turbidity runs by filter"),
                                                        showOutput("run_bar_plot","nvd3"),
                                                        h4("This plot shows the counts, by filter over the priod selected, of filter runs with acceptable and unacceptable levels of turbidity. Unacceptable runs have a 99th percentile turbidity greater than 0.1 NTU or have a mean turbidity that is significantly different to the mean over the whole period."),
                                                        h1("Mean time between events"),
                                                        showOutput("mtbf_plt","nvd3")
                                               ),
                                               tabPanel("Dominant event types",
                                                        h1("Breakdown of event types"),
                                                        htmlOutput("sanky"),
                                                        h4("This sankey plot shows us some context about the runs identified as having unacceptable turbidity within the period specified. These unacceptable filter runs are arranged on the basis of some contextual data. Width of connections is relative to the number of failing runs within a given context. On the left failing runs are split into those which have the highest turbidity at different stages of the run. Filters with high turbidiy early in the run are more likely to have exhibited poor ripening or an innefective filter to waste or slow start procedure. Filters with high turbidity at the end of the run are exhibiting breakthrough which can occur for a number of reasons. Filter runs exhibiting the higest turbidity during the middle of the run may indicate subject to some process shock. In the middle the filter is identified. On the right the performance of other filters within the WTW is indicated. Issues which affect only one filter are more likely to indicate a filter in poor condition for some reason. Where other filters exhibit issues at the same time an upstream issue is likely to be a more dominant cause of the poorer performance. Performance issues confied within a single stream can allow focus of further investigations. ")
                                               ),
                                               tabPanel("High turbidity runs over time",
                                                        h1("Breakdown of run quality events by over time"),
                                                        htmlOutput("Timeline"),
                                                        h4("This timeline plot identifies runs with unacceptable turbidiy over the period of interest. The colour indicates the period of the run with the highest turbididy. As such a long term overview of treatment performance can be gained. Periods of treatment challenge can be easily identified and investigated further. ")
                                               ),
#                                                tabPanel("Comparison of filters",
#                                                         h1("Comparison between filters"),
#                                                         plotOutput("boxplot")
#                                                ),
                                               tabPanel("Signal trend",
                                                        h1("Signals aggregated by filter run"),
                                                        dygraphOutput("dygraph_trend"),
                                                        h4("This plot can be used to examine the trends of different signals aggregated over the period of operation for different filter runs. Changes in the upstream signals can provide an indication of treatment purturbations which may be associated with poor filtration performance.")
                                               ),
                                               tabPanel("Cusum signal trend",
                                                        h1("Cusum charts for filter runs"),
                                                        dygraphOutput("dygraph_trend_cusum"),
                                                        h4("")
                                               ),
    #                                            tabPanel("Signal selector",
    #                                                     h1("Diagnostic signal identification"),
    #                                                     plotOutput("sig_whisker")
    #                                            ),
#                                                tabPanel("Classification tree",
#                                                         h1("Classification tree for filter event"),
#                                                         plotOutput("class_tree")
#                                                ),
                                                tabPanel("Diagnosis",
#                                                          h1("Plot of random forest"),
#                                                          plotOutput("rf_plot"),
#                                                          h1("Variable importance for event prediction"),
#                                                          plotOutput("var_imp"),
#                                                          h1("Contingency table from RF prediction"),
#                                                          tableOutput("rf_cont"),
                                                         h1("Suggested dominant causes of run event over period"),
                                                         showOutput("cause_plot", "nvd3"),
                                                         h2("Model performance"),                  
                                                         verbatimTextOutput(outputId="table_conf")
                                                ),
                                               tabPanel("Data table",
                                                        h1("Data viewer"),
                                                        dataTableOutput(outputId="table_runstats"),
                                                        textOutput("text1")
                                               )
                                               )
                        )
              )
))
