library(shiny)
library(googleAuthR)
library(googleID)
library(googleAnalyticsR)
library(listviewer)
library(testthat)
library(gentelellaShiny)

menuItems <- list(
  sideBarElement(" Resources ",
                 icon = icon("book"),
                 list(a(href="http://code.markedmondson.me/googleAnalyticsR", 
                        HTML(paste(icon("line-chart"), "googleAnalyticsR"))),
                      a(href="https://developers.google.com/analytics/devguides/reporting/core/v4/", 
                        HTML(paste(icon("google"), "API V4 Docs"))))
  ),
  sideBarElement(" Contact ",
                 icon = icon("envelope"),
                 list(a(href="http://twitter.com/HoloMarkeD",
                        HTML(paste(icon("twitter"), "@HoloMarkeD"))),
                      a(href="http://code.markedmondson.me",
                        HTML(paste(icon("rss"), " Blog"))),
                      a(href="https://github.com/MarkEdmondson1234/",
                        HTML(paste(icon("github"), " Github"))))
  ),
  sideBarElement(column(width = 12, googleAuthR::googleAuthUI("login"),
                        icon = NULL))
  )


gentelellaPage(title_tag = "Google Analytics v4 API Demo",
               site_title = a(class = "site_title", icon("line-chart"), "GA API v4"),
               tracking = trackingTags("gtm", "GTM-WFFMBH"), 
               menuItems = menuItems,

               authDropdownUI("auth_menu", inColumns = TRUE),
               tabsetPanel(
                 tabPanel("Segments",
                      fluidRow(
                        graph_box(
                          boxtitle = "v4 Segments", 
                          subtitle = "More powerful, but more complex",
                          datepicker = dateRangeInput("date_seg", NULL, start = Sys.Date() - 30),
                          tagList(column(width = 6,
                                         segmentBuilderUI("demo_segments")
                          ),
                          column(width = 6,
                                 column(width = 6,
                                        multi_selectUI("metric_seg", "Metric", width = "100%")  
                                 ),
                                 column(width = 6,
                                        multi_selectUI("dim_seg", "Dimensions", width = "100%")
                                 ),
                                 dashboard_box(width = 12, height = 500, box_title = "Data download",
                                               actionButton("get_seg", 
                                                            "Fetch Segment Data", 
                                                            icon = icon("download"), 
                                                            class = "btn-success"),
                                   DT::dataTableOutput("segment_table")
                                 ),
                                 dashboard_box(width = 12, height = 500, box_title = "Segment JSON Object",
                                   jsoneditOutput("segment_object")                
                                 )

                          )
                          )) 
                      )

                          ),
                 tabPanel("Calculated Metrics", 
                          graph_box(boxtitle = "Calculated Metrics", 
                                    subtitle = "Create your own metrics on the fly",
                                    width = 12, datepicker = dateRangeInput("date_clac", NULL, start = Sys.Date() - 30),
                                    column(width = 6,
                                                   textInput("calculated_name",
                                                             label = "Calculated Name",
                                                             value = "Sessions Per Pageview")
                                    ),
                                    column(width = 6,
                                           textInput("calculated_exp",
                                                     label = "Calculated Expression",
                                                     value = "ga:sessions / ga:pageviews")
                                    ),
                                    column(width = 6,
                                           multi_selectUI("metric_calc", "Normal Metrics")     
                                    ),
                                    column(width = 6,
                                           multi_selectUI("dim_calc", "Dimensions")  
                                    )
                          ),
                          column(width = 12, h4("Data output")),
                          dashboard_box(width = 6, height = 500, box_title = "Calculated Metrics Data",
                                        actionButton("get_calc", 
                                                     "Fetch Calculated Metric data", 
                                                     icon = icon("download"), 
                                                     class = "btn-success"),
                                        DT::dataTableOutput("calc_table")
                          ),
                          dashboard_box(width = 6, height = 500, box_title = "Calculated metrics JSON Object",
                                        jsoneditOutput("calc_object")                
                          )
                          ),
                 tabPanel("Pivots", 
                          graph_box(boxtitle = "Pivots", 
                                    subtitle = "Reshape the data you can fetch",
                                    width = 12, datepicker = dateRangeInput("date_pivot", NULL, start = Sys.Date() - 30),
                                    column(width = 6,
                                           multi_selectUI("metric_pivot", "Metric"),
                                           multi_selectUI("dim_pivot", "Dimensions") 
         
                                    ),
                                    column(width = 6,
                                           multi_selectUI("metric_pivot2", "Pivot Metric"),
                                           multi_selectUI("dim_pivot2", "Pivot Dimension")
                   
                                    )
                          ),
                          column(width = 12, h4("Data output")),
                          dashboard_box(width = 6, height = 500, box_title = "Pivots Data",
                                        actionButton("get_pivot", 
                                                     "Fetch Pivot data", 
                                                     icon = icon("download"), 
                                                     class = "btn-success"),
                                        DT::dataTableOutput("pivot_table")
                          ),
                          dashboard_box(width = 6, height = 500, box_title = "Pivot JSON Object",
                                        jsoneditOutput("pivot_object")                
                          )
                          
                          ),
                 tabPanel("Multiple Dates",
                          graph_box(boxtitle = "Multiple Dates", 
                                    subtitle = "Comparisons without two API calls",
                                    width = 12, datepicker = NULL,
                                    column(width = 6,
                                           multi_selectUI("metric_md", "Metric"),
                                           multi_selectUI("dim_md", "Dimensions")
                                    ),
                                    column(width = 6,
                                           dateRangeInput("date1_md", "Date Range 1", start = Sys.Date() - 30),
                                           dateRangeInput("date2_md", "Date Range 2", start = Sys.Date() - 60, end = Sys.Date() - 31)
                                    )
                          ),
                          column(width = 12, h4("Data output")),
                          dashboard_box(width = 6, height = 500, box_title = "Multiple Dates Data",
                                        actionButton("get_md", 
                                                     "Fetch Multiple Dates data", 
                                                     icon = icon("download"), 
                                                     class = "btn-success"),
                                        DT::dataTableOutput("md_table")
                          ),
                          dashboard_box(width = 6, height = 500, box_title = "Multiple Dates JSON Object",
                                        jsoneditOutput("md_object")                
                          )
                          
                          ),
                 tabPanel("Cohorts",
                          graph_box(boxtitle = "Cohorts", 
                                    subtitle = "User bucket changes over time",
                                    width = 12, datepicker = dateRangeInput("", NULL),
                                    column(width = 4,
                                           multi_selectUI("metric_coh", "Metric"),
                                           multi_selectUI("dim_coh", "Dimensions") 
                                    ),
                                    dashboard_box(box_title = "Cohort defintions", width = 8, height = 400,
                                      fluidRow(
                                        column(width = 6,
                                               textInput("coh1", label = "Cohort 1 Name", value = "Cohort 1")     
                                        ),
                                        column(width = 6,
                                               dateRangeInput("date_coh1", "Cohort 1 Range", start = Sys.Date() - 30, end = Sys.Date())
                                        )
                                      ),
                                      fluidRow(
                                        column(width = 6,
                                               textInput("coh2", label = "Cohort 2 Name", value = "Cohort 2")
                                        ),
                                        column(width = 6,
                                               dateRangeInput("date_coh2", "Cohort 2 Range", start = Sys.Date() - 60, end = Sys.Date() - 30)
                                        )
                                      ),
                                      fluidRow(
                                        column(width = 6,
                                               textInput("coh3", label = "Cohort 3 Name", value = "Cohort 3")
                                        ),
                                        column(width = 6,
                                               dateRangeInput("date_coh3", "Cohort 3 Range", start = Sys.Date() - 90, end = Sys.Date() - 60) 
                                        )
                                      ),
                                      fluidRow(
                                        column(width = 6,
                                               textInput("coh4", label = "Cohort 4 Name", value = "Cohort 4")   
                                        ),
                                        column(width = 6,
                                               dateRangeInput("date_coh4", "Cohort 4 Range", start = Sys.Date() - 120, end = Sys.Date() - 90)
                                        )) 
                                      
                                      
                                    )

                          ),
                          column(width = 12, h4("Data output")),
                          dashboard_box(width = 6, height = 500, box_title = "Cohorts Data",
                                        actionButton("get_cohort", 
                                                     "Fetch Cohorts data", 
                                                     icon = icon("download"), 
                                                     class = "btn-success"),
                                        DT::dataTableOutput("cohort_table")
                          ),
                          dashboard_box(width = 6, height = 500, box_title = "Cohort JSON Object",
                                        jsoneditOutput("cohort_object")                
                          )
               )
))