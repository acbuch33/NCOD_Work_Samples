dashboardPage(
  
  dashboardHeader(title = "Consulting Report Generator"),
  dashboardSidebar(
    sidebarMenu(
      id = "Time",
      menuItem("Time 1", tabName = "Time1"),
      menuItem("Time 2", tabName = "Time2"),
      menuItem("User Guide", tabName = "Userguide")
    )
  ),
  dashboardBody(
      add_busy_spinner(spin = "fading-circle", timeout=500),
          inlineCSS(".shinybusy{
            top:50% !important;
            left: 50% !important;
            right: 0% !important;
            height: 300px !important;
            width: 300px !important;
          }"),
    tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #636c80;
        }
        .skin-blue .main-header .logo:hover {
                              background-color: #636c80;
        }
        /* navbar */
        .skin-blue .main-header .navbar {
                              background-color: #636c80;
                              }
                              
        #sidebarItemExpanded > ul > li.active > a{
                              color: #fff;
                              background: #1e282c;
                              border-left-color: #636c80;
        }
        
        #sidebarItemExpanded > ul > li:hover > a{
                              color: #fff;
                              background: #636c80;
                              border-left-color: #fff;
        }
        .sidebar-menu li a { font-size: 20px; }
        .shiny-output-error-validation {color:red;}
        .box {
              border-top: none;
        }
        .sidebar-toggle{
              display:none;                
        }
        .info-box{
                  box-shadow:3px 3px 1px rgba(0,0,0,.1);            
        }
        .info-box-number{
                          display: block;
                          font-weight: normal;
                          font-size: 15px;
        }
        .info-box-content{
                          padding: 5px 10px 0px 10px;    
        }                      
        .info-box-icon{
                      background-color: #c69e5d !important;        
        }
        #response_count > div > div.inner {
                        background-color: #809e94;
                        padding: 10px 10px 1px 10px;       
        }
        #response_count_t1 > div > div.inner {
                        background-color: #809e94;
                        padding: 10px 10px 1px 10px;       
        }
        #response_count_t2 > div > div.inner {
                        background-color: #809e94;
                        padding: 10px 10px 1px 10px;       
        }
        .small-box .icon-large{
                              position: absolute;
                              top: 1%;
                              bottom: 5px;
                              right: 5px;
                              font-size: 70px;
                              color: rgba(0, 0, 0, 0.15);
        }
        .sweet-alert button.cancel{
                              background-color: #f57b6c !important;
        }
        .sweet-alert button.cancel:hover{
                              background-color: #f99184 !important;
        }
        .confirm{
                              background-color: #1bb5fb !important;
        }
        .confirm:hover{
                              background-color: #aedef4 !important;
        }
        .guide{
                              padding-left: 10px;
                              font-size: 18px;
        }
        .guide_list{
                              font-size: 18px;        
        }                      
        img {
                              display:block; 
                              margin-left:auto; 
                              margin-right:auto; 
                              border-radius: 10px;
                              box-shadow: 5px 5px 5px 5px #636c80;
        }
                              '))),
    shinyjs::useShinyjs(),
    tabItems(
      tabItem("Time1",
              # fluidRow(
              #   box(
              #     selectInput("consultant", "Who administered the survey?", choices=as.list(consultants$Name), multiple=FALSE),
              #     selectInput("type_report", "Which Report is this?", choices=list("ETA" = "Executive Team Assessment (ETA)",
              #                                                                      "LIA" = "Leadership Impact Assessment (LIA)",
              #                                                                      "GES" = "Group Experience Survey",
              #                                                                      "TA" = "Team Assessment 2.0"), multiple=FALSE),
              #     uiOutput("surveys"),
              #     dateRangeInput('dateRange',
              #                    label = 'Dates of Administration',
              #                    start = Sys.Date(), 
              #                    end = Sys.Date()+1,
              #                    format = "M-dd-yyyy",
              #     ),
              #     actionButton("pull", "Pull Data")
              #   ),
              #   box(id = "txtinptbox_t1",
              #       conditionalPanel("output.ui_t1=='1'",
              #                        conditionalPanel('input.type_report == "Leadership Impact Assessment (LIA)" || input.type_report == "Group Experience Survey"',
              #                                         numericInput('resp_total','Number of respondents invited:',value = 1,min=1,step=1)
              #                        ),
              #                        textInput('team_name', "Team Name", placeholder = "e.g., Jackson VAMC ELT"),
              #                        # textInput('start_end_date', "Administration Dates", placeholder = "e.g., January 1 - January 15, 2023"),
              #                        conditionalPanel("output.runReport_t1 == 'yes'",
              #                                         downloadButton("report", "Download Report")
              #                        )
              #       )
              #       
              #       
              #       # uiOutput("dl_t1")
              #   )
              #   
              # ),
              fluidRow(
                box(width = 6,
                    #test=as.list(consultants$Name),
                    # selectizeInput("type_report", "Which Report is this?", choices=list("LTA" = "Leadership Team Assessment (LTA)",
                    #                                                                     "LIA" = "Leadership Impact Assessment (LIA)",
                    #                                                                     "GES" = "Group Experience Survey",
                    #                                                                     "TA" = "Team Assessment 2.0"), multiple=TRUE,options = list(maxItems = 1)),
                    selectizeInput("type_report", "Which Report is this?", choices=list("LTA" = "Leadership Team Assessment (LTA)",
                                                                                        "LIA" = "Leadership Impact Assessment (LIA)",
                                                                                        "GES" = "Group Experience Survey"), multiple=TRUE,options = list(maxItems = 1)),
                    selectizeInput("consultant","Who administered the survey?", choices=as.list(consultants$Name),multiple=T,options = list(maxItems = 1)),  
                  #selectInput("consultant", "Who administered the survey?", choices=as.list(consultants$Name), multiple=FALSE),
                  
                ),
                box(
                    width = 6,
                    uiOutput("surveys"),
                    dateRangeInput('dateRange',
                                   label = 'Dates of Administration',
                                   start = NA, 
                                   end = NA,
                                   format = "M-dd-yyyy",
                    ),
                    # uiOutput("dl_t1")
                )
                
              ),
              fluidRow(
                column(width=4),
                column(width=4,
                       actionButton("pull", "Pull Data"),
                       align = "center")
              ),
              tags$br(),
              tags$style(".small-box.bg-teal { background-color: #00A394 !important;}"),
              fluidRow(
                column(width = 6,
                       box(id = "teamname_t1",
                           width = 12,
                           conditionalPanel("output.ui_t1=='1'",
                                            # conditionalPanel('input.type_report == "Leadership Impact Assessment (LIA)" || input.type_report == "Group Experience Survey"',
                                            #                  numericInput('resp_total','Number of respondents invited:',value = 1,min=1,step=1)
                                            # ),
                                            textInput('team_name', "Team Name", placeholder = "e.g., Jackson VAMC ELT"),
                                            # textInput('start_end_date', "Administration Dates", placeholder = "e.g., January 1 - January 15, 2023"),
                                            # conditionalPanel("output.runReport_t1 == 'yes'",
                                            #                  downloadButton("report", "Download Report")
                                            # )
                           )
                       )),
                column(width = 6,
                       box(id = "numresponses_t1",
                           width = 12,
                           conditionalPanel("output.ui_t1=='1'",
                                        conditionalPanel('input.type_report == "Leadership Impact Assessment (LIA)" || input.type_report == "Group Experience Survey"',
                                                         numericInput('resp_total','Number of respondents invited:',value = 1,min=1,step=1)
                                        ),
                                        #textInput('team_name', "Team Name", placeholder = "e.g., Jackson VAMC ELT"),
                                        # textInput('start_end_date', "Administration Dates", placeholder = "e.g., January 1 - January 15, 2023"),
                                        # conditionalPanel("output.runReport_t1 == 'yes'",
                                        #                  downloadButton("report", "Download Report")
                                        # )
                       )
                    ),
                    
                ),
              #   column(width=4,
              #          box(width = 12,
              #              conditionalPanel("output.ui_t1=='1'",
              #                               conditionalPanel('input.type_report == "Leadership Impact Assessment (LIA)" || input.type_report == "Group Experience Survey"',
              #                                                numericInput('resp_total','Number of respondents invited:',value = 1,min=1,step=1)
              #                               )))
              # )
              ),
              fluidRow(
                column(width=4),
                column(width = 4,
                       align = "center",
                       #tags$br(),
                       conditionalPanel("output.ui_t1=='1'",
                                        conditionalPanel("output.runReport_t1 == 'yes'",
                                                         downloadButton("report", "Download Report"))),
                       tags$br()
                )
              ),
              fluidRow(
                box(id = "time1_box",
                    width = 12,
                    fluidRow(
                      valueBoxOutput("response_count"),
                      infoBoxOutput("T1_infobox", width = 8),
                      # box(id = "T1_info_box",
                      #     title = "Did you remember to check:",
                      #     div(HTML("<ul>
                      #                 <li>Number of Responses</li>
                      #                 <li>VISN and/or Organization</li>
                      #                 <li>Role</li>
                      #              </ul>")),
                      # )
                    ),
                    reactableOutput("data_survey"),
                    tags$br(),
                    actionButton("rm","Remove Selected Rows")
                )
              )
      ),
      tabItem("Time2",
              # fluidRow(
              #   box(
              #     selectInput("consultant_t2", "Who administered the survey?", choices=as.list(consultants$Name), multiple=FALSE),
              #     selectInput("type_report_t2", "Which Report is this?", choices=list("ETA" = "Executive Team Assessment (ETA)",
              #                                                                         "LIA" = "Leadership Impact Assessment (LIA)",
              #                                                                         "GES" = "Group Experience Survey",
              #                                                                         "TA" = "Team Assessment 2.0"), multiple=FALSE),
              #     uiOutput("surveys_t1"),
              #     uiOutput("surveys_t2"),
              #     # uiOutput("test_file"),
              #     # fileInput("t1_data","Upload T1 Data", accept = c(".csv",".xlsx")),
              #     dateRangeInput('dateRange_t1',
              #                    label = 'Dates of Administration (T1)',
              #                    start = Sys.Date(), 
              #                    end = Sys.Date()+1,
              #                    format = "M-dd-yyyy",
              #     ),
              #     dateRangeInput('dateRange_t2',
              #                    label = 'Dates of Administration (T2)',
              #                    start = Sys.Date(), 
              #                    end = Sys.Date()+1,
              #                    format = "M-dd-yyyy",
              #     ),
              #     #actionButton("pull_t2", "Pull Data")
              #   ),
                fluidRow(
                  box(width = 4,
                      # selectizeInput("type_report_t2", "Which Report is this?", choices=list("LTA" = "Leadership Team Assessment (LTA)",
                      #                                                                        "LIA" = "Leadership Impact Assessment (LIA)",
                      #                                                                        "GES" = "Group Experience Survey",
                      #                                                                        "TA" = "Team Assessment 2.0"), multiple=TRUE, options=list(maxItems = 1)),
                      selectizeInput("type_report_t2", "Which Report is this?", choices=list("LTA" = "Leadership Team Assessment (LTA)",
                                                                                             "LIA" = "Leadership Impact Assessment (LIA)"), multiple=TRUE, options=list(maxItems = 1)),
                      selectizeInput("consultant_t2", "Who administered the survey?", choices=as.list(consultants$Name), multiple=TRUE, options=list(maxItems = 1)),
                  ),
                  box(width = 4,
                    uiOutput("surveys_t1"),
                    dateRangeInput('dateRange_t1',
                                   label = 'Dates of Administration (T1)',
                                   start = NA, 
                                   end = NA,
                                   format = "M-dd-yyyy",
                    )),
                  box(width = 4,
                    uiOutput("surveys_t2"),
                    # uiOutput("test_file"),
                    # fileInput("t1_data","Upload T1 Data", accept = c(".csv",".xlsx")),
                    
                    dateRangeInput('dateRange_t2',
                                   label = 'Dates of Administration (T2)',
                                   start = NA, 
                                   end = NA,
                                   format = "M-dd-yyyy",
                    )),
                    
                  #),
                # box(id = "txtinptbox_t2",
                #     conditionalPanel("output.ui_t2=='1'",
                # 
                #                      textInput('team_name_t2', "Team Name", placeholder = "e.g., Jackson VAMC ELT"),
                #                      conditionalPanel('input.type_report_t2 == "Leadership Impact Assessment (LIA)" || input.type_report_t2 == "Group Experience Survey"',
                #                                       numericInput('resp_total_t1','Number of respondents invited (T1):', value = 1, min=1,step=1)
                #                      ),
                #                      # textInput('start_end_date_t1', "Administration Dates (T1)", placeholder = "e.g., January 1 - January 15, 2023"),
                #                      conditionalPanel('input.type_report_t2 == "Leadership Impact Assessment (LIA)" || input.type_report_t2 == "Group Experience Survey"',
                #                                       numericInput('resp_total_t2','Number of respondents invited (T2):', value = 1, min=1,step=1)
                #                      ),
                #                      # textInput('start_end_date_t2', "Administration Dates (T2)", placeholder = "e.g., January 1 - January 15, 2024"),
                #                      conditionalPanel("output.runReport_t2 == 'yes'",
                #                                       downloadButton("report_t2", "Download Report")
                #                      )
                #     )
                # )
              ),
              fluidRow(
                column(width=4),
                column(width=4,
                       actionButton("pull_t2", "Pull Data"),
                       align = "center")
              ),
              tags$br(),
              fluidRow(
                column(width = 4,
                       box(id = "teamname_t2",
                           width = 12,
                           align = "center",
                           conditionalPanel("output.ui_t2=='1'",
                                            
                                            textInput('team_name_t2', "Team Name", placeholder = "e.g., Jackson VAMC ELT"),
                                            # conditionalPanel('input.type_report_t2 == "Leadership Impact Assessment (LIA)" || input.type_report_t2 == "Group Experience Survey"',
                                            #                  numericInput('resp_total_t1','Number of respondents invited (T1):', value = 1, min=1,step=1)
                                            # ),
                                            # textInput('start_end_date_t1', "Administration Dates (T1)", placeholder = "e.g., January 1 - January 15, 2023"),
                                            # conditionalPanel('input.type_report_t2 == "Leadership Impact Assessment (LIA)" || input.type_report_t2 == "Group Experience Survey"',
                                            #                  numericInput('resp_total_t2','Number of respondents invited (T2):', value = 1, min=1,step=1)
                                            # ),
                                            # textInput('start_end_date_t2', "Administration Dates (T2)", placeholder = "e.g., January 1 - January 15, 2024"),
                                            # conditionalPanel("output.runReport_t2 == 'yes'",
                                            #                  downloadButton("report_t2", "Download Report")
                                            # )
                           )
                       )
                    ),
                column(width = 4,
                       box(id = "numresponses_t1_t2",
                           width = 12,
                           align = "center",
                           conditionalPanel("output.ui_t2=='1'",
                                            
                                            #textInput('team_name_t2', "Team Name", placeholder = "e.g., Jackson VAMC ELT"),
                                            conditionalPanel('input.type_report_t2 == "Leadership Impact Assessment (LIA)" || input.type_report_t2 == "Group Experience Survey"',
                                                             numericInput('resp_total_t1','Number of respondents invited (T1):', value = 1, min=1,step=1)
                                            ),
                                            # textInput('start_end_date_t1', "Administration Dates (T1)", placeholder = "e.g., January 1 - January 15, 2023"),
                                            # conditionalPanel('input.type_report_t2 == "Leadership Impact Assessment (LIA)" || input.type_report_t2 == "Group Experience Survey"',
                                            #                  numericInput('resp_total_t2','Number of respondents invited (T2):', value = 1, min=1,step=1)
                                            # ),
                                            # textInput('start_end_date_t2', "Administration Dates (T2)", placeholder = "e.g., January 1 - January 15, 2024"),
                                            # conditionalPanel("output.runReport_t2 == 'yes'",
                                            #                  downloadButton("report_t2", "Download Report")
                                            # )
                           )
                       )
                ),
                column(width = 4,
                       box(id = "numresponses_t2",
                           width = 12,
                           align = "center",
                           conditionalPanel("output.ui_t2=='1'",
                                            
                                            #textInput('team_name_t2', "Team Name", placeholder = "e.g., Jackson VAMC ELT"),
                                            # conditionalPanel('input.type_report_t2 == "Leadership Impact Assessment (LIA)" || input.type_report_t2 == "Group Experience Survey"',
                                            #                  numericInput('resp_total_t1','Number of respondents invited (T1):', value = 1, min=1,step=1)
                                            # ),
                                            # textInput('start_end_date_t1', "Administration Dates (T1)", placeholder = "e.g., January 1 - January 15, 2023"),
                                            conditionalPanel('input.type_report_t2 == "Leadership Impact Assessment (LIA)" || input.type_report_t2 == "Group Experience Survey"',
                                                             numericInput('resp_total_t2','Number of respondents invited (T2):', value = 1, min=1,step=1)
                                            ),
                                            # textInput('start_end_date_t2', "Administration Dates (T2)", placeholder = "e.g., January 1 - January 15, 2024"),
                                            # conditionalPanel("output.runReport_t2 == 'yes'",
                                            #                  downloadButton("report_t2", "Download Report")
                                            # )
                           )
                       )
                ),
              ),
              fluidRow(
                column(width = 4),
                column(width=4,
                       align = "center",
                       conditionalPanel("output.ui_t2=='1'",
                                        conditionalPanel("output.runReport_t2 == 'yes'",
                                                         downloadButton("report_t2", "Download Report")))
                )
              ),
              tags$br(),
              fluidRow(
                box(id = "time2_t1_box",
                    div(HTML("<p style='font-size: 30px; padding:5px;'>Time 1</p>")),
                    width = 12,
                    fluidRow(
                      valueBoxOutput("response_count_t1"),
                      infoBoxOutput("T2_infobox_t1", width = 8),
                      #infoBoxOutput("response_count_t1")
                    ),
                    reactableOutput("data_survey_t1"),
                    tags$br(),
                    actionButton("rm_t1","Remove Selected Rows")
                )
              ),
              tags$br(),
              fluidRow(
                box(id = "time2_box",
                    div(HTML("<p style='font-size: 30px; padding:5px;'>Time 2</p>")),
                    width = 12,
                    fluidRow(
                      valueBoxOutput("response_count_t2"),
                      infoBoxOutput("T2_infobox_t2", width = 8),
                    ),
                    reactableOutput("data_survey_t2"),
                    tags$br(),
                    actionButton("rm_t2","Remove Selected Rows")
                )
              )
      ),
      tabItem("Userguide",
              box(width = 12,
                  div(HTML("<h2 id='top'>Table of Contents</h2>

                           <br>
                           <div class='guide'>
                              <ul>
                                 <li><a href='#function'>What does this app do?</a></li>
                                 <li><a href='#running'>Running Reports</a></li>
                                 <li><a href='#remove'>Removing Rows/Data Management</a></li>
                                 <li><a href='#contact'>Contact Info</a></li>
                              </ul>
                           </div>

                           <br>

                           <h2 id='function'>What does this app do?</h2>

                           <br>

                           <div class='guide'>

                              <p>This app generates reports for the following consulting assessments:<p>

                              <br>

                                <ul class = 'guide_list'>
                                  <li>Leadership Team Assessment (LTA, Time 1 & Time 2)</li>
                                  <li>Leadership Impact Assessment (LIA, Time 1 & Time 2)</li>
                                  <li>Group Experience Survey (GES, Time 1)</li>
                                  <li>Team Assessment (TA, Time 1)</li>
                                </ul>

                              <br>

                              <p><em>Note:</em> Time 2 reports for the GES and TA will be added later.</p>

                              <br>

                              <p>The final product is an HTML file of the report which can be shared as is, printed, or saved as a PDF.</p>

                           </div>

                           <br>

                           <h2 id='running'>Running Reports</h2>

                           <br>

                           <ol class='guide_list'>
                               <li>Select the appropriate b