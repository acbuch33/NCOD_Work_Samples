#dir = getwd()

options(shiny.maxRequestSize=100*1024^2)

shinyServer(function(input, output, session) {
  
  observeEvent(input$Time, {
    # http://127.0.0.1:6172/#dashboard
    # http://127.0.0.1:6172/#widgets
    clientData <- reactiveValuesToList(session$clientData)
    newURL <- with(clientData, paste0(url_protocol, "//", url_hostname, ":", url_port, url_pathname, "#", input$Time))
    updateQueryString(newURL, mode = "replace", session)
  })
  
  observe({
    currentTab <- sub("#", "", session$clientData$url_hash)
    if(!is.null(currentTab)){
      updateTabItems(session, "Time", selected = currentTab)
    }
  })
  
  theme <- reactableTheme(
    headerStyle = list(background = "#809e94",
                       color = "white")
  )
  
 # shinyjs::reset("dateRange")
  observeEvent(input$dateRange,{
    print(input$dateRange)
  })
  # Time 1 ####  
  # observe({
  
  # if(input$Time == "Time 1"){
  #print(input$pull)
  api=reactive({
    qualtrics_api_credentials(
      api_key=consultants$keys[consultants$Name==input$consultant],
      base_url="gov1.qualtrics.com",
      overwrite = TRUE,
      install = FALSE
    )
  })
  # 
  output$surveys=renderUI({
    if(!is.null(input$consultant) & !is.null(input$type_report)){
    api()
    ids=all_surveys()
    ids_dropdown=as.list(ids$id)
    names(ids_dropdown)=ids$name
    ids_dropdown=ids_dropdown[grepl(input$type_report, names(ids_dropdown) , fixed = TRUE) &
                                !grepl('Master', names(ids_dropdown)) &
                                # !grepl("SHARED", names(ids_dropdown)) &
                                !grepl('MASTER COPY', names(ids_dropdown)) &
                                # !grepl("Source", names(ids_dropdown)) &
                                !grepl("Test", names(ids_dropdown))
                                #grepl("T1", names(ids_dropdown)) &
                                ]
      #selectInput("survey_list","Choose a Survey:", choices=ids_dropdown, multiple=T)
      selectizeInput("survey_list","Choose a Survey:", choices=ids_dropdown, multiple=T,options = list(maxItems = 1))
      # selectInput("survey_list", "Choose a Survey:", choices=ids_dropdown[grepl(input$type_report, names(ids_dropdown) , fixed = TRUE) & 
      #                                                                       #!grepl('Master', names(ids_dropdown)) & 
      #                                                                       !grepl("SHARED", names(ids_dropdown)) &
      #                                                                       #!grepl('MASTER COPY', names(ids_dropdown)) &
      #                                                                       !grepl("Source", names(ids_dropdown)) &
      #                                                                       !grepl("Test", names(ids_dropdown))
      #                                                                     #grepl("T1", names(ids_dropdown)) &
      # ], multiple=FALSE) #& !grepl('Master', names(ids_dropdown)) & !grepl('MASTER COPY', names(ids_dropdown))
    } else {
      selectizeInput("survey_list","Choose a Survey:", choices=c(""),multiple=T,options = list(maxItems = 1))
     }
    
  })
  
  ####Tylers test code#####
  # Time 1
  pull_t1_count=reactiveValues(a=0)
  
  onevent("click","pull",{
    pull_t1_count$a=1
  })
  onevent("change","type_report",{
    pull_t1_count$a=0
  })
  onevent("change","consultant",{
    pull_t1_count$a=0
  })
  onevent("change","survey_list",{
    pull_t1_count$a=0
  })
  
  checks_dl_t1=reactive({
    if(!is.null(input$type_report)){
        if (input$type_report %in%  c("Leadership Team Assessment (LTA)","Team Assessment 2.0")){
          # if(input$team_name != "" & input$start_end_date != ""){
            if(input$team_name != ""){
            out="yes"
          }else {
            out="no"
          }
        }else if(input$type_report %in%  c("Leadership Impact Assessment (LIA)","Group Experience Survey")){
          if(input$team_name != ""  & !is.na(input$resp_total)){ #input$start_end_date != ""
            if(input$resp_total>0){
              out="yes"
            }else{
              out="no"
            }
          }else{
            out="no"
          }
        }else{
          out="no"
        }
      out
    }
  })
  
  output$runReport_t1=renderText({
    checks_dl_t1()
  })
  output$ui_t1=renderText({
    pull_t1_count$a
  })
  
  outputOptions(output, "runReport_t1", suspendWhenHidden = FALSE)
  outputOptions(output, "ui_t1", suspendWhenHidden = FALSE)
  
  #Time 2
  pull_t2_count=reactiveValues(a=0)
  
  onevent("click","pull_t2",{
    pull_t2_count$a=1
  })
  onevent("change","type_report_t2",{
    pull_t2_count$a=0
  })
  onevent("change","consultant_t2",{
    pull_t2_count$a=0
  })
  onevent("change","survey_list_t2",{
    pull_t2_count$a=0
  })
  
  checks_dl_t2=reactive({
    if(!is.null(input$type_report_t2)){
          if (input$type_report_t2 %in%  c("Leadership Team Assessment (LTA)","Team Assessment 2.0")){
            if(input$team_name_t2 != ""){
              
            # if(input$team_name_t2 != "" & input$start_end_date_t2 != "" & input$start_end_date_t1 != "" & updat$data %in% "go"){
              out="yes"
            }else {
              out="no"
            }
          }else if(input$type_report_t2 %in%  c("Leadership Impact Assessment (LIA)","Group Experience Survey")){
            if(input$team_name_t2 != "" & !is.na(input$resp_total_t2) & !is.na(input$resp_total_t1)){
              
            # if(input$team_name_t2 != "" & input$start_end_date_t2 != "" & !is.na(input$resp_total_t2) & input$start_end_date_t1 != "" & !is.na(input$resp_total_t1) & updat$data %in% "go"){
              if(input$resp_total_t1>0 & input$resp_total_t2>0){
                out="yes"
              }else{
                out="no"
              }
            }else{
              out="no"
            }
          }else{
            out="no"
          }
          out
    }
  })
  
  output$runReport_t2=renderText({
    global$response
    checks_dl_t2()
  })
  output$ui_t2=renderText({
    # print(pull_t2_count$a)
    pull_t2_count$a
  })
  
  outputOptions(output, "runReport_t2", suspendWhenHidden = FALSE)
  outputOptions(output, "ui_t2", suspendWhenHidden = FALSE)
  
  #######
  
  observe({
    #print(input$t1_data)
    #print(input$t1_data)
    #hide("pull")
    hide("time1_box")
    hide("teamname_t1")
    hide("numresponses_t1")
    shinyjs::disable("team_name")
    shinyjs::disable("start_end_date")
    #disable("end_date")
    shinyjs::disable("resp_total")
  })

  # output$actbtn_t1=renderUI({
  #   input$Time
  #   print("hi_1")
  #   actionButton("pull", "Pull Data")
  # })
  
  # checking to see which Tab is being viewed
  # observe({ 
  #   print(input$Time)
  # })
  
  # observeEvent(input$type_report, {
  #   # print(input$type_report)
  #   if (input$type_report == 'Leadership Impact Assessment (LIA)'){
  #     enable('resp_total')
  #   } else if (input$type_report == "Group Experience Survey") {
  #     enable('resp_total')
  #   } else {
  #     disable('resp_total')
  #   }
  # })
  
  pull_data=eventReactive(input$pull,{
    
    
    if(input$pull > 0){
      
      # ETA
      if (input$type_report == "Leadership Team Assessment (LTA)"){
        dat = as.data.frame(fetch_survey(surveyID=c(input$survey_list),add_var_labels = F,convert=F,force_request=T))
        dat <- dat %>% filter(StartDate < input$dateRange[2] + 1)
        iteminfo = read.csv("iteminfo_ETA_HTML_March_2025.csv") # may need changed
        colnames(dat)[colnames(dat) %in% iteminfo$tag5] <- iteminfo$abbr[iteminfo$tag5 %in% colnames(dat)]
        items = iteminfo$abbr[!iteminfo$factor %in% 'demo' & !iteminfo$factor %in% "" & !iteminfo$abbr %in% 'perform_role']
        items_all <- iteminfo$abbr[!iteminfo$factor %in% 'demo' & !iteminfo$factor %in% ""]
        
        dat$rm=apply(dat[,items_all],1, function(x) sum(is.na(x))) 
        dat=dat %>% filter(!Status %in% "Survey Preview", rm != 34)
        
        return(dat)
        
        # LIA
      } else if (input$type_report == "Leadership Impact Assessment (LIA)") {
        dat = as.data.frame(fetch_survey(surveyID=c(input$survey_list),add_var_labels = F,convert=F,force_request=T))
        dat <- dat %>% filter(StartDate < input$dateRange[2] + 1)
        dat <- as.data.frame(sapply(dat, as.character))
        iteminfo = read.csv("LIA Item Info_HTML_feb 2025.csv") # may need changed
        colnames(dat) = iteminfo$abbr #iteminfo$abbr[grepl('QID', iteminfo$ImportId)]
        items = iteminfo$abbr[!is.na(iteminfo$factor)]
        dat[dat==""]=NA
        dat$missing<-apply(dat[,items], 1, FUN=function(x)length(x[is.na(x)]))
        dat<-dat[dat$missing < length(items),]
        dat$missing=NULL
        dat <- dat %>% filter(!`Response Type` %in% "Survey Preview")
        
        return(dat)
        
        # GES  
      } else if (input$type_report == "Group Experience Survey") {
        dat = as.data.frame(fetch_survey(surveyID=c(input$survey_list),add_var_labels = F,convert=F,force_request=T))
        dat <- dat %>% filter(StartDate < input$dateRange[2] + 1)
        iteminfo= read.csv("GES iteminfo.csv")
        setDT(dat)
        ges_order <- iteminfo$tag[grepl("Q",iteminfo$tag)]
        setcolorder(dat,ges_order)
        colnames(dat)[colnames(dat) %in% iteminfo$tag] <- iteminfo$abbr[iteminfo$tag %in% colnames(dat)]
        dat = as.data.frame(dat)
        dat=filter(dat, !Status %in% "Survey Preview")
        return(dat)
        
        # TA   
      } else if (input$type_report == "Team Assessment 2.0"){
        dat = as.data.frame(fetch_survey(surveyID=c(input$survey_list),add_var_labels = F,convert=F,force_request=T))
        dat <- dat %>% filter(StartDate < input$dateRange[2] + 1)
        return(dat)
      }
    }
    
    #return(as.data.frame(fetch_survey(surveyID=c(input$survey_list),add_var_labels = F,convert=F,force_request=T)))
    
  })
  
  selected <- reactive(getReactableState("data_survey", "selected"))
  
  finalData=reactiveValues(a=NULL)
  survey_id = reactiveValues(a=NULL)
  
  observeEvent(input$pull,{
    #print(input$pull)
    if(input$pull > 0){
      finalData$a=pull_data()
      survey_id$a = input$survey_list
      # observe({
      #   if(survey_id$a == input$survey_list){
      #     enable("report")
      #     enable("rm")
      #   } else{
      #     disable("report")
      #     disable("rm")
      #   }
      # })
      output$response_count=renderValueBox({
        valueBox(
          paste0(nrow(finalData$a)), "Responses", icon = icon(NULL, class = "fa-solid fa-people-group"), width = 6, color = "teal"
        )
        # prints number of responses of data pulled in
        #p(strong('bold'))
      })
      output$T1_infobox=renderInfoBox({
        infoBox(
          title = "Did you check for irregularities in:",
          icon = icon(NULL, class="fa-solid fa-circle-exclamation"),
          div(HTML("<ul>
                      <li>Number of Responses</li>
                      <li>Network and/or Organization</li>
                      <li>Role</li>
                   </ul>"))
        )
      })
    }
  })
  
  ### validate report text fields - think about how the below will be implemented with respect to the download button. may need to disable the download button and also require these fields.
  iv <- InputValidator$new()
  iv$add_rule("team_name", sv_required())
  #iv$add_rule("end_date", sv_required())
  
  iv_dateRange <- InputValidator$new()
  iv_dateRange$add_rule("dateRange", function(value){
    if(sum(is.na(value)) == 2){
      "Please provide administration dates"
    }
    else if(is.na(value[1])){
      "Please provide a Start date"
    } 
    else if(is.na(value[2])) {
      "Please provide an End date"
    } 
    else if(value[1] > value[2]){
      "Start date must precede End date"
  }
  })

  # observe({
  #   print(iv_dateRange$is_valid())
  # })
  
  # using conditional subforms to make sure that both of these conditions are used for the validation conditional
  LIA_iv <- InputValidator$new()
  LIA_iv$condition(~ input$type_report == "Leadership Impact Assessment (LIA)")
  LIA_iv$add_rule("resp_total", function(value){
    if(is.na(value)){
      "Must be greater than 0"
    } else if(value < 0){
      "Must be greater than 0"
    }else if(value == 0){
      "Must be greater than 0"
    }
  })
  
  GES_iv <- InputValidator$new()
  GES_iv$condition(~ input$type_report == "Group Experience Survey")
  GES_iv$add_rule("resp_total", function(value){
    if(is.na(value)){
      "Must be greater than 0"
    } else if(value < 0){
      "Must be greater than 0"
    }else if(value == 0){
      "Must be greater than 0"
    }
  })  
  
  resp_total_iv <- InputValidator$new()
  resp_total_iv$add_validator(LIA_iv)
  resp_total_iv$add_validator(GES_iv)
  
  iv_dateRange$enable()
  
  # hiding the pull data button until both dates have been selected in the date range input and the start date precedes the end date
  observeEvent(input$dateRange,{
    if(sum(is.na(input$dateRange)) == 0 & iv_dateRange$is_valid()){
      show("pull")
    } else{
      hide("pull")
    }
  })
  
  # disabling the download and remove rows button until the pull data button is pressed
  observeEvent(input$pull, {
    if(input$pull > 0){
      if (input$type_report == 'Leadership Impact Assessment (LIA)'){
        show("numresponses_t1")
        enable('resp_total')
      } else if (input$type_report == "Group Experience Survey") {
        show("numresponses_t1")
        enable('resp_total')
      } else {
        disable('resp_total')
      }
      iv$enable() # validation visualization triggered when pull data is
      resp_total_iv$enable()
      GES_iv$enable()
      #resp_total_iv2$enable()
      # disable("report")
      shinyjs::show("rm")
      shinyjs::show("response_count")
      enable("team_name")
      enable("start_end_date")
      # enable("end_date")
      enable("rm")
      show("time1_box")
      show("teamname_t1")
      #show("numresponses_t1")
    } else {
      
      # disable("report")
      # shinyjs::hide("rm")
      disable("rm")
    }
  })
  
  # keeping the download report button disable until all required fields have been filled in
  # observe({
  #   if(input$type_report == "Leadership Impact Assessment (LIA)" | input$type_report == "Group Experience Survey"){
  #     if(input$team_name != "" & input$start_end_date != "" & !is.na(input$resp_total)){
  #       enable("report")
  #     } else {
  #       disable("report")
  #     }
  #   } else{
  #     if(input$team_name != "" & input$start_end_date != ""){
  #       enable("report")
  #     } else {
  #       disable("report")
  #     }
  #   }
  # })
  
  
  observeEvent(input$rm,{
    shinyalert("Don't do something you'll regret!",
               text = 'Removing rows inside this app does not remove the rows in Qualtrics. <br><br>
               If you remove them here and need a Time 2 later, these rows will show up in that report unless you remove them in Qualtrics. 
               Be kind to your future self, go ahead and open up <a href="https://ssologon.iam.va.gov/centrallogin/Default.aspx?appname=core&URL=https://ssologon.iam.va.gov/centrallogin/core/redirect.aspx&TYPE=33619969&REALMOID=06-345d7582-c96e-4888-9e5f-6e86468bf060&GUID=&SMAUTHREASON=0&METHOD=GET&SMAGENTNAME=-SM-JDZx1AxYAhQguyl0rfvd%2f5f46jynE%2bEoBtr6BQQU4NWuWIwCZNPIPHj210fDMxqs&TARGET=-SM-HTTPS%3a%2f%2flogon%2eiam%2eva%2egov%2faffwebservices%2fredirectjsp%2fredirect%2ejsp%3fapp%3dQUALTRICSEES%26SAMLRequest%3djZJBT-%2BswEIT-%2FiuV74tgN0FhNUQEhKoFeRQMHLmibbIqlxA5eJ-%2FDzX2iogAviaHlnxjufF-%2BfvbcMG9GSczbmME87Qlq4ydp-%2Fzh-%2BI6mvPz5YKgbTq96sOLvcfXHimwUWdJHy5y3nurHZAhbaFF0qHU29XdrVZxojvvgitdw9mKCH0Ygy6dpb5Fv0U-%2FmBIf7m9z-%2FhJCR1qIARCpxspD28V7N8j4tYcmeFNSXLpWNG5vrBikIHLiI16JCmvomxBRx9nV-%2BDZjIRzWOXqOGmdjA208wIengLp-%2Bwx1N8SS6fteYcnIbbTm7dr7Ew7o5r6Eh5Gx9lfPnZKfSeZWdJIlMTxRU6UxlKkMFaXUqQY3lrWkDRGbALyFRj2tLAWzIuUpUGiXzKMkKOdezTMuzeCblE2ebz54ujJ3a-%2F63U3TRE-%2BqYoNtHm37bg7PFIcRzgEzN9CPffYP1uC0dCfPlHHgvxLWc5nX7-%2BlOV-%2F%26RelayState%3dLNS--_3596cbf853309e089b7b0387df373373%26SigAlg%3dhttp-%3A-%2F-%2Fwww%2ew3%2eorg-%2F2001-%2F04-%2Fxmldsig--more-%23rsa--sha256%26Signature%3dCG6Wr2Q3njw59DotYLtqwIwfvDIKVoIWx6PJr-%2BN5p5arUqa2SPlrzE8SqoAgiC-%2BKtjHuFBN3LMBzzrAyX4-%2FHxVSc7R5fkpJio1fGI-%2FF9VN9niSdG4pxqR8QGFwHADv87W2GIHCf-%2F1MiRd8LM8sAQ3yklFy008-%2FDQgFsq0xQifWvml-%2FBUOvmu8zqBjZPVkgGZ2ZhzsKYjfcjPIK-%2FyGNIOuiOpqV1F9WO8Q8izwriK15D2X22xsGBUFZ97-%2BlnY0OOhJS9-%2FPsNe2LI5d4UmTUrKAdeY0HmidUkrjpsXh1-%2FN8sL3hrYi-%2BNmR51sgocks8Rl4r-%2BfCBYhunsNWf26kbl2mEd4chGZrKmjgdvipxtHrCNH-%2B5VpI2MPh3S9X94xlD-%2FO7L7uxFlnnQ8BuCzZktJejXkkHtMFpPgL1nngcRYaV3VhlxYLblv-%2BIILCt2QTSQzGrt9JKkpvTT2zo3O66JgiW7foOXRel3huZvp7QDJ0SQ2sE7M23BAT6ze-%2FdSYH5S93ZJ4qthMwn3DvUlTjhdbc-%2Be-%2FWDmIDWjxSVlsS7pSmTyywCf-%2Fg-%2FiDXE60aB-%2FaneJG8zITpOPWggMEFsSp5rDZBrk7pmhOm5LMrHxve-%2Bl0QXMoAxhNqR-%2BKDljYPixBtOYXmgui8cyi-%2FV4o0GnNQWO2x6KKOT6NihSF8C8p6h5OzIeCA-%3D%26SMPORTALURL%3dhttps-%3A-%2F-%2Flogon%2eiam%2eva%2egov-%2Faffwebservices-%2Fpublic-%2Fsaml2sso%26SAMLTRANSACTIONID%3d10857a5e--6fc4de32--9abd06d9--aa4b4d99--515ba0c4--a6d" target = "_blank">Qualtrics</a> real quick',
               type = "warning",
               html = TRUE,
               confirmButtonText = "Remove Rows Anyway",
               cancelButtonText = "Do Not Remove Rows",
               closeOnEsc = FALSE,
               showCancelButton = TRUE)
  })
  
  # observe({
  #   print(input$shinyalert)
  # })
  
  
  observeEvent(input$shinyalert,{
    if(input$shinyalert == "TRUE"){
      if(!is.null(selected())){
        finalData$a=finalData$a[-selected(),, drop=F]
      }
      output$response_count=renderValueBox({
        valueBox(
          paste0(nrow(finalData$a)), "Responses", icon = icon(NULL, class = "fa-solid fa-people-group"), width = 6, color = "teal"
        )
        # prints number of responses of data pulled in
        #p(strong('bold'))
      })
    }
  })
  
  # observeEvent(input$rm,{
  #   if(!is.null(selected())){
  #     finalData$a=finalData$a[-selected(),, drop=F]
  #   }
  #   output$response_count=renderInfoBox({
  #     infoBox(
  #       "Number of Responses:", nrow(finalData$a), icon = icon("list")
  #       #paste0('Number of responses: ', nrow(finalData$a)), icon = icon("list")
  #     )
  #      # updates number of responses based on number of rows removed
  #   })
  # })
  
  # output$response_count <- renderInfoBox({
  #   infoBox(
  #     "Number of Responses:", paste0(nrow(finalData$a)), icon = icon("list")
  #     #paste0('Number of responses: ', nrow(finalData$a)), icon = icon("list")
  #   )
  #   # updates number of responses based on number of rows removed
  # })
  
  ### clearing final data when inputs are changed (resets reactable and number of responses output)
  
  # Consultant
  observeEvent(input$consultant, {
    finalData$a = NULL
    iv$disable()
    resp_total_iv$disable()
    # shinyjs::hide("rm")
    # shinyjs::hide("response_count")
    updateTextInput(session,"team_name", value="")
    updateTextInput(session,"start_end_date", value="")
    updateDateRangeInput(session,"dateRange",start=NA,end=NA)
    #updateTextInput(session,"end_date", value="")
    updateTextInput(session,"resp_total", value="")
    hide("time1_box")
    hide("teamname_t1")
    hide("numresponses_t1")
    disable("team_name")
    disable("start_end_date")
    #disable("end_date")
    disable("resp_total")
  })
  
  # report type
  observeEvent(input$type_report, {
    finalData$a = NULL
    iv$disable()
    resp_total_iv$disable()
    # shinyjs::hide("rm")
    # shinyjs::hide("response_count")
    updateTextInput(session,"team_name", value="")
    updateTextInput(session,"start_end_date", value="")
    updateDateRangeInput(session,"dateRange",start=NA,end=NA)
    #updateTextInput(session,"end_date", value="")
    updateTextInput(session,"resp_total", value="")
    disable("team_name")
    disable("start_end_date")
    hide("time1_box")
    hide("teamname_t1")
    hide("numresponses_t1")
    #disable("end_date")
    disable("resp_total")
  })
  
  # survey list
  observeEvent(input$survey_list, {
    finalData$a = NULL
    iv$disable()
    resp_total_iv$disable()
    # shinyjs::hide("rm")
    # shinyjs::hide("response_count")
    updateTextInput(session,"team_name", value="")
    updateTextInput(session,"start_end_date", value="")
    updateDateRangeInput(session,"dateRange",start=NA,end=NA)
    #updateTextInput(session,"end_date", value="")
    updateTextInput(session,"resp_total", value="")
    hide("time1_box")
    hide("teamname_t1")
    hide("numresponses_t1")
    disable("team_name")
    disable("start_end_date")
    #disable("end_date")
    disable("resp_total")
  })
  
  observeEvent(input$dateRange, {
    finalData$a = NULL
    iv$disable()
    resp_total_iv$disable()
    # shinyjs::hide("rm")
    # shinyjs::hide("response_count")
    updateTextInput(session,"team_name", value="")
    updateTextInput(session,"start_end_date", value="")
    #updateDateRangeInput(session,"dateRange",start=NA,end=NA)
    #updateTextInput(session,"end_date", value="")
    updateTextInput(session,"resp_total", value="")
    hide("time1_box")
    hide("teamname_t1")
    hide("numresponses_t1")
    disable("team_name")
    disable("start_end_date")
    #disable("end_date")
    disable("resp_total")
  })
  
  # reset consultant when switching between tabs - this will reset everything except the selected type of report and the selected survey
  observeEvent(input$Time, {
    
    finalData$a = NULL
    iv$disable()
    resp_total_iv$disable()
    # shinyjs::hide("rm")
    # shinyjs::hide("response_count")
    updateSelectInput(session, "survey_list")
    updateSelectInput(session, "consultant", choices=consultants$Name)
    # updateSelectInput(session, "type_report", choices = list("LTA" = "Leadership Team Assessment (LTA)",
    #                                                           "LIA" = "Leadership Impact Assessment (LIA)",
    #                                                           "GES" = "Group Experience Survey",
    #                                                           "TA" = "Team Assessment 2.0"))
    updateSelectInput(session, "type_report", choices = list("LTA" = "Leadership Team Assessment (LTA)",
                                                             "LIA" = "Leadership Impact Assessment (LIA)",
                                                             "GES" = "Group Experience Survey"))
    updateTextInput(session,"team_name", value="")
    updateDateRangeInput(session,"dateRange",start=NA,end=NA)
    hide("time1_box")
    hide("txtinptbox_t1")
    # updateTextInput(session,"start_end_date", value="")
    #updateTextInput(session,"end_date", value="")
    updateTextInput(session,"resp_total", value="")
    disable("team_name")
    # disable("start_end_date")
    #disable("end_date")
    disable("resp_total")
  })
  
  
  
  output$data_survey=renderReactable({
    if(!is.null(finalData$a)){
      if (input$type_report == "Leadership Team Assessment (LTA)"){
        table <- finalData$a %>% select(-c(Status:UserLanguage))
        names(table)[6] <- "Role"
        reactable(table,
                  selection = "multiple", 
                  onClick = "select", 
                  compact = T, 
                  highlight = T, 
                  striped = T, 
                  theme = theme, 
                  defaultColDef = colDef(width = 175, align = 'center'))#,
        
        # LIA  
      } else if (input$type_report == "Leadership Impact Assessment (LIA)"){
        table <- finalData$a %>% dplyr::select(-c(`Response Type`:`User Language`))
        reactable(table,
                  selection = "multiple", 
                  onClick = "select", 
                  compact = T, 
                  highlight = T, 
                  striped = T, 
                  theme = theme, 
                  defaultColDef = colDef(width = 175, align = 'center'),
                  defaultPageSize = 5,
                  showPageSizeOptions = TRUE,
                  pageSizeOptions = c(5,10,15,20,nrow(table)))
        
        # GES  
      } else if (input$type_report == "Group Experience Survey"){
        table <- finalData$a %>% dplyr::select(c(StartDate,EndDate,`Supervisor Supports Development`:`Clear Expectations`)) # Columns names slightly different
        reactable(table,selection = "multiple", onClick = "select", compact = T, highlight = T, striped = T, theme = theme, defaultColDef = colDef(width = 175, align = 'center'))
        
        # TA  
      } else if (input$type_report == "Team Assessment 2.0"){
        table <- finalData$a %>% dplyr::select(-c(Status:UserLanguage, block2progress:FL_6_DO_FL_8))
        reactable(table,selection = "multiple", onClick = "select", compact = T, highlight = T, striped = T, theme = theme, defaultColDef = colDef(width = 175, align = 'center'))
      }
    }
  })
  
  output$report <- downloadHandler(
    
    filename = function() {
    #   if (input$type_report == "Executive Team Assessment (ETA)"){
    #     paste0(input$team_name,"_ETA_T1.zip")
    #   } else if (input$type_report == "Leadership Impact Assessment (LIA)") {
    #     paste0(input$team_name,"_LIA_T1.zip")
    #   } else if (input$type_report == "Group Experience Survey"){
    #     paste0(input$team_name,"_GES.zip")
    #   } else {
    #     paste0(input$team_name,"_TA.zip")
    #   }
    # },
        if (input$type_report == "Leadership Team Assessment (LTA)"){
          paste0(input$team_name,"_LTA_T1.html")
        } else if (input$type_report == "Leadership Impact Assessment (LIA)") {
          paste0(input$team_name,"_LIA_T1.html")
        } else if (input$type_report == "Group Experience Survey"){
          paste0(input$team_name,"_GES.html")
        } else {
          paste0(input$team_name,"_TA.html")
        }
    },
    content = function(file) {
      # shiny::withProgress(
      #   message
      #write.csv(finalData$a, "test.csv")
      if (input$type_report == "Leadership Team Assessment (LTA)"){
        filename2 = paste0(input$team_name,"_ETA_T1.html")
        #filename2 = "ETA_T1_HTML.html"
      } else if (input$type_report == "Leadership Impact Assessment (LIA)"){
        filename2 = paste0(input$team_name,"_LIA_T1.html")
        #filename2 = "LIA_T1_HTML.html"
      } else if (input$type_report == "Group Experience Survey"){
        filename2 = paste0(input$team_name,"_GES.html")
      } else {
        filename2 = paste0(input$team_name,"_TA.html")
      }
      
      if (input$type_report == "Leadership Team Assessment (LTA)"){
        req(iv$is_valid()) # makes sure report will not run if all the required fields are not filled in
        
        dat2 <- finalData$a
        #dat <- dat %>% filter(StartDate < input$dateRange[2] + 1)
        dat2 <- as.data.frame(sapply(dat2, as.character))
        
        iteminfo <- read.csv("iteminfo_ETA_HTML_March_2025.csv")
        
        
        colnames(dat2)[colnames(dat2) %in% iteminfo$tag5] <- iteminfo$abbr[iteminfo$tag5 %in% colnames(dat2)]
        
        items_all <- iteminfo$abbr[!iteminfo$factor %in% 'demo' & !iteminfo$factor %in% ""]
        
        dat2$rm=apply(dat2[,items_all],1, function(x) sum(is.na(x))) # this line only for fake data TB generated. 
        dat2=dat2 %>% filter(rm != 34)
        
        dat2[dat2==""]=NA
        dat2$Group=input$team_name
        dat2$Time="T1"
        dat2$Consultant=input$consultant
        
        #data confidentiality stuff
        dat2a=dat2[,c(1:22, 57:76)]
        dat2b=dat2[,c(9,74:76,23:56)]
        ids=data.frame(ResponseId=dat2a$ResponseId)
        ids$new_id=IDGen(nrow(ids))
        dat2b=merge(ids,dat2b, "ResponseId")
        dat2b$ResponseId=NULL
        
        
        write.csv(dat2b, paste0(input$team_name,"_ETA_T1_Cleaned Data_quant.csv"), row.names=F)
        write.csv(dat2a, paste0(input$team_name,"_ETA_T1_Cleaned Data_demogs.csv"), row.names=F)
        write.csv(ids, paste0(input$team_name,"_ETA_T1_Cleaned Data_ids.csv"), row.names=F)
        
        
        attachment_use=c(paste0(input$team_name,"_ETA_T1_Cleaned Data_quant.csv"),
                         paste0(input$team_name,"_ETA_T1_Cleaned Data_demogs.csv"),
                         paste0(input$team_name,"_ETA_T1_Cleaned Data_ids.csv"))
        
        # test1=read.csv(paste0(input$team_name,"_ETA_T1_Cleaned Data_quant.csv"))
        # test2=read.csv(paste0(input$team_name,"_ETA_T1_Cleaned Data_demogs.csv"))
        # test3=read.csv(paste0(input$team_name,"_ETA_T1_Cleaned Data_ids.csv")) 
        # test2=merge(test3, test2, c("new_id"))
        # test1=merge(test2,test1, c("ResponseId","Group","Time","Consultant" ))               
        
        email <- envelope()
        email<-email %>%
          from("tyler.barnes@va.gov") %>%
          to("tyler.barnes@va.gov","matthew.ellis3@va.gov") %>%
          # to("tyler.barnes@va.gov") %>%
          subject(paste0("ETA T1 - ",input$team_name)) %>% 
          html("See the <b>ETA T1</b> data attached for data for the group.") %>%
          attachment(paste0(input$team_name,"_ETA_T1_Cleaned Data_quant.csv")) %>% 
          attachment(paste0(input$team_name,"_ETA_T1_Cleaned Data_demogs.csv")) %>% 
          attachment(paste0(input$team_name,"_ETA_T1_Cleaned Data_ids.csv"))
          
        
        smtp(email, verbose = FALSE)
        
        rmarkdown::render(input="ETA_T1_HTML.Rmd", output_file = file)
        #rmarkdown::render(input="ETA_T1_HTML.Rmd", output_file = filename2) #output="text_tex.tex"
        #files <- c(filename2, paste0(input$team_name,"_ETA_T1_Cleaned Data.csv"))
        #files <- c(paste0(input$team_name,"_ETA_T1_Cleaned Data.csv"))
        # file.copy(paste0(input$team_name,"_ETA_T1.pdf"),file)
        #data = finalData$a
       # zip::zipr(file, files)
      } else if (input$type_report == "Leadership Impact Assessment (LIA)"){
        req(iv$is_valid()) # makes sure report will not run if all the required fields are not filled in
        # dat<<-finalData$a
        dat2 <- finalData$a
        dat2 <- as.data.frame(sapply(dat2, as.character))
        iteminfo <- read.csv("LIA Item Info_HTML_feb 2025.csv")
        
        
        colnames(dat2) <- iteminfo$abbr
        
        items = iteminfo$abbr[!is.na(iteminfo$factor)]
        
        dat2[dat2==""]=NA
        dat2$missing<-apply(dat2[,items], 1, FUN=function(x)length(x[is.na(x)]))
        dat2<-dat2[dat2$missing < length(items),]
        dat2$missing=NULL
        dat2 <- dat2 %>% filter(!`Response Type` %in% "Survey Preview")
        
        dat2$Group=input$team_name
        dat2$Time="T1"
        dat2$Consultant=input$consultant
        
        colnames(dat2)[colnames(dat2)=="Response ID"]="ResponseId"
        dat2a=dat2[,c(1:26,28:34, 36:46, 48, 50:61)]
        dat2b=dat2[,c(1:2,9,27,35,47,49, 60:61)]
        ids=data.frame(ResponseId=dat2a$ResponseId)
        ids$new_id=IDGen(nrow(ids))
        dat2b=merge(ids,dat2b, "ResponseId")
        dat2b$ResponseId=NULL
        
        
        write.csv(dat2a, paste0(input$team_name,"_LIA_T1_Cleaned Data_quant.csv"), row.names=F) 
        write.csv(dat2b, paste0(input$team_name,"_LIA_T1_Cleaned Data_text.csv"), row.names=F) 
        write.csv(ids, paste0(input$team_name,"_LIA_T1_Cleaned Data_ids.csv"), row.names=F) 
        

        email <- envelope()
        email<-email %>%
          from("tyler.barnes@va.gov") %>%
          to("tyler.barnes@va.gov","matthew.ellis3@va.gov") %>%
          # to("tyler.barnes@va.gov") %>%
          subject(paste0("LIA T1 - ",input$team_name)) %>% 
          html("See the <b>LIA T1</b> data attached for data for the group.") %>%
          attachment(paste0(input$team_name,"_LIA_T1_Cleaned Data_quant.csv")) %>% 
          attachment(paste0(input$team_name,"_LIA_T1_Cleaned Data_text.csv")) %>% 
          attachment(paste0(input$team_name,"_LIA_T1_Cleaned Data_ids.csv"))
        smtp(email, verbose = FALSE)
        
        rmarkdown::render(input="LIA_T1_HTML.Rmd", output_file = file)
        #rmarkdown::render(input="LIA_T1_HTML.Rmd", output_file = filename2) # changed text_tex.tex to text_tex1.tex, then changed it back
        #files <- c(filename2, paste0(input$team_name,"_LIA_T1_Cleaned Data.csv"))
        #file.copy(paste0(input$team_name,"_LIA_T1.pdf"),file)
        #zip::zipr(file, files)
      } else if (input$type_report == "Group Experience Survey"){
        req(iv$is_valid()) # makes sure report will not run if all the required fields are not filled in
        
        dat2 <- finalData$a
        dat2 <- as.data.frame(sapply(dat2, as.character))
        
        iteminfo= read.csv("GES iteminfo.csv")
        
        setDT(dat2)
        ges_order <- iteminfo$abbr[grepl("Q",iteminfo$tag)]
        setcolorder(dat2,ges_order)
        colnames(dat2)[colnames(dat2) %in% iteminfo$tag] <- iteminfo$abbr[iteminfo$tag %in% colnames(dat2)]
        dat2 = as.data.frame(dat2)
        
        dat2=filter(dat2, !Status %in% "Survey Preview")
        dat2=select(dat2, any_of(iteminfo$abbr))
        
        dat2$Group=input$team_name
        dat2$Time="T1"
        dat2$Consultant=input$consultant
        write.csv(dat2, paste0(input$team_name,"_GES_Cleaned Data.csv"), row.names=F)
        
        attachment_use=paste0(input$team_name,"_GES_Cleaned Data.csv")
        email <- envelope()
        email<-email %>%
          from("tyler.barnes@va.gov") %>%
          to("tyler.barnes@va.gov","matthew.ellis3@va.gov") %>%
          subject(paste0("GES T1 - ",input$team_name)) %>% 
          html("See the <b>GES T1</b> data attached for data for the group.") %>%
          attachment(attachment_use)
        
        smtp(email, verbose = FALSE)
        
        rmarkdown::render(input="GES_Template.Rmd", output_file = file)
        #rmarkdown::render(input="GES_Template.Rmd", output_file = filename2) # changed text_tex.tex to text_tex1.tex, then changed it back
        #files <- c(filename2, paste0(input$team_name,"_GES_Cleaned Data.csv"))
        #file.copy(paste0(input$team_name,"_LIA_T1.pdf"),file)
        #zip::zipr(file, files)
      } else {
        req(iv$is_valid()) # makes sure report will not run if all the required fields are not filled in
        
        dat2 <- finalData$a
        dat2=dat2[,19:46]
        dat2$Group=input$team_name
        dat2$Time="T1"
        dat2$Consultant=input$consultant
        write.csv(dat2, paste0(input$team_name,"_TA_T1_Cleaned Data.csv"), row.names=F)
        
        attachment_use=paste0(input$team_name,"_TA_T1_Cleaned Data.csv")
        email <- envelope()
        email<-email %>%
          from("tyler.barnes@va.gov") %>%
          to("tyler.barnes@va.gov","matthew.ellis3@va.gov") %>%
          subject(paste0("TA T1 - ",input$team_name)) %>% 
          html("See the <b>TA T1</b> data attached for data for the group.") %>%
          attachment(attachment_use)
        
        smtp(email, verbose = FALSE)
        
        rmarkdown::render(input="TA Report Template 2023 Test.Rmd", output_file = file)
        #rmarkdown::render(input="TA Report Template 2023 Test.Rmd", output_file = filename2) # changed text_tex.tex to text_tex1.tex, then changed it back
        #files <- c(filename2, paste0(input$team_name,"_TA_Cleaned Data.csv"))
        #file.copy(paste0(input$team_name,"_LIA_T1.pdf"),file)
        #zip::zipr(file, files)
      }
      # print(tinytex_root())
    }
    # print(tinytex_root())
  )
  # }else if(input$Time == "Time 2"){ #Time 2
  # Time 2 ####      
  api_t2=reactive({
    qualtrics_api_credentials(
      api_key=consultants$keys[consultants$Name==input$consultant_t2],
      base_url="gov1.qualtrics.com",
      overwrite = TRUE,
      install = FALSE
    )
  })
  
  output$surveys_t2=renderUI({
    if(!is.null(input$consultant_t2) & !is.null(input$type_report_t2)){
         api_t2()
         ids=all_surveys()
         ids_dropdown=as.list(ids$id)
         names(ids_dropdown)=ids$name
         ids_dropdown=ids_dropdown[grepl(input$type_report_t2, names(ids_dropdown) , fixed = TRUE) &
                                     !grepl('Master', names(ids_dropdown)) &
                                     # !grepl("SHARED", names(ids_dropdown)) &
                                     !grepl('MASTER COPY', names(ids_dropdown)) &
                                     # !grepl("Source", names(ids_dropdown)) &
                                     !grepl("Test", names(ids_dropdown))
                                   #grepl("T1", names(ids_dropdown)) &
         ]
         selectizeInput("survey_list_t2","Choose a T2 Survey:", choices=ids_dropdown, multiple=T, options = list(maxItems = 1))
    # selectInput("survey_list_t2", "Choose a T2 Survey:", choices=ids_dropdown[grepl(input$type_report_t2, names(ids_dropdown) , fixed = TRUE) & 
    #                                                                             !grepl('Master', names(ids_dropdown)) & 
    #                                                                             !grepl('MASTER COPY', names(ids_dropdown)) & 
    #                                                                             !grepl("SHARED", names(ids_dropdown)) &
    #                                                                             !grepl("Source", names(ids_dropdown)) &
    #                                                                             #grepl('T2', names(ids_dropdown))
    #                                                                             !grepl("Test", names(ids_dropdown))], multiple=FALSE) #& !grepl('Master', names(ids_dropdown)) & !grepl('MASTER COPY', names(ids_dropdown))
    # 
    } else {
      selectizeInput("survey_list_t2","Choose a T2 Survey:", choices=c(""),multiple=T,options = list(maxItems = 1))
    }
  })
  
  output$surveys_t1=renderUI({
    if(!is.null(input$consultant_t2) & !is.null(input$type_report_t2)){
        api_t2()
        ids=all_surveys()
        ids_dropdown=as.list(ids$id)
        names(ids_dropdown)=ids$name
        ids_dropdown=ids_dropdown[grepl(input$type_report_t2, names(ids_dropdown) , fixed = TRUE) &
                                    !grepl('Master', names(ids_dropdown)) &
                                    !grepl("SHARED", names(ids_dropdown)) &
                                    !grepl('MASTER COPY', names(ids_dropdown)) &
                                    # !grepl("Source", names(ids_dropdown)) &
                                    !grepl("Test", names(ids_dropdown))
                                  #grepl("T1", names(ids_dropdown)) &
        ]
        selectizeInput("survey_list_t1","Choose a T1 Survey:", choices=ids_dropdown, multiple=T, options = list(maxItems = 1))
    # selectInput("survey_list_t1", "Choose a T1 Survey:", choices=ids_dropdown[grepl(input$type_report_t2, names(ids_dropdown) , fixed = TRUE) & 
    #                                                                             #!grepl('Master', names(ids_dropdown)) & 
    #                                                                             #!grepl('MASTER COPY', names(ids_dropdown)) & 
    #                                                                             !grepl("SHARED", names(ids_dropdown)) &
    #                                                                             !grepl("Source", names(ids_dropdown)) &
    #                                                                             #grepl('T2', names(ids_dropdown))
    #                                                                             !grepl("Test", names(ids_dropdown))], multiple=FALSE) #& !grepl('Master', names(ids_dropdown)) & !grepl('MASTER COPY', names(ids_dropdown))
    # 
    } else{
      selectizeInput("survey_list_t2","Choose a T1 Survey:", choices=c(""),multiple=T,options = list(maxItems = 1))
    }
  })
  
  observe({
    #print(global$response)
    hide("time2_t1_box")
    hide("time2_box")
    hide("teamname_t2")
    hide("numresponses_t1_t2")
    hide("numresponses_t2")
    # disable("team_name_t2")
    # disable("start_end_date_t1")
    # disable("start_end_date_t2")
    # disable("resp_total_t1")
    # disable("resp_total_t2")
  })
  
  # observeEvent(input$type_report_t2, {
  #   # print(input$type_report_t2)
  #   if (input$type_report_t2 == 'Leadership Impact Assessment (LIA)'){
  #     enable('resp_total_t1')
  #     enable('resp_total_t2')
  #   } else if (input$type_report_t2 == "Group Experience Survey") {
  #     enable('resp_total_t1')
  #     enable('resp_total_t2')
  #   } else {
  #     disable('resp_total_t1')
  #     disable('resp_total_t2')
  #   }
  # })
  
    # # T1 Pull
    # pull_data_t1=eventReactive(input$pull_t1,{
    #   #print("t1 pull data")
    #   # ETA
    #   if (input$type_report_t2 == "Executive Team Assessment (ETA)"){
    #     t1 = as.data.frame(fetch_survey(surveyID=c(input$survey_list_t1),add_var_labels = F,convert=F,force_request=T))
    #     t1 <- t1 %>% filter(StartDate < input$dateRange_t1[2] + 1)
    #     # iteminfo = read.csv("iteminfo_ETA_HTML_March_2025.csv") # may need changed
    #     # colnames(t1)[colnames(t1) %in% iteminfo$tag5] <- iteminfo$abbr[iteminfo$tag5 %in% colnames(t1)]
    #     # items = iteminfo$abbr[!iteminfo$factor %in% 'demo' & !iteminfo$factor %in% "" & !iteminfo$abbr %in% 'perform_role']
    #     # items_all <- iteminfo$abbr[!iteminfo$factor %in% 'demo' & !iteminfo$factor %in% ""]
    #     # 
    #     # t1$rm=apply(t1[,items_all],1, function(x) sum(is.na(x))) 
    #     # t1=t1 %>% filter(!Status %in% "Survey Preview", rm != 34)
    #     
    #     return(t1)
    #     
    #     # LIA
    #   } else if (input$type_report_t2 == "Leadership Impact Assessment (LIA)") {
    #     t1 = as.data.frame(fetch_survey(surveyID=c(input$survey_list_t1),add_var_labels = F,convert=F,force_request=T))
    #     t1 <- t1 %>% filter(StartDate < input$dateRange_t1[2] + 1)
    #     t1 <- as.data.frame(sapply(t1, as.character))
    #     iteminfo = read.csv("LIA Item Info_HTML_Nov 2023.csv") # may need changed
    #     colnames(t1) = iteminfo$abbr #iteminfo$abbr[grepl('QID', iteminfo$ImportId)]
    #     items = iteminfo$abbr[!is.na(iteminfo$factor)]
    #     t1[t1==""]=NA
    #     t1$missing<-apply(t1[,items], 1, FUN=function(x)length(x[is.na(x)]))
    #     t1<-t1[t1$missing < length(items),]
    #     t1$missing=NULL
    #     t1 <- t1 %>% filter(!`Response Type` %in% "Survey Preview")
    #     
    #     return(t1)
    #   }
    #   
    #   # will need to address this later when the GES t1 and TA t1 are ready
    #   #   # GES  
    #   # } else if (input$type_report == "Group Experience Survey") {
    #   #   dat = as.data.frame(fetch_survey(surveyID=c(input$survey_list),add_var_labels = F,convert=F,force_request=T))
    #   #   iteminfo= read.csv("S:/NCOD/NCOD Staff Folders/Tyler Barnes/Group Experience Survey Report/NPO GES iteminfo.csv")
    #   #   setDT(dat)
    #   #   ges_order <- iteminfo$tag[grepl("Q",iteminfo$tag)]
    #   #   setcolorder(dat,ges_order)
    #   #   colnames(dat)[colnames(dat) %in% iteminfo$tag] <- iteminfo$abbr[iteminfo$tag %in% colnames(dat)]
    #   #   dat = as.data.frame(dat)
    #   #   dat=filter(dat, !Status %in% "Survey Preview")
    #   #   return(dat)
    #   #   
    #   #   # TA   
    #   # } else if (input$type_report == "Team Assessment 2.0"){
    #   #   dat = as.data.frame(fetch_survey(surveyID=c(input$survey_list),add_var_labels = F,convert=F,force_request=T))
    #   #   return(dat)
    #   # }
    #   
    #   #return(as.data.frame(fetch_survey(surveyID=c(input$survey_list),add_var_labels = F,convert=F,force_request=T)))
    #   
    # })
    
    # T2 pull
  pull_data_t2=eventReactive(input$pull_t2,{
    #print("T2 pull data")
    # ETA
    if (input$type_report_t2 == "Leadership Team Assessment (LTA)"){
      
      # library(qualtRics)
      # library(shiny)
      # library(reactable)
      # library(knitr)
      # library(shinybusy)
      # library(shinyjs)
      # library(dplyr)
      # library(data.table)
      # library(tidyr)
      # library(shinyvalidate)
      # library(plotly)
      # library(tibble)
      # library(stringr)
      # library(fontawesome)
      # library(purrr)
      # library(zip)
      # library(rmarkdown)
      # library(tools)
      # library(shinyalert)
      # library(shinydashboard)
      # library(stringr)
      # library(emayili)
      # 
      # setwd("S:/NCOD/ETAC_Consulting_Reports/Consulting_report_Generator/HTML Reports/Report_Generator_Final_date/")
      # consultants=read.csv("qualtrics keys.csv")
      # input=list(survey_list_t2="SV_6L6Orj6mWwWpJXw",
      #            survey_list_t1="SV_3xV5UntVdhW5MzA",
      #            consultant="Tyler")
      # 
      # qualtrics_api_credentials(
      #   api_key=consultants$keys[consultants$Name==input$consultant],
      #   base_url="gov1.qualtrics.com",
      #   overwrite = TRUE,
      #   install = FALSE
      # )
      
      # T1 Pull
      t1 = as.data.frame(fetch_survey(surveyID=c(input$survey_list_t1),add_var_labels = F,convert=F,force_request=T))
      t1 <- t1 %>% filter(StartDate < input$dateRange_t1[2] + 1)
      iteminfo = read.csv("iteminfo_ETA_HTML_March_2025.csv") # may need changed
      colnames(t1)[colnames(t1) %in% iteminfo$tag5] <- iteminfo$abbr[iteminfo$tag5 %in% colnames(t1)]
      items = iteminfo$abbr[!iteminfo$factor %in% 'demo' & !iteminfo$factor %in% "" & !iteminfo$abbr %in% 'perform_role']
      items_all <- iteminfo$abbr[!iteminfo$factor %in% 'demo' & !iteminfo$factor %in% ""]
      #
      t1$rm=apply(t1[,items_all],1, function(x) sum(is.na(x)))
      t1=t1 %>% filter(!Status %in% "Survey Preview", rm != 34)

      #return(t1)

      # T2 Pull
      t2 = as.data.frame(fetch_survey(surveyID=c(input$survey_list_t2),add_var_labels = F,convert=F,force_request=T))
      t2 <- as.data.frame(sapply(t2, as.character))
      iteminfo = read.csv("eta_t2_item information_revised_march2025.csv") # may need changed
      
      iteminfo$wording = enc2utf8(iteminfo$wording)
      Encoding(iteminfo$wording) = "UTF-8" # use this method for making sure wording is encoded correctly so hidden characters can be manipulated.
      iteminfo$wording=iconv(iteminfo$wording,"UTF-8","UTF-8",sub=" ")
      iteminfo$wording=trimws(iteminfo$wording)
      
      
      
      colnames(t2)[22:61] = iteminfo$tag[iteminfo$tag_t2 %in% colnames(t2)[22:61]]
      t2_items=iteminfo$tag[iteminfo$rm %in% "no"]
      #t2=dplyr::select(t2, all_of(t2_items))
      
      t2[t2==""]=NA
      t2[t2=="-"]=NA
      t2[t2=="Do Not Know"]=NA
      
      t2$rm=apply(t2[,t2_items],1, function(x) sum(is.na(x)))
      
      t2=t2 %>%
        dplyr::filter(rm != 40) %>%
        dplyr::select(-rm)
      
      # dat$rm=apply(dat[,items_all],1, function(x) sum(is.na(x))) 
      # dat=dat %>% filter(Status %nin% "Survey Preview", rm != 34)
      
      return(list(t1=t1,
                  t2=t2))
      
      # LIA
    } else if (input$type_report_t2 == "Leadership Impact Assessment (LIA)") {
      
      # setwd("S:/NCOD/ETAC_Consulting_Reports/Consulting_report_Generator/HTML Reports/Report_Generator_Final_date/")
      # 
      # input=list(consultant="Tyler",
      #            survey_list_t1="SV_3t9unEzcIBHQFL0",
      #            survey_list_t2="SV_czKIUXC360lz78O")
      # qualtrics_api_credentials(
      #   api_key=consultants$keys[consultants$Name==input$consultant],
      #   base_url="gov1.qualtrics.com",
      #   overwrite = TRUE,
      #   install = FALSE
      # )

      
      
      # dat = as.data.frame(fetch_survey(surveyID=c(input$survey_list_t1),add_var_labels = F,convert=F,force_request=T))
      # dat=dat[,c(1:17,20:53,57:64)]
      
      # T1 Pull
      t1 = as.data.frame(fetch_survey(surveyID=c(input$survey_list_t1),add_var_labels = F,convert=F,force_request=T))
      ####Need to remove- just for kipps time 3####
      # write.csv(t1, "C:/Users/VHACINBarneT/Desktop/cleveland t3 test.csv", row.names = FALSE)
      # t1=t1[,c(1:17,18:19,20:53,57:64)]
      # t1 <- t1 %>% filter(StartDate < input$dateRange_t1[2] + 1)
      # t1 <- as.data.frame(sapply(t1, as.character))
      # iteminfo = read.csv("LIA Item Info_HTML_Nov 2023.csv") # may need changed
      # colnames(t1) = c(iteminfo$abbr[1:17], "Q1","Q2", iteminfo$abbr[18:59]) #iteminfo$abbr[grepl('QID', iteminfo$ImportId)]
      # t1[t1==""]=NA
      # items = c(iteminfo$abbr[!is.na(iteminfo$factor)], "Q1","Q2")
      # t1$missing<-apply(t1[,items], 1, FUN=function(x)length(x[is.na(x)]))
      # t1<-t1[t1$missing < length(items),]
      # t1$missing=NULL
      # t1 <- t1 %>% filter(!`Response Type` %in% "Survey Preview")
      # t1$Q1=NULL
      # t1$Q2=NULL
      #####
      
      
      t1 <- t1 %>% filter(StartDate < input$dateRange_t1[2] + 1)
      t1 <- as.data.frame(sapply(t1, as.character))
      iteminfo = read.csv("LIA Item Info_HTML_feb 2025.csv") # may need changed
      colnames(t1) = iteminfo$abbr #iteminfo$abbr[grepl('QID', iteminfo$ImportId)]
      items = iteminfo$abbr[!is.na(iteminfo$factor)]
      t1[t1==""]=NA
      t1$missing<-apply(t1[,items], 1, FUN=function(x)length(x[is.na(x)]))
      t1<-t1[t1$missing < length(items),]
      t1$missing=NULL
      t1 <- t1 %>% filter(!`Response Type` %in% "Survey Preview")
      
      #return(t1)
      
      # T2 Pull
      t2 = as.data.frame(fetch_survey(surveyID=c(input$survey_list_t2),add_var_labels = F,convert=F,force_request=T))
      t2 <- as.data.frame(sapply(t2, as.character))
      iteminfo=as.data.frame(fread("LIA_Item_Info_T2_revised_Feb2025.csv"))
      
      
      

      
      iteminfo[iteminfo == ""] = NA
      iteminfo$wording=iconv(iteminfo$wording,"UTF-8","UTF-8",sub=" ")
      iteminfo$wording2=iconv(iteminfo$wording2,"UTF-8","UTF-8",sub=" ")
      
      colnames(t2)[colnames(t2) %in% iteminfo$tag3] <- iteminfo$tag[iteminfo$tag3 %in% colnames(t2)]

      # t2[t2=="Strongly Agree"]=5
      # t2[t2=="Agree"]=4
      # t2[t2=="Neutral"]=3
      # t2[t2=="Disagree"]=2
      # t2[t2=="Strongly Disagree"]=1
      # t2[t2=="Not Observed"]=NA
      # t2[t2==""]=NA
      # t2[t2=="Not Applicable"]=NA
      # t2[t2=="Very Satisfied"]=5
      # t2[t2=="Satisfied"]=4
      # t2[t2=="Dissatisfied"]=2
      # t2[t2=="Very Dissatisfied"]=1
      # t2[t2=="I Do Not Know"]=NA
      
      rm_items <- iteminfo$tag[iteminfo$scale!="text"]
      # rm_items[!rm_items %in% colnames(t2)]
      t2$rm=apply(t2[,rm_items],1,function(x) sum(is.na(x))) #
      t2=filter(t2, rm < 39)
      

      return(list(t1=t1,
                  t2=t2))
    }
    
    # will need to address this later when the GES T2 and TA T2 are ready
    #   # GES  
    # } else if (input$type_report == "Group Experience Survey") {
    #   dat = as.data.frame(fetch_survey(surveyID=c(input$survey_list),add_var_labels = F,convert=F,force_request=T))
    #   iteminfo= read.csv("S:/NCOD/NCOD Staff Folders/Tyler Barnes/Group Experience Survey Report/NPO GES iteminfo.csv")
    #   setDT(dat)
    #   ges_order <- iteminfo$tag[grepl("Q",iteminfo$tag)]
    #   setcolorder(dat,ges_order)
    #   colnames(dat)[colnames(dat) %in% iteminfo$tag] <- iteminfo$abbr[iteminfo$tag %in% colnames(dat)]
    #   dat = as.data.frame(dat)
    #   dat=filter(dat, !Status %in% "Survey Preview")
    #   return(dat)
    #   
    #   # TA   
    # } else if (input$type_report == "Team Assessment 2.0"){
    #   dat = as.data.frame(fetch_survey(surveyID=c(input$survey_list),add_var_labels = F,convert=F,force_request=T))
    #   return(dat)
    # }
    
    #return(as.data.frame(fetch_survey(surveyID=c(input$survey_list),add_var_labels = F,convert=F,force_request=T)))
    
  })
  
  # T1 data
  selected_t1 <- reactive(getReactableState("data_survey_t1", "selected"))
  
  finalData_t1=reactiveValues(a=NULL)
  survey_id_t1 = reactiveValues(a=NULL)
  
  # T2 data
  selected_t2 <- reactive(getReactableState("data_survey_t2", "selected"))
  
  finalData_t2=reactiveValues(a=NULL)
  survey_id_t2 = reactiveValues(a=NULL)
  
 observeEvent(input$pull_t2,{
    
    # T1
    finalData_t1$a=pull_data_t2()$t1
    # df1<<-finalData_t1$a=pull_data_t2()$t1
    survey_id_t1$a = input$survey_list_t1
    
    output$response_count_t1=renderValueBox({
      valueBox(
        paste0(nrow(finalData_t1$a)), "Responses", icon = icon(NULL, class = "fa-solid fa-people-group"), width = 6, color = "teal"
      )
    })
    
    output$T2_infobox_t1=renderInfoBox({
      infoBox(
        title = "Did you check for irregularities in:",
        icon = icon(NULL, class="fa-solid fa-circle-exclamation"),
        div(HTML("<ul>
                      <li>Number of Responses</li>
                      <li>Network and/or Organization</li>
                      <li>Role</li>
                   </ul>"))
      )
    })
    
    # output$response_count_t1=renderInfoBox({
    #   infoBox(
    #     paste0(title = "Time 1<br>", value = nrow(finalData_t1$a)), subtitle = "Responses", icon = icon(NULL, class = "fa-solid fa-people-group"), width = 6, color = "teal"
    #   )
    # })
    
    # T2
    finalData_t2$a=pull_data_t2()$t2
    # df2<<-finalData_t2$a=pull_data_t2()$t2
    survey_id_t2$a = input$survey_list_t2
    
   output$response_count_t2=renderValueBox({
      valueBox(
        paste0(nrow(finalData_t2$a)), "Responses", icon = icon(NULL, class = "fa-solid fa-people-group"), width = 6, color = "teal"
      )
     
     
      
      # output$response_count_t2=renderText({
      #   paste0('Number of responses: ', nrow(finalData_t2$a))# prints number of responses of data pulled in
      #   #p(strong('bold'))
    })
   output$T2_infobox_t2=renderInfoBox({
     infoBox(
       title = "Did you check for irregularities in:",
       icon = icon(NULL, class="fa-solid fa-circle-exclamation"),
       div(HTML("<ul>
                      <li>Number of Responses</li>
                      <li>Network and/or Organization</li>
                      <li>Role</li>
                   </ul>"))
     )
   })
  })
  
  ## validate report text fields - think about how the below will be implemented with respect to the download button. may need to disable the download button and also require these fields.
  iv_t2 <- InputValidator$new()
  iv_t2$add_rule("team_name_t2", sv_required())
  # iv_t2$add_rule("start_end_date_t1", sv_required())
  # iv_t2$add_rule("start_end_date_t2", sv_required())
  # iv_t2$add_rule("t1_data", sv_required(message = "T1 Data Required"))
  #iv_t2$add_rule("end_date_t2", sv_required())
  
  # using conditional subforms to make sure that both of these conditions are used for the validation conditional
  LIA_iv_t2 <- InputValidator$new()
  LIA_iv_t2$condition(~ input$type_report_t2 == "Leadership Impact Assessment (LIA)")
  LIA_iv_t2$add_rule("resp_total_t1", function(value){
    if(is.na(value)){
      "Must be greater than 0"
    } else if(value < 0){
      "Must be greater than 0"
    }else if(value == 0){
      "Must be greater than 0"
    }
  })
  LIA_iv_t2$add_rule("resp_total_t2", function(value){
    if(is.na(value)){
      "Must be greater than 0"
    } else if(value < 0){
      "Must be greater than 0"
    }else if(value == 0){
      "Must be greater than 0"
    }
  })
  GES_iv_t2 <- InputValidator$new()
  GES_iv_t2$condition(~ input$type_report_t2 == "Group Experience Survey")
  GES_iv_t2$add_rule("resp_total_t1", function(value){
    if(is.na(value)){
      "Must be greater than 0"
    } else if(value < 0){
      "Must be greater than 0"
    }else if(value == 0){
      "Must be greater than 0"
    }
  }) 
  GES_iv_t2$add_rule("resp_total_t2", function(value){
    if(is.na(value)){
      "Must be greater than 0"
    } else if(value < 0){
      "Must be greater than 0"
    }else if(value == 0){
      "Must be greater than 0"
    }
  })  
  
  resp_total_iv_t2 <- InputValidator$new()
  resp_total_iv_t2$add_validator(LIA_iv_t2)
  resp_total_iv_t2$add_validator(GES_iv_t2)
  
  iv_dateRange_t1 <- InputValidator$new()
  iv_dateRange_t1$add_rule("dateRange_t1", function(value){
    if(sum(is.na(value)) == 2){
      "Please provide T1 administration dates"
    }
    else if(is.na(value[1])){
      "Please provide a Start date"
    } 
    else if(is.na(value[2])) {
      "Please provide an End date"
    } 
    else if(value[1] > value[2]){
      "Start date must precede End date"
    }
  })
  
  iv_dateRange_t2 <- InputValidator$new()
  iv_dateRange_t2$add_rule("dateRange_t2", function(value){
    if(sum(is.na(value)) == 2){
      "Please provide T2 administration dates"
    }
    else if(is.na(value[1])){
      "Please provide a Start date"
    } 
    else if(is.na(value[2])) {
      "Please provide an End date"
    } 
    else if(value[1] > value[2]){
      "Start date must precede End date"
    }
  })

  iv_dateRange_t1$enable()
  iv_dateRange_t2$enable()
  
  observe({
    if(sum(is.na(input$dateRange_t1)) == 0 & iv_dateRange_t1$is_valid() & sum(is.na(input$dateRange_t2)) == 0 & iv_dateRange_t2$is_valid()){
      show("pull_t2")
    } else{
      hide("pull_t2")
    }
  })

  observeEvent(input$pull_t2,{
    if(input$pull_t2 > 0){
      if (input$type_report_t2 == 'Leadership Impact Assessment (LIA)'){
        show("numresponses_t1_t2")
        show("numresponses_t2")
        enable('resp_total_t1')
        enable('resp_total_t2')
      } else if (input$type_report_t2 == "Group Experience Survey") {
        show("numresponses_t1_t2")
        show("numresponses_t2")
        enable('resp_total_t1')
        enable('resp_total_t2')
      } else {
        disable('resp_total_t1')
        disable('resp_total_t2')
      }
      iv_t2$enable() # validation visualization triggered when pull data is
      resp_total_iv_t2$enable()
      #resp_total_iv2$enable()
      # disable("report_t2")
      shinyjs::show("rm_t2")
      shinyjs::show("response_count_t2")
      enable("rm_t2")
      enable("team_name_t2")
      # enable("start_end_date_t1")
      # enable("start_end_date_t2")
      show("time2_t1_box")
      show("time2_box")
      show("teamname_t2")
      
    } else {
      # disable("report_t2")
      # shinyjs::hide("rm_t2")
      disable("rm_t2")
    }
  })
  
  # observe({
  #   if(input$type_report_t2 == "Leadership Impact Assessment (LIA)" | input$type_report_t2 == "Group Experience Survey"){
  #     if(input$team_name_t2 != "" & input$start_end_date_t2 != "" & !is.na(input$resp_total_t1) & !is.na(input$resp_total_t2) & !is.null(input$t1_data)){
  #       enable("report_t2")
  #     } else {
  #       disable("report_t2")
  #     }
  #   } else{
  #     if(input$team_name_t2 != "" & input$start_end_date_t2 != "" & !is.null(input$t1_data)){
  #       enable("report_t2")
  #     } else {
  #       disable("report_t2")
  #     }
  #   }
  # })
  
  observeEvent(input$rm_t1,{
    shinyalert("Don't do something you'll regret!",
               text = 'Removing rows inside this app does not remove the rows in Qualtrics. <br><br>
               If you remove them here and need to rerun this report later, these rows will show up in that report unless you remove them in Qualtrics. 
               Be kind to your future self, go ahead and open up <a href="https://ssologon.iam.va.gov/centrallogin/Default.aspx?appname=core&URL=https://ssologon.iam.va.gov/centrallogin/core/redirect.aspx&TYPE=33619969&REALMOID=06-345d7582-c96e-4888-9e5f-6e86468bf060&GUID=&SMAUTHREASON=0&METHOD=GET&SMAGENTNAME=-SM-JDZx1AxYAhQguyl0rfvd%2f5f46jynE%2bEoBtr6BQQU4NWuWIwCZNPIPHj210fDMxqs&TARGET=-SM-HTTPS%3a%2f%2flogon%2eiam%2eva%2egov%2faffwebservices%2fredirectjsp%2fredirect%2ejsp%3fapp%3dQUALTRICSEES%26SAMLRequest%3djZJBT-%2BswEIT-%2FiuV74tgN0FhNUQEhKoFeRQMHLmibbIqlxA5eJ-%2FDzX2iogAviaHlnxjufF-%2BfvbcMG9GSczbmME87Qlq4ydp-%2Fzh-%2BI6mvPz5YKgbTq96sOLvcfXHimwUWdJHy5y3nurHZAhbaFF0qHU29XdrVZxojvvgitdw9mKCH0Ygy6dpb5Fv0U-%2FmBIf7m9z-%2FhJCR1qIARCpxspD28V7N8j4tYcmeFNSXLpWNG5vrBikIHLiI16JCmvomxBRx9nV-%2BDZjIRzWOXqOGmdjA208wIengLp-%2Bwx1N8SS6fteYcnIbbTm7dr7Ew7o5r6Eh5Gx9lfPnZKfSeZWdJIlMTxRU6UxlKkMFaXUqQY3lrWkDRGbALyFRj2tLAWzIuUpUGiXzKMkKOdezTMuzeCblE2ebz54ujJ3a-%2F63U3TRE-%2BqYoNtHm37bg7PFIcRzgEzN9CPffYP1uC0dCfPlHHgvxLWc5nX7-%2BlOV-%2F%26RelayState%3dLNS--_3596cbf853309e089b7b0387df373373%26SigAlg%3dhttp-%3A-%2F-%2Fwww%2ew3%2eorg-%2F2001-%2F04-%2Fxmldsig--more-%23rsa--sha256%26Signature%3dCG6Wr2Q3njw59DotYLtqwIwfvDIKVoIWx6PJr-%2BN5p5arUqa2SPlrzE8SqoAgiC-%2BKtjHuFBN3LMBzzrAyX4-%2FHxVSc7R5fkpJio1fGI-%2FF9VN9niSdG4pxqR8QGFwHADv87W2GIHCf-%2F1MiRd8LM8sAQ3yklFy008-%2FDQgFsq0xQifWvml-%2FBUOvmu8zqBjZPVkgGZ2ZhzsKYjfcjPIK-%2FyGNIOuiOpqV1F9WO8Q8izwriK15D2X22xsGBUFZ97-%2BlnY0OOhJS9-%2FPsNe2LI5d4UmTUrKAdeY0HmidUkrjpsXh1-%2FN8sL3hrYi-%2BNmR51sgocks8Rl4r-%2BfCBYhunsNWf26kbl2mEd4chGZrKmjgdvipxtHrCNH-%2B5VpI2MPh3S9X94xlD-%2FO7L7uxFlnnQ8BuCzZktJejXkkHtMFpPgL1nngcRYaV3VhlxYLblv-%2BIILCt2QTSQzGrt9JKkpvTT2zo3O66JgiW7foOXRel3huZvp7QDJ0SQ2sE7M23BAT6ze-%2FdSYH5S93ZJ4qthMwn3DvUlTjhdbc-%2Be-%2FWDmIDWjxSVlsS7pSmTyywCf-%2Fg-%2FiDXE60aB-%2FaneJG8zITpOPWggMEFsSp5rDZBrk7pmhOm5LMrHxve-%2Bl0QXMoAxhNqR-%2BKDljYPixBtOYXmgui8cyi-%2FV4o0GnNQWO2x6KKOT6NihSF8C8p6h5OzIeCA-%3D%26SMPORTALURL%3dhttps-%3A-%2F-%2Flogon%2eiam%2eva%2egov-%2Faffwebservices-%2Fpublic-%2Fsaml2sso%26SAMLTRANSACTIONID%3d10857a5e--6fc4de32--9abd06d9--aa4b4d99--515ba0c4--a6d" target = "_blank">Qualtrics</a> real quick',
               type = "warning",
               html = TRUE,
               confirmButtonText = "Remove Rows Anyway",
               cancelButtonText = "Do not Remove Rows",
               closeOnEsc = FALSE,
               showCancelButton = TRUE,
               inputId = "shinyalert_t1")
  })
  
  observeEvent(input$rm_t2,{
    shinyalert("Don't do something you'll regret!",
               text = 'Removing rows inside this app does not remove the rows in Qualtrics. <br><br>
               If you remove them here and need to rerun this report later, these rows will show up in that report unless you remove them in Qualtrics. 
               Be kind to your future self, go ahead and open up <a href="https://ssologon.iam.va.gov/centrallogin/Default.aspx?appname=core&URL=https://ssologon.iam.va.gov/centrallogin/core/redirect.aspx&TYPE=33619969&REALMOID=06-345d7582-c96e-4888-9e5f-6e86468bf060&GUID=&SMAUTHREASON=0&METHOD=GET&SMAGENTNAME=-SM-JDZx1AxYAhQguyl0rfvd%2f5f46jynE%2bEoBtr6BQQU4NWuWIwCZNPIPHj210fDMxqs&TARGET=-SM-HTTPS%3a%2f%2flogon%2eiam%2eva%2egov%2faffwebservices%2fredirectjsp%2fredirect%2ejsp%3fapp%3dQUALTRICSEES%26SAMLRequest%3djZJBT-%2BswEIT-%2FiuV74tgN0FhNUQEhKoFeRQMHLmibbIqlxA5eJ-%2FDzX2iogAviaHlnxjufF-%2BfvbcMG9GSczbmME87Qlq4ydp-%2Fzh-%2BI6mvPz5YKgbTq96sOLvcfXHimwUWdJHy5y3nurHZAhbaFF0qHU29XdrVZxojvvgitdw9mKCH0Ygy6dpb5Fv0U-%2FmBIf7m9z-%2FhJCR1qIARCpxspD28V7N8j4tYcmeFNSXLpWNG5vrBikIHLiI16JCmvomxBRx9nV-%2BDZjIRzWOXqOGmdjA208wIengLp-%2Bwx1N8SS6fteYcnIbbTm7dr7Ew7o5r6Eh5Gx9lfPnZKfSeZWdJIlMTxRU6UxlKkMFaXUqQY3lrWkDRGbALyFRj2tLAWzIuUpUGiXzKMkKOdezTMuzeCblE2ebz54ujJ3a-%2F63U3TRE-%2BqYoNtHm37bg7PFIcRzgEzN9CPffYP1uC0dCfPlHHgvxLWc5nX7-%2BlOV-%2F%26RelayState%3dLNS--_3596cbf853309e089b7b0387df373373%26SigAlg%3dhttp-%3A-%2F-%2Fwww%2ew3%2eorg-%2F2001-%2F04-%2Fxmldsig--more-%23rsa--sha256%26Signature%3dCG6Wr2Q3njw59DotYLtqwIwfvDIKVoIWx6PJr-%2BN5p5arUqa2SPlrzE8SqoAgiC-%2BKtjHuFBN3LMBzzrAyX4-%2FHxVSc7R5fkpJio1fGI-%2FF9VN9niSdG4pxqR8QGFwHADv87W2GIHCf-%2F1MiRd8LM8sAQ3yklFy008-%2FDQgFsq0xQifWvml-%2FBUOvmu8zqBjZPVkgGZ2ZhzsKYjfcjPIK-%2FyGNIOuiOpqV1F9WO8Q8izwriK15D2X22xsGBUFZ97-%2BlnY0OOhJS9-%2FPsNe2LI5d4UmTUrKAdeY0HmidUkrjpsXh1-%2FN8sL3hrYi-%2BNmR51sgocks8Rl4r-%2BfCBYhunsNWf26kbl2mEd4chGZrKmjgdvipxtHrCNH-%2B5VpI2MPh3S9X94xlD-%2FO7L7uxFlnnQ8BuCzZktJejXkkHtMFpPgL1nngcRYaV3VhlxYLblv-%2BIILCt2QTSQzGrt9JKkpvTT2zo3O66JgiW7foOXRel3huZvp7QDJ0SQ2sE7M23BAT6ze-%2FdSYH5S93ZJ4qthMwn3DvUlTjhdbc-%2Be-%2FWDmIDWjxSVlsS7pSmTyywCf-%2Fg-%2FiDXE60aB-%2FaneJG8zITpOPWggMEFsSp5rDZBrk7pmhOm5LMrHxve-%2Bl0QXMoAxhNqR-%2BKDljYPixBtOYXmgui8cyi-%2FV4o0GnNQWO2x6KKOT6NihSF8C8p6h5OzIeCA-%3D%26SMPORTALURL%3dhttps-%3A-%2F-%2Flogon%2eiam%2eva%2egov-%2Faffwebservices-%2Fpublic-%2Fsaml2sso%26SAMLTRANSACTIONID%3d10857a5e--6fc4de32--9abd06d9--aa4b4d99--515ba0c4--a6d" target = "_blank">Qualtrics</a> real quick',
               type = "warning",
               html = TRUE,
               confirmButtonText = "Remove Rows Anyway",
               cancelButtonText = "Do not Remove Rows",
               closeOnEsc = FALSE,
               showCancelButton = TRUE,
               inputId = "shinyalert_t2")
  })
  
  
  observe({
    print(input$shinyalert_t1)
    print(input$shinyalert_t2)
  })
  
  observeEvent(input$shinyalert_t1,{
    if(input$shinyalert_t1 == "TRUE"){
      if(!is.null(selected_t1())){
        finalData_t1$a=finalData_t1$a[-selected_t1(),, drop=F]
      }
      output$response_count_t1=renderValueBox({
        valueBox(
          paste0(nrow(finalData_t1$a)), "Responses", icon = icon(NULL, class = "fa-solid fa-people-group"), width = 6, color = "teal"
        )
        
        # output$response_count_t1=renderText({
        #   paste0('Number of responses: ', nrow(finalData_t1$a))# prints number of responses of data pulled in
        #   #p(strong('bold'))
      })
    }
  })
  
  observeEvent(input$shinyalert_t2,{
    if(input$shinyalert_t2 == "TRUE"){
      if(!is.null(selected_t2())){
        finalData_t2$a=finalData_t2$a[-selected_t2(),, drop=F]
      }
      output$response_count_t2=renderValueBox({
        valueBox(
          paste0(nrow(finalData_t2$a)), "Responses", icon = icon(NULL, class = "fa-solid fa-people-group"), width = 6, color = "teal"
        )
        
        # output$response_count_t2=renderText({
        #   paste0('Number of responses: ', nrow(finalData_t2$a))# prints number of responses of data pulled in
        #   #p(strong('bold'))
      })
    }
  })
  
  ### clearing final data when inputs are changed (resets reactable and number of responses output)
  upload_dat=reactiveValues(a=NULL)
  
  # Consultant
  observeEvent(input$consultant_t2, {
    finalData_t2$a = NULL
    iv_t2$disable()
    resp_total_iv_t2$disable()
    # shinyjs::hide("rm_t2")
    # shinyjs::hide("response_count_t2")
    updateTextInput(session,"team_name_t2", value="")
    # updateTextInput(session,"start_end_date_t1", value="")
    # updateTextInput(session,"start_end_date_t2", value="")
    updateTextInput(session,"resp_total_t1", value="")
    updateTextInput(session,"resp_total_t2", value="")
    updateDateRangeInput(session,"dateRange_t1",start=NA,end=NA)
    updateDateRangeInput(session,"dateRange_t2",start=NA,end=NA)
    hide("time2_t1_box")
    hide("time2_box")
    hide("teamname_t2")
    hide("numresponses_t1_t2")
    hide("numresponses_t2")
    #hide("txtinptbox_t2")
    disable("team_name_t2")
    # disable("start_end_date_t1")
    # disable("start_end_date_t2")
    disable("resp_total_t1")
    disable("resp_total_t2")
    # reset("t1_data")
    upload_dat$a=NULL
    # updat$data="stop"
  })
  
  # report type
  observeEvent(input$type_report_t2, {
    finalData_t2$a = NULL
    iv_t2$disable()
    resp_total_iv_t2$disable()
    # shinyjs::hide("rm_t2")
    # shinyjs::hide("response_count_t2")
    updateTextInput(session,"team_name_t2", value="")
    # updateTextInput(session,"start_end_date_t1", value="")
    # updateTextInput(session,"start_end_date_t2", value="")
    updateTextInput(session,"resp_total_t1", value="")
    updateTextInput(session,"resp_total_t2", value="")
    updateDateRangeInput(session,"dateRange_t1",start=NA,end=NA)
    updateDateRangeInput(session,"dateRange_t2",start=NA,end=NA)
    hide("time2_t1_box")
    hide("time2_box")
    # hide("txtinptbox_t2")
    hide("teamname_t2")
    hide("numresponses_t1_t2")
    hide("numresponses_t2")
    disable("team_name_t2")
    # disable("start_end_date_t1")
    # disable("start_end_date_t2")
    disable("resp_total_t1")
    disable("resp_total_t2")
    # reset("t1_data")
    upload_dat$a=NULL
    # updat$data="stop"
  })
  
  # survey list
  observeEvent(input$survey_list_t1,{
    updateDateRangeInput(session,"dateRange_t1",start=NA,end=NA)
  })
  
  observeEvent(input$survey_list_t2, {
    finalData_t2$a = NULL
    iv_t2$disable()
    resp_total_iv_t2$disable()
    # shinyjs::hide("rm_t2")
    # shinyjs::hide("response_count_t2")
    updateTextInput(session,"team_name_t2", value="")
    # updateTextInput(session,"start_end_date_t1", value="")
    # updateTextInput(session,"start_end_date_t2", value="")
    updateTextInput(session,"resp_total_t1", value="")
    updateTextInput(session,"resp_total_t2", value="")
    #updateDateRangeInput(session,"dateRange_t1",start=NA,end=NA)
    updateDateRangeInput(session,"dateRange_t2",start=NA,end=NA)
    hide("time2_t1_box")
    hide("time2_box")
    # hide("txtinptbox_t2")
    # hide("teamname_t2")
    hide("numresponses_t1_t2")
    hide("numresponses_t2")
    disable("team_name_t2")
    # disable("start_end_date_t1")
    # disable("start_end_date_t2")
    disable("resp_total_t1")
    disable("resp_total_t2")
    # reset("t1_data")
    upload_dat$a=NULL
    # updat$data="stop"
  })
  
  # reset consultant when switching between tabs - this will reset everything except the selected type of report and the selected survey
  observeEvent(input$Time, {
    finalData$a = NULL
    iv_t2$disable()
    resp_total_iv_t2$disable()
    # shinyjs::hide("rm_t2")
    # shinyjs::hide("response_count_t2")
    # updateSelectInput(session, "type_report_t2", choices = list("LTA" = "Leadership Team Assessment (LTA)",
    #                                                          "LIA" = "Leadership Impact Assessment (LIA)",
    #                                                          "GES" = "Group Experience Survey",
    #                                                          "TA" = "Team Assessment 2.0"))
    updateSelectInput(session, "type_report_t2", choices = list("LTA" = "Leadership Team Assessment (LTA)",
                                                                "LIA" = "Leadership Impact Assessment (LIA)"))
    updateSelectInput(session, "consultant_t2", choices=consultants$Name)
    updateTextInput(session,"team_name_t2", value="")
    # updateTextInput(session,"start_end_date_t1", value="")
    # updateTextInput(session,"start_end_date_t2", value="")
    updateTextInput(session,"resp_total_t1", value="")
    updateTextInput(session,"resp_total_t2", value="")
    updateDateRangeInput(session,"dateRange_t1",start=NA,end=NA)
    updateDateRangeInput(session,"dateRange_t2",start=NA,end=NA)
    hide("time2_t1_box")
    hide("time2_box")
    # hide("txtinptbox_t2")
    hide("teamname_t2")
    hide("numresponses_t1_t2")
    hide("numresponses_t2")
    disable("team_name_t2")
    # disable("start_end_date_t1")
    # disable("start_end_date_t2")
    disable("resp_total_t1")
    disable("resp_total_t2")
    # reset("t1_data")
    upload_dat$a=NULL
    # updat$data="stop"
  })
  
  observeEvent(input$dateRange_t1, {
    finalData_t2$a = NULL
    iv_t2$disable()
    resp_total_iv_t2$disable()
    # shinyjs::hide("rm_t2")
    # shinyjs::hide("response_count_t2")
    updateTextInput(session,"team_name_t2", value="")
    # updateTextInput(session,"start_end_date_t1", value="")
    # updateTextInput(session,"start_end_date_t2", value="")
    updateTextInput(session,"resp_total_t1", value="")
    updateTextInput(session,"resp_total_t2", value="")
    #updateDateRangeInput(session,"dateRange_t1",start=NA,end=NA)
    #updateDateRangeInput(session,"dateRange_t2",start=NA,end=NA)
    hide("time2_t1_box")
    hide("time2_box")
    # hide("txtinptbox_t2")
    hide("teamname_t2")
    hide("numresponses_t1_t2")
    hide("numresponses_t2")
    disable("team_name_t2")
    # disable("start_end_date_t1")
    # disable("start_end_date_t2")
    disable("resp_total_t1")
    disable("resp_total_t2")
    # reset("t1_data")
    upload_dat$a=NULL
    # updat$data="stop"
  })
  
  observeEvent(input$dateRange_t2, {
    finalData_t2$a = NULL
    iv_t2$disable()
    resp_total_iv_t2$disable()
    # shinyjs::hide("rm_t2")
    # shinyjs::hide("response_count_t2")
    updateTextInput(session,"team_name_t2", value="")
    # updateTextInput(session,"start_end_date_t1", value="")
    # updateTextInput(session,"start_end_date_t2", value="")
    updateTextInput(session,"resp_total_t1", value="")
    updateTextInput(session,"resp_total_t2", value="")
    #updateDateRangeInput(session,"dateRange_t1",start=NA,end=NA)
    #updateDateRangeInput(session,"dateRange_t2",start=NA,end=NA)
    hide("time2_t1_box")
    hide("time2_box")
    # hide("txtinptbox_t2")
    hide("teamname_t2")
    hide("numresponses_t1_t2")
    hide("numresponses_t2")
    disable("team_name_t2")
    # disable("start_end_date_t1")
    # disable("start_end_date_t2")
    disable("resp_total_t1")
    disable("resp_total_t2")
    # reset("t1_data")
    upload_dat$a=NULL
    # updat$data="stop"
  })
  
  
  global <- reactiveValues(response=FALSE)
  updat <- reactiveValues(data="stop")
  
  # this wipes the Uploaded file when the wrong file type is uploaded
  # observeEvent(input$t1_data, {
  #   uploaded_data <- input$t1_data
  #   
  #   if (is.null(uploaded_data))
  #     return(NULL)
  #   
  #   if(tools::file_ext(uploaded_data$name) != "csv"){
  #     updat$data="stop"
  #     upload_dat$a=NULL
  #     shinyalert::shinyalert(title="Wrong File Formating!",
  #                            text="Your file that you uploaded is not a .csv file. Only upload data that is a result of running the report from this app. If you accidentally uploaded a .zip file, be sure to unzip the file and save the contents on your computer or on the S drive. This will allow you to upload only the .csv file of cleaned data.",
  #                            type="error",
  #                            html=FALSE,
  #                            closeOnEsc = FALSE,
  #                            callbackR = function(x){
  #                              global$response <- x
  #                              # print(global$response)
  #                            })
  #     
  #     
  #   }else{
  #     updat$data="go"
  #     upload_dat$a=input$t1_data$datapath
  #   }
  # })
  
  output$test_file=renderText({
    validate(need(!is.null(upload_dat$a),HTML("*Upload T1 Data")))
  })
  # 
  # outputOptions(output, "test_file", suspendWhenHidden = FALSE)
  
  observe({
    if(global$response == TRUE){
      #updat$data <- NULL
      global$response = FALSE

      # reset("t1_data")
      upload_dat$a=NULL
      # updat$data="stop"
      # onevent("mouseleave","t1_data",{
      #   print(input$t1_data)
      #   iv_t2$enable()
      # })
      
    }
  })

  # T1 Output
  output$data_survey_t1=renderReactable({
    if(!is.null(finalData_t1$a)){
      if (input$type_report_t2 == "Leadership Team Assessment (LTA)"){
        table <- finalData_t1$a %>% dplyr::select(-c(Status:UserLanguage,Intro))
        names(table)[6] <- "Role"
        reactable(table,selection = "multiple", 
                  onClick = "select", 
                  compact = T, 
                  highlight = T, 
                  striped = T, 
                  theme = theme, 
                  defaultColDef = colDef(width = 175, align = 'center'))#,
        
        # LIA  
      } else if (input$type_report_t2 == "Leadership Impact Assessment (LIA)"){
        table <- finalData_t1$a %>% dplyr::select(-c(`Response Type`:`User Language`))
        reactable(table,selection = "multiple", 
                  onClick = "select", 
                  compact = T, 
                  highlight = T, 
                  striped = T, 
                  theme = theme, 
                  defaultColDef = colDef(width = 175, align = 'center'),
                  defaultPageSize = 5,
                  showPageSizeOptions = TRUE,
                  pageSizeOptions = c(5,10,15,20,nrow(table)))
      }
      #   # GES  
      # } else if (input$type_report_t1 == "Group Experience Survey"){
      #   table <- finalData$a %>% dplyr::select(-c(StartDate:UserLanguage)) # Columns names slightly different
      #   reactable(table,selection = "multiple", onClick = "select", compact = T, highlight = T, striped = T, theme = theme, defaultColDef = colDef(width = 175, align = 'center'))
      #   
      #   # TA  
      # } else if (input$type_report_t1 == "Team Assessment 2.0"){
      #   table <- finalData$a %>% dplyr::select(-c(StartDate:UserLanguage, block2progress:FL_6_DO_FL_8))
      #   reactable(table,selection = "multiple", onClick = "select", compact = T, highlight = T, striped = T, theme = theme, defaultColDef = colDef(width = 175, align = 'center'))
      # }
    }
  })
  
  # T2 Output
  output$data_survey_t2=renderReactable({
    if(!is.null(finalData_t2$a)){
      if (input$type_report_t2 == "Leadership Team Assessment (LTA)"){
        table <- finalData_t2$a %>% dplyr::select(-c(Status:UserLanguage))
        table_iteminfo = read.csv("eta_t2_item information_revised_march2025.csv")
        colnames(table)[colnames(table) %in% table_iteminfo$tag] = table_iteminfo$abbr[table_iteminfo$tag %in% colnames(table)]
        colnames(table)[3:6] <- c("Agency","Network","Organization","Role")
        reactable(table,
                  selection = "multiple", 
                  onClick = "select", 
                  compact = T, 
                  highlight = T, 
                  striped = T, 
                  theme = theme, 
                  defaultColDef = colDef(width = 175, align = 'center'))#,
        
        # LIA  
      } else if (input$type_report_t2 == "Leadership Impact Assessment (LIA)"){
        

        
        
        table <- finalData_t2$a %>% dplyr::select(-c(Status:UserLanguage))
        table_iteminfo=as.data.frame(fread("LIA_Item_Info_T2_revised_Feb2025.csv"))
        names(table)[c(3:4,8:49)] <- table_iteminfo$abbr_t2[names(table)[c(3:4,8:49)] %in% table_iteminfo$tag]
        names(table)[c(5:7)] <- c("Agency","Network","Organization")
        table <- table[,c(1:2,5:7,3:4,8:ncol(table))]
        reactable(table[,1:49],
                  selection = "multiple", 
                  onClick = "select", 
                  compact = T, 
                  highlight = T, 
                  striped = T, 
                  theme = theme, 
                  defaultColDef = colDef(width = 175, align = 'center'),
                  defaultPageSize = 5,
                  showPageSizeOptions = TRUE,
                  pageSizeOptions = c(5,10,15,20,nrow(table)))
      }
      #   # GES  
      # } else if (input$type_report_t2 == "Group Experience Survey"){
      #   table <- finalData$a %>% dplyr::select(-c(StartDate:UserLanguage)) # Columns names slightly different
      #   reactable(table,selection = "multiple", onClick = "select", compact = T, highlight = T, striped = T, theme = theme, defaultColDef = colDef(width = 175, align = 'center'))
      #   
      #   # TA  
      # } else if (input$type_report_t2 == "Team Assessment 2.0"){
      #   table <- finalData$a %>% dplyr::select(-c(StartDate:UserLanguage, block2progress:FL_6_DO_FL_8))
      #   reactable(table,selection = "multiple", onClick = "select", compact = T, highlight = T, striped = T, theme = theme, defaultColDef = colDef(width = 175, align = 'center'))
      # }
    }
  })
  
  output$report_t2 <- downloadHandler(
    
    filename = function() {
      # if (input$type_report_t2 == "Executive Team Assessment (ETA)"){
      #   paste0(input$team_name_t2,"_ETA_T2.zip")
      # } else if (input$type_report_t2 == "Leadership Impact Assessment (LIA)") {
      #   paste0(input$team_name_t2,"_LIA_T2.zip")
      # } else if (input$type_report_t2 == "Group Experience Survey"){
      #   paste0(input$team_name_t2,"_GES_T2.zip")
      # } else {
      #   paste0(input$team_name_t2,"_TA_T2.zip")
      # }
      if (input$type_report_t2 == "Leadership Team Assessment (LTA)"){
        paste0(input$team_name_t2,"_LTA_T2.html")
      } else if (input$type_report_t2 == "Leadership Impact Assessment (LIA)") {
        paste0(input$team_name_t2,"_LIA_T2.html")
      } else if (input$type_report_t2 == "Group Experience Survey"){
        paste0(input$team_name_t2,"_GES_T2.html")
      } else {
        paste0(input$team_name_t2,"_TA_T2.html")
      }
    },
    content = function(file) {
      # shiny::withProgress(
      #   message
      #write.csv(finalData$a, "test.csv")
      # if (input$type_report_t2 == "Executive Team Assessment (ETA)"){
      #   filename2 = paste0(input$team_name_t2,"_ETA_T2.html")
      #   #filename2 = "ETA_T2_HTML.html"
      # } else if (input$type_report_t2 == "Leadership Impact Assessment (LIA)"){
      #   filename2 = paste0(input$team_name_t2,"_LIA_T2.html")
      #   #filename2 = "LIA_T2_HTML.html"
      # } else if (input$type_report_t2 == "Group Experience Survey"){
      #   filename2 = paste0(input$team_name_t2,"_GES_T2.html")
      # } else {
      #   filename2 = paste0(input$team_name_t2,"_TA_T2.html")
      # }
      
      if (input$type_report_t2 == "Leadership Team Assessment (LTA)"){
        req(iv_t2$is_valid()) # makes sure report will not run if all the required fields are not filled in
        
        t2 <- finalData_t2$a
        iteminfo=as.data.frame(fread("eta_t2_item information_revised_march2025.csv"))
        colnames(t2)=iteminfo$abbr
        agree_scale=c("Strongly Disagree","Disagree","Neutral","Agree","Strongly Agree")
        agree_scale_num = c("1","2","3","4","5")
        yn_scale=c("Yes","No")
        yn_scale_num=c("1","2")
        improve_scale=c("Declined A Lot","Declined","No Change","Improved","Improved A Lot")
        improve_scale_num = c("1","2","3","4","5")
        
        items_agree=iteminfo$abbr[iteminfo$scale %in% "agree"]
        items_yn=iteminfo$abbr[iteminfo$scale %in% "y/n"]
        items_improve=iteminfo$abbr[iteminfo$scale %in% "improve"]
        items_conflict=iteminfo$abbr[iteminfo$scale %in% "selected"]
        
        t2_items=iteminfo$abbr[iteminfo$rm %in% "no"]
        

        
        
        t2[t2==""]=NA
        t2[t2=="-"]=NA
        t2[t2=="Do Not Know"]=NA
        
        # t2=select(t2, all_of(t2_items))
        
        t2$rm=apply(t2[,22:61],1, function(x) sum(is.na(x)))
        
        t2=t2 %>%
          filter(rm !=40) %>%
          select(-rm)
        
        t2$Group=input$team_name_t2
        t2$Time="T2"
        t2$Consultant=input$consultant_t2
        
        #data confidentiality stuff
        dat2a=t2[,c(1:21, 62:80)]
        dat2b=t2[,c(9,78:80,22:61)]
        ids=data.frame(ResponseId=dat2a$ResponseId)
        ids$new_id=IDGen(nrow(ids))
        dat2b=merge(ids,dat2b, "ResponseId")
        dat2b$ResponseId=NULL
        
        
        write.csv(dat2b, paste0(input$team_name_t2,"_ETA_T2_Cleaned Data_quant.csv"), row.names=F)
        write.csv(dat2a, paste0(input$team_name_t2,"_ETA_T2_Cleaned Data_demogs.csv"), row.names=F)
        write.csv(ids, paste0(input$team_name_t2,"_ETA_T2_Cleaned Data_ids.csv"), row.names=F)
        
        email <- envelope()
        email<-email %>%
          from("tyler.barnes@va.gov") %>%
          to("tyler.barnes@va.gov","matthew.ellis3@va.gov") %>%
          subject(paste0("ETA T2 - ",input$team_name_t2)) %>% 
          html(paste0("See the <b>ETA T2</b> data attached for data for ",input$team_name_t2)) %>%
          attachment(paste0(input$team_name_t2,"_ETA_T2_Cleaned Data_quant.csv")) %>% 
          attachment(paste0(input$team_name_t2,"_ETA_T2_Cleaned Data_demogs.csv")) %>% 
          attachment(paste0(input$team_name_t2,"_ETA_T2_Cleaned Data_ids.csv"))
        
        smtp(email, verbose = FALSE)
        
        rmarkdown::render(input="ETA_T2_HTML.Rmd", output_file = file)
        #rmarkdown::render(input="ETA_T2_HTML.Rmd", output_file = filename2) #output="text_tex.tex"
        #files <- c(filename2, paste0(input$team_name_t2,"_ETA_T2_Cleaned Data.csv"))
        #files <- c(paste0(input$team_name_t2,"_ETA_T2_Cleaned Data.csv"))
        #file.copy(paste0(input$team_name_t2,"_ETA_T2.pdf"),file)
        #data = finalData$a
        #zip::zipr(file, files)
      } else if (input$type_report_t2 == "Leadership Impact Assessment (LIA)"){
        req(iv_t2$is_valid()) # makes sure report will not run if all the required fields are not filled in

        
        t2 <- finalData_t2$a
        t2 <- as.data.frame(sapply(t2, as.character))
        
        iteminfo=as.data.frame(fread("LIA_Item_Info_T2_revised_Feb2025.csv"))
        iteminfo[iteminfo == ""] = NA
        iteminfo$wording=iconv(iteminfo$wording,"UTF-8","UTF-8",sub=" ")
        iteminfo$wording2=iconv(iteminfo$wording2,"UTF-8","UTF-8",sub=" ")
        
        iteminfo$wording=qdapRegex::rm_non_ascii(iteminfo$wording)
        iteminfo$wording1=qdapRegex::rm_non_ascii(iteminfo$wording1)
        

        ########
        
        t2[t2=="Strongly Agree"]=5
        t2[t2=="Agree"]=4
        t2[t2=="Neutral"]=3
        t2[t2=="Disagree"]=2
        t2[t2=="Strongly Disagree"]=1
        t2[t2=="Not Observed"]=NA
        t2[t2==""]=NA
        t2[t2=="Not Applicable"]=NA
        t2[t2=="Very Satisfied"]=5
        t2[t2=="Satisfied"]=4
        t2[t2=="Dissatisfied"]=2
        t2[t2=="Very Dissatisfied"]=1
        
        rm_items <- iteminfo$tag[iteminfo$scale!="text"]
        
        t2$rm=apply(t2[,rm_items],1,function(x) sum(is.na(x))) #
        t2=filter(t2, rm < 39)
        t2$Group=input$team_name_t2
        t2$Time="T2"
        t2$Consultant=input$consultant_t2
        
        colnames(t2)[colnames(t2)=="Response ID"]="ResponseId"
        dat2a=t2[,c(1:28,30:36, 38:48, 50:56, 58:67)]
        dat2b=t2[,c(1:2,9,29,37,49,57, 66:67)]
        ids=data.frame(ResponseId=dat2a$ResponseId)
        ids$new_id=IDGen(nrow(ids))
        dat2b=merge(ids,dat2b, "ResponseId")
        dat2b$ResponseId=NULL
        
        
        write.csv(dat2a, paste0(input$team_name_t2,"_LIA_T2_Cleaned Data_quant.csv"), row.names=F) 
        write.csv(dat2b, paste0(input$team_name_t2,"_LIA_T2_Cleaned Data_text.csv"), row.names=F) 
        write.csv(ids, paste0(input$team_name_t2,"_LIA_T2_Cleaned Data_ids.csv"), row.names=F) 
        
        
        # write.csv(t2, paste0(input$team_name_t2,"_LIA_T2_Cleaned Data.csv"), row.names=F)
        # 
        # attachment_use=paste0(input$team_name_t2,"_LIA_T2_Cleaned Data.csv")
        email <- envelope()
        email<-email %>%
          from("tyler.barnes@va.gov") %>%
          to("tyler.barnes@va.gov","matthew.ellis3@va.gov") %>%
          subject(paste0("LIA T2 - ",input$team_name_t2)) %>% 
          html("See the <b>LIA T2</b> data attached for data for ",input$team_name_t2) %>%
          attachment(paste0(input$team_name_t2,"_LIA_T2_Cleaned Data_quant.csv")) %>% 
          attachment(paste0(input$team_name_t2,"_LIA_T2_Cleaned Data_text.csv")) %>%
          attachment(paste0(input$team_name_t2,"_LIA_T2_Cleaned Data_ids.csv"))
          
        
        smtp(email, verbose = FALSE)
        
        rmarkdown::render(input="LIA_T2_HTML.Rmd", output_file = file)
        #rmarkdown::render(input="LIA_T2_HTML.Rmd", output_file = filename2) # changed text_tex.tex to text_tex1.tex, then changed it back
        #files <- c(filename2, paste0(input$team_name_t2,"_LIA_T2_Cleaned Data.csv"))
        #file.copy(paste0(input$team_name_t2,"_LIA_T2.pdf"),file)
        #zip::zipr(file, files)
      } else if (input$type_report_t2 == "Group Experience Survey"){
        req(iv_t2$is_valid()) # makes sure report will not run if all the required fields are not filled in
        
        rmarkdown::render(input="GES_Template.Rmd", output_file = file)
        #rmarkdown::render(input="GES_Template.Rmd", output_file = filename2) # changed text_tex.tex to text_tex1.tex, then changed it back
        #files <- c(filename2, paste0(input$team_name_t2,"_GES_Cleaned Data.csv"))
        #file.copy(paste0(input$team_name_t2,"_LIA_T1.pdf"),file)
        #zip::zipr(file, files)
      } else {
        req(iv_t2$is_valid()) # makes sure report will not run if all the required fields are not filled in
        
        rmarkdown::render(input="TA Report Template 2023 Test.Rmd", output_file = file)
        #rmarkdown::render(input="TA Report Template 2023 Test.Rmd", output_file = filename2) # changed text_tex.tex to text_tex1.tex, then changed it back
        #files <- c(filename2, paste0(input$team_name_t2,"_TA_Cleaned Data.csv"))
        #file.copy(paste0(input$team_name_t2,"_LIA_T1.pdf"),file)
        #zip::zipr(file, files)
      }
      # print(tinytex_root())
    }
    # print(tinytex_root())
  )  
  
  observeEvent(input$Time,{
    # print(input$Time)
    if(input$Time == "Time1"){
      if(!is.null(input$consultant)){
            qualtrics_api_credentials(
              api_key=consultants$keys[consultants$Name==input$consultant],
              base_url="gov1.qualtrics.com",
              overwrite = TRUE,
              install = FALSE
            )
      }
    }
    if(input$Time == "Time2"){
      if(!is.null(input$consultant_t2)){
            qualtrics_api_credentials(
              api_key=consultants$keys[consultants$Name==input$consultant_t2],
              base_url="gov1.qualtrics.com",
              overwrite = TRUE,
              install = FALSE
            )
      }
    }
  })
  
  # observeEvent(input$resp_total_t1,{
  #   print("test")
  # })
  # observeEvent(input$team_name,{
  #   print("test")
  # })
  observeEvent(input$team_name_t2,{
    print("test")
  })
  
  
})  


