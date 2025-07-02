
nested_reactable_ACT_reports <- function(long_df,demo_list,demo_vector,items, mean_column, sata = FALSE){
  
  if(sata == TRUE){
    overall <- filter(long_df, long_df$Demo %in% "Overall" & long_df$Item %in% items)
    demos <- filter(long_df, long_df$Demo %nin% "Overall" & long_df$Item %in% items)
    
    table <- reactable(overall,
                       compact = T,
                       highlight = T,
                       striped = T,
                       theme = theme,
                       defaultPageSize = nrow(overall),
                       #pageSizeOptions = c(10,20,50,100,nrow(council)),
                       defaultColDef = colDef(align = 'center', format= colFormat(percent = TRUE, digits = 1)),
                       columns = list(
                         Demo = colDef(show = F),
                         Group = colDef(show = F),
                         Item = colDef(show = F),
                         Question = colDef(show = F),
                         Response = colDef(align = "left", width = 500,html=T),
                         #Percent = colDef(format = colFormat(percent = F)),
                         N = colDef(format = colFormat(percent = F))
                       ),
                       details = function(index){
                         a = index
                         demo_cats <- data.frame(a = demo_list)
                         htmltools::div(style = 'padding: 32px',
                                        reactable(demo_cats,
                                                  compact = T,
                                                  highlight = T,
                                                  striped = T,
                                                  theme = theme,
                                                  outlined = T,
                                                  details = function(index){
                                                    demo_vector <- demo_vector[demo_vector %nin% "Overall"]
                                                    demo_agr_test <- demos[demos$Demo == demo_vector[index] & demos$Item == items[a],]
                                                    htmltools::div(style = 'padding: 32px',
                                                                   reactable(demo_agr_test,
                                                                             compact = T,
                                                                             highlight = T,
                                                                             striped = T,
                                                                             theme = theme,
                                                                             defaultColDef = colDef(align = 'center', format= colFormat(percent = TRUE, digits = 1)),
                                                                             outlined = T,
                                                                             columns = list(
                                                                               Demo = colDef(show = F),
                                                                               Group = colDef(align = 'left', width = 250),
                                                                               Item = colDef(show = F),
                                                                               Question = colDef(show = F, html = T),
                                                                               Response = colDef(align = "left", width = 300, html=T),
                                                                               #Percent = colDef(format = colFormat(percent = F)),
                                                                               N = colDef(format = colFormat(percent = F))
                                                                             )))
                                                  }))
                       })
    return(table)
  }
  
  else if(sata == FALSE){
  
  # overall <- long_df[long_df$Demo %in% "Overall" & long_df$Item %in% items]
  # demos <- long_df[long_df$Demo %nin% "Overall" & long_df$Item %in% items]
  
  overall <- filter(long_df, long_df$Demo %in% "Overall" & long_df$Item %in% items)
  demos <- filter(long_df, long_df$Demo %nin% "Overall" & long_df$Item %in% items)
  
  if(mean_column == F & "Mean" %in% colnames(overall)){
    stop("Data frame currently has a 'Mean' column. Use mean_column = TRUE")
    
  } else if(mean_column == F & "Mean" %nin% colnames(overall)){
    table <- reactable(overall,
                       compact = T,
                       highlight = T,
                       striped = T,
                       theme = theme,
                       defaultPageSize = nrow(overall),
                       #pageSizeOptions = c(10,20,50,100,nrow(council)),
                       defaultColDef = colDef(align = 'center', format= colFormat(percent = TRUE, digits = 1)),
                       columns = list(
                         Demo = colDef(show = F),
                         Group = colDef(show = F),
                         Item = colDef(show = F),
                         Question = colDef(width = 300, align = 'left',html=T),
                         #Mean = colDef(format = colFormat(percent = F)),
                         N = colDef(format = colFormat(percent = F))
                       ),
                       details = function(index){
                         a = index
                         demo_cats <- data.frame(a = demo_list)
                         htmltools::div(style = 'padding: 32px',
                                        reactable(demo_cats,
                                                  compact = T,
                                                  highlight = T,
                                                  striped = T,
                                                  theme = theme,
                                                  outlined = T,
                                                  details = function(index){
                                                    demo_vector <- demo_vector[demo_vector %nin% "Overall"]
                                                    demo_agr_test <- demos[demos$Demo == demo_vector[index] & demos$Item == items[a],]
                                                    htmltools::div(style = 'padding: 32px',
                                                                   reactable(demo_agr_test,
                                                                             compact = T,
                                                                             highlight = T,
                                                                             striped = T,
                                                                             theme = theme,
                                                                             defaultColDef = colDef(align = 'center', format= colFormat(percent = TRUE, digits = 1)),
                                                                             outlined = T,
                                                                             columns = list(
                                                                               Demo = colDef(show = F),
                                                                               Group = colDef(align = 'left', width = 250),
                                                                               Item = colDef(show = F),
                                                                               Question = colDef(show = F,html=T),
                                                                               #Mean = colDef(format = colFormat(percent = F)),
                                                                               N = colDef(format = colFormat(percent = F))
                                                                             )))
                                                  }))
                       })
    return(table)
  } 
  else if(mean_column == F & "Percent" %in% colnames(overall)){
    table <- reactable(overall,
                       compact = T,
                       highlight = T,
                       striped = T,
                       theme = theme,
                       defaultPageSize = nrow(overall),
                       #pageSizeOptions = c(10,20,50,100,nrow(council)),
                       defaultColDef = colDef(align = 'center', format= colFormat(percent = TRUE, digits = 1)),
                       columns = list(
                         Demo = colDef(show = F),
                         Group = colDef(show = F),
                         Item = colDef(show = F),
                         Question = colDef(width = 300, align = 'left',html=T),
                         #Percent = colDef(format = colFormat(percent = F)),
                         N = colDef(format = colFormat(percent = F))
                       ),
                       details = function(index){
                         a = index
                         demo_cats <- data.frame(a = demo_list)
                         htmltools::div(style = 'padding: 32px',
                                        reactable(demo_cats,
                                                  compact = T,
                                                  highlight = T,
                                                  striped = T,
                                                  theme = theme,
                                                  outlined = T,
                                                  details = function(index){
                                                    demo_vector <- demo_vector[demo_vector %nin% "Overall"]
                                                    demo_agr_test <- demos[demos$Demo == demo_vector[index] & demos$Item == items[a],]
                                                    htmltools::div(style = 'padding: 32px',
                                                                   reactable(demo_agr_test,
                                                                             compact = T,
                                                                             highlight = T,
                                                                             striped = T,
                                                                             theme = theme,
                                                                             defaultColDef = colDef(align = 'center', format= colFormat(percent = TRUE, digits = 1)),
                                                                             outlined = T,
                                                                             columns = list(
                                                                               Demo = colDef(show = F),
                                                                               Group = colDef(align = 'left', width = 250),
                                                                               Item = colDef(show = F),
                                                                               Question = colDef(show = F),
                                                                               #Percent = colDef(format = colFormat(percent = F)),
                                                                               N = colDef(format = colFormat(percent = F))
                                                                             )))
                                                  }))
                       })
    return(table)
  }
  else if(mean_column == T & "Mean" %in% colnames(overall)){
    table <- reactable(overall,
                       compact = T,
                       highlight = T,
                       striped = T,
                       theme = theme,
                       defaultPageSize = nrow(overall),
                       #pageSizeOptions = c(10,20,50,100,nrow(council)),
                       defaultColDef = colDef(align = 'center', format= colFormat(percent = TRUE, digits = 1)),
                       columns = list(
                         Demo = colDef(show = F),
                         Group = colDef(show = F),
                         Item = colDef(show = F),
                         Question = colDef(width = 300, align = 'left',html=T),
                         Mean = colDef(format = colFormat(percent = F)),
                         N = colDef(format = colFormat(percent = F))
                       ),
                       details = function(index){
                         a = index
                         demo_cats <- data.frame(a = demo_list)
                         htmltools::div(style = 'padding: 32px',
                                        reactable(demo_cats,
                                                  compact = T,
                                                  highlight = T,
                                                  striped = T,
                                                  theme = theme,
                                                  outlined = T,
                                                  details = function(index){
                                                    demo_vector <- demo_vector[demo_vector %nin% "Overall"]
                                                    demo_agr_test <- demos[demos$Demo == demo_vector[index] & demos$Item == items[a],]
                                                    htmltools::div(style = 'padding: 32px',
                                                                   reactable(demo_agr_test,
                                                                             compact = T,
                                                                             highlight = T,
                                                                             striped = T,
                                                                             theme = theme,
                                                                             defaultColDef = colDef(align = 'center', format= colFormat(percent = TRUE, digits = 1)),
                                                                             outlined = T,
                                                                             columns = list(
                                                                               Demo = colDef(show = F),
                                                                               Group = colDef(align = 'left', width = 250),
                                                                               Item = colDef(show = F),
                                                                               Question = colDef(show = F),
                                                                               Mean = colDef(format = colFormat(percent = F)),
                                                                               N = colDef(format = colFormat(percent = F))
                                                                             )))
                                                  }))
                       })
    return(table)
  } 
  else if(mean_column == T & "Mean" %nin% colnames(overall)){
    stop("Data frame does not have a mean column. Use mean_column = FALSE")
  }
  }
}

