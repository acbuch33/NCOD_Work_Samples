library(qualtRics)
library(shiny)
library(reactable)
library(knitr)
library(shinybusy)
library(shinyjs)
library(dplyr)
library(data.table)
library(tidyr)
library(shinyvalidate)
library(plotly)
library(tibble)
library(stringr)
library(fontawesome)
library(purrr)
library(rmarkdown)
library(tools)
library(shinyalert)
library(shinydashboard)
library(emayili)
# remove.packages("cluster")
# packageurl <- "C:/Users/VHACINBarneT/Downloads/curl_5.2.1.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")
# packageurl <- "C:/Users/VHACINBarneT/Downloads/MASS_7.3-58.2.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")
# packageurl <- "C:/Users/VHACINBarneT/Downloads/Matrix_1.5-4.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")
# packageurl <- "C:/Users/VHACINBarneT/Downloads/nlme_3.1-162.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")
# packageurl <- "C:/Users/VHACINBarneT/Downloads/mgcv_1.8-42.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")



# packageurl <- "C:/Users/VHACINBarneT/Downloads/archive_1.1.3.tar.gz"
# 
# install.packages(packageurl, repos=NULL, type="source")



consultants=read.csv("qualtrics keys.csv")
months_df=data.frame(number=c("01","02","03","04","05","06","07","08","09","10","11","12"),
                     name=c("January","February","March","April","May","June","July","August",
                            "September","October","November","December"))

convert_date=function(x,months_df_arg=months_df){
  split_data=unlist(str_split(x,"-"))
  year=split_data[1]
  month=months_df$name[months_df$number %in% split_data[2]]
  day=split_data[3]
  day=str_remove(day, "^0+")
  return(list(year=year, month=month, day=day))
}

# start=convert_date("2023-12-01")
# end=convert_date("2024-08-02")

format_admin=function(start,end){
  test_same_year=start$year==end$year
  test_same_month=start$month==end$month
  #Not possible to be same month different year
  if(!test_same_year){
    out=paste0(start$month," ", start$day,", ", start$year, " - ", end$month," ", end$day,", ", end$year)
  } else if(test_same_year & !test_same_month){
    out=paste0(start$month," ", start$day," - ", end$month," ", end$day,", ", end$year)
  }else if(test_same_year & test_same_month){
    out=paste0(start$month," ", start$day," - ",end$day,", ", end$year)
  }
  return(out)
}

source("Character Cleaning2.R")



smtp <- server(
  host = "smtp.va.gov",
  port = 25)

IDGen <- function(n = 5000) {
  a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
}


# install.packages("curl")
