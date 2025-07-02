rm(list = ls())
library(dplyr)
library(data.table)
library(readxl)
pacman::p_load(dplyr,data.table,readxl,Hmisc,diffdf,readr)
options(scipen=999)

source("S:/NCOD/NCOD Staff Folders/Tyler Barnes/AES 2021/Delta_All Items_July 2021.R")

data0=as.data.frame(fread("C:/Users/VHAV10BuchaA/Desktop/AES Data Files/aes22to23final.csv"))

colnames(data0)=tolower(colnames(data0))
data0$d4_1[data0$uniqueid %in% 152199 & data0$year %in% 2023]=NA


iteminfo=as.data.frame(read_excel("S:/NCOD/18 - OASC/AES/2023 AES/Instrument/iteminfo23.xlsx", sheet=2))
this.year=2023
last.year=2022

# hierarchy matching function 
ids_list <- function(master_list, hierarchy){
  hierarchy <- hierarchy[`Number Of Employees` %nin% 0]
  
  colnames(hierarchy)[29]="1G"
  test=hierarchy$`10G`
  
  ids_levels=list()
  for(a in 1:length(master_list$groupid)){
    for(i in colnames(hierarchy)[colnames(hierarchy) %in% paste0(12:1,"G")]){
      if(master_list$groupid[a] %in% hierarchy[,get(i)]){
        ids_levels[[a]] = data.table(groupID = master_list$groupid[a], level = i)
      } 
    }
  }
  ids_levels <- rbindlist(ids_levels, use.names = FALSE)
  
  g_cols <- colnames(hierarchy)[colnames(hierarchy) %in% paste0(12:1,"G")]
  hier <- hierarchy[,..g_cols]
  
  # grabbing overall field roll ups ids - VHA, VBA
  field_rollups <- hierarchy[grepl("VISN", `9N`) | grepl("VBA Field Rollup", `10N`)]
  visns_districts <- unique(field_rollups$`9G`)
  admin_rollups <- unique(field_rollups$`10G`)
  ovr_admin <- unique(field_rollups$`11G`)
  
  # grabbing VACO, NCA, and VA ids
  admin <- hierarchy[grepl("Central", `11N`) | grepl("Cemetery", `11N`)]
  admin_ids <- unique(admin$`11G`)
  va <- unique(admin$`12G`)
  
  # combining all ids
  ab_ids <- c(master_list$groupid,visns_districts,admin_rollups,ovr_admin,admin_ids,va)
  
  non_master_list_ids <- c(visns_districts,admin_rollups,ovr_admin,admin_ids,va)
  non_master_list_df <- data.table(groupID = non_master_list_ids, level = NA)
  non_master_list_df$level = ifelse(non_master_list_ids %in% visns_districts, "9G",
                                    ifelse(non_master_list_ids %in% admin_rollups, "10G",
                                           ifelse(non_master_list_ids %in% ovr_admin | non_master_list_ids %in% admin_ids, "11G","12G")))
  
  all_ids <- rbind(ids_levels,non_master_list_df)
  
  # loop that gets all the unique ids for each groupID and puts them in a name list based on groupID
  ids <- list()
  for (i in 1:nrow(all_ids)){
    # i = 1
    
    level <- all_ids$level[i]
    id <- all_ids$groupID[i]
    
    id_col <- which(g_cols %in% level)
    cols <- g_cols[(id_col):length(g_cols)]
    test <- hier[get(level) %in% id, ..cols]
    filtered_ids <- unlist(lapply(test, unique))
    ids[[i]] <- unique(filtered_ids)
  }
  names(ids) <- all_ids$groupID
  return(ids)
}

ids.list=readRDS("S:/NCOD/NCOD Staff Folders/Tyler Barnes/AES 2023/station and up comparisons_workgroupIDs_2023.rds") #
#ids.list = lapply(sites$hid, getIDs) 
#names(ids.list)=sites$orgname

ids.list_old=readRDS("S:/NCOD/NCOD Staff Folders/Tyler Barnes/AES 2023/station and up comparisons_workgroupIDs_2022.rds") #lapply(sites$groupid, getIDs_old) 
#names(ids.list_old)=sites$orgname

nm1 <- intersect(names(ids.list), names(ids.list_old))
ids.list <- ids.list[nm1]
ids.list_old <- ids.list_old[nm1]

data0$Overall_Count = rep(0:1, c(nrow(data0)/2, nrow(data0)/2)) # adding this for the percentage counts items to capture friggin western new york.
#table(data0$Overall_Count)
items<-c("Overall_Count", iteminfo[iteminfo$reported %in% 1, "tag"])
#items<-c("s83", "s84", "s85", "s86", "cmtpct", "bnt_23", "bptw_ind")



# empty strings to NA 
for(i in names(data0)){
  #i = names(data0)[1]
  if(is.character(data0[,i])){
    data0[,i][data0[,i] %in% ""]<-NA
  }
}

# sl gets reported as integer
data0$sl<-round(data0$sl, 0)

# discrimination = yes
#data0$s83<-ifelse(data0$s83 %in% 1, 1, 0)

# round hpw
data0$hpw<-round(data0$hpw, 6)


#data0$bnt_23<-ifelse(data0$bnt_2 %in% 1 | data0$bnt_high %in% 1, 1, 0)

# These items DO NOT need to be recoded. leave them commented out.
# for(i in 1:5){
#   data0[,paste0("m12_20_07_", i)]<-ifelse(data0$m12_20_07 %in% i, 1, 0)
#   data0[,paste0("m12_22_01_", i)]<-ifelse(data0$m12_22_01 %in% i, 1, 0)
# }

# item = all NA, then make it 0. item all [1, NA], make NA = 0 to allow means
for(i in items){
  if(all(data0[data0$year %in% last.year:this.year, i] %in% c(1, NA))){
    data0[data0$year %in% last.year:this.year, i][is.na(data0[data0$year %in% last.year:this.year, i])]<-0
  }
}

# These items DO NOT need to be recoded. leave them commented out.
# make sure binaries can be averaged -> this code didn't run 070723. Not terrible since they are not needed in this run anyway.
# for(i in c(paste0("s78_", 1:10), paste0("s79_", 1:6), paste0("s81_", 1:8))){
#   data0[,i]<-ifelse(data0[,i] %in% 1, 1, 0)
# }

#Age
data0$d2[data0$d2 %in% 1:4]<-"Under 50 *"
data0$d2[data0$d2 %in% 5:6]<-"Over 50"

#Race
# double checking here to make sure my race code matches the race column d30 in the data.
table(data0$d30)

data0$rs<-rowSums(data0[,paste0("d30_", 1:5)], na.rm = TRUE)

data0$race<-NA
data0$race<-ifelse(data0$rs %in% 1 & data0$d30_1 %in% 1, 1, data0$race)
data0$race<-ifelse(data0$rs %in% 1 & data0$d30_2 %in% 1, 2, data0$race)
data0$race<-ifelse(data0$rs %in% 1 & data0$d30_3 %in% 1, 3, data0$race)
data0$race<-ifelse(data0$rs %in% 1 & data0$d30_4 %in% 1, 4, data0$race)
data0$race<-ifelse(data0$rs %in% 1 & data0$d30_5 %in% 1, 5, data0$race)
data0$race<-ifelse(data0$d30_6 %in% 1, 7, data0$race)
data0$race<-ifelse(data0$rs %in% 2:5, 6, data0$race)
#table(data0$race) #just a check here
data0$rs<-NULL

table(data0$race)

data0$race[data0$race==1]="White *"
data0$race[data0$race==2]="Black"
data0$race[data0$race==3]="Native American"
data0$race[data0$race==4]="Asian"
data0$race[data0$race==5]="Pacific Islander"
data0$race[data0$race==6]="Multiracial"
data0$race[data0$race==7]="Prefer not to answer"

#Ethnicity
data0$d3[data0$d3==1]="Hispanic"
data0$d3[data0$d3==2]="Non-Hispanic *"

#Veteran status
data0$d10[data0$d10==1]="Veteran"
data0$d10[data0$d10==2]="Non-Veteran *"

#Disability
data0$d14[data0$d14==1]="Yes"
data0$d14[data0$d14==2]="No *"

#Sexual Orientation
data0$d29[data0$d29==1]="Heterosexual *"
data0$d29[data0$d29==3]="Bisexual"
data0$d29[data0$d29==4]="Not listed"
data0$d29[data0$d29==5]="Gay"
data0$d29[data0$d29==6]="Lesbian"
data0$d29[data0$d29==7]="Queer"
data0$d29[data0$d29==8]="Asexual"
data0$d29[data0$d29==9]="Prefer not to answer"

#Gender
data0$d28[data0$d28==1]="Male *"
data0$d28[data0$d28==2]="Female"
data0$d28[data0$d28==3]="Intersex"
data0$d28[data0$d28==4]="Non-binary"
data0$d28[data0$d28==5]="Not listed"
data0$d28[data0$d28==6]="Prefer not to answer"

#Transgender

data0$d26[data0$d26==1]="Yes"
data0$d26[data0$d26==2]="No *"
data0$d26[data0$d26==3]="Prefer not to answer"


demos=c("d2","race","d10","d3","d14","d29","d28","d26")

#Must have * group first
#overall_list = data.frame("Overall" = "Overall")
gender_list=data.frame("Gender"=c("Male *","Female","Intersex","Non-binary","Not listed","Prefer not to answer"))
age_list=data.frame("Age"=c("Under 50 *","Over 50"))
race_list=data.frame("Race"=c("White *","Black","Native American","Asian","Pacific Islander","Multiracial","Prefer not to answer"))
ethnicity_list=data.frame("Ethnicity"=c("Non-Hispanic *","Hispanic"))
veteran_list=data.frame("Veteran Status"=c("Non-Veteran *","Veteran"))
disability_list=data.frame("Disability"=c("No *","Yes"))
sexual_list=data.frame("Sexual Orientation"=c("Heterosexual *","Bisexual","Not listed","Gay","Lesbian","Queer","Asexual","Prefer not to answer"))
transgender_list=data.frame("Transgender/Diverse"=c("No *","Yes","Prefer not to answer"))


data0=select(data0, year,workgroup, all_of(demos), all_of(items))

possible<-lapply(items, function(x) sort(unique(na.omit(data0[,x]))))
names(possible)=items

final <- list(NULL)
for (i in 1:323){
  final[[i]]=data.table(matrix(nrow = 3616, ncol = 16))
}
#which(names(ids.list) == 1003489)
#unique(dat$workgroup)

system.time({
  #profvis::profvis({
  for(id in 1:length(ids.list)){ #:length(ids.list)
    # id = 57
    dat=filter(data0, workgroup %in% ids.list[[id]], year == this.year)
    dat_old=filter(data0, workgroup %in% ids.list_old[[id]], year == last.year)
    table(dat_old$bnt_0,dat_old$d2)
    # dat = data0[workgroup %in% ids.list[[id]] & year %in% this.year]
    # dat_old = data0[workgroup %in% ids.list[[id]] & year %in% last.year]
    
    #Station
    all=MDELTA(a=as.data.frame(dat[,items]),
               b=as.data.frame(dat[,items]), possible)
    all$Na = all$Nb-all$Na
    # all=as.data.frame(SUMDELTA(ref=dat[,items],
    #            focal=dat[,items]))
    
    all_change=MDELTA(a=as.data.frame(dat_old[,items]),
                      b=as.data.frame(dat[,items]), possible)
    # all_change=as.data.frame(SUMDELTA(ref=as.data.frame(dat_old[,items]),
    #                   focal=as.data.frame(dat[,items]),
    #                   nest = FALSE))
    
    #all_change[,`:=` (label = "All", variable = "All")]
    all_change$label="All"
    all_change$variable="All"
    
    all.out=merge(all,all_change, "item")
    
    #Gender
    gender.out=list(NULL)
    for(cc in 1:length(gender_list[,1])){
      #cc = 1
      cc_name=gender_list[cc,1]
      if(cc_name == "Male *"){
        maj_to_combo = MDELTA(a = as.data.frame(dat[dat$d28 != cc_name,items]),
                              b = as.data.frame(dat[dat$d28 == cc_name, items]), possible)
        
        change = MDELTA(a = as.data.frame(dat_old[dat_old$d28 == cc_name, items]),
                              b = as.data.frame(dat[dat$d28 == cc_name, items]), possible)
        
        #combo_change[,`:=` (label = cc_name, variable = colnames(gender_list))]
        change$label=cc_name
        change$variable=colnames(gender_list)
        
        gender.out[[cc]] <- merge(maj_to_combo, change, "item")
        
      } else {
        current=MDELTA(a=as.data.frame(dat[dat$d28==gender_list[1,1],items]),
                       b=as.data.frame(dat[dat$d28==cc_name,items]), possible)
        
        change=MDELTA(a=as.data.frame(dat_old[dat_old$d28==cc_name,items]),
                      b=as.data.frame(dat[dat$d28==cc_name,items]), possible)
        
        #change[,`:=` (label = cc_name, variable = colnames(gender_list))]
        change$label=cc_name
        change$variable=colnames(gender_list)
        
        gender.out[[cc]]=merge(current, change, "item")
        
      }
      
      # delta comparison of majority to combo of all minority here
      
    }
    
    #gender.out=do.call(rbind, gender.out)
    gender.out=rbindlist(gender.out, use.names = TRUE)
    
    #Age
    age.out=list(NULL)
    for(cc in 1:length(age_list[,1])){
      cc_name=age_list[cc,1]
      
      if(cc_name == "Under 50 *"){
        maj_to_combo = MDELTA(a = as.data.frame(dat[dat$d2!=cc_name,items]),
                              b = as.data.frame(dat[dat$d2 == cc_name, items]), possible)
        
        change = MDELTA(a = as.data.frame(dat_old[dat_old$d2 == cc_name, items]),
                        b = as.data.frame(dat[dat$d2 == cc_name, items]), possible)
        
        #combo_change[,`:=` (label = cc_name, variable = colnames(race_list))]
        change$label=cc_name
        change$variable=colnames(age_list)
        
        age.out[[cc]] <- merge(maj_to_combo, change, "item")
        
      } else{
        current=MDELTA(a=as.data.frame(dat[dat$d2==age_list[1,1],items]),
                       b=as.data.frame(dat[dat$d2==cc_name,items]), possible)
        
        change=MDELTA(a=as.data.frame(dat_old[dat_old$d2==cc_name,items]),
                      b=as.data.frame(dat[dat$d2==cc_name,items]), possible)
        
        #change[,`:=` (label = cc_name, variable = colnames(age_list))]
        change$label=cc_name
        change$variable=colnames(age_list)
        
        age.out[[cc]]=merge(current, change, "item")
      }
      
    }
    
    #age.out=do.call(rbind, age.out)
    age.out=rbindlist(age.out, use.names = TRUE)
    
    #Race
    race.out=list(NULL)
    for(cc in 1:length(race_list[,1])){
      #cc = 7
      cc_name=race_list[cc,1]
      if(cc_name == "White *"){
        maj_to_combo = MDELTA(a = as.data.frame(dat[dat$race!=cc_name,items]),
                              b = as.data.frame(dat[dat$race == cc_name, items]), possible)
        
        change = MDELTA(a = as.data.frame(dat_old[dat_old$race == cc_name, items]),
                              b = as.data.frame(dat[dat$race == cc_name, items]), possible)
        
        #combo_change[,`:=` (label = cc_name, variable = colnames(race_list))]
        change$label=cc_name
        change$variable=colnames(race_list)
        
        race.out[[cc]] <- merge(maj_to_combo, change, "item")
        
      } else {
        current=MDELTA(a=as.data.frame(dat[dat$race==race_list[1,1],items]),
                       b=as.data.frame(dat[dat$race==cc_name,items]), possible)
        
        change=MDELTA(a=as.data.frame(dat_old[dat_old$race==cc_name,items]),
                      b=as.data.frame(dat[dat$race==cc_name,items]), possible)
        
        #change[,`:=` (label = cc_name, variable = colnames(race_list))]
        change$label=cc_name
        change$variable=colnames(race_list)
        
        race.out[[cc]]=merge(current, change, "item")
      }
      
    }
    
    #race.out=do.call(rbind, race.out)
    race.out=rbindlist(race.out, use.names = TRUE)
    
    #Ethnicity
    ethnicity.out=list(NULL)
    for(cc in 1:length(ethnicity_list[,1])){
      cc_name=ethnicity_list[cc,1]
      
      if(cc_name == "Non-Hispanic *"){
        maj_to_combo=MDELTA(a=as.data.frame(dat[dat$d3!=cc_name,items]),
                            b=as.data.frame(dat[dat$d3==cc_name,items]), possible)
        
        change=MDELTA(a=as.data.frame(dat_old[dat_old$d3==cc_name,items]),
                      b=as.data.frame(dat[dat$d3==cc_name,items]), possible)
        
        #change[,`:=` (label = cc_name, variable = colnames(disability_list))]
        #address(change)
        change$label=cc_name
        change$variable=colnames(ethnicity_list)
        
        ethnicity.out[[cc]]=merge(maj_to_combo, change, "item")
        
      } else{
        current=MDELTA(a=as.data.frame(dat[dat$d3==ethnicity_list[1,1],items]), #ethnicity_list[1,1]
                       b=as.data.frame(dat[dat$d3==cc_name,items]), possible)
        
        change=MDELTA(a=as.data.frame(dat_old[dat_old$d3==cc_name,items]),
                      b=as.data.frame(dat[dat$d3==cc_name,items]), possible)
        
        #change[,`:=` (label = cc_name, variable = colnames(ethnicity_list))]
        change$label=cc_name
        change$variable=colnames(ethnicity_list)
        
        ethnicity.out[[cc]]=merge(current, change, "item")
      }
      
    }
    
    #ethnicity.out=do.call(rbind, ethnicity.out)
    ethnicity.out=rbindlist(ethnicity.out, use.names = TRUE)
    
    #Veteran status
    veteran.out=list(NULL)
    for(cc in 1:length(veteran_list[,1])){
      #cc = 1
      cc_name=veteran_list[cc,1]
      
      if(cc_name == 'Non-Veteran *'){
        maj_to_combo=MDELTA(a=as.data.frame(dat[dat$d10!=cc_name,items]),
                            b=as.data.frame(dat[dat$d10==cc_name,items]), possible)
        
        change=MDELTA(a=as.data.frame(dat_old[dat_old$d10==cc_name,items]),
                      b=as.data.frame(dat[dat$d10==cc_name,items]), possible)
        
        #change[,`:=` (label = cc_name, variable = colnames(disability_list))]
        #address(change)
        change$label=cc_name
        change$variable=colnames(veteran_list)
        
        veteran.out[[cc]]=merge(maj_to_combo, change, "item")
        
      } else {
        current=MDELTA(a=as.data.frame(dat[dat$d10==veteran_list[1,1],items]),
                     b=as.data.frame(dat[dat$d10==cc_name,items]), possible)
      
        change=MDELTA(a=as.data.frame(dat_old[dat_old$d10==cc_name,items]),
                      b=as.data.frame(dat[dat$d10==cc_name,items]), possible)
        
        #change[,`:=` (label = cc_name, variable = colnames(veteran_list))]
        change$label=cc_name
        change$variable=colnames(veteran_list)
        
        veteran.out[[cc]]=merge(current, change, "item")
      }
      
    }
    
    #veteran.out=do.call(rbind, veteran.out)
    veteran.out=rbindlist(veteran.out, use.names = TRUE)
    
    #Disability
    
    disability.out=list(NULL)
    for(cc in 1:length(disability_list[,1])){
      #cc = 1
      cc_name=disability_list[cc,1]
      
      if(cc_name == "No *"){
      maj_to_combo=MDELTA(a=as.data.frame(dat[dat$d14!=cc_name,items]),
                     b=as.data.frame(dat[dat$d14==cc_name,items]), possible)
      
      change=MDELTA(a=as.data.frame(dat_old[dat_old$d14==cc_name,items]),
                    b=as.data.frame(dat[dat$d14==cc_name,items]), possible)
      
      #change[,`:=` (label = cc_name, variable = colnames(disability_list))]
      #address(change)
      change$label=cc_name
      change$variable=colnames(disability_list)
      
      disability.out[[cc]]=merge(maj_to_combo, change, "item")
      
    } else{
      current=MDELTA(a=as.data.frame(dat[dat$d14==disability_list[1,1],items]),
                          b=as.data.frame(dat[dat$d14==cc_name,items]), possible)
      
      change=MDELTA(a=as.data.frame(dat_old[dat_old$d14==cc_name,items]),
                    b=as.data.frame(dat[dat$d14==cc_name,items]), possible)
      
      change$label=cc_name
      change$variable=colnames(disability_list)
      
      disability.out[[cc]]=merge(current, change, "item")
    }
    }
    disability.out=rbindlist(disability.out, use.names = TRUE)
    
    #disability.out=do.call(rbind, disability.out)

    #Sexual Orientation - Use this as a template for the other Majority to combo comparisons.  Need to double check with ryan to make sure that's what he wants.  
    sexual.out=list(NULL)
    for(cc in 1:length(sexual_list[,1])){
      #cc = 1
      cc_name=sexual_list[cc,1]
      if(cc_name == "Heterosexual *"){
        maj_to_combo = MDELTA(a = as.data.frame(dat[dat$d29!=cc_name,items]),
                              b = as.data.frame(dat[dat$d29 == sexual_list[1,1], items]), possible)

        change=MDELTA(a=as.data.frame(dat_old[dat_old$d29==cc_name,items]),
                      b=as.data.frame(dat[dat$d29==cc_name,items]), possible)

        #combo_change[,`:=` (label = cc_name, variable = colnames(sexual_list))]
        change$label=cc_name
        change$variable=colnames(sexual_list)

        sexual.out[[cc]] <- merge(maj_to_combo, change, "item")

      #}
      
      
      # if(cc_name == "Non-Hetero"){
      #   maj_to_combo = MDELTA(a = as.data.frame(dat[dat$d29==sexual_list[1,1],items]),
      #                         b = as.data.frame(dat[dat$SO_combo == cc_name, items]), possible)
      #   
      #   combo_change = MDELTA(a = as.data.frame(dat_old[dat_old$SO_combo == cc_name, items]),
      #                         b = as.data.frame(dat[dat$SO_combo == cc_name, items]), possible)
      #   
      #   #combo_change[,`:=` (label = cc_name, variable = colnames(sexual_list))]
      #   combo_change$label=cc_name
      #   combo_change$variable=colnames(sexual_list)
      #   
      #   sexual.out[[cc]] <- merge(maj_to_combo, combo_change, "item")
        
      } else {
        current=MDELTA(a=as.data.frame(dat[dat$d29==sexual_list[1,1],items]),
                       b=as.data.frame(dat[dat$d29==cc_name,items]), possible)
        
        change=MDELTA(a=as.data.frame(dat_old[dat_old$d29==cc_name,items]),
                      b=as.data.frame(dat[dat$d29==cc_name,items]), possible)
        
        #change[,`:=` (label = cc_name, variable = colnames(sexual_list))]
        
        change$label=cc_name
        change$variable=colnames(sexual_list)
        
        sexual.out[[cc]]=merge(current, change, "item")
      }
      
      
    }
    #sexual.out=do.call(rbind, sexual.out)
    sexual.out=rbindlist(sexual.out, use.names = TRUE)
    
    #Transgender
    transgender.out=list(NULL)
    for(cc in 1:length(transgender_list[,1])){
      # cc = 1
      cc_name=transgender_list[cc,1]
      
      if(cc_name == "No *"){
        maj_to_combo = MDELTA(a = as.data.frame(dat[dat$d26!=cc_name,items]),
                              b = as.data.frame(dat[dat$d26 == cc_name, items]), possible)
        
        change=MDELTA(a=as.data.frame(dat_old[dat_old$d26==cc_name,items]),
                      b=as.data.frame(dat[dat$d26==cc_name,items]), possible)
        
        #change[,`:=` (label = cc_name, variable = colnames(transgender_list))]
        change$label=cc_name
        change$variable=colnames(transgender_list)
        
        transgender.out[[cc]] <- merge(maj_to_combo, change, "item")
      } else {
        current=MDELTA(a=as.data.frame(dat[dat$d26==transgender_list[1,1],items]),
                       b=as.data.frame(dat[dat$d26==cc_name,items]), possible)
        
        change=MDELTA(a=as.data.frame(dat_old[dat_old$d26==cc_name,items]),
                      b=as.data.frame(dat[dat$d26==cc_name,items]), possible)
        
        #change[,`:=` (label = cc_name, variable = colnames(transgender_list))]
        change$label=cc_name
        change$variable=colnames(transgender_list)
        
        transgender.out[[cc]]=merge(current, change, "item")
      }
      
    }
    #transgender.out=do.call(rbind, transgender.out)
    transgender.out=rbindlist(transgender.out, use.names = TRUE)
    
    
    out=rbind(all.out,gender.out,age.out,race.out,ethnicity.out,veteran.out,disability.out,sexual.out,transgender.out)
    #out=rbindlist(list(all.out,gender.out,age.out,race.out,ethnicity.out,veteran.out,disability.out,sexual.out,transgender.out), use.names = T)
    #address(out)
    #out[,`:=` (a.sort = sites$groupid[id])]
    out$a.sort=names(ids.list)[id]
    final[[id]]=out
  }
  #})
})

out=do.call(rbind,final)

out=setDT(out)

#saveRDS(out, "C:/Users/VHAV10BuchaA/Documents/AES Data pulls/2023/Delta Run/tyler_example.rds")
# test_aaron <- ODI_aaron[102,]
# test_aaron$flag.change = NULL
# test_aaron[, `:=` (flag.change = ifelse(p.change <= .05 & delta.change >= .04, 1,
#                                  ifelse(p.change <= .05 & delta.change <= -.04, -1,555)))]
# 
#test[item %in% reversed, `:=` (flag = flag*-1, flag.change = flag.change*-1)][, `:=` (flag = ifelse(flag == -555,flag*-1,flag), flag.change = ifelse(flag.change == -555, flag.change * -1, flag.change))]
# 

out$amean.x[out$Na.x<5]=NA
out$bmean.x[out$Nb.x<5]=NA
out$amean.y[out$Na.y<5]=NA
out$bmean.y[out$Nb.y<5]=NA

#out$Na.x[out$Na.x<5]=NA
out$Nb.x[out$Nb.x<5]=NA
out$Na.y[out$Na.y<5]=NA
out$Nb.y[out$Nb.y<5]=NA

out$changescore=out$bmean.y-out$amean.y
out[,c("delta.x",'sig.x',"bmean.x","amean.x","delta.y","sig.y","bmean.y","amean.y","changescore")] <- lapply(out[,c("delta.x",'sig.x',"bmean.x","amean.x","delta.y","sig.y","bmean.y","amean.y","changescore")], function(x) round(x,6))

# creating the flags
out[, `:=` (flag.x = ifelse(sig.x <= .05 & delta.x >= .04, 1,
                            ifelse(sig.x <= .05 & delta.x <= -.04, -1,555)))]
out[, `:=` (flag.y = ifelse(sig.y <= .05 & delta.y >= .04, 1,
                            ifelse(sig.y <= .05 & delta.y <= -.04, -1,555)))]

reversed=iteminfo$tag[iteminfo$favorability %in% "low" & iteminfo$reported %in% 1]

out[item %in% reversed, `:=` (flag.x = flag.x*-1, flag.y = flag.y*-1)][, `:=` (flag.x = ifelse(flag.x == -555,flag.x*-1,flag.x), flag.y =  ifelse(flag.y == -555, flag.y * -1, flag.y))]

# rearranging and renaming columns to match ryan's nomenclature
out <- select(out, a.sort, variable, label, item, delta.x, sig.x, Nb.x, Na.x, bmean.x, amean.x, delta.y, sig.y, Nb.y, Na.y, bmean.y, amean.y, flag.x, flag.y, changescore)
names(out) <- c("a.sort","variable","label","item","delta","p","nfoc","nref","meanfoc","meanref","delta.change","p.change","nfoc.change","nref.change","meanfoc.change","meanref.change","flag","flag.change","changescore")

# current focal, < 5
out[out$nfoc %in% c(NA, 0:4), c("delta", "p", "meanfoc", "nfoc", "flag", "delta.change", "p.change",
                                "meanfoc.change", "nfoc.change", "meanref.change", "nref.change", "flag.change")]<-NA

# current ref < 5 = no current comparison flag
out[out$nref %in% c(NA, 0:4), c("delta", "p", "meanref", "flag")]<-NA

# change ref < 5 = no change and no current flag
out[out$nref.change %in% c(NA, 0:4), c("delta.change", "p.change", "meanfoc.change", "nref.change", "flag.change", "meanref.change")]<-NA

saveRDS(out, "C:/Users/VHAV10BuchaA/Documents/AES Data pulls/2023/Delta Run/ODI run_long.rds")


# will need to ask tyler about this part - not sure why it's included 
# out2=select(out, a.sort, variable, label, item, bmean.x,Nb.x, flag.x,flag.y, changescore)
# colnames(out2)=c( "a.sort","variable","label","item","meanfoc","nfoc","flag","flag.change","changescore")
# 
# out2$variable[out2$variable=="Sexual.Orientation"]="Sexual Orientation"
# out2$variable[out2$variable=="Transgender.Diverse"]="Transgender/Diverse"
# out2$variable[out2$variable=="Veteran.Status"]="Veteran Status"
# 
# saveRDS(out2,"S:/NCOD/NCOD Staff Folders/Tyler Barnes/AES 2022/ODI run_long.rds")
# 
# out2=select(out2, -nfoc)
# wide=list(NULL)
# for(cc in 1:length(unique(out2$item))){
#   
#   single=filter(out2, item %in% unique(out2$item)[cc])
#   
#   single=select(single, -item)
#   colnames(single)[4:7]=paste0(colnames(single)[4:7], ".", unique(out2$item)[cc])
#   wide[[cc]]=single
# }
# 
# final_out=wide[[1]]  
# 
# for(cc in 2:length(wide)){
#   final_out=merge(final_out, wide[[cc]], c("a.sort","variable","label"))
# }

# unique(final_out$variable)


# 295*31+295 number of rows 9440

#saveRDS(final_out,"S:/NCOD/NCOD Staff Folders/Tyler Barnes/AES 2022/ODI run.rds")
#tyler <- readRDS("S:/NCOD/NCOD Staff Folders/Tyler Barnes/AES 2022/ODI run_long.rds")

rm(list = ls())

# Getting western new york in
ODI_aaron=readRDS("C:/Users/VHAV10BuchaA/Documents/AES Data pulls/2023/Delta Run/ODI run_long.rds")

wny_counts <- ODI_aaron[variable == "All" & item %in% "Overall_Count"]

ODI_aaron <- ODI_aaron[item %nin% "Overall_Count"]

# ryan=as.data.frame(fread("S:/NCOD/NCOD Staff Folders/Ryan Derickson/Delta/2022/odi run for db download.csv"))
#ryan_long=as.data.frame(fread("S:/NCOD/NCOD Staff Folders/Ryan Derickson/Delta/2022/odi run for db download long.csv"))

# uncomment these if ryan makes a new file
# ryan_long=as.data.frame(fread("S:/NCOD/NCOD Staff Folders/Ryan Derickson/Delta/2023/odi run long.csv"))
# fwrite(ryan_long, "C:/Users/VHAV10BuchaA/Documents/AES Data pulls/2023/Delta Run/ryan_long_odi_run.csv")

#ODI_ryan_long <- fread("C:/Users/VHAV10BuchaA/Documents/AES Data pulls/2023/Delta Run/ryan_long_odi_run.csv") # old file

ODI_ryan_long <- fread("C:/Users/VHAV10BuchaA/Documents/AES Data pulls/2023/Delta Run/ryan_odi run long2.csv")
#unique(out$a.sort)[unique(out$a.sort) %nin% unique(ryan_long$g1)]

# tyler=readRDS("S:/NCOD/NCOD Staff Folders/Tyler Barnes/AES 2022/ODI run.rds")
#tyler2=readRDS("S:/NCOD/NCOD Staff Folders/Tyler Barnes/AES 2022/ODI run_long.rds")
#ODI_aaron=readRDS("C:/Users/VHAV10BuchaA/Documents/AES Data pulls/2023/Delta Run/ODI run_long.rds")
ODI_aaron[ODI_aaron == "Native American"] = "Native American/Alaskan"
ODI_aaron[ODI_aaron == "Pacific Islander"] = "Hawaiian/Pacific Islander"
ODI_aaron[ODI_aaron == "Sexual.Orientation"] = "Sexual Orientation"
ODI_aaron[ODI_aaron == "Transgender.Diverse"] = "Transgender"
ODI_aaron[ODI_aaron == "Veteran.Status"] = "Veteran"
ODI_aaron$label[ODI_aaron$variable == "Sexual Orientation" & ODI_aaron$label == "Not listed"] = "Not listed above"

ODI_aaron <- ODI_aaron[order(a.sort,item,variable,label)]

ODI_aaron[variable == "All", `:=` (nref = NA)]

ODI_ryan_long=rename(ODI_ryan_long,"a.sort"="g1")

#ODI_ryan_long=select(ryan_long, colnames(tyler2))

# ODI_aaron_1 <- filter(aaron, variable == 'Race')
# ODI_ryan_long1 <- filter(ryan_long, variable == 'Race')
# 
# length(unique(ryan_long1$g1))
# length(unique(aaron_1$a.sort))

ODI_ryan_long <- ODI_ryan_long[order(a.sort,item,variable,label)]

ODI_ryan_long[demos == "dummy", `:=` (nref = NA)]

# Fix for Western New York Counts (2022) ####
# finding the mismatch, plugging in the correct nref.changes. 
mismatch <- ODI_aaron[ODI_aaron$nref.change != ODI_ryan_long$nref.change]
mismatch_ids <- unique(mismatch$a.sort)

# getting a data frame of the total nrow counts needed
wny_counts <- wny_counts[a.sort %in% mismatch_ids]

# this loop creates a list, each element being a row where nref.change has been replaced with the appropriate number
fix <- list()
for (i in 1:235){
  #i = 1
  fix[[i]] <- ODI_aaron[ODI_aaron$nref.change != ODI_ryan_long$nref.change][i][, `:=` (nref.change = ifelse(a.sort %in% wny_counts$a.sort, wny_counts$nref.change[a.sort == wny_counts$a.sort], nref.change))]
}
fix <- rbindlist(fix, use.names = FALSE)

# this line is replaced ODI_aaron$nref.change with the nref.change from fix since they are in the same order. 
ODI_aaron[ODI_aaron$nref.change != ODI_ryan_long$nref.change, `:=` (nref.change = fix$nref.change)]

saveRDS(ODI_aaron, "C:/Users/VHAV10BuchaA/Documents/AES Data pulls/2023/Delta Run/ODI run_long_fixed.rds") # final, saved and sorted data frame

# test <- ODI_aaron[ODI_aaron$nref.change != ODI_ryan_long$nref.change][1]
# test[, `:=` (nref.change = ifelse(a.sort %in% wny_counts$a.sort, wny_counts$nref.change[a.sort == wny_counts$a.sort], nref.change))]


#ODI_aaron[ODI_aaron$nref.change != ODI_ryan_long$nref.change][i, ]
# sum(data0$stano == 656 & data0$year == 2023)
# test = filter(data0, stano == 656 & year == 2023)
# table(test$d2)
#ODI_aaron$delta[17] == ODI_ryan_long$delta[17]

rm(list = ls())

# checking final data frames ####
#ab <- readRDS("C:/Users/VHAV10BuchaA/Documents/AES Data pulls/2023/Delta Run/ODI run_long.rds")
ab_fixed <- readRDS("C:/Users/VHAV10BuchaA/Documents/AES Data pulls/2023/Delta Run/ODI run_long_fixed.rds")
rd <- fread("C:/Users/VHAV10BuchaA/Documents/AES Data pulls/2023/Delta Run/ryan_odi run long2.csv")  

rd=rename(rd,"a.sort"="g1")
rd <- rd[order(a.sort,item,variable,label)]
rd[demos == "dummy", `:=` (nref = NA)]

diffdf(ab_fixed,
       rd,
       tolerance = .00001,
       strict_numeric = FALSE,
       file = "C:/Users/VHAV10BuchaA/Documents/AES Data pulls/2023/Delta Run/ab rd dei long file check_08312023_rerun.txt")

# diffdf(ODI_aaron,
#        ODI_ryan_long,
#        tolerance = .00001,
#        strict_numeric = FALSE,
#        file = "C:/Users/VHAV10BuchaA/Documents/AES Data pulls/2023/Delta Run/ab rd dei long file check_08312023_rerun.txt")
# tb =me, rd=Ryan
tb=tyler2
rd=ryan_long
rd$nfoc[rd$nfoc<5]=99999 # Has values of < 5 here
tb[is.na(tb)]=99999
rd[is.na(rd)]=99999

#used for rounding- items2 will be rounded to 2 and items4 is rounded to 4
iteminfo=as.data.frame(read_excel("S:/NCOD/18 - OASC/AES/2022 AES/Instrument/iteminfo22.xlsx", sheet=2))
items2<-iteminfo[iteminfo$reported %in% 1 & iteminfo$scale %in% c("1-5", "0-6", "0-100"), "tag"]
items4<-iteminfo[iteminfo$reported %in% 1 & iteminfo$scale %in% c("0-1","1-2"), "tag"]

#rounding
tb$meanfoc[tb$item %in% items2]=round(tb$meanfoc[tb$item %in% items2],2)
tb$changescore[tb$item %in% items2]=round(tb$changescore[tb$item %in% items2],2)

tb$meanfoc[tb$item %in% items4]=round(tb$meanfoc[tb$item %in% items4],4)
tb$changescore[tb$item %in% items4]=round(tb$changescore[tb$item %in% items4],4)

#nfoc on Ryans is all the same by id, variable and label... Took one of the % scores
nfoc_tb=filter(tb, item=="bnt_0")
nfoc_tb=select(nfoc_tb,"a.sort","variable","label","nfoc")
tb=merge(tb, nfoc_tb, c("a.sort","variable","label"))

tb=select(tb, -nfoc.x)
tb=rename(tb, "nfoc"="nfoc.y")
tb=select(tb, colnames(rd))



out=merge(tb,rd, c("a.sort","variable","label","item"))

test=out$meanfoc.x-out$meanfoc.y
min(test)
max(test)

test=out$changescore.x-out$changescore.y
min(test)
max(test)

test=out$nfoc.x-out$nfoc.y
min(test)
max(test)

test=out$flag.x-out$flag.y
min(test)
max(test)

test=out$flag.change.x-out$flag.change.y
min(test)
max(test)

## testing ####
