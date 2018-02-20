#
#
library("RSQLite")
library(stringr)
library(rJava)
library(xlsx)
library(lubridate)
library(png)
#
wd_folder<-getwd()
output_folder<-paste0(wd_folder,"/Outputs/")
input_folder<-paste0(wd_folder,"/Inputs/")
#
db_files<-list.files(path=input_folder, pattern = "*.db")
#
nmbr_of_files <- length(db_files)
#
start_date_ok <- FALSE
while (!start_date_ok){
  start_time <- readline(prompt="Start Time (dd.mm.yyyy hh:mm): ")   # Read start time from console 
  base_time<-as.POSIXct(start_time,format="%d.%m.%Y %H:%M")
  if (!is.na(base_time)) {start_date_ok<-TRUE}
  else {
    cat("Incorrect time format, please enter start time as dd.mm.yyyy hh:mm","\n")
    }
  }
base_time_numeric <- as.numeric(base_time)
#
time_window_in_hours_ok <- FALSE
while (!time_window_in_hours_ok){  
  time_window_in_hours <- readline(prompt="Time window in hours (1-24 hours): ")   # Read time window from console 
  if ((as.numeric(time_window_in_hours)>0)&&(as.numeric(time_window_in_hours)<169)) {time_window_in_hours_ok<-TRUE}
  else { 
    cat("Incorrect time window, please enter value in range 1-24","\n") 
    }
  }
#
end_time<-base_time + hours(as.numeric(time_window_in_hours))
end_time_numeric <- as.numeric(end_time)
#
wb = createWorkbook()
sheet = createSheet(wb, "Results, Graphs")
sheet_flwr = createSheet(wb, "Results, Flowers")
xls_start_row <- 1
#
# Title and sub title styles
TITLE_STYLE <- (CellStyle(wb)+ Font(wb,heightInPoints=16,color="blue",isBold=TRUE))
#SUB_TITLE_STYLE <- (CellStyle(wb) + Font(wb, heightInPoints=14,isItalic=TRUE, isBold=FALSE))

# Styles for the data table row/column names
TABLE_ROWNAMES_STYLE <- ((CellStyle(wb)+Font(wb, isBold=TRUE)))
TABLE_COLNAMES_STYLE <- (CellStyle(wb) + Font(wb, isBold=TRUE)+Alignment(wrapText=TRUE,horizontal="ALIGN_CENTER") +
                           Border(color="black", position=c("BOTTOM"), pen=c("BORDER_THIN")))
#
current_time<-as.POSIXct(Sys.time())
cur_year<-year(current_time)
cur_month<-month(current_time)
cur_day<-day(current_time)
cur_hour<-hour(current_time)
cur_min<-minute(current_time)
cur_sec<-trunc(second(current_time))
#
cat("\n")
#
for (file_cntr in 1:nmbr_of_files){  
  log_name<-db_files[file_cntr]
  db_name<-paste0(input_folder,log_name)
  # 
  con = dbConnect(RSQLite::SQLite(), dbname=db_name)
  #
  MM_db_all = dbGetQuery( con,'select * from datalog' )
  #
  wb_for_one = createWorkbook()
  sheet_for_one = createSheet(wb_for_one, "Results, Graphs")
  sheet_for_one_flwr = createSheet(wb_for_one, "Results, Flowers")
  TITLE_STYLE_for_one <- (CellStyle(wb_for_one)+ Font(wb_for_one,heightInPoints=16,color="blue",isBold=TRUE))
  TABLE_ROWNAMES_STYLE_for_one <- ((CellStyle(wb_for_one)+Font(wb_for_one, isBold=TRUE)))
  TABLE_COLNAMES_STYLE_for_one <- (CellStyle(wb_for_one) + Font(wb_for_one, isBold=TRUE)+Alignment(wrapText=TRUE,horizontal="ALIGN_CENTER") +
                             Border(color="black", position=c("BOTTOM"), pen=c("BORDER_THIN")))
  xls_for_one_start_row <- 1
  #
  # Skip values before given start time
  #
  keep_searching<-TRUE
  data_available_for_analysis<-FALSE
  sample_start_index <-0
  offset_time<-0
  while ((keep_searching)&&(sample_start_index<nrow(MM_db_all))) {
    sample_start_index<-sample_start_index+1
    if (base_time_numeric<=MM_db_all[sample_start_index,1]) {
      keep_searching<-FALSE
      data_available_for_analysis<-TRUE
    }
  }
  # 
  if(!keep_searching){
    keep_searching<-TRUE
    sample_end_index <- sample_start_index-1
    while (keep_searching) {
      sample_end_index<-sample_end_index+1
      if (sample_end_index<=nrow(MM_db_all)){
        if (end_time_numeric<=MM_db_all[sample_end_index,1]) {
          keep_searching<-FALSE
          }
        }else{
          keep_searching<-FALSE
        }
      }
    }
  #
  #
  cat(log_name, "\n")
  #
    if (!data_available_for_analysis){cat("No data available for given time window","\n")}
  #
  #
  #---------------------------------------------------------------------------------------------
  #
  if (data_available_for_analysis){
  #
  #
  MM_db<-MM_db_all[sample_start_index:sample_end_index,]
  MM_table <- matrix(nrow = nrow(MM_db),ncol = 9)
  MM_tbl_dnames<-c("Time", "Time String", "MM number",	"SCV (% of SCL)",	"SCR freq. (SCR/min)", "SCL (uS)",	"Step count",	"awake? (1/0)", "Raw MM")
  dimnames(MM_table)[[2]] <- MM_tbl_dnames
  sample_start_index <-1
  #
  MM_temp_tbl <- matrix(nrow = nrow(MM_db),ncol = 3)
  MM_tmp_tbl_dnames<-c("Conv", "Max cut",	"First ave")
  dimnames(MM_temp_tbl)[[2]] <- MM_tmp_tbl_dnames
  #
  for (i in 1:nrow(MM_temp_tbl)) {  
    MM_temp_tbl[i,1]<-200*as.numeric(MM_db[i,3])/as.numeric(MM_db[i,6])
    MM_temp_tbl[i,2]<-MM_temp_tbl[i,1]
    if (!is.nan(MM_temp_tbl[i,2])) {
      if (MM_temp_tbl[i,2]>100) {MM_temp_tbl[i,2]<-100}
      }
    if (is.nan(MM_temp_tbl[i,1])){MM_temp_tbl[i,1]<-NA}
    if (is.nan(MM_temp_tbl[i,2])){MM_temp_tbl[i,2]<-NA}
    }  
  #
  for (i in 1:nrow(MM_temp_tbl)) {
    j<-i-5
    if (j<1) {j<-1}
    mm_ave<-0
    mm_cntr<-0
    k<-min((i+5),nrow(MM_temp_tbl))
    for (sample_cntr in j:k) {
      if(abs(MM_db[sample_cntr,1]-MM_db[i,1])<8*60){
        mm_ave<-mm_ave+MM_temp_tbl[sample_cntr,2]
        mm_cntr<-mm_cntr+1
        }
    }
    MM_temp_tbl[i,3]<-mm_ave/mm_cntr
  }
  #
  MM_table[,2]<-as.character(MM_table[,2])  
  #
  for (i in 1:nrow(MM_table)) {  
    MM_table[i,1]<-MM_db[i,1]
    MM_table[i,2] <- as.character(as.POSIXct(MM_db[i,1],origin = "1970-01-01 0:00:00"))
    MM_table[i,4]<-0.0244140625*as.numeric(MM_db[i,3])
    MM_table[i,5]<-MM_db[i,2]
    if (MM_table[i,5]>30) {MM_table[i,5]<-30}  
    MM_table[i,6]<-15.625
    if (MM_db[i,4]>=1){MM_table[i,6]<-as.numeric(MM_table[i,6])/as.numeric(MM_db[i,4])}
    MM_table[i,7]<-MM_db[i,5]
    #MM_table[i,7]<-MM_db....
    MM_table[i,9]<-MM_db[i,3]
    }
  #
  for (i in 1:nrow(MM_table)) {
    j<-i-5
    if (j<1) {j<-1}
    mm_ave<-0
    mm_cntr<-0
    k<-min((i+5),nrow(MM_table))
    for (sample_cntr in j:k) {
      if(abs(MM_db[sample_cntr,1]-MM_db[i,1])<8*60){
        mm_ave<-mm_ave+MM_temp_tbl[sample_cntr,3]
        mm_cntr<-mm_cntr+1
      }
    }
    MM_table[i,3]<-mm_ave/mm_cntr
  }
  #
  #----------------------------------------------------------------------------
  #
  mlen <- length(MM_table[,1]) 
  m1 <- as.POSIXlt(as.numeric(MM_table[sample_start_index,1]),origin = "1970-01-01 0:00:00")  # first sample time
  mx <- as.POSIXlt(as.numeric(MM_table[mlen,1]),origin = "1970-01-01 0:00:00")  # last sample time
  mz <- end_time
  mx <- min(mx,mz)

  mlen_min <- as.numeric(difftime(mx,m1,units = "min")) + 1 
  mlen_min <- trunc(mlen_min)    # number of 1 min samples
  
  MM_table_dimnanes<-dimnames(MM_table)[[2]]
  
  MM_table_final <- matrix(data = 0, nrow = mlen_min, ncol = ncol(MM_table), dimnames = list(c(1:mlen_min),
                                                                       MM_table_dimnanes), byrow=TRUE)   
  
  for (i in 1:ncol(MM_table)) {MM_table_final[1,i]<-MM_table[sample_start_index,i]}
 
  timecount <- trunc(m1,"mins") - hours(offset_time) 
  MM_table_final[1,2] <- strftime(timecount,"%Y-%m-%d %H:%M")  # time stamp for first sample 
  #
  ii <- 2
  jj <- 2+sample_start_index-1
  while (ii <= mlen_min) {
    mnext <- as.POSIXlt(as.numeric(MM_table[jj,1]), origin = "1970-01-01 0:00:00") - hours(offset_time)
    mdiff <- trunc(as.numeric(difftime(mnext,timecount,units = "secs")))
    while (mdiff > 60 && ii <= mlen_min) {
      timecount <- timecount + minutes(1)
      MM_table_final[ii,1] <- as.numeric(MM_table_final[(ii-1),1])+60
      MM_table_final[ii,2] <- strftime(timecount,"%Y-%m-%d %H:%M") 
      MM_table_final[ii,3:9] <- 0 
      mdiff <- (mdiff - 60) 
      ii <- (ii + 1) 
    } 
    if (mdiff > 35) {
      timecount <- timecount + minutes(1)
    }
    else {
      ii <- (ii - 1)     # override fill value
    }
    if (ii <= mlen_min) {
      MM_table_final[ii,1] <- MM_table[jj,1]
      MM_table_final[ii,2] <- strftime(timecount,"%Y-%m-%d %H:%M")
      MM_table_final[ii,3:9] <- MM_table[jj,3:9]
      }
    ii <- (ii + 1)
    jj <- (jj + 1)
    
  }
  #
  #=======================================================================
  #
  # Draw curve
  #
  #
  keep_searching<-TRUE
  print_start_index <-0
  while (keep_searching) {
    print_start_index<-print_start_index+1
    if (base_time_numeric<=MM_table_final[print_start_index,1]) {
      keep_searching<-FALSE
    }
  }
  #  
  keep_searching<-TRUE
  print_end_index <-0
  while (keep_searching&(print_end_index<nrow(MM_table_final))) {
    print_end_index<-print_end_index+1
    if (end_time_numeric<=MM_table_final[print_end_index,1]) {
      keep_searching<-FALSE
    }  
  }
  #
  MM_values <- MM_table_final[,3]
  #
  if ((MM_values[1]>0.0)&(MM_values[2]==0.0)&(MM_values[3]>0.0)){
    MM_table_final[2,3]<-(as.numeric(MM_table_final[1,3])+as.numeric(MM_table_final[3,3]))/2
  } 
  #
  for (i in (print_start_index+2):(print_end_index-2)) {
    if ((MM_values[(i-2)]==0.0)&(MM_values[(i-1)]==0.0)&(MM_values[i]>0.0)&(MM_values[(i+1)]==0.0)&(MM_values[(i+2)]==0.0)){
      MM_table_final[i,3]<-0.0
      } 
    if ((MM_values[(i-1)]>0.0)&(MM_values[i]==0.0)&(MM_values[(i+1)]>0.0)){
       MM_table_final[i,3]<-(as.numeric(MM_table_final[(i-1),3])+as.numeric(MM_table_final[(i+1),3]))/2
     } 
  }
  #
  if ((MM_values[(print_end_index-2)]>0.0)&(MM_values[(print_end_index-1)]==0.0)&(MM_values[(print_end_index)]>0.0)){
    MM_table_final[print_end_index-1,3]<-(as.numeric(MM_table_final[(print_end_index-2),3])+as.numeric(MM_table_final[(print_end_index),3]))/2
  } 
  #
  #-----------------------------------------------------------------------------------------------------
  #
  mm_colours_limits_for_trend <-c(100.0,86.67,73.33,60.0,46.67,33.33)
  mm_colours_limits <-c(100.0,90.0,80.0,70.0,60.0,50.0)
  mm_colours <- c("#F1471D", "#824D90", "#008F67", "#4BB494","#98C399","#E4D599","#008F67")
  #
  start_offset<-trunc((as.numeric(MM_table_final[print_start_index,1])-base_time_numeric)/60)
  end_offset<-trunc((end_time_numeric-as.numeric(MM_table_final[print_end_index,1]))/60)
  
  ttt<-as.POSIXlt(MM_table_final[print_start_index:print_end_index,2],origin = "1970-01-01 0:00:00")
  
  if (start_offset>0){
    tt<-vector(length = start_offset)
    start_fill_time <- base_time_numeric
    for (tt_i in 1:start_offset){
      tt[tt_i]<-start_fill_time
      start_fill_time<-start_fill_time+60}
    tt<-as.POSIXlt(tt,origin = "1970-01-01 0:00:00")
    ttt<-c(tt,ttt)
  }
  
  if (end_offset>0){
    tttt<-vector(length = end_offset)
    end_fill_time <- end_time_numeric
    for (tttt_i in end_offset:1){
      tttt[tttt_i]<-end_fill_time
      end_fill_time<-end_fill_time-60}
    tttt<-as.POSIXlt(tttt,origin = "1970-01-01 0:00:00")
    ttt<-c(ttt,tttt)
  }

  mmxrange<-range(ttt)
  mmyrange<-c(0,100)
  #
  Na_indxs<-is.na(as.numeric(MM_table_final[,3]))
  MM_table_final[Na_indxs,3]<-0.0
  #
  dev.new(width=1200, height=800)
  par(bg = "white")
  sub_log_name<-str_sub(log_name, start=1, end=-4)
  main_title <- paste(sub_log_name,"  Samples: ",(nrow(MM_db)-8)," min                              ")
  plot(mmxrange,mmyrange,type="n",main = main_title,xlab="Time",
       ylab="MM")
  #
  x_val<-c(ttt[1],ttt,ttt[length(ttt)])
  #
  y_val_fill_s<-matrix(nrow = start_offset+1,ncol = 6)
  y_val_fill_e<-matrix(nrow = end_offset+1,ncol = 6)
  y_val<-matrix(nrow = (print_end_index-print_start_index+1),ncol = 6)
  y_val<-rbind(y_val_fill_s,y_val,y_val_fill_e)
  y_val[,]<-0.0
  #
  y_val[(start_offset+2):(nrow(y_val)-end_offset-1),1]<-MM_table_final[print_start_index:print_end_index,3]
  #
  for (i in 2:(nrow(y_val)-1)) {
    for(ii in 2:6){
      y_val[i,ii]<-mm_colours_limits_for_trend[ii]
      if (as.numeric(y_val[i,1])<=mm_colours_limits_for_trend[ii]){
        y_val[i,ii]<-y_val[i,1]
        }
    }
  }
  #
  for(i in 1:6){polygon(x_val,y_val[,i],col=mm_colours[i],border = NA)}
  lines(ttt,y_val[2:(nrow(y_val)-1),1],type="l",col=mm_colours[7],lwd=2)   
  #
  plot_base_name<-str_sub(log_name, start=1, end=-4) 
  plot_name<-paste0(output_folder,plot_base_name,".png") 
  dev.copy(png,plot_name,width=1200, height=800) 
  dev.off() 
  #
  addPicture(plot_name, sheet, scale = 1, startRow = xls_start_row,
             startColumn = 1)
  addPicture(plot_name, sheet_for_one, scale = 1, startRow = 1,
             startColumn = 1)
  res<-file.remove(plot_name)
  #
  # Add the data frames
  #
  data_sheet_name<-paste0(plot_base_name," Data") 
  sheet_for_values = createSheet(wb, data_sheet_name)
  sheet_for_one_for_values = createSheet(wb_for_one, "Data")
  #
  addDataFrame(MM_table_final[print_start_index:print_end_index,], sheet=sheet_for_values, row.names=TRUE, startRow=1,
               colnamesStyle = TABLE_COLNAMES_STYLE, rownamesStyle = TABLE_ROWNAMES_STYLE)
  #
  addDataFrame(MM_table_final[print_start_index:print_end_index,], sheet=sheet_for_one_for_values, row.names=TRUE, startRow=1,
               colnamesStyle = TABLE_COLNAMES_STYLE_for_one, rownamesStyle = TABLE_ROWNAMES_STYLE_for_one)
  #
  #=======================================================================
  #
  # Draw flower
  #
  start_with_day_flower<-FALSE
  start_with_nigth_flower<-FALSE
  #
  base_time_vector<-as.POSIXlt(base_time_numeric,origin = "1970-01-01 0:00:00")
  end_time_vector<-as.POSIXlt(end_time_numeric,origin = "1970-01-01 0:00:00")
  base_time_vector[[2]]<-0
  end_time_vector[[2]]<-0
  #
  if ((base_time_vector[[3]]>=6)&&(base_time_vector[[3]]<=18)){
    start_with_day_flower<-TRUE
    base_time_vector[[3]]<-6
    } else {
      start_with_nigth_flower<-TRUE
      if(base_time_vector[[3]]<6){
        base_time_vector<-base_time_vector-hours(24)
        base_time_vector[[3]]<-18
        }else{ 
          base_time_vector[[3]]<-18
      }
    }
  #
  if ((end_time_vector[[3]]>=6)&&(end_time_vector[[3]]<=18)){
    end_time_vector[[3]]<-18
  } else {
    if(end_time_vector[[3]]<6){
      end_time_vector[[3]]<-6
    }else{ 
      end_time_vector<-end_time_vector+hours(24)
      end_time_vector[[3]]<-6
    }
  }
  #
  #start_flwr<- as.numeric(as.POSIXct("18.1.2017 6:00",format="%d.%m.%Y %H:%M"))
  #end_flwr<- as.numeric(as.POSIXct("18.1.2017 18:00",format="%d.%m.%Y %H:%M"))
  #
  start_time_flwr<-as.numeric(base_time_vector)
  end_time_flwr<-as.numeric(end_time_vector)
  #
  flower_nmbr<-(end_time_flwr-start_time_flwr)/3600/12
  #
  xls_for_one_start_column<-1
  mm_flwr_index <-0
  keep_searching<-TRUE
  while (keep_searching) {
    mm_flwr_index<-mm_flwr_index+1
    if (start_time_flwr<=MM_table_final[mm_flwr_index,1]) {
      keep_searching<-FALSE
    }
  }
  #
  #----------------------------------------------------------------------------->
  #
  for (flwr_cnt in 1:flower_nmbr){
  #
    start_flwr<-start_time_flwr+(flwr_cnt-1)*12*3600
    end_flwr<-start_flwr+12*3600
    
  flwr_val <-matrix(nrow=(60*12),ncol = 13)
  curr_t<-as.numeric(start_flwr)
  #
  for (i in 1:(60*12)) { 
    if ((curr_t<MM_table_final[mm_flwr_index,1])||(curr_t>MM_table_final[print_end_index,1])) {
      flwr_val[i,1]<- 0.0
    }else{
      flwr_val[i,1]<-MM_table_final[mm_flwr_index,3]
      mm_flwr_index<-mm_flwr_index+1
    }
    flwr_val[i,2]<- cos(((1-((curr_t-start_flwr)/(end_flwr-start_flwr)))*2*pi)-0.5*pi)*(as.numeric(flwr_val[i,1])*75/100+25)/100  #x value
    flwr_val[i,3]<- sin(((1-((curr_t-start_flwr)/(end_flwr-start_flwr)))*2*pi)-0.5*pi)*(as.numeric(flwr_val[i,1])*75/100+25)/100  #y value
    if (flwr_val[i,1]==0.0){
      flwr_val[i,(2)]<-0.0
      flwr_val[i,(3)]<-0.0
      }
    #
    for(ii in 2:6) {
      flwr_val[i,(2*ii)]<- cos(((1-((curr_t-start_flwr)/(end_flwr-start_flwr)))*2*pi)-0.5*pi)*(mm_colours_limits[ii]/100)  #x value 
      flwr_val[i,(2*ii+1)]<- sin(((1-((curr_t-start_flwr)/(end_flwr-start_flwr)))*2*pi)-0.5*pi)*(mm_colours_limits[ii]/100)  #y value
      if((as.numeric(flwr_val[i,1])*75/100+25)<=mm_colours_limits[ii]){ 
        flwr_val[i,(2*ii)]<- cos(((1-((curr_t-start_flwr)/(end_flwr-start_flwr)))*2*pi)-0.5*pi)*(as.numeric(flwr_val[i,1])*75/100+25)/100  #x value 
        flwr_val[i,(2*ii+1)]<- sin(((1-((curr_t-start_flwr)/(end_flwr-start_flwr)))*2*pi)-0.5*pi)*(as.numeric(flwr_val[i,1])*75/100+25)/100  #y value
        if (flwr_val[i,1]==0.0){
          flwr_val[i,(2*ii)]<-0.0
          flwr_val[i,(2*ii+1)]<-0.0
          }
        }
    }
   #
    curr_t<-curr_t+60
  }  
  #
  mmxflwrrange<-c(-1.055,1.055)
  mmyflwrrange<-c(-1.055,1.055)
  #
  dev.new(width=760, height=800)
  par(bg = "white",col.axis = NA, xaxt= "n", yaxt= "n", bty="n")
  plot(mmxflwrrange,mmyflwrrange,type="n",main = main_title, xlab=NA,
       ylab=NA) 
  #
  for(i in 1:6){polygon(flwr_val[,(2*i)],flwr_val[,(2*i+1)],col=mm_colours[i],border = NA)}
  polygon(flwr_val[,2],flwr_val[,3],col=NA, border=mm_colours[7], lwd=1) 
  #
  if(start_with_day_flower){background_image <- readPNG("Flower_Background_Day_B5.png")}
  else{background_image <- readPNG("Flower_Background_Night_B3.png")}
  #
  lim <- par()
  rasterImage(background_image, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
  #
  plot_name_flwr<-paste0(output_folder,plot_base_name,"flower.png") 
  dev.copy(png,plot_name_flwr,width=760, height=800) 
  dev.off() 
  #
  addPicture(plot_name_flwr, sheet_flwr, scale = 1, startRow = xls_start_row,
             startColumn = xls_for_one_start_column)
  #
  addPicture(plot_name_flwr, sheet_for_one_flwr, scale = 1, startRow = xls_for_one_start_row,
             startColumn = xls_for_one_start_column)
  #
  xls_for_one_start_column<-xls_for_one_start_column+12
  #
  res<-file.remove(plot_name_flwr)
  #
  start_flwr<-start_flwr+12*3600
  #
  start_with_day_flower<-!start_with_day_flower
  }
  #<-------------------------------------------------------------------------------------------------------------------
  #
  }
  #
  xls_start_row<-xls_start_row+42
  #
  xls_for_one_name<-paste0(output_folder,plot_base_name,"_Results_",cur_year,cur_month,cur_day,cur_hour,cur_min,cur_sec,".xlsx")
  saveWorkbook(wb_for_one, xls_for_one_name)
  #
  } 
#
cat("\n")
#
#---------------------------
#
xls_name<-paste0(output_folder,"Combined_Results_",cur_year,cur_month,cur_day,cur_hour,cur_min,cur_sec,".xlsx")
saveWorkbook(wb, xls_name)
#
##################################################################################################
#
