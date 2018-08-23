source('lib/pre_function.R')

source('lib/function.R')

#source('lib/nesting_task.R')

bigpbs_interval_00 <- read.csv("data/BigChunks/pbs_interval_00.csv")

pin_var <- data.frame(ls(pattern = "bigpbs_*"))

ncluster = 11
cluster <- create_cluster(ncluster)


colnames(pin_var) <- "pin"

#head(rr_pin_var)
start_time = Sys.time()

for (pin in pin_var[,]){
  #nesting_function(pin,"rr")
  funv <- get(pin)

  #if(data_set=="rr"){
    #file_name <- paste("output/RR/RROutput_",pin,".csv",sep = '')
  #}else{
    file_name <- paste("output/BIG/Output_",pin,".csv",sep = '')
  #}

  if (dim(funv)==0){
    message(pin, " doesnot have any data in it")
  }else{

	  print('1')
    funv %>%
      mutate(drug_duration = floor(qty/2),
             spply_dt=as.Date(spply_dt),
             pbs_code=as.character(pbs_code),
             end_date = spply_dt+drug_duration,
             rownum = row_number())%>%
      rename(start = spply_dt) %>%
      rename(end = end_date)%>%
#      partition( pin , cluster=cluster) %>%
      {.} ->funv_interval_detail
	print('2')

    funv_interval_detail %>%
      group_by(pin)%>%
      select( pin, rownum, pbs_code, start, end) %>%
      nest( rownum, pbs_code, start, end) %>%
      partition(pin,cluster=cluster) %>%
      { . } -> nested_funv_interval
	print('3')

    nested_funv_interval %>%
      inner_join( nested_funv_interval,by=c("pin") ) %>%
      #partition( pin , cluster=cluster)  %>%
      { . } -> funcv_df_both
	print('4')

#   group <- rep(1:ncluster, length.out = nrow(funcv_df_both))
#   funcv_df_both = funcv_df_both %>%
#     bind_cols( tibble(group) )  %>%
#          partition( group , cluster=cluster)

   funcv_df_both %>% cluster_library(c('tidyverse','fuzzyjoin', "IRanges" ))


    funcv_df_both %>% group_by(pin)%>%
      do( joined = interval_inner_join( data.frame(.$data.x),
                                        data.frame(.$data.y),
                                        by=c('start','end'),
                                        minoverlap = 7
      ))  %>%
      collect() %>%
      ungroup() %>%
      unnest() %>%
      mutate( start = year( start.x)) %>%
      filter(rownum.x!=rownum.y) %>%
      filter(rownum.x < rownum.y) %>%
      { .} ->func_final_output

    #hello <- "rahulmaharjan"

    #data <- c(1,2,3)

    if (file.exists(file_name)==TRUE){
      file.remove(file_name)
    }
    write.csv(func_final_output,file = file_name)
  }
}


end_time = Sys.time()
print( end_time-start_time)
