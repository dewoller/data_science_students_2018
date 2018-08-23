source('lib/pre_function.R')

source('lib/function.R')

#source('lib/nesting_task.R')

my_db_get_query("Select pin,pbs_code,spply_dt,pbs_rgltn24_adjst_qty qty from pbs_rr where pin like '00%' ")%>%
  as.tibble()%>%
  { . } -> rr_pbs_interval_00

my_db_get_query("Select pin,pbs_code,spply_dt,pbs_rgltn24_adjst_qty qty from pbs_rr where pin like '01%' ")%>%
  as.tibble()%>%
  { . } -> rr_pbs_interval_01

my_db_get_query("Select pin,pbs_code,spply_dt,pbs_rgltn24_adjst_qty qty from pbs_rr where pin like '02%' ")%>%
  as.tibble()%>%
  { . } -> rr_pbs_interval_02

my_db_get_query("Select pin,pbs_code,spply_dt,pbs_rgltn24_adjst_qty qty from pbs_rr where pin like '03%' ")%>%
  as.tibble()%>%
  { . } -> rr_pbs_interval_03

my_db_get_query("Select pin,pbs_code,spply_dt,pbs_rgltn24_adjst_qty qty from pbs_rr where pin like '04%' ")%>%
  as.tibble()%>%
  { . } -> rr_pbs_interval_04

my_db_get_query("Select pin,pbs_code,spply_dt,pbs_rgltn24_adjst_qty qty from pbs_rr where pin like '05%' ")%>%
  as.tibble()%>%
  { . } -> rr_pbs_interval_05

my_db_get_query("Select pin,pbs_code,spply_dt,pbs_rgltn24_adjst_qty qty from pbs_rr where pin like '06%' ")%>%
  as.tibble()%>%
  { . } -> rr_pbs_interval_06

my_db_get_query("Select pin,pbs_code,spply_dt,pbs_rgltn24_adjst_qty qty from pbs_rr where pin like '07%' ")%>%
  as.tibble()%>%
  { . } -> rr_pbs_interval_07

my_db_get_query("Select pin,pbs_code,spply_dt,pbs_rgltn24_adjst_qty qty from pbs_rr where pin like '08%' ")%>%
  as.tibble()%>%
  { . } -> rr_pbs_interval_08

my_db_get_query("Select pin,pbs_code,spply_dt,pbs_rgltn24_adjst_qty qty from pbs_rr where pin like '09%' ")%>%
  as.tibble()%>%
  { . } -> rr_pbs_interval_09

my_db_get_query("Select pin,pbs_code,spply_dt,pbs_rgltn24_adjst_qty qty from pbs_rr where pin like '1%' ")%>%
  as.tibble()%>%
  { . } -> rr_pbs_interval_1


rr_pin_var <- data.frame(ls(pattern = "rr_pbs_*"))

ncluster = 11
cluster <- create_cluster(ncluster)


colnames(rr_pin_var) <- "pin"

#head(rr_pin_var)
start_time = Sys.time()

for (pin in rr_pin_var[,]){
  #nesting_function(pin,"rr")
  funv <- get(pin)

  #if(data_set=="rr"){
    file_name <- paste("output/RR/RROutput_",pin,".csv",sep = '')
  #}else{
  #  file_name <- paste("output/BIG/Output_",var_name,".csv",sep = '')
  #}

  if (dim(funv)==0){
    message(pin, " doesnot have any data in it")
  }else{
    funv %>%
      mutate(drug_duration = floor(qty/2),
             end_date = spply_dt+drug_duration,
             rownum = row_number())%>%
      rename(start = spply_dt) %>%
      rename(end = end_date) %>%
      {.} ->funv_interval_detail

    funv_interval_detail %>%
      group_by(pin)%>%
      select( pin, rownum, pbs_code, start, end) %>%
      nest( rownum, pbs_code, start, end) %>%
      { . } -> nested_funv_interval

    nested_funv_interval %>%
      inner_join( nested_funv_interval,by=c("pin") ) %>%
#      partition( pin , cluster=cluster)  %>%
      { . } -> funcv_df_both

   group <- rep(1:ncluster, length.out = nrow(funcv_df_both)) 
   funcv_df_both = funcv_df_both %>% 
     bind_cols( tibble(group) )  %>%
          partition( group , cluster=cluster)  

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
