nesting_function <- function(var_name,data_set) {

  funv <- get(var_name)

  if(data_set=="rr"){
  file_name <- paste("output/RR/RROutput_",var_name,".csv",sep = '')
  }else{
    file_name <- paste("output/BIG/Output_",var_name,".csv",sep = '')
  }

  if (dim(funv)==0){
    message(var_name," doesnot have any data in it")
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
    { . } -> funcv_df_both


  funcv_df_both %>% group_by(pin)%>%
    do( joined = interval_inner_join( data.frame(.$data.x),
                                      data.frame(.$data.y),
                                      by=c('start','end'),
                                      minoverlap = 7
    ))  %>%
    ungroup() %>%
    unnest() %>%
    mutate( start = year( start.x)) %>%
    { . } -> funcv_df_intersect


  funcv_df_intersect %>%
    filter(rownum.x!=rownum.y) %>%
    filter(rownum.x < rownum.y) %>%
    { .} ->func_final_output

  #hello <- "rahulmaharjan"

  #data <- c(1,2,3)

  if (file.exists(file_name)==TRUE){
    file.remove(file_name)
  }else{
    write.csv(func_final_output,file = file_name)
  }
  }

}



