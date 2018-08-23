
pbs_variable %>%
  mutate(drug_duration = floor(qty/2),
         end_date = spply_dt+drug_duration,
         rownum = row_number())%>%
  rename(start = spply_dt) %>%
  rename(end = end_date) %>%
  {.} ->interval_detail

interval_detail %>%
  group_by(pin)%>%
  select( pin, rownum, pbs_code, start, end) %>% 
  nest( rownum, pbs_code, start, end) %>%
  { . } -> nested_pbs_interval

nested_pbs_interval %>%
  inner_join( nested_pbs_interval,by=c("pin") ) %>% 
  { . } -> df_both


df_both %>% group_by(pin)%>%
  do( joined = interval_inner_join( data.frame(.$data.x), 
                                    data.frame(.$data.y),
                                    by=c('start','end'),
                                    minoverlap = 7
  ))  %>%
  ungroup() %>%
  unnest() %>% 
  mutate( start = year( start.x)) %>%
  { . } -> df_intersect


df_intersect %>% 
  filter(rownum.x!=rownum.y) %>%
  filter(rownum.x < rownum.y) %>%
  { .} ->final_output
