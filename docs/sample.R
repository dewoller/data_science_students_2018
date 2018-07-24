#!/usr/bin/env Rscript

# utilities
library("wrapr" )
library("magrittr")

# string utiliities
library("stringr")

# date utilities
library("lubridate")

# display utilities
library("knitr")
library("kableExtra")
library("pander")
library(workflowr)

# map functions
#library("tmap", 'https://cloud.r-project.org')
#library("tmaptools", 'https://cloud.r-project.org')

# the most important library!! omnibus
library("tidyverse")


# get data from database function
library("RPostgreSQL")
library("keyring")

# -------------------------------------------------
get_continuing_df <- function( 
                              base_table="continuing_rr", 
                              include_barb=FALSE
                              ) {

  type_code_limit = ifelse( include_barb, 11, 10 )
  query  <-  paste0( "
                    SELECT pin, gender, age, state, lga, scheme, item_code, type_code, type_name, supply_date, quantity, unit_wt, ddd_mg_factor, days_multiplier from continuing."
                    , base_table
                    , " r JOIN continuing.item i USING (item_code) 
                    JOIN public.generictype USING (type_code)
                    where (type_code < ", type_code_limit, "  
                           AND EXTRACT( YEAR FROM supply_date ) != '2017'
                           AND state in ('NSW', 'VIC')
                           AND (lga like '1%' OR lga like '2%'))"
                    )

  my_db_get_query( query ) %>%
    as.tibble() %>%
    mutate( n_dose = (unit_wt * quantity / ddd_mg_factor ),
           agen=ifelse( age=='100+', 101, as.numeric( age )),
           age = cut( agen, 
                     c(0,19,44,64,9999), 
                     labels=qw("0-19 20-44 45-64 65+")
                     )
           ) %>%
    rename(sex=gender) 
}

# -------------------------------------------------
generate_data_frames = function( ) {

  if (TRUE) {
    df <- get_continuing_df('continuing') ; multiplier = 1
  } else {
    df <- get_continuing_df('continuing_rr') ; multiplier = 8459157/9480
  }

  age_groups = structure(1:4, .Label = c("0-19", 
                                         "20-44", 
                                         "45-64", 
                                         "65+"), 
                         class = "factor")

  # 
  # select out patients
  #
  #

  df %>% 
    distinct (pin, sex, age, state, lga) %>% 
    mutate( sex = as.factor(sex)) %>%
    inner_join( df_patient_scheme, by="pin") %>%
    {.} -> df_patient

  #
  #
  df %<>% 
    mutate( quarter = quarter(supply_date, with_year = TRUE), 
           supply_year = as.factor(year(supply_date))
           )


  # doses by year, type and patient
  # for calculation of DDD

  df%>%  
    group_by(pin,  supply_year) %>%
    summarise(
              n_dose = sum(n_dose),
              quantity = sum(quantity),
              n_script = n()
              ) %>%
    ungroup() %>%
    {.} -> df_patient_dose

  #
  df%>%
    group_by(pin) %>%
    summarise( 
              n_quarter = n_distinct( quarter ),
              usage_category= cut( n_quarter, 
                                  c(-1, 1,7,13, 999999), 
                                  labels = qw("one-off short-term long-term regular"),
                                  ordered_result=TRUE
                                  ) 
              ) %>%
    {.} -> df_patient_usage

  list( "df_patient_usage" = df_patient_usage, 
       "df_patient" = df_patient,
       "df_patient_scheme" = df_patient_scheme,
       "df_patient_dose" = df_patient_dose,
       "age_groups" = age_groups,
       "multiplier" = multiplier, 
       "df"=df
       )
}




keep <- function(x, name) {assign(as.character(substitute(name)), x, pos = 1)}
qw <- function(x) unlist(strsplit(x, "[[:space:]]+"))


destring <- function(x,keep="0-9.-") {
  return( as.numeric(gsub(paste("[^",keep,"]+",sep=""),"",x)) )
}

# my.year.length  ------------------------------------------------------------------
#
my_year_length <- function( year ) {
	sapply( year, function(year) { year.length(as.character(year )) })
}


# seqNext ------------------------------------------------------------------
seqNext <- function(x1, y1) {
  dat <- data.frame( x= x1
  			 , y=y1) 
  unname(
    predict(lm(y ~ x, data=dat), newdata=list(x=c(2016)))
  )
}

# bothDiff  ------------------------------------------------------------------
bothDiff <- function ( set1, set2 ) {
	print(setdiff( set1, set2 ))
	print(setdiff(set2, set1))
}

# -------------------------------------------------
my_db_get_query <- function ( query ) {


  # loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = "mofi",
          host = "localhost", port = 5432,
          user = "dewoller", password = key_get('mofi', 'dewoller'))
  on.exit(dbDisconnect(con))
  dbGetQuery( con, query )

}
