source('lib/pre_function.R')

source('lib/function.R')

source('lib/nesting_task.R')

rr_pin_var <- data.frame(ls(pattern = "rr_pbs_*"))

colnames(rr_pin_var) <- "pin"

#head(rr_pin_var)

cluster <- create_cluster(4)

for (pin in rr_pin_var[,]){
  nesting_function(pin,"rr")
}
