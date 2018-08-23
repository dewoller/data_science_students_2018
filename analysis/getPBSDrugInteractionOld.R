source('lib/pre_function.R')

source('lib/function.R')

source('lib/nesting_task.R')

all_pin_var <- data.frame(ls(pattern = "bigpbs_*"))

colnames(all_pin_var) <- "pin"

View(all_pin_var)

cluster <- create_cluster(4)

for (pin in all_pin_var[,]){
  nesting_function(pin,"Big")
}
