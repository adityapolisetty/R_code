myPaths <- .libPaths()
myPaths <- c(myPaths, "C:/Users/apoliset/Desktop/Dropbox/My_files/r_libs")
# myPaths <- c(myPaths, "C:\\Users\\adpol\\Dropbox\\My_files\\r_libs")
# myPaths <- c(myPaths, "D:\\Dropbox\\Dropbox\\My_files\\r_libs")

.libPaths(myPaths)  # add new path

library(nomisr)
library(dplyr)
library(ggplot2)
library(data.table)

keyword_list <- c("*earning*","*employment*","*business*","*workforce*")

data_list= list()
for (i in keyword_list) {

  a <- nomis_search(name = i )
  all_data <- a %>% select("id","name.value")
  data_list[[i]] <- all_data

}

all_data = do.call(rbind, data_list)

# 3, 11, 5, 6, 7, 8, 12, 16
# 9, 15 - NULL

for (row in 45:nrow(all_data)) {
  
  var = all_data[row, "id"]
  var_name = all_data[row, "name.value"]
  print(var)
  tryCatch({
    
  data <- nomis_get_data(id = var ,time = "2000-01-2022-01", geography = "TYPE464")
  fwrite(data, paste("X:\\ephemeral\\",var, ".csv"))
  
  }, error=function(e){})
}

