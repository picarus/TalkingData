
myfilename <- "test.csv"
rdsFile <- paste0(myfilename,".RDS")
if ( file.exists(rdsFile) ) {
  load(rdsFile, envir=.GlobalEnv)
} else {
  test<- read_csv(myfilename, progress=T)
  save(test, file = rdsFile)
} 


# myfilename <- "test_supplement.csv"
# rdsFile <- paste0(myfilename,".RDS")
# if ( file.exists(rdsFile) ) {
#   load(rdsFile, envir=.GlobalEnv)
# } else {
#   test_sup<- read_csv(myfilename, progress=T)
#   save(test_sup, file = rdsFile)
# } 
# 
# 
# myfilename <- "train.csv"
# rdsFile <- paste0(myfilename,".RDS")
# if ( file.exists(rdsFile) ) {
#   load(rdsFile, envir=.GlobalEnv)
# } else {
#   train<- read_csv(myfilename, progress=T)
#   train %<>% addTimeVars()
#   save(train, file = rdsFile)
# }






# train %<>% mutate(tr=T)
# load("../test_supplement.csv.RDS")
# 
# test_sup %<>% addTimeVars()
# test_sup %>% mutate(tr=F) %>% select(-click_id) -> test_sup
# 
# all <- bind_rows(train, test_sup)
# #rm(train, test_sup)
# 
save(all, file="all.RDS")
  

  