myfilename <- "test_supplement.csv"
rdsFile <- paste0(myfilename,".RDS")
if ( file.exists(rdsFile) ) {
  load(rdsFile, envir=.GlobalEnv)
} else {
  test_sup <- read_csv(myfilename, progress=T)
  save(test_sup, file = rdsFile)
} 