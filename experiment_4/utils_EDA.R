calc_diff <- function(train){
  
  train %>% distinct(app) -> train_app
  train %>% distinct(os) -> train_os
  train %>% distinct(channel) -> train_channel

  list(app=train_app, os=train_os, channel=train_channel)

}


test_diff <- function(test, train_distincts ){
  #test %>% distinct(ip) %>% nrow()-> test_ip
  test %>% distinct(app)  -> test_app
  test %>% distinct(os)  -> test_os
  test %>% distinct(channel)  -> test_channel
  
  #test_ip %>% anti_join(train_ip, by="ip") -> test_ip_not
  test_app %>% anti_join(train_distincts$app, by="app") -> test_app_not
  test_channel %>% anti_join(train_distincts$channel, by="channel") -> test_channel_not
  test_os %>% anti_join(train_distincts$os, by="os") -> test_os_not
  
  print( nrow(test_app_not) )
  print( nrow(test_os_not) )
  print( nrow(test_channel_not) )
  
  print( nrow(test_app_not) / nrow(test_app) )
  print( nrow(test_os_not) / nrow(test_os) )
  print( nrow(test_channel_not) / nrow(test_channel) )
}

calculateDistinct <- function(df){
  df %>% group_by(ip, day) %>% 
    summarize(n=n(),a=sum(is_attributed))   
}