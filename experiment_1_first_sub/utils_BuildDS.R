
zero_set <- function(pt){
  pt %<>% left_join(tr_app, by="app")
  pt %<>% left_join(tr_channel, by="channel") 
  pt %<>% left_join(tr_os, by="os") 
  pt %<>% left_join(tr_device, by="device") 
  pt %<>% left_join(tr_channel_device_os, by=c("device","channel","os")) 
  pt %<>% left_join(tr_device_app, by=c("device","app")) 
  pt %<>% left_join(tr_device_os, by=c("device","os")) 
  pt
}


first_set <- function(pt){
  
  pt %<>% left_join(tb_ip, by=c("ip") )
  pt %<>% left_join(tb_ip_app, by=c("ip","app") )
  pt %<>% left_join(tb_ip_channel, by=c("ip","channel") )
  pt %<>% left_join(tb_ip_device, by=c("ip","device") )
  pt %<>% left_join(tb_ip_os, by=c("ip","os") )
  pt
  
}

second_set <- function(pt){
  
  pt %<>% left_join(tb_ip_app_channel, by=c("ip","app","channel"))
  pt %<>% left_join(tb_ip_channel_device, by=c("ip","channel","device") )
  pt %<>% left_join(tb_ip_device_os, by=c("ip","device","os") )
  pt
  
}

third_set <- function(pt){
  
  pt %<>% left_join(tb_ip_channel_device_os, by=c("ip","channel","device","os") )
  pt
  
}

adjust_set <- function(pt){
  

  pt %<>% mutate(hour=as.numeric(hour), ip=as.numeric(ip), os=as.numeric(os), 
                 channel=as.numeric(channel), device=as.numeric(device), app=as.numeric(app))
  pt %<>% mutate(n_app=as.numeric(n_app), n_channel=as.numeric(n_channel), n_os=as.numeric(n_os),
                 diff_mean=as.numeric(diff_mean),
                 n_device=as.numeric(n_device), n_device_os=as.numeric(n_device_os), 
                 n_channel_device_os=as.numeric(n_channel_device_os))
  pt %<>% mutate(diff_mean=ifelse(is.nan(diff_mean),NA,diff_mean)) 
  pt
  
}