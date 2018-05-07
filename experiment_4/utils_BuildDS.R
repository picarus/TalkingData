

createIPRange <- function(df){
  df %>% mutate( rngNumber = ifelse( ip < 126400, 1, ifelse( ip < 212750, 2, ifelse(ip < 287000, 3, 4)))) 
}

addTimeVars <- function(df){
  df %>% 
    mutate(day=as.numeric(format(click_time,"%d")), hour=as.numeric(format(click_time,"%H") )) %>% 
    mutate(tm=day*24+hour+8) %>% 
    mutate(day=tm %/% 24, hour=tm %% 24 ) %>% 
    select(-tm)
}

counts <- function(pt){
  pt %>% 
    group_by(ip, day, channel, device, os, app ) %>% 
    mutate(n_day=n(), r_day=1:n()) %>% 
    group_by(hour, add=T) %>% 
    mutate(n_hour=n(), r_hour=1:n()) %>% 
    ungroup()
}


zero_set <- function(pt){
  
  pt %<>% left_join(tr_hour, by="hour")
  pt %<>% left_join(tr_app, by="app")
  pt %<>% left_join(tr_channel, by="channel") 
  pt %<>% left_join(tr_os, by="os") 
  pt %<>% left_join(tr_device, by="device") 
  pt %<>% left_join(tr_channel_device_os, by=c("device","channel","os")) 
  pt %<>% left_join(tr_device_app, by=c("device","app")) 
  pt %<>% left_join(tr_device_os, by=c("device","os")) 
  pt %<>% left_join(tr_hour_channel_device_os, by=c("hour","channel","device","os") )
  pt %<>% left_join(tr_channel_hour, by=c("hour","channel") )
  pt %<>% left_join(tr_hour_device_app, by=c("hour","device","app") )
  pt %<>% left_join(tr_hour_device_os, by=c("hour","device","os") )
  pt
  
}


first_set <- function(pt){
  
  pt %<>% left_join(tb_ip, by="ip")
  pt %<>% left_join(tb_rng %>%  select(-min_ip,-max_ip,-pct_mean), by="rngNumber") 
  pt %<>% left_join(tb_ip_app, by=c("rngNumber","app") )
  pt %<>% left_join(tb_ip_channel, by=c("rngNumber","channel") )
  pt %<>% left_join(tb_ip_device, by=c("rngNumber","device") )
  pt %<>% left_join(tb_ip_os, by=c("rngNumber","os") )
  pt
  
}


second_set <- function(pt){
  
  pt %<>% left_join(tb_ip_app_channel, by=c("rngNumber","app","channel"))
  pt %<>% left_join(tb_ip_channel_device, by=c("rngNumber","channel","device") )
  pt %<>% left_join(tb_ip_device_os, by=c("rngNumber","device","os") )
  pt %<>% left_join(tb_ip_os_channel, by=c("rngNumber","os","channel")) 
  pt %<>% left_join(tb_ip_device_app, by=c("rngNumber","app","device"))  
  pt %<>% left_join(tb_ip_os_app, by=c("rngNumber","app","os"))
  pt
  
}

third_set <- function(pt){
  
  pt %<>% left_join(tb_ip_channel_device_os, by=c("rngNumber","channel","device","os") )
  pt %<>% left_join(tb_ip_device_os_app, by=c("rngNumber","app","device","os") ) 
  pt %<>% left_join(tb_ip_channel_device_app, by=c("rngNumber","channel","device","app"))  
  pt %<>% left_join(tb_ip_channel_os_app, by=c("rngNumber","channel","app","os"))
  pt
  
}

addNB <- function(pt) {
  
  pt %>% mutate(pct_NB=pct_att_hour+pct_att_app+pct_att_channel+pct_att_os+pct_att_device+pct_att_channel_device_os+
                  pct_att_device_app+pct_att_device_os+pct_att_hour_channel_device_os+
                  pct_att_channel_hour+pct_att_hour_device_app+pct_att_hour_device_os+
                  pct_ip_app+pct_ip_channel+pct_ip_device+pct_ip_os+
                  pct_ip_app_channel+pct_ip_channel_device+pct_ip_device_device_os+pct_ip_os_channel+pct_ip_device_app+
                  pct_ip_os_app+pct_ip_channel_device_os+pct_ip_device_os_app+pct_ip_channel_device_app+pct_ip_channel_os_app)

}

adjust_set <- function(pt){
  
 # pt %<>% select(-tm)
  pt %<>% mutate(hour=as.numeric(hour), ip=as.numeric(ip), os=as.numeric(os), 
                 channel=as.numeric(channel), device=as.numeric(device), app=as.numeric(app))
  pt %<>% mutate(n_app=as.numeric(n_app), n_channel=as.numeric(n_channel), n_os=as.numeric(n_os),
                # diff_mean=as.numeric(diff_mean),
                 n_device=as.numeric(n_device), n_device_os=as.numeric(n_device_os), 
                 n_channel_device_os=as.numeric(n_channel_device_os))
#  pt %<>% mutate(diff_mean=ifelse(is.nan(diff_mean),NA,diff_mean)) 
  pt
  
}