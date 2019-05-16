  #This code is written in R v 3.5.1
  #Install required packages
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(scales)

  #read raw data file for one month
  filename <- paste0("Signal number",".csv")
  
  dat <- read.csv(filename, header = F, col.names = 
                    c("ID","Datetime", "Event_code", "Event_parameter"),
                  stringsAsFactors = F)
  
  #select one movement group
  
  SB <- filter(dat, Event_code %in% c(0:20,31:40) |
                 
                 (Event_code %in% c(81:100) & Event_parameter %in% c(4,
                                                                     31:43)))
  
  #format date-time
  SB$datetime_format <- as.POSIXct(SB$Datetime, 
                                   format="%Y-%m-%d %H:%M:%OS")
  
  SB$hr = as.POSIXlt(SB$datetime_format)$hour
  
  SB$date = as.POSIXlt(SB$datetime_format)$mday
  
  #select one dat of data
  SB_day1 <- filter(SB, as.POSIXlt(SB$datetime_format)$mday == 6)
  
  #order data
  SB_day1 <- SB_day1[with(SB_day1,order(datetime_format, -Event_code)),]
  
  #set cycle as the end of yellow
  j<-0
  for (i in 1: nrow(SB_day1)){
    if(SB_day1$Event_code[i] == 11 & SB_day1$Event_parameter[i] == 6){
      
      j<- j+1
    }
    
    SB_day1$Cycle[i] <- j
    
    if( i == nrow(SB_day1) | i == 1){
      print(Sys.time())
    }
  }
  
  #create some empty dataset
  thru_summary <- thru[0,]
  summ_all_l <- summ[0,]
  summ_all_l_os <- summ[0,]
  
  #select detector id
  #l <- 42
  
  #loop over lanes
  for(l in 41:43){
    
    residual <- 0
    
    #loop over cycles
    for(k in c(1:260, 262:485,487:598)){
      
      
      
      #select one cycle length and a movement
      onecycle <- SB_day1 %>% filter(Cycle == k)
      
      #find termination reason
      reason <- onecycle$Event_code[which(onecycle$Event_code %in% c(4:6) & 
                                            
                                            onecycle$Event_parameter ==6)]
      
      C <- as.numeric(difftime(max(onecycle$datetime_format),
                               
                               min(onecycle$datetime_format), units = "secs"))
      
      #check if start and end time make sense
      
      which(onecycle$Event_code == 0 & onecycle$Event_parameter ==6)
      which(onecycle$Event_code == 7 & onecycle$Event_parameter ==6)
      
      #set initial queue of this cycle as the residual of the previous
      
      Qi <- residual
      
      #calculate green time for a movement
      
      g_start <- onecycle$datetime_format[which(onecycle$Event_code == 1 & 
                                                  onecycle$Event_parameter ==6)]
      
      g_end <- onecycle$datetime_format[which(onecycle$Event_code == 7 & 
                                                onecycle$Event_parameter ==6)]
      
      C_start <- onecycle$datetime_format[which(onecycle$Event_code == 11 & 
                                                  onecycle$Event_parameter ==6)]
      
      g <- as.numeric(difftime(g_end, g_start), units = "secs")
      
      r <- C - g
      
      #select one lane
      
      
      thru <- onecycle %>% filter((Event_code == 82 & Event_parameter == l)) %>%
        mutate(start = g_start, end = g_end, g = g)
      
      if(nrow(thru) >0){
        
        #add departure event
        thru <- thru %>% mutate(depart = c(1:nrow(thru)))
        
        #calculate time difference between the 1st departure and g start
        h1 <- as.numeric(difftime(thru$datetime_format[1],g_start))
        
        #calculate headway, cum. headway, and moving avg.
        thru$headway <- c(h1, diff(thru$datetime_format))
        
        thru$cumprev_headway[1] = 0
        thru$movavg[1] = 0
        
        if(nrow(thru) > 1){
          
          for(i in 2 : nrow(thru)){
            
            thru$cumprev_headway[i] = thru$cumprev_headway[i-1] + thru$headway[i-1]
            
            thru$movavg[i] = thru$cumprev_headway[i]/(i - 1)
          }
        } else{}
        
        #delta of running mean
        thru$delta <- thru$headway - thru$movavg
        
        #decision on queuing by setting threshold
        threshold_1 <- ifelse(us_count_all[k] >4, 40, 4)
        threshold_2 <- 3
        
        #if the first vehicle comes during green, everything is unqueued
        if(h1 > threshold_1){
          
          thru$decision <- "F"
          
        } else {
          
          condition <-"Q"
          
          thru$decision[1] <- "Q"
          
          if(nrow(thru) > 1){
            
            for (i in 2: nrow(thru)){
              
              if(thru$delta[i] > threshold_2){
                
                condition<- "F"
              }
              
              thru$decision[i] <- condition
            }
          } else{}
        }
        
        thru$lane <- l
        
        thru_summary <- rbind(thru_summary, thru)
        #in case you need to select Q and F manually
        #thru$decision <- c(rep("Q",6),rep("F",14))
        #plot delta
        # print(
        #   ggplot(data = thru, aes(datetime_format,depart, label = decision))+
        #     
        #     geom_step() + 
        #     
        #     geom_label(size = 3) + theme_classic(base_size = 15) +
        #     
        #     labs(title= unique(thru$Cycle),  x="Time (hh:mm:ss)",y = "Cumulative departure", 
        #          subtitle = paste0("det=",l,", 1st depart=", round(h1,2)))+
        #     
        #     scale_x_datetime(labels=date_format("%H:%M:%S")) 
        # )
        # 
        #calculate average headways
        havg <- ifelse(sum(thru$decision == "Q") <2, 2.5,
                       
                       mean(thru$headway[which(thru$decision[2:nrow(thru)] == "Q")] ) )
        
        #calculate gq and gu
        gq <- ifelse(thru$decision[1] == "F", 0, 
                     
                     havg + sum(thru$headway[1: max(which(thru$decision == "Q"))] ) )
        
        
        gu <- g - gq
        
        
        #calculate demand flow, delay, and residual queue
        #based on 3 conditions
        
        if(sum(thru$decision == "F") ==0 & gu > havg){
          
          #condition 2: undersaturated, all queued
          
          condition <- 2
          
          qg <- NA
          
          qr <- NA
          
          q <- (gq/havg - Qi) / (r + gq)
          
          residual <- 0
          
          Qr <- Qi + q*r
          
          delay_cycle <- 0.5 * (Qi * r + Qr*(r + gq))
          
          delay_veh <- delay_cycle / (gq/havg)
          
          aor <- q*r/nrow(thru) *100
          
          #cap <- (g - h1 + havg)/C/havg
          
          
        } else if (sum(thru$decision == "F") ==0 & gu < 2){
          
          #condition 1: oversaturated, all queued, residuals exist
          
          condition <- 1
          
          qg <- NA
          
          qr <- NA
          
          q <- NA
          
          #if it is the 1st pass, use this formula
          #q <- 0.5/havg * (1 + g/C)
          
          #for second pass, use the following q
          #q<- oversat_summ$demand[1]
          
          residual <- NA
          #residual <- max(Qi + (q*C) - ((g - h1)/havg),0)
          
          Qr <- NA
          #Qr <- Qi + (q*r)
          
          delay_cycle<- NA
          #delay_cycle <- 0.5 * Qr * C + 0.5*(Qi * r + residual*g)
          
          delay_veh <- NA
          #delay_veh <- delay_cycle/q/C
          
          aor <- NA
          #aor <- q*r/nrow(thru) *100
          
          
        } else{
          #condition 0: undersaturated, queued and unqueued with or without residuals
          
          condition <- 0
          
          qg <- sum(thru$decision == "F")/gu
          
          qr <- max((nrow(thru) - (qg * g))/r,0)
          
          q <- (qg * g + qr * r)/C
          
          residual <- 0
          
          Qr <- Qi + (qr*r)
          
          delay_cycle <- 0.5 * (Qr + Qi) * r + 0.5 * gq * Qr
          
          delay_veh <- delay_cycle/nrow(thru)
          
          aor <- qr*r/nrow(thru) *100
          
        }
        
        #get movement info and moe results
        
        summ <- data.frame(CycleNo = unique(thru$Cycle), C_start, C, g, r, g_start, 
                           
                           g_end, reason=6,l, N = nrow(thru),gu,h1, condition, qg, qr, q, residual,Qr,
                           
                           delay_cycle, delay_veh, aor)
        
      } else {
        
        summ <- data.frame(CycleNo = k, C_start, C, g, r, g_start, 
                           
                           g_end, reason=6,l, N = nrow(thru),gu=g,h1 =NA, condition =0, qg =0, 
                           
                           qr =0, q =0, residual = 0,Qr = 0,
                           
                           delay_cycle = 0, delay_veh = 0, aor = 0)
        
      }
      #for 1st pass
      summ_all_l <- rbind(summ_all_l, summ)
      
      #for the 2nd pass, use a different database
      #summ_all_l_os <- rbind(summ_all_l_os, summ)
      
      
      
      
    }
    
  }
  
  #set the first row as undersaturated. That's precondition
  summ_all_l$oversat[1] <- "U"
  
  for( i in 2: nrow(summ_all_l)) {
    
    summ_all_l$oversat[i] <- ifelse(summ_all_l$condition[i] ==1 | summ_all_l$condition[i-1] ==1,
                                    
                                    "O", "U")
    
  }
  
  #check for oversaturation
  sum(summ_all_l$oversat == "O")
  
  
  #filter the oversaturation groups
  
  oversat <- summ_all_l %>% filter(oversat == "O")
  
  ##create a group number column. First,  calculate the first difference of the cycle number
  
  oversat$cycledif <- c(0, diff(oversat$CycleNo))
  
  #then, number the groups
  g<- 1
  
  for(j in 1:nrow(oversat)){
    
    if(oversat$cycledif[j] >1){
      
      g<- g+1
      
      oversat$group[j] <- g
      
    } else{
      
      oversat$group[j] <- g
    }
    
  }
  
  
  
  #create row for each group
  
  oversat_summ <- oversat %>% group_by(group) %>% summarise(cycle_st = min(CycleNo), 
                                                            
                                                            Cycle_end = max(CycleNo), Count = sum(N), st_time = min(C_start),
                                                            
                                                            end_time = max(g_end)) %>%  mutate(duration = 
                                                                                                 
                                                                                                 as.numeric(difftime(end_time, st_time, units = "secs")), demand = Count/duration)
  #create an empty dataset
  thru_summary <- thru[0,]
  summ_all_l <- summ[0,]
  summ_all_l_os <- summ[0,]
  
  #select detector id
  #l <- 43
  
  #select group number of oversaturation
  og <- 1
  
  #loop over lanes
  #for(l in 41:43){
  
  residual <- 0
  
  #loop over cycles
  for(k in oversat_summ$cycle_st[og]:oversat_summ$Cycle_end[og]){
    
    #set initial queue of this cycle as the residual of the previous
    
    #select one cycle length and a movement
    onecycle <- SB_day1 %>% filter(Cycle == k)
    
    #find termination reason
    reason <- onecycle$Event_code[which(onecycle$Event_code %in% c(4:6) & 
                                          
                                          onecycle$Event_parameter ==6)]
    
    C <- as.numeric(difftime(max(onecycle$datetime_format),
                             
                             min(onecycle$datetime_format), units = "secs"))
    
    #check if start and end time make sense
    
    which(onecycle$Event_code == 1 & onecycle$Event_parameter ==6)
    which(onecycle$Event_code == 7 & onecycle$Event_parameter ==6)
    
    #set initial queue of this cycle as the residual of the previous
    
    Qi <- residual
    
    #calculate green time for a movement
    
    g_start <- onecycle$datetime_format[which(onecycle$Event_code == 1 & 
                                                onecycle$Event_parameter ==6)]
    
    g_end <- onecycle$datetime_format[which(onecycle$Event_code == 7 & 
                                              onecycle$Event_parameter ==6)]
    
    C_start <- onecycle$datetime_format[which(onecycle$Event_code == 9 & 
                                                onecycle$Event_parameter ==6)]
    
    g <- as.numeric(difftime(g_end, g_start), units = "secs")
    
    r <- C - g
    
    #select one lane
    thru <- onecycle %>% filter((Event_code == 82 & Event_parameter == l)) %>%
      mutate(start = g_start, end = g_end, g = g)
    
    #add departure event
    thru <- thru %>% mutate(depart = c(1:nrow(thru)))
    
    #calculate time difference between the 1st departure and g start
    h1 <- as.numeric(difftime(thru$datetime_format[1],g_start))
    
    #calculate headway, cum. headway, and moving avg.
    thru$headway <- c(h1, diff(thru$datetime_format))
    
    thru$cumprev_headway[1] = 0
    thru$movavg[1] = 0
    
    for(i in 2 : nrow(thru)){
      
      thru$cumprev_headway[i] = thru$cumprev_headway[i-1] + thru$headway[i-1]
      
      thru$movavg[i] = thru$cumprev_headway[i]/(i - 1)
    }
    
    #delta of running mean
    thru$delta <- thru$headway - thru$movavg
    
    #decision on queuing by setting threshold
    threshold_1 <- 3.5
    threshold_2 <- 3
    
    #if the first vehicle comes during green, everything is unqueued
    if(h1 > threshold_1){
      
      thru$decision <- "F"
      
    } else {
      
      condition <-"Q"
      
      thru$decision[1] <- "Q"
      
      for (i in 2: nrow(thru)){
        
        if(thru$delta[i] > threshold_2){
          
          condition<- "F"
        }
        
        thru$decision[i] <- condition
      }
    }
    
    thru$lane <- l
    
    thru_summary <- rbind(thru_summary, thru)
    #in case you need to select Q and F manually
    #thru$decision <- c(rep("Q",6),rep("F",14))
    # #plot delta
    #   print(
    #   ggplot(data = thru, aes(datetime_format,depart, label = decision))+
    #   
    #   geom_step() + 
    #   
    #   geom_label(size = 3) + theme_classic(base_size = 15) +
    #   
    #   labs(title= unique(thru$Cycle),  x="Time (hh:mm:ss)",y = "Cumulative departure", 
    #        subtitle = paste0("det=",l,", 1st depart=", round(h1,2)))+
    #     
    #     scale_x_datetime(labels=date_format("%H:%M:%S")) 
    # )
    
    #calculate average headways
    havg <- ifelse(sum(thru$decision == "Q") <2, 2.5,
                   
                   mean(thru$headway[which(thru$decision[2:nrow(thru)] == "Q")] ) )
    
    #calculate gq and gu
    gq <- ifelse(thru$decision[1] == "F", 0, 
                 
                 havg + sum(thru$headway[1: max(which(thru$decision == "Q"))] ) )
    
    
    gu <- g - gq
    
    
    #calculate demand flow, delay, and residual queue
    #based on 3 conditions
    
    if(sum(thru$decision == "F") ==0 & gu > havg){
      
      #condition 2: undersaturated, all queued
      
      condition <- 2
      
      qg <- NA
      
      qr <- NA
      
      q <- (gq/havg - Qi) / (r + gq)
      
      residual <- 0
      
      Qr <- Qi + q*r
      
      delay_cycle <- 0.5 * (Qi * r + Qr*(r + gq))
      
      delay_veh <- delay_cycle / (gq/havg)
      
      aor <- q*r/nrow(thru) *100
      
      #cap <- (g - h1 + havg)/C/havg
      
      
    } else if (sum(thru$decision == "F") ==0 & gu < 2){
      
      #condition 2: oversaturated, all queued, residuals exist
      
      condition <- 1
      
      qg <- NA
      
      qr <- NA
      
      #if it is the 1st pass, use this formula
      #q <- 0.5/havg * (1 + g/C)
      
      #for second pass, use the following q
      q<- oversat_summ$demand[og]
      
      residual <- max(Qi + (q*C) - ((g - h1)/havg),0)
      
      Qr <- Qi + (q*r)
      
      delay_cycle <- 0.5 * Qr * C + 0.5*(Qi * r + residual*g)
      
      delay_veh <- delay_cycle/q/C
      
      aor <- q*r/nrow(thru) *100
      
      #cap <- (g - h1 + havg)/C/havg
      
    } else{
      #condition 0: undersaturated, queued and unqueued with or without residuals
      
      condition <- 0
      
      qg <- sum(thru$decision == "F")/gu
      
      qr <- max((nrow(thru) - (qg * g))/r,0)
      
      q <- (qg * g + qr * r)/C
      
      residual <- 0
      
      Qr <- Qi + (qr*r)
      
      delay_cycle <- 0.5 * (Qr + Qi) * r + 0.5 * gq * Qr
      
      delay_veh <- delay_cycle/q/C
      
      aor <- qr*r/nrow(thru) *100
      
    }
    
    #get movement info and moe results
    
    summ <- data.frame(CycleNo = unique(thru$Cycle), C_start, C, g, r, g_start, 
                       
                       g_end, reason,l, N = nrow(thru),gu,h1, condition, qg, qr, q, residual,Qr,
                       
                       delay_cycle, delay_veh, aor)
    
    
    #for 1st pass
    #summ_all_l <- rbind(summ_all_l, summ)
    
    #for the 2nd pass, use a different database
    summ_all_l_os <- rbind(summ_all_l_os, summ)
    
    
    
    
  }
  
  #}
  
  #combine oversaturated and undersaturated data
  summ_all_l_os$oversat <- "O"
  summ_all <- rbind(summ_all_l_os, summ_all_l[summ_all_l$oversat == "U",])
  
  
  
  write.csv(summ_all,"Output.csv")
  
  