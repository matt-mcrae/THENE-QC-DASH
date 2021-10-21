SPC_PLOT <- function(dat, spec, rv, prpty, ndays, SPC){
  # VARIABLE DATE RANGE ----
  
  #convert number of days into a start time (number of days before now)
  st <- today() - ndays
  
  #Create the date axis intervals and format (for creating ggplot objects much later on)
  #Start labels at 05:00 on the 'start date'; end on 05:00 the day after current date
  if(ndays<=7){
    dateb <- seq.POSIXt(as.POSIXct(st)+5*60*60,as.POSIXct(force_tz(today(),tzone="UTC"))+29*60*60, 12*60*60)
    datea <- 45
    datel <- "%a %R"
    datev <- 1} else if(ndays<=14){
      dateb <- seq.POSIXt(as.POSIXct(st)+5*60*60,as.POSIXct(force_tz(today(),tzone="UTC"))+29*60*60, 12*60*60)
      datea <- 90
      datel <- "%d-%m %R"
      datev <- 0.5} else if(ndays<=21){
        dateb <- seq.POSIXt(as.POSIXct(st)+5*60*60,as.POSIXct(force_tz(today(),tzone="UTC"))+29*60*60, 24*60*60)
        datea <- 45
        datel <- "%d-%b"
        datev <- 1} else{
          dateb <- seq.POSIXt(as.POSIXct(st)+5*60*60,as.POSIXct(force_tz(today(),tzone="UTC"))+29*60*60, 24*60*60)
          datea <- 90
          datel <- "%d-%b"
          datev <- 0.5}
  
  #Filter to just the data of interest based on arguments
  RSLT <- dat %>% filter(TMPLT_NAME == rv, 
                          PRPRTY_NAME == prpty,
                          SMPL_DT_TM >= st)
  
  #Initialise new columns for the grouping variable and rule violations
  RSLT$GROUP <- 0
  RSLT$RULE <- 0

  #Check how much data there are after filtering:
  #If there are none because only natural grades are being made, return nothing 
  #so the UI doesn't even display it
  if(nrow(RSLT) == 0 & prpty == "Ash"){return(NULL)} else
  #Otherwise, if there are 1 or 0 rows, just return a blank plot
  #NOTE for 1 data point, can't draw plots properly and errors are caused so just
  #tell the user there is "no data" (this is an edge case but is easier to handle this way)
    if(nrow(RSLT) <= 1){return(ggplotly(
    ggplot(tibble(), aes(NA,NA, label=paste(rv,prpty,"<b>NO DATA IN DATE RANGE</b>"))) + 
      geom_text() + theme_void()) %>% 
      layout(xaxis=list("visible"=F), yaxis=list("visible"=F)) %>%
      config(displayModeBar = F))}
  
  
  #Apply group numbers for each DATA element in the list
  for(j in 1:nrow(RSLT)){
    
    #first row will always be group 1
    if(j == 1){RSLT$GROUP[j] <- j}
    
    #check if the grade for the current row is the same as the previous
    else if(RSLT$PRDCT_NAME[j] == RSLT$PRDCT_NAME[j-1]){
      #if it is, it has the same group number as previous
      RSLT$GROUP[j] <- RSLT$GROUP[j-1]}  
    #if it is not, it starts a new group
    else RSLT$GROUP[j] <- RSLT$GROUP[j-1] + 1 
  }
  
  # CREATE SUMMARY STATISTICS FOR THE TEST DATA BY REACTOR, PROPERTY AND GROUP ----
  SMRY <- RSLT %>% 
    group_by(GROUP) %>% 
    summarise("GRADE" = first(PRDCT_NAME),
              "PROP" = first(PRPRTY_NAME),
              "DATE" = first(SMPL_DT_TM),
              "MU" = mean(RSLT_NUMERIC_VALUE, na.rm=T),
              "SD" = sd(RSLT_NUMERIC_VALUE, na.rm=T)) %>% 
    mutate("ts" = DATE,
           "tf" = lead(DATE),
           "p1s"= MU + SD,
           "p2s"= MU + 2*SD,
           "p3s"= MU + 3*SD,
           "m1s"= MU - SD,
           "m2s"= MU - 2*SD,
           "m3s"= MU - 3*SD) %>% 
    inner_join(spec, by = c("GRADE"="PRDCT_NAME", "PROP"="PRPRTY_NAME"))
  
  #Replace NA values in 'tf' column with the last timestamp in the Data
  SMRY$tf[length(SMRY$tf)] <- last(RSLT$SMPL_DT_TM)
  
  # CHECK FOR SPECIAL CAUSE VARIATION (FROM WESTERN ELECTRIC RULES) ----
  #Negative values indicate violation below the mean
  for (k in 1:last(RSLT$GROUP)) {
    rules <- 
      tibble("tst1a" = RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k] > SMRY$p3s[SMRY$GROUP==k], 
             "tst1b" = RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k] < SMRY$m3s[SMRY$GROUP==k],
             "a1a" = RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k] > SMRY$p2s[SMRY$GROUP==k],
             "a2a" = lag(RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k]) > SMRY$p2s[SMRY$GROUP==k],
             "a3a" = lag(RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k],2) > SMRY$p2s[SMRY$GROUP==k],
             "tst2a" = a1a+a2a+a3a >= 2,
             "a1b" = RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k] < SMRY$m2s[SMRY$GROUP==k],
             "a2b" = lag(RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k]) < SMRY$m2s[SMRY$GROUP==k],
             "a3b" = lag(RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k],2) < SMRY$m2s[SMRY$GROUP==k],
             "tst2b" = a1b+a2b+a3b >= 2,
             "b1a" = RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k] > SMRY$p1s[SMRY$GROUP==k],
             "b2a" = lag(RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k]) > SMRY$p1s[SMRY$GROUP==k],
             "b3a" = lag(RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k],2) > SMRY$p1s[SMRY$GROUP==k],
             "b4a" = lag(RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k],3) > SMRY$p1s[SMRY$GROUP==k],
             "b5a" = lag(RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k],4) > SMRY$p1s[SMRY$GROUP==k],
             "tst3a" = b1a+b2a+b3a+b4a+b5a >= 4,
             "b1b" = RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k] < SMRY$m1s[SMRY$GROUP==k],
             "b2b" = lag(RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k]) < SMRY$m1s[SMRY$GROUP==k],
             "b3b" = lag(RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k],2) < SMRY$m1s[SMRY$GROUP==k],
             "b4b" = lag(RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k],3) < SMRY$m1s[SMRY$GROUP==k],
             "b5b" = lag(RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k],4) < SMRY$m1s[SMRY$GROUP==k],
             "tst3b" = b1b+b2b+b3b+b4b+b5b >= 4,
             "c1a" = RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k] > SMRY$MU[SMRY$GROUP==k],
             "c2a" = lag(RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k]) > SMRY$MU[SMRY$GROUP==k],
             "c3a" = lag(RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k],2) > SMRY$MU[SMRY$GROUP==k],
             "c4a" = lag(RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k],3) > SMRY$MU[SMRY$GROUP==k],
             "c5a" = lag(RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k],4) > SMRY$MU[SMRY$GROUP==k],
             "c6a" = lag(RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k],5) > SMRY$MU[SMRY$GROUP==k],
             "c7a" = lag(RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k],6) > SMRY$MU[SMRY$GROUP==k],
             "c8a" = lag(RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k],7) > SMRY$MU[SMRY$GROUP==k],
             "c9a" = lag(RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k],8) > SMRY$MU[SMRY$GROUP==k],
             "tst4a" = c1a+c2a+c3a+c4a+c5a+c6a+c7a+c8a+c9a == 9,
             "c1b" = RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k] < SMRY$MU[SMRY$GROUP==k],
             "c2b" = lag(RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k]) < SMRY$MU[SMRY$GROUP==k],
             "c3b" = lag(RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k],2) < SMRY$MU[SMRY$GROUP==k],
             "c4b" = lag(RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k],3) < SMRY$MU[SMRY$GROUP==k],
             "c5b" = lag(RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k],4) < SMRY$MU[SMRY$GROUP==k],
             "c6b" = lag(RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k],5) < SMRY$MU[SMRY$GROUP==k],
             "c7b" = lag(RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k],6) < SMRY$MU[SMRY$GROUP==k],
             "c8b" = lag(RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k],7) < SMRY$MU[SMRY$GROUP==k],
             "c9b" = lag(RSLT$RSLT_NUMERIC_VALUE[RSLT$GROUP==k],8) < SMRY$MU[SMRY$GROUP==k],
             "tst4b" = c1b+c2b+c3b+c4b+c5b+c6b+c7b+c8b+c9b == 9)
    
    
    #Don't want rules 3 and 4 applied to granules per gram
    if (prpty != "Granules per Gram"){
      RSLT$RULE[RSLT$GROUP==k] <- replace(RSLT$RULE[RSLT$GROUP==k], rules$tst4a, 4)
      RSLT$RULE[RSLT$GROUP==k] <- replace(RSLT$RULE[RSLT$GROUP==k], rules$tst4b, -4)
      RSLT$RULE[RSLT$GROUP==k] <- replace(RSLT$RULE[RSLT$GROUP==k], rules$tst3a, 3)
      RSLT$RULE[RSLT$GROUP==k] <- replace(RSLT$RULE[RSLT$GROUP==k], rules$tst3b, -3)
    }
    
    RSLT$RULE[RSLT$GROUP==k] <- replace(RSLT$RULE[RSLT$GROUP==k], rules$tst2a, 2)
    RSLT$RULE[RSLT$GROUP==k] <- replace(RSLT$RULE[RSLT$GROUP==k], rules$tst2b, -2)
    RSLT$RULE[RSLT$GROUP==k] <- replace(RSLT$RULE[RSLT$GROUP==k], rules$tst1a, 1)
    RSLT$RULE[RSLT$GROUP==k] <- replace(RSLT$RULE[RSLT$GROUP==k], rules$tst1b, -1)
    
  }
  
  # ASSIGN GGPLOT2 OBJECTS (FOR PLOTTING LATER) ----
  
  PLOT <-   
    ggplot(data = SMRY) + theme_bw() +
    #Remove the gridlines
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    #draw a yellow rectangle to indicate the zone outside of QC but within QA
    geom_rect(aes(xmin=ts, xmax=tf, ymin=LSL, ymax=USL), fill="lightgoldenrod") +
    #draw a green rectangle to indicate the zone within QC
    geom_rect(aes(xmin=ts, xmax=tf, ymin=LCL, ymax=UCL), fill="lightgreen") +
    #draw line for the target value
    geom_segment(aes(x=ts,y=AIM,xend=tf,yend=AIM), colour = 'dodgerblue', size = 0.75) +
    #draw a faint box around the data points (since geom_label doesn't work with ggplotly):
    geom_rect(aes(xmin=ts, xmax=tf, ymin=m3s, ymax=p3s), fill="ivory", alpha = 0.2) +
    #Plot the data points as a line chart (by group)
    geom_line(data=RSLT, mapping=aes(SMPL_DT_TM, RSLT_NUMERIC_VALUE, group = GROUP), 
              na.rm=T, colour = "dimgrey") +
    #Plot individual data as black points
    geom_point(data=RSLT, mapping=aes(SMPL_DT_TM, RSLT_NUMERIC_VALUE, 
                                      text = paste0("<b>",RSLT_NUMERIC_VALUE,"</b>",
                                                    " ",
                                                    "<em>", UNITS, "</em>",
                                                    "<br>",
                                                    SMPL_DT_TM 
                                      )), na.rm=T, size = 1.5) +
    #Add labels above each group area
    geom_text(aes(x=ts+0.5*(tf-ts), y=p3s, label=paste0("<b>",GRADE)), 
              nudge_y=mean(SMRY$SD*0.5, na.rm=T)) +
    #Adjust the x-axis to have breaks at 5AM/PM intervals over one week
    scale_x_datetime(breaks = dateb, date_labels = paste0("<b>",datel), expand=c(0,0)) +
    #Rotate the x-axis labels by 45 degrees
    theme(axis.text.x = element_text(angle = datea, hjust=1, vjust = datev)) +
    #Remove the x-axis label and show the property and units on the y-axis
    labs(x=NULL, y=paste0(RSLT$PRPRTY_NAME[1]," (",RSLT$UNITS[1],")")) +
    #Make the typeface of the y-axis bold and larger than default
    theme(axis.title.y = element_text(face = "bold", size = 14)) +
    #Make the background colour red
    theme(panel.background = element_rect(fill = "lightcoral")) 
  
  if(SPC){PLOT <- PLOT +
    #Conditionally draw sigma limits and SPC rule violations if SPC layer enabled:
    geom_segment(aes(x=ts,y=p3s,xend=tf,yend=p3s), colour = 'red', linetype = 2, size = 0.4) +
    geom_segment(aes(x=ts,y=p2s,xend=tf,yend=p2s), colour = 'red', linetype = 3, size = 0.2) +
    geom_segment(aes(x=ts,y=p1s,xend=tf,yend=p1s), colour = 'red', linetype = 3, size = 0.1) +
    geom_segment(aes(x=ts,y=MU,xend=tf,yend=MU), colour = 'forestgreen', size = 0.6) +
    geom_segment(aes(x=ts,y=m1s,xend=tf,yend=m1s), colour = 'red', linetype = 3, size = 0.1) +
    geom_segment(aes(x=ts,y=m2s,xend=tf,yend=m2s), colour = 'red', linetype = 3, size = 0.2) +
    geom_segment(aes(x=ts,y=m3s,xend=tf,yend=m3s), colour = 'red', linetype = 2, size = 0.4)
  
  #Only draw 'out of control' points if there are actually any rule violations:
  if(sum(RSLT$RULE==0) < nrow(RSLT)){
    PLOT <- PLOT +
      #Plot 'out of control' data as red points...
      geom_point(data=RSLT[RSLT$RULE!=0,],
                 mapping=aes(SMPL_DT_TM, RSLT_NUMERIC_VALUE, 
                             text = paste0("<b>",RSLT_NUMERIC_VALUE,"</b>",
                                           " ",
                                           "<em>", UNITS, "</em>",
                                           "<br>",
                                           SMPL_DT_TM)), 
                 colour='red', na.rm=T, size = 1.5) +
      #...with red text labels showing rule no. that was violated
      geom_text(data=RSLT[RSLT$RULE!=0,], 
                mapping=aes(SMPL_DT_TM, RSLT_NUMERIC_VALUE),
                label = RSLT$RULE[RSLT$RULE!=0],
                nudge_y=sign(RSLT$RULE[RSLT$RULE!=0])*
                  mean(SMRY$SD, na.rm=T)*0.5, 
                colour = 'red3')
  }
  }
    

  PLOT <- ggplotly(PLOT, tooltip = "text") %>%
    style(hoverinfo = "none", traces = c(1,2,3,4,5,7,16)) %>% 
    config(displayModeBar = F)
  

  
#Final output is the ggplotly object
return(PLOT)

  
  
}