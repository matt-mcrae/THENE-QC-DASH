SPC <- function(ndays = 7){

# VARIABLE DATE RANGE ----
  
  #convert number of days into a start time (number of days before now)
  st <- today() - ndays
  
  #Create date string for the DB query (otherwise it plays up)
  st1 <- format(st, format = "%d-%b-%Y")
  
  #Create the date axis intervals and format (for creating ggplot objects much later on)
  if(ndays<=7){
    dateb <- seq.POSIXt(as.POSIXct(st)+5*60*60,as.POSIXct(force_tz(today(),tzone="UTC"))+17*60*60, 12*60*60)
    datea <- 45
    datel <- "%a %R"
    datev <- 1} else if(ndays<=14){
    dateb <- seq.POSIXt(as.POSIXct(st)+5*60*60,as.POSIXct(force_tz(today(),tzone="UTC"))+17*60*60, 12*60*60)
    datea <- 90
    datel <- "%d/%m %R"
    datev <- 0.5} else if(ndays<=21){
    dateb <- seq.POSIXt(as.POSIXct(st)+5*60*60,as.POSIXct(force_tz(today(),tzone="UTC"))+17*60*60, 24*60*60)
    datea <- 45
    datel <- "%d/%m %R"
    datev <- 1} else{
    dateb <- seq.POSIXt(as.POSIXct(st)+5*60*60,as.POSIXct(force_tz(today(),tzone="UTC"))+17*60*60, 24*60*60)
    datea <- 90
    datel <- "%d/%m %R"
    datev <- 0.5}
  
# LIMS DATABASE QUERIES ----
  con <- dbConnect(odbc::odbc(), "Alkathene")
  
  #Extract tables with LIMS data of interest
  SMPL <- tbl(con, "IP_SMPL")
  RSLT <- tbl(con, "IP_TST_RSLT")
  LIMS.SPECS <- tbl(con, "IP_PRDCT_SPC")
  LIMS.DETAIL <- tbl(con, "IP_PRDCT_SPC_DETAIL")
  
  #Perform the test data query using dplyr:
  qry <- inner_join(SMPL,RSLT,"SMPL_NAME") %>% 
    filter(SMPL_DT >= st1 & 
           EQ_NAME == "AUTOGRADER") %>% 
    select(SMPL_DT_TM,
           TMPLT_NAME,
           PRDCT_NAME,
           PRPRTY_NAME,
           RSLT_NUMERIC_VALUE,
           UNITS) %>%
    filter(PRPRTY_NAME %in% c("ASH", "DENS_COLUMN", "GOOD CUTS",
                              "CUT_GRAN", "GOTTFERT SWELL R", 
                              "DAVENPORT SWELL R")) %>%
    arrange(SMPL_DT_TM) %>% 
    collect()
  
  #Perform the specification data query using dplyr:
  SPECS.QRY <- inner_join(LIMS.SPECS,LIMS.DETAIL,"PRDCT_SPC_NAME") %>%
    select(PRDCT_SPC_NAME, 
           PRDCT_NAME,
           PRPRTY_NAME,
           MIN_VALUE,
           AIM_VALUE,
           MAX_VALUE) %>%
    filter(PRDCT_NAME %in% c("LD0220MS", "LD1217", "LD6622", "LDD201", "LDD203",
                             "LDD204", "LDD205", "LDF433", "LDH210", "LDH215", 
                             "LDJ225", "LDJ226", "LDN248", "WNC199", "WRM124", 
                             "XDS34", "XJF143", "XLC177", "XLF197") &
             PRPRTY_NAME %in% c("ASH", "DENS_COLUMN", "GOOD CUTS",
                                "CUT_GRAN", "GOTTFERT SWELL R")) %>% 
    collect()
  
  #Close the database connection:
  dbDisconnect(con)

# BUILD SPECIFICATION TABLES ----
  
  options(dplyr.summarise.inform = F)
  
  suppressWarnings(
  
  SPECS <- SPECS.QRY %>% group_by(PRDCT_NAME, PRPRTY_NAME) %>% summarise(
    "USL" = as.double(max(MAX_VALUE, na.rm = T)),
    "UCL" = as.double(min(MAX_VALUE, na.rm = T)),
    "LCL" = as.double(max(MIN_VALUE, na.rm = T)),
    "LSL" = as.double(min(MIN_VALUE, na.rm = T)))
  
  )
  
  options(dplyr.summarise.inform = T)
  
# RENAME TEST DATA FOR NICER PRINTING TO CHARTS ----
  #Property names
  LIMS_prop <- c("ASH"="Ash", 
                 "DENS_COLUMN"="Density", 
                 "GOOD CUTS"="Good Granules", 
                 "% GOOD GRANULES"="Good Granules",
                 "CUT_GRAN"="Granules per Gram", 
                 "GOTTFERT SWELL R"="Swell Ratio", 
                 "DAVENPORT SWELL R"="Swell Ratio")
  
  qry$PRPRTY_NAME <- unname(LIMS_prop[qry$PRPRTY_NAME])
  SPECS$PRPRTY_NAME <- unname(LIMS_prop[SPECS$PRPRTY_NAME])
  
  #Reactor Vessel Names
  qry$TMPLT_NAME <- paste0("RV",substr(qry$TMPLT_NAME,11,11))
  
  #Issues with single-sided spec. limits, manually override:
  SPECS$UCL[SPECS$PRPRTY_NAME == "Good Granules"] <- 100
  SPECS$USL[SPECS$PRPRTY_NAME == "Good Granules"] <- SPECS$LCL[SPECS$PRPRTY_NAME == "Good Granules"]
  
  SPECS$USL[SPECS$PRPRTY_NAME == "Granules per Gram"] <- NA
  SPECS$LSL[SPECS$PRPRTY_NAME == "Granules per Gram"] <- NA
  
  SPECS$USL[SPECS$PRPRTY_NAME == "Swell Ratio" &
              SPECS$PRDCT_NAME %in% c("LD1217", "LDN248", "WNC199", "XLC177")] <- 
    SPECS$LCL[SPECS$PRPRTY_NAME == "Swell Ratio" &
                SPECS$PRDCT_NAME %in% c("LD1217", "LDN248", "WNC199", "XLC177")]
  
  SPECS$UCL[SPECS$PRPRTY_NAME == "Swell Ratio" &
              SPECS$PRDCT_NAME %in% c("LD1217", "LDN248", "WNC199", "XLC177")] <- 2
  
# BREAK THE TEST DATA DOWN INTO SUBSETS ----
  #names for each subset (RV# and property combo) into an empty list
  subqry <- rep(list(vector(mode = "list", length = 3)),15)
  names(subqry) <-  c("RV2A","RV2C","RV2D","RV2G","RV2S",
                      "RV3A","RV3C","RV3D","RV3G","RV3S",
                      "RV4A","RV4C","RV4D","RV4G","RV4S")
  
  #Filter parameters to match with names
  rv <- c(rep("RV2",5), rep("RV3",5), rep("RV4",5))
  prp <- rep(c("Ash","Good Granules","Density","Granules per Gram","Swell Ratio"),3)

# ASSIGN GROUPS BASED ON WHEN THE GRADE CHANGES ----
  for(i in 1:15){
  
  #Name the sub-lists appropriately
  names(subqry[[i]]) <- c("DATA","SUMMARY","PLOT")
    
  #Populate the list with data, iteratively  
  subqry[[i]][[1]] <- qry %>% filter(TMPLT_NAME == rv[i], PRPRTY_NAME == prp[i])
  
  #Initialise new columns for the grouping variable and rule violations
  subqry[[i]][[1]]$GROUP <- 0
  subqry[[i]][[1]]$RULE <- 0
  
  #Check that there are any rows of data and skip if blank
  if(nrow(subqry[[i]][[1]]) == 0){next}
  
  #Apply group numbers for each DATA element in the list
    for(j in 1:nrow(subqry[[i]][[1]])){

      #first row will always be group 1
      if(j == 1){subqry[[i]][[1]]$GROUP[j] <- j}
      
      #check if the grade for the current row is the same as the previous
      else if(subqry[[i]][[1]]$PRDCT_NAME[j] == subqry[[i]][[1]]$PRDCT_NAME[j-1]){
        #if it is, it has the same group number as previous
        subqry[[i]][[1]]$GROUP[j] <- subqry[[i]][[1]]$GROUP[j-1]}  
        #if it is not, it starts a new group
      else subqry[[i]][[1]]$GROUP[j] <- subqry[[i]][[1]]$GROUP[j-1] + 1 
    }
  
# CREATE SUMMARY STATISTICS FOR THE TEST DATA BY REACTOR, PROPERTY AND GROUP ----
  subqry[[i]][[2]] <- subqry[[i]][[1]] %>% 
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
    inner_join(SPECS, by = c("GRADE"="PRDCT_NAME", "PROP"="PRPRTY_NAME"))
  
  #Replace NA values in 'tf' column with the last timestamp in the Data
  subqry[[i]][[2]]$tf[length(subqry[[i]][[2]]$tf)] <- last(subqry[[i]][[1]]$SMPL_DT_TM)
  
# CHECK FOR SPECIAL CAUSE VARIATION (FROM WESTERN ELECTRIC RULES) ----
  #Negative values indicate violation below the mean
  for (k in 1:last(subqry[[i]]$DATA$GROUP)) {
    rules <- 
      tibble("tst1a" = subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k] > subqry[[i]]$SUMMARY$p3s[subqry[[i]]$SUMMARY$GROUP==k], 
             "tst1b" = subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k] < subqry[[i]]$SUMMARY$m3s[subqry[[i]]$SUMMARY$GROUP==k],
             "a1a" = subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k] > subqry[[i]]$SUMMARY$p2s[subqry[[i]]$SUMMARY$GROUP==k],
             "a2a" = lag(subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k]) > subqry[[i]]$SUMMARY$p2s[subqry[[i]]$SUMMARY$GROUP==k],
             "a3a" = lag(subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k],2) > subqry[[i]]$SUMMARY$p2s[subqry[[i]]$SUMMARY$GROUP==k],
             "tst2a" = a1a+a2a+a3a >= 2,
             "a1b" = subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k] < subqry[[i]]$SUMMARY$m2s[subqry[[i]]$SUMMARY$GROUP==k],
             "a2b" = lag(subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k]) < subqry[[i]]$SUMMARY$m2s[subqry[[i]]$SUMMARY$GROUP==k],
             "a3b" = lag(subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k],2) < subqry[[i]]$SUMMARY$m2s[subqry[[i]]$SUMMARY$GROUP==k],
             "tst2b" = a1b+a2b+a3b >= 2,
             "b1a" = subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k] > subqry[[i]]$SUMMARY$p1s[subqry[[i]]$SUMMARY$GROUP==k],
             "b2a" = lag(subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k]) > subqry[[i]]$SUMMARY$p1s[subqry[[i]]$SUMMARY$GROUP==k],
             "b3a" = lag(subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k],2) > subqry[[i]]$SUMMARY$p1s[subqry[[i]]$SUMMARY$GROUP==k],
             "b4a" = lag(subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k],3) > subqry[[i]]$SUMMARY$p1s[subqry[[i]]$SUMMARY$GROUP==k],
             "b5a" = lag(subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k],4) > subqry[[i]]$SUMMARY$p1s[subqry[[i]]$SUMMARY$GROUP==k],
             "tst3a" = b1a+b2a+b3a+b4a+b5a >= 4,
             "b1b" = subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k] < subqry[[i]]$SUMMARY$m1s[subqry[[i]]$SUMMARY$GROUP==k],
             "b2b" = lag(subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k]) < subqry[[i]]$SUMMARY$m1s[subqry[[i]]$SUMMARY$GROUP==k],
             "b3b" = lag(subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k],2) < subqry[[i]]$SUMMARY$m1s[subqry[[i]]$SUMMARY$GROUP==k],
             "b4b" = lag(subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k],3) < subqry[[i]]$SUMMARY$m1s[subqry[[i]]$SUMMARY$GROUP==k],
             "b5b" = lag(subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k],4) < subqry[[i]]$SUMMARY$m1s[subqry[[i]]$SUMMARY$GROUP==k],
             "tst3b" = b1b+b2b+b3b+b4b+b5b >= 4,
             "c1a" = subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k] > subqry[[i]]$SUMMARY$MU[subqry[[i]]$SUMMARY$GROUP==k],
             "c2a" = lag(subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k]) > subqry[[i]]$SUMMARY$MU[subqry[[i]]$SUMMARY$GROUP==k],
             "c3a" = lag(subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k],2) > subqry[[i]]$SUMMARY$MU[subqry[[i]]$SUMMARY$GROUP==k],
             "c4a" = lag(subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k],3) > subqry[[i]]$SUMMARY$MU[subqry[[i]]$SUMMARY$GROUP==k],
             "c5a" = lag(subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k],4) > subqry[[i]]$SUMMARY$MU[subqry[[i]]$SUMMARY$GROUP==k],
             "c6a" = lag(subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k],5) > subqry[[i]]$SUMMARY$MU[subqry[[i]]$SUMMARY$GROUP==k],
             "c7a" = lag(subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k],6) > subqry[[i]]$SUMMARY$MU[subqry[[i]]$SUMMARY$GROUP==k],
             "c8a" = lag(subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k],7) > subqry[[i]]$SUMMARY$MU[subqry[[i]]$SUMMARY$GROUP==k],
             "c9a" = lag(subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k],8) > subqry[[i]]$SUMMARY$MU[subqry[[i]]$SUMMARY$GROUP==k],
             "tst4a" = c1a+c2a+c3a+c4a+c5a+c6a+c7a+c8a+c9a == 9,
             "c1b" = subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k] < subqry[[i]]$SUMMARY$MU[subqry[[i]]$SUMMARY$GROUP==k],
             "c2b" = lag(subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k]) < subqry[[i]]$SUMMARY$MU[subqry[[i]]$SUMMARY$GROUP==k],
             "c3b" = lag(subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k],2) < subqry[[i]]$SUMMARY$MU[subqry[[i]]$SUMMARY$GROUP==k],
             "c4b" = lag(subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k],3) < subqry[[i]]$SUMMARY$MU[subqry[[i]]$SUMMARY$GROUP==k],
             "c5b" = lag(subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k],4) < subqry[[i]]$SUMMARY$MU[subqry[[i]]$SUMMARY$GROUP==k],
             "c6b" = lag(subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k],5) < subqry[[i]]$SUMMARY$MU[subqry[[i]]$SUMMARY$GROUP==k],
             "c7b" = lag(subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k],6) < subqry[[i]]$SUMMARY$MU[subqry[[i]]$SUMMARY$GROUP==k],
             "c8b" = lag(subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k],7) < subqry[[i]]$SUMMARY$MU[subqry[[i]]$SUMMARY$GROUP==k],
             "c9b" = lag(subqry[[i]]$DATA$RSLT_NUMERIC_VALUE[subqry[[i]]$DATA$GROUP==k],8) < subqry[[i]]$SUMMARY$MU[subqry[[i]]$SUMMARY$GROUP==k],
             "tst4b" = c1b+c2b+c3b+c4b+c5b+c6b+c7b+c8b+c9b == 9)
    
    
    #Don't want rules 3 and 4 applied to granules per gram
    if (i %in% c(1:3, 5:8, 10:13, 15)){
    subqry[[i]]$DATA$RULE[subqry[[i]]$DATA$GROUP==k] <- replace(subqry[[i]]$DATA$RULE[subqry[[i]]$DATA$GROUP==k], rules$tst4a, 4)
    subqry[[i]]$DATA$RULE[subqry[[i]]$DATA$GROUP==k] <- replace(subqry[[i]]$DATA$RULE[subqry[[i]]$DATA$GROUP==k], rules$tst4b, -4)
    subqry[[i]]$DATA$RULE[subqry[[i]]$DATA$GROUP==k] <- replace(subqry[[i]]$DATA$RULE[subqry[[i]]$DATA$GROUP==k], rules$tst3a, 3)
    subqry[[i]]$DATA$RULE[subqry[[i]]$DATA$GROUP==k] <- replace(subqry[[i]]$DATA$RULE[subqry[[i]]$DATA$GROUP==k], rules$tst3b, -3)
    }
    
    subqry[[i]]$DATA$RULE[subqry[[i]]$DATA$GROUP==k] <- replace(subqry[[i]]$DATA$RULE[subqry[[i]]$DATA$GROUP==k], rules$tst2a, 2)
    subqry[[i]]$DATA$RULE[subqry[[i]]$DATA$GROUP==k] <- replace(subqry[[i]]$DATA$RULE[subqry[[i]]$DATA$GROUP==k], rules$tst2b, -2)
    subqry[[i]]$DATA$RULE[subqry[[i]]$DATA$GROUP==k] <- replace(subqry[[i]]$DATA$RULE[subqry[[i]]$DATA$GROUP==k], rules$tst1a, 1)
    subqry[[i]]$DATA$RULE[subqry[[i]]$DATA$GROUP==k] <- replace(subqry[[i]]$DATA$RULE[subqry[[i]]$DATA$GROUP==k], rules$tst1b, -1)
    
    }
  
# ASSIGN GGPLOT2 OBJECTS (FOR PLOTTING LATER) ----
  subqry[[i]][[3]] <-   
    ggplot(data = subqry[[i]][[2]]) + theme_bw() +
    #Remove the gridlines
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    #draw an amber rectangle to indicate the zone outside of QC but within QA
    geom_rect(aes(xmin=ts, xmax=tf, ymin=LSL, ymax=USL), fill="lightgoldenrod", alpha = 1) +
    #draw a green rectangle to indicate the zone within QC
    geom_rect(aes(xmin=ts, xmax=tf, ymin=LCL, ymax=UCL), fill="lightgreen", alpha = 1) +
    #draw lines for the mean and 1,2,3 sigma limits
    geom_segment(aes(x=ts,y=p3s,xend=tf,yend=p3s), colour = 'red', linetype = 5, size = 0.75) +
    geom_segment(aes(x=ts,y=p2s,xend=tf,yend=p2s), colour = 'red', alpha = 0.75, linetype = 2) +
    geom_segment(aes(x=ts,y=p1s,xend=tf,yend=p1s), colour = 'red', alpha = 0.55, linetype = 2) +
    geom_segment(aes(x=ts,y=MU,xend=tf,yend=MU), colour = 'blue', size = 0.75) +
    geom_segment(aes(x=ts,y=m1s,xend=tf,yend=m1s), colour = 'red', alpha = 0.5, linetype = 2) +
    geom_segment(aes(x=ts,y=m2s,xend=tf,yend=m2s), colour = 'red', alpha = 0.75, linetype = 2) +
    geom_segment(aes(x=ts,y=m3s,xend=tf,yend=m3s), colour = 'red', linetype = 5, size = 0.75) +
    #Plot the actual data points as a line chart (by group)
    geom_line(data=subqry[[i]][[1]], 
              mapping=aes(SMPL_DT_TM, RSLT_NUMERIC_VALUE, group = GROUP), na.rm=T, size = 0.75) +
    #Plot data that are in control as black points
    geom_point(data=subqry[[i]][[1]][subqry[[i]][[1]]$RULE==0,], 
               mapping=aes(SMPL_DT_TM, RSLT_NUMERIC_VALUE), na.rm=T, size = 3) +
    #Plot 'out of control' data as red points...
    geom_point(data=subqry[[i]][[1]][subqry[[i]][[1]]$RULE!=0,],
               mapping=aes(SMPL_DT_TM, RSLT_NUMERIC_VALUE), colour='red', na.rm=T, size = 3) +
    #...with red text labels showing rule no. that was violated
    geom_text(data=subqry[[i]][[1]][subqry[[i]][[1]]$RULE!=0,], 
              mapping=aes(SMPL_DT_TM, RSLT_NUMERIC_VALUE),
              label = subqry[[i]][[1]]$RULE[subqry[[i]][[1]]$RULE!=0],
              nudge_y=sign(subqry[[i]][[1]]$RULE[subqry[[i]][[1]]$RULE!=0])*
                mean(subqry[[i]][[2]]$SD, na.rm=T)*0.5, 
              colour = 'red3', fontface = "bold") +
    #Add labels above each group area
    geom_label(aes(x=ts+0.5*(tf-ts), y=p3s, label=GRADE), 
               nudge_y=mean(subqry[[i]][[2]]$SD, na.rm=T)) +
    #Adjust the x-axis to have breaks at 5AM/PM intervals over one week
    scale_x_datetime(breaks = dateb, date_labels = datel, expand=c(0,0)) +
    #Rotate the x-axis labels by 45 degrees
    theme(axis.text.x = element_text(angle = datea, hjust=1, vjust = datev, face = "bold", size = 11)) +
    #Remove the x-axis label and show the property and units on the y-axis
    labs(x=NULL, y=paste0(subqry[[i]][[1]]$PRPRTY_NAME[1]," (",subqry[[i]][[1]]$UNITS[1],")")) +
    #Make the typeface of the y-axis bold and larger than default
    theme(axis.title.y = element_text(face = "bold", size = 20)) +
    #Make the y-axis text more readable:
    theme(axis.text.y = element_text(face = "bold", size = 11)) +
    #Make the background colour red
    theme(panel.background = element_rect(fill = "lightcoral"))

  }

  return(subqry)

}
