SPC <- function(ndays = 7){

  #convert number of days into a start time (number of days before now)
  st <- today() - ndays
  
  #Create date string for the DB query (otherwise it plays up)
  st1 <- format(st, format = "%d-%b-%Y")
  
  #Create the date axis intervals and format (for creating ggplot objects much later on)
  if(ndays<=7){
    dateb <- seq.POSIXt(as.POSIXct(st)+5*60*60,as.POSIXct(force_tz(today(),tzone="UTC"))+17*60*60, 12*60*60)
    datea <- 45
    datel <- "%a %R"
    datev <- 1}
  else if(ndays<=14){
    dateb <- seq.POSIXt(as.POSIXct(st)+5*60*60,as.POSIXct(force_tz(today(),tzone="UTC"))+17*60*60, 12*60*60)
    datea <- 90
    datel <- "%d/%m %R"
    datev <- 0.5}
  else if(ndays<=21){
    dateb <- seq.POSIXt(as.POSIXct(st)+5*60*60,as.POSIXct(force_tz(today(),tzone="UTC"))+17*60*60, 24*60*60)
    datea <- 45
    datel <- "%d/%m %R"
    datev <- 1}
  else{
    dateb <- seq.POSIXt(as.POSIXct(st)+5*60*60,as.POSIXct(force_tz(today(),tzone="UTC"))+17*60*60, 24*60*60)
    datea <- 90
    datel <- "%d/%m %R"
    datev <- 0.5}
  
  #Open DB connection object: ----
  con <- dbConnect(odbc::odbc(), "Alkathene")
  
  #Extract the two tables with the LIMS data of interest
  SMPL <- tbl(con, "IP_SMPL")
  RSLT <- tbl(con, "IP_TST_RSLT")
  
  #Join the tables into one
  THENE <- inner_join(SMPL,RSLT,"SMPL_NAME")
  
  #Perform the query using dplyr (just FY21 for testing):
  qry <- THENE %>% 
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
  
  #Close the database connection:
  dbDisconnect(con)

  #Manipulate the data into more human-friendly names: ----
  #Property names
  LIMS_prop <- c("ASH"="Ash", 
                 "DENS_COLUMN"="Density", 
                 "GOOD CUTS"="Good Granules", 
                 "% GOOD GRANULES"="Good Granules",
                 "CUT_GRAN"="Granules per Gram", 
                 "GOTTFERT SWELL R"="Swell Ratio", 
                 "DAVENPORT SWELL R"="Swell Ratio")
  
  qry$PRPRTY_NAME <- unname(LIMS_prop[qry$PRPRTY_NAME])
  
  #Reactor Vessel Names
  qry$TMPLT_NAME <- paste0("RV",substr(qry$TMPLT_NAME,11,11))
  
  # Create subsets of the data ----
  #names for each subset (RV# and property combo) into an empty list
  subqry <- rep(list(vector(mode = "list", length = 3)),15)
  names(subqry) <-  c("RV2A","RV2C","RV2D","RV2G","RV2S",
                      "RV3A","RV3C","RV3D","RV3G","RV3S",
                      "RV4A","RV4C","RV4D","RV4G","RV4S")
  
  #Filter parameters to match with names
  rv <- c(rep("RV2",5), rep("RV3",5), rep("RV4",5))
  prp <- rep(c("Ash","Good Granules","Density","Granules per Gram","Swell Ratio"),3)

  #'Group' the data based on grade changes ----
  for(i in 1:15){
  
  #Name the sub-lists appropriately
  names(subqry[[i]]) <- c("DATA","SUMMARY","PLOT")
    
  #Populate the list with data, iteratively  
  subqry[[i]][[1]] <- qry %>% filter(TMPLT_NAME == rv[i], PRPRTY_NAME == prp[i])
  
  #Initialise new columns for the grouping variable and rule violations
  subqry[[i]][[1]]$GROUP <- 0
  subqry[[i]][[1]]$RULE <- 0
  
  #Check that there are any rows of data and skip if blank (assign placeholder ggplot object too)
  if(nrow(subqry[[i]][[1]]) == 0){subqry[[i]][[3]] <- ggplot()}
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
  
  #Create summary statistics for each group (for each list element) ----
  subqry[[i]][[2]] <- subqry[[i]][[1]] %>% 
    group_by(GROUP) %>% 
    summarise("GRADE" = first(PRDCT_NAME),
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
           "m3s"= MU - 3*SD)
  
  #Replace NA values in 'tf' column with the last timestamp in the Data
  subqry[[i]][[2]]$tf[length(subqry[[i]][[2]]$tf)] <- last(subqry[[i]][[1]]$SMPL_DT_TM)
  
  #Check for special cause variation (from Western ELectric Rules) ----
  # and update RULE with result. Negative values indicate violation below the mean
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
    
    subqry[[i]]$DATA$RULE[subqry[[i]]$DATA$GROUP==k] <- replace(subqry[[i]]$DATA$RULE[subqry[[i]]$DATA$GROUP==k], rules$tst4a, 4)
    subqry[[i]]$DATA$RULE[subqry[[i]]$DATA$GROUP==k] <- replace(subqry[[i]]$DATA$RULE[subqry[[i]]$DATA$GROUP==k], rules$tst4b, -4)
    subqry[[i]]$DATA$RULE[subqry[[i]]$DATA$GROUP==k] <- replace(subqry[[i]]$DATA$RULE[subqry[[i]]$DATA$GROUP==k], rules$tst3a, 3)
    subqry[[i]]$DATA$RULE[subqry[[i]]$DATA$GROUP==k] <- replace(subqry[[i]]$DATA$RULE[subqry[[i]]$DATA$GROUP==k], rules$tst3b, -3)
    subqry[[i]]$DATA$RULE[subqry[[i]]$DATA$GROUP==k] <- replace(subqry[[i]]$DATA$RULE[subqry[[i]]$DATA$GROUP==k], rules$tst2a, 2)
    subqry[[i]]$DATA$RULE[subqry[[i]]$DATA$GROUP==k] <- replace(subqry[[i]]$DATA$RULE[subqry[[i]]$DATA$GROUP==k], rules$tst2b, -2)
    subqry[[i]]$DATA$RULE[subqry[[i]]$DATA$GROUP==k] <- replace(subqry[[i]]$DATA$RULE[subqry[[i]]$DATA$GROUP==k], rules$tst1a, 1)
    subqry[[i]]$DATA$RULE[subqry[[i]]$DATA$GROUP==k] <- replace(subqry[[i]]$DATA$RULE[subqry[[i]]$DATA$GROUP==k], rules$tst1b, -1)
    
    }
  
  #Create a ggplot2 object with the data (but do not plot) ----
  subqry[[i]][[3]] <-   
    ggplot(data = subqry[[i]][[2]]) + theme_bw() +
    #draw a semitransparent rectangle as a background for each group
    geom_rect(aes(xmin=ts, xmax=tf, ymin=m3s, ymax=p3s), fill="wheat",alpha=0.25) +
    #draw lines for the mean and 1,2,3 sigma limits
    geom_segment(aes(x=ts,y=p3s,xend=tf,yend=p3s), colour = 'red') +
    geom_segment(aes(x=ts,y=p2s,xend=tf,yend=p2s), colour = 'red', alpha = 0.5) +
    geom_segment(aes(x=ts,y=p1s,xend=tf,yend=p1s), colour = 'red', alpha = 0.25) +
    geom_segment(aes(x=ts,y=MU,xend=tf,yend=MU), colour = 'green') +
    geom_segment(aes(x=ts,y=m1s,xend=tf,yend=m1s), colour = 'red', alpha = 0.25) +
    geom_segment(aes(x=ts,y=m2s,xend=tf,yend=m2s), colour = 'red', alpha = 0.5) +
    geom_segment(aes(x=ts,y=m3s,xend=tf,yend=m3s), colour = 'red') +
    #Plot the actual data points as a line chart (by group)
    geom_line(data=subqry[[i]][[1]], 
              mapping=aes(SMPL_DT_TM, RSLT_NUMERIC_VALUE, group = GROUP), na.rm=T) +
    #Plot data that are in control as black points
    geom_point(data=subqry[[i]][[1]][subqry[[i]][[1]]$RULE==0,], 
               mapping=aes(SMPL_DT_TM, RSLT_NUMERIC_VALUE), na.rm=T) +
    #Plot 'out of control' data as red points...
    geom_point(data=subqry[[i]][[1]][subqry[[i]][[1]]$RULE!=0,],
               mapping=aes(SMPL_DT_TM, RSLT_NUMERIC_VALUE), colour='red', na.rm=T) +
    #...with red text labels showing rule no. that was violated
    geom_text(data=subqry[[i]][[1]][subqry[[i]][[1]]$RULE!=0,], 
              mapping=aes(SMPL_DT_TM, RSLT_NUMERIC_VALUE),
              label = subqry[[i]][[1]]$RULE[subqry[[i]][[1]]$RULE!=0],
              nudge_y=sign(subqry[[i]][[1]]$RULE[subqry[[i]][[1]]$RULE!=0])*
                mean(subqry[[i]][[2]]$SD, na.rm=T)*0.25, 
              colour = 'red') +
    #Add labels above each group area
    geom_label(aes(x=ts+0.5*(tf-ts), y=p3s, label=GRADE), 
               nudge_y=mean(subqry[[i]][[2]]$SD, na.rm=T)) +
    #Adjust the x-axis to have breaks at 5AM/PM intervals over one week
    scale_x_datetime(breaks = dateb, date_labels = datel) +
    #Rotate the x-axis labels by 45 degrees
    theme(axis.text.x = element_text(angle = datea, hjust=1, vjust = datev, face = "bold", size = 11)) +
    #Remove the x-axis label and show the property and units on the y-axis
    labs(x=NULL, y=paste0(subqry[[i]][[1]]$PRPRTY_NAME[1]," (",subqry[[i]][[1]]$UNITS[1],")")) +
    #Make the typeface of the y-axis bold and larger than default
    theme(axis.title.y = element_text(face = "bold", size = 14))
    
  }

  return(subqry)
  
}
