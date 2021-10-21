LIMS <- function(){
options(dplyr.summarise.inform=F)
#    QUERIES ----

#Create date string for the DB query (otherwise it plays up)
st <- format(today()-28, format = "%d-%b-%Y")

# Create a database connection object
con <- dbConnect(odbc::odbc(), "Alkathene")

#Assign tables with LIMS data of interest
LIMS.SMPL <- tbl(con, "IP_SMPL")
LIMS.RSLT <- tbl(con, "IP_TST_RSLT")
LIMS.SPEC <- tbl(con, "IP_PRDCT_SPC")
LIMS.DETL <- tbl(con, "IP_PRDCT_SPC_DETAIL")

#Query the test data (for SPC charts etc.):
QRY.RSLT <- inner_join(LIMS.SMPL, LIMS.RSLT, "SMPL_NAME") %>% 
  filter(SMPL_DT >= st & EQ_NAME == "AUTOGRADER") %>% 
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

#Query the specification limits:
QRY.SPEC <- inner_join(LIMS.SPEC,LIMS.DETL,"PRDCT_SPC_NAME") %>%
  select(PRDCT_SPC_NAME, 
         PRDCT_NAME,
         PRPRTY_NAME,
         MIN_VALUE,
         MAX_VALUE) %>%
  filter(PRDCT_NAME %in% c("LD0220MS", "LD1217", "LD6622", "LDD201", "LDD203",
                           "LDD204", "LDD205", "LDF433", "LDH210", "LDH215", 
                           "LDJ225", "LDJ226", "LDN248", "WNC199", "WRM124", 
                           "XDS34", "XJF143", "XLC177", "XLF197") &
           PRPRTY_NAME %in% c("ASH", "DENS_COLUMN", "GOOD CUTS",
                              "CUT_GRAN", "GOTTFERT SWELL R")) %>% 
  collect()

#Query the floss data:
QRY.FLOS <- inner_join(LIMS.SMPL, LIMS.RSLT, "SMPL_NAME") %>% 
  filter(SMPL_DT >= st & 
         TMPLT_NAME == "T-BULK TANKER" &
         PRPRTY_NAME == "FLOSS_FINES" &
         EQ_NAME %in% c("BULK SILO 1", "BULK SILO 2", "BULK SILO 3", 
                        "BULK SILO 4", "BULK SILO 5", "EB1")) %>% 
  select(SMPL_DT_TM,
         EQ_NAME,
         PRDCT_NAME,
         LOT_NUMBER,
         CNTNR_NO,
         RSLT_NUMERIC_VALUE) %>%
  arrange(SMPL_DT_TM, LOT_NUMBER, CNTNR_NO) %>% 
  collect()

#Close the database connection:
dbDisconnect(con)


#    CLEANUP ----

#Rename PRPRTY_NAME with more print-friendly names:
PROP <- c("ASH"="Ash", 
          "DENS_COLUMN"="Density", 
          "GOOD CUTS"="Good Granules", 
          "% GOOD GRANULES"="Good Granules",
          "CUT_GRAN"="Granules per Gram", 
          "GOTTFERT SWELL R"="Swell Ratio", 
          "DAVENPORT SWELL R"="Swell Ratio")

QRY.RSLT$PRPRTY_NAME <- unname(PROP[QRY.RSLT$PRPRTY_NAME])
QRY.SPEC$PRPRTY_NAME <- unname(PROP[QRY.SPEC$PRPRTY_NAME])


#RESULTS----

#Assign Reactor Vessel Names instead of 'AUTOGRADER'
QRY.RSLT$TMPLT_NAME <- paste0("RV",substr(QRY.RSLT$TMPLT_NAME,11,11))


#FLOSS----

#Summarise by date and Bulk Silo; proportion of containers for each floss 'level'
SUM.FLOS <-  QRY.FLOS %>% group_by("DATE" = round_date(SMPL_DT_TM, "day"), EQ_NAME) %>% summarise(
  "Low" = sum(RSLT_NUMERIC_VALUE <= 25)/n(),
  "Moderate" = sum(RSLT_NUMERIC_VALUE > 25 & RSLT_NUMERIC_VALUE < 100)/n(),
  "High" = sum(RSLT_NUMERIC_VALUE >= 100)/n())

#Transform the 4 floss columns into two columns (category and value)
FLOS <- pivot_longer(SUM.FLOS, 3:5, names_to = "category")

#Change the category type to ordered factor (in reverse order due to ggplot2 being weird)
FLOS$category <- factor(FLOS$category, levels = c("High","Moderate","Low"), ordered = T)
#And the silo names to ordered factors to display the facets properly
FLOS$EQ_NAME <- factor(FLOS$EQ_NAME, levels = c("BULK SILO 2", "EB1", "BULK SILO 4",
                                                "BULK SILO 3", "BULK SILO 1", "BULK SILO 5"), ordered = T)


#SPECS----

#Create a summary table for the grade specifications (and suppress warnings about NA values)
suppressWarnings(
    SPEC <- QRY.SPEC %>% group_by(PRDCT_NAME, PRPRTY_NAME) %>% summarise(
    "USL" = as.double(max(MAX_VALUE, na.rm = T)),
    "UCL" = as.double(min(MAX_VALUE, na.rm = T)),
    "LCL" = as.double(max(MIN_VALUE, na.rm = T)),
    "LSL" = as.double(min(MIN_VALUE, na.rm = T))
    )
  )

#Create a target value for each spec based on the mid-point of the UCL and LCL
SPEC <- SPEC %>% mutate("AIM" = (UCL + LCL)/2)


#TRAFFIC LIGHTS----
#Collect results (and specs for these) over previous 24hrs:
TRAF.SUM <- inner_join(QRY.RSLT, SPEC, by=c("PRDCT_NAME", "PRPRTY_NAME")) %>% 
  filter(SMPL_DT_TM >= (force_tz(now()-24*60*60, tzone = "UTC")) & 
           !is.na(RSLT_NUMERIC_VALUE)) %>%
#Initialise a column for storing the result of the spec. check:
  mutate("COLOUR" = "G")

#IF there are any data over the past 24hrs,
#Create a dataframe for display on the web app (all lights set to 'GREEN' initially 
#and update values if any points are identified as R or Y in TRAF.SUM));
#otherwise, just return a blank dataframe
if(nrow(TRAF.SUM)>0){
TRAF <- data.frame("RV2"=rep("G",5),"RV3"=rep("G",5),"RV4"=rep("G",5), 
                   row.names=c("Density","Swell Ratio","Ash",
                               "Good Granules", "Granules per Gram"))

#Check each row for violations of the spec. limits:
for(i in 1:nrow(TRAF.SUM)){
  
  #YELLOW (between CL and SL)
  #Upper limits may not exist
  if(!is.na(TRAF.SUM$USL[i] && TRAF.SUM$UCL[i])){
    if(TRAF.SUM$RSLT_NUMERIC_VALUE[i] > TRAF.SUM$UCL[i] && TRAF.SUM$RSLT_NUMERIC_VALUE[i] < TRAF.SUM$USL[i]){
      TRAF.SUM$COLOUR[i] <- "Y"}}
  if(TRAF.SUM$RSLT_NUMERIC_VALUE[i] < TRAF.SUM$LCL[i] && TRAF.SUM$RSLT_NUMERIC_VALUE[i] > TRAF.SUM$LSL[i]){
    TRAF.SUM$COLOUR[i] <- "Y"}
    
  #RED (Outside CL)
  #Upper limits may not exist
  if(!is.na(TRAF.SUM$USL[i])){if(TRAF.SUM$RSLT_NUMERIC_VALUE[i] > TRAF.SUM$USL[i]){TRAF.SUM$COLOUR[i] <- "R"}}
  if(TRAF.SUM$RSLT_NUMERIC_VALUE[i] < TRAF.SUM$LSL[i]){TRAF.SUM$COLOUR[i] <- "R"}
  
}

#Summarise the results - if ANY results are red or yellow, traffic light will show red or yellow
#Values are the HTML reference strings required to display images:
TRAF.SUM <- TRAF.SUM %>% group_by(PRPRTY_NAME, TMPLT_NAME) %>%
  summarise("COLOUR" = if(sum(COLOUR=="R")>0){'<img src="R.png" height="60" width="60"></img>'}
            else if(sum(COLOUR=="Y")>0){'<img src="Y.png" height="60" width="60"></img>'}
            else{'<img src="G.png" height="60" width="60"></img>'}) %>%
  #split the columns into three (one for each RV) to match the TRAF table from earlier
  pivot_wider(names_from = TMPLT_NAME, values_from = COLOUR)

#Overwrite the values in the TRAF table - if no data in last 24hrs, assign NA
if("RV2" %in% colnames(TRAF.SUM)){TRAF$RV2 <- TRAF.SUM$RV2[match(rownames(TRAF),TRAF.SUM$PRPRTY_NAME)]}else{TRAF$RV2 <- NA}
if("RV3" %in% colnames(TRAF.SUM)){TRAF$RV3 <- TRAF.SUM$RV3[match(rownames(TRAF),TRAF.SUM$PRPRTY_NAME)]}else{TRAF$RV3 <- NA}
if("RV4" %in% colnames(TRAF.SUM)){TRAF$RV4 <- TRAF.SUM$RV4[match(rownames(TRAF),TRAF.SUM$PRPRTY_NAME)]}else{TRAF$RV4 <- NA}

} else {
  TRAF <- data.frame("RV2"=rep("No Data",5),"RV3"=rep("No Data",5),"RV4"=rep("No Data",5), 
                             row.names=c("Density","Swell Ratio","Ash",
                                         "Good Granules", "Granules per Gram"))
}

#SPEC OVERRIDES----
#Issues with single-sided spec. limits, manually override:

#Set max. value of 100% and let USL=LCL to properly define the region between
SPEC$UCL[SPEC$PRPRTY_NAME == "Good Granules"] <- 100
SPEC$USL[SPEC$PRPRTY_NAME == "Good Granules"] <- SPEC$LCL[SPEC$PRPRTY_NAME == "Good Granules"]

#No USL/LSL defined for any grade, so ensure values are NA
SPEC$USL[SPEC$PRPRTY_NAME == "Granules per Gram"] <- NA
SPEC$LSL[SPEC$PRPRTY_NAME == "Granules per Gram"] <- NA

#Let USL=LCL to properly define the region between (for Extrusion Coating grades)
SPEC$USL[SPEC$PRPRTY_NAME == "Swell Ratio" &
            SPEC$PRDCT_NAME %in% c("LD1217", "LDN248", "WNC199", "XLC177")] <- 
  SPEC$LCL[SPEC$PRPRTY_NAME == "Swell Ratio" &
              SPEC$PRDCT_NAME %in% c("LD1217", "LDN248", "WNC199", "XLC177")]

#No upper limit is defined, so arbitrarily set at 2
SPEC$UCL[SPEC$PRPRTY_NAME == "Swell Ratio" &
            SPEC$PRDCT_NAME %in% c("LD1217", "LDN248", "WNC199", "XLC177")] <- 2


#    OUTPUT DATA AS NAMED LIST----
DATA <- list("RSLT" = QRY.RSLT, "SPEC" = SPEC, "FLOS" = FLOS, "TRAF" = TRAF)

options(dplyr.summarise.inform=T)

return(DATA)

}
