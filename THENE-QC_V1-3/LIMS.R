LIMS <- function(){

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
         AIM_VALUE,
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


#SPECS

#Create a summary table for the grade specifications (and suppress warnings about NA values)
suppressWarnings(
    SPEC <- QRY.SPEC %>% group_by(PRDCT_NAME, PRPRTY_NAME) %>% summarise(
    "USL" = as.double(max(MAX_VALUE, na.rm = T)),
    "UCL" = as.double(min(MAX_VALUE, na.rm = T)),
    "LCL" = as.double(max(MIN_VALUE, na.rm = T)),
    "LSL" = as.double(min(MIN_VALUE, na.rm = T))
    )
  )


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


#RESULTS

#Assign Reactor Vessel Names instead of 'AUTOGRADER'
QRY.RSLT$TMPLT_NAME <- paste0("RV",substr(QRY.RSLT$TMPLT_NAME,11,11))


#FLOSS

#Summarise by date and Bulk Silo; proportion of containers for each floss 'level'
SUM.FLOS <-  QRY.FLOS %>% group_by("DATE" = round_date(SMPL_DT_TM, "day"), EQ_NAME) %>% summarise(
  "Negligible" = sum(RSLT_NUMERIC_VALUE <= 25)/n(),
  "Low" = sum(RSLT_NUMERIC_VALUE == 50)/n(),
  "Moderate" = sum(RSLT_NUMERIC_VALUE == 75)/n(),
  "High" = sum(RSLT_NUMERIC_VALUE >= 100)/n())

#Transform the 4 floss columns into two columns (category and value)
FLOS <- pivot_longer(SUM.FLOS, 3:6, names_to = "category")

#Change the category type to ordered factor (in reverse order due to ggplot2 being weird)
FLOS$category <- factor(FLOS$category, levels = c("High","Moderate","Low","Negligible"), ordered = T)


#    OUTPUT DATA AS NAMED LIST
DATA <- list("RSLT" = QRY.RSLT, "SPEC" = SPEC, "FLOS" = FLOS)

assign("DATA", DATA, envir = .GlobalEnv) #NOTE this will not work with >1 user

}
