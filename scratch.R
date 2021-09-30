library(DBI)
library(dplyr)
library(tibble)


SPECS.CON <- dbConnect(odbc::odbc(), "Alkathene")

#Extract the two tables with the LIMS data of interest
LIMS.SPECS <- tbl(SPECS.CON, "IP_PRDCT_SPC")
LIMS.DETAIL <- tbl(SPECS.CON, "IP_PRDCT_SPC_DETAIL")

#Join the tables into one
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

dbDisconnect(SPECS.CON)


#use the min of min as QA and the max of min as QC, etc
#and put the results for each group into the summary list subqry[[2]]

#Build an empty structure to hold the data (nested lists)
#Each grade-property combination will have upper and lower control and spec limits
#These can be contained in a vector length 4: [USL, UCL, LCL, LSL]

#Each grade has 5 properties of interest:
SPECS.PROP <- list("ASH" = vector(mode = "double", length = 4), 
  "DENS_COLUMN" = vector(mode = "double", length = 4), 
  "GOOD CUTS" = vector(mode = "double", length = 4), 
  "CUT_GRAN" = vector(mode = "double", length = 4), 
  "GOTTFERT SWELL R" = vector(mode = "double", length = 4))

#There are 19 grades of interest
SPECS <- vector(mode = "list", length = 19)

names(SPECS) <- c("LD0220MS", "LD1217", "LD6622", "LDD201", "LDD203",
  "LDD204", "LDD205", "LDF433", "LDH210", "LDH215", 
  "LDJ225", "LDJ226", "LDN248", "WNC199", "WRM124", 
  "XDS34", "XJF143", "XLC177", "XLF197")

for (i in 1:19){SPECS[[i]] <- SPECS.PROP}

#So a list of 19 grades... each element has a list of properties and each
#property has a vector of length 4 with the actual values from LIMS

#Do a nested for loop - for GRADE, then for PROPERTY
for (j in 1:19){
  suppressWarnings(
  for (k in 1:5){
    
    #USL
    SPECS[[j]][[k]][1] <- max(as.double(SPECS.QRY$MAX_VALUE[
      SPECS.QRY$PRDCT_NAME == names(SPECS[j]) & 
        SPECS.QRY$PRPRTY_NAME == names(SPECS[[j]][k])]), na.rm = T)
    
    if (is.infinite(SPECS[[j]][[k]][1])) {SPECS[[j]][[k]][1] <- NA}
    
    #UCL
    SPECS[[j]][[k]][2] <- min(as.double(SPECS.QRY$MAX_VALUE[
      SPECS.QRY$PRDCT_NAME == names(SPECS[j]) & 
        SPECS.QRY$PRPRTY_NAME == names(SPECS[[j]][k])]), na.rm = T)
    
    if (is.infinite(SPECS[[j]][[k]][2])) {SPECS[[j]][[k]][2] <- NA}
    
    #LCL
    SPECS[[j]][[k]][3] <- max(as.double(SPECS.QRY$MIN_VALUE[
      SPECS.QRY$PRDCT_NAME == names(SPECS[j]) & 
        SPECS.QRY$PRPRTY_NAME == names(SPECS[[j]][k])]), na.rm = T)
    
    if (is.infinite(SPECS[[j]][[k]][3])) {SPECS[[j]][[k]][3] <- NA}
    
    #LSL
    SPECS[[j]][[k]][4] <- min(as.double(SPECS.QRY$MIN_VALUE[
      SPECS.QRY$PRDCT_NAME == names(SPECS[j]) & 
        SPECS.QRY$PRPRTY_NAME == names(SPECS[[j]][k])]), na.rm = T)
    
    if (is.infinite(SPECS[[j]][[k]][4])) {SPECS[[j]][[k]][4] <- NA}
    
  })
  
  names(SPECS[[j]]) <- c("Ash","Density","Good Granules","Granules per Gram","Swell Ratio")
  
}

# Assuming the spc object has been collected already, test how to link specs with these data
spc[[3]] #returns all data for RV2D
spc[[3]][[2]] #returns summary statistics for RV2D (used as the data for the plots)
first(spc[[3]][[1]]$PRPRTY_NAME) #returns property name for RV2D
spc[[3]][[2]]$GRADE #returns grade for RV2D

spc[[3]][[2]]$GRADE == names(SPECS) #Matching grade name

names(SPECS[spc[[3]][[2]]$GRADE == names(SPECS)][[1]]) == first(spc[[3]][[1]]$PRPRTY_NAME) #Matching property name


SPECS[names(SPECS) == "WNC199"][[1]] #equivalent to SPECS$WNC199

names(SPECS[names(SPECS) == "WNC199"][[1]]) == "Density"

SPECS[names(SPECS) == "WNC199"][[1]][names(SPECS[names(SPECS) == "WNC199"][[1]]) == "Density"][[1]] #vector of interest








subqry[[i]][[1]] %>% 
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
         "m3s"= MU - 3*SD,
         "USL"= SPECS[names(SPECS) == subqry[[i]][[2]]$GRADE][[1]][names(SPECS[names(SPECS) == subqry[[i]][[2]]$GRADE][[1]]) == prp[i]][[1]][1],
         "UCL"= SPECS[names(SPECS) == GRADE][[1]][names(SPECS[names(SPECS) == GRADE][[1]]) == prp[i]][[1]][2],
         "LCL"= SPECS[names(SPECS) == GRADE][[1]][names(SPECS[names(SPECS) == GRADE][[1]]) == prp[i]][[1]][3],
         "LSL"= SPECS[names(SPECS) == GRADE][[1]][names(SPECS[names(SPECS) == GRADE][[1]]) == prp[i]][[1]][4])







# Instead of nested lists, a single list with nested tibbles would do

SPECS <- vector(mode = "list", length = 19)

names(SPECS) <- c("LD0220MS", "LD1217", "LD6622", "LDD201", "LDD203",
                  "LDD204", "LDD205", "LDF433", "LDH210", "LDH215", 
                  "LDJ225", "LDJ226", "LDN248", "WNC199", "WRM124", 
                  "XDS34", "XJF143", "XLC177", "XLF197")

SPECS[1]

tibble("ASH" = NA, 
       "DENS_COLUMN" = NA, 
       "GOOD CUTS" = NA, 
       "CUT_GRAN" = NA, 
       "GOTTFERT SWELL R" = NA)


SPECS.QRY[names(SPECS[1]) == SPECS.QRY$PRDCT_NAME,]




#USL
max(as.double(SPECS.QRY$MAX_VALUE[SPECS.QRY$PRDCT_NAME == names(SPECS[j]) & 
    SPECS.QRY$PRPRTY_NAME == names(SPECS[[j]][k])]), na.rm = T)




#THIS IS THE NEATEST WAY TO DO THINGS SO FAR!
options(dplyr.summarise.inform = F)

SPECS <- SPECS.QRY %>% group_by(PRDCT_NAME, PRPRTY_NAME) %>% summarise(
  "USL" = as.double(max(MAX_VALUE, na.rm = T)),
  "UCL" = as.double(min(MAX_VALUE, na.rm = T)),
  "LCL" = as.double(max(MIN_VALUE, na.rm = T)),
  "LSL" = as.double(min(MIN_VALUE, na.rm = T)))

#Create a dummy version of the summary tibble
tst <- tibble("GRADE" = c("WNC199", "LDF433", "XDS34"))

tst %>% group_by(GRADE) %>% summarise(
  "UCL" = SPECS$UCL[SPECS$PRDCT_NAME %in% GRADE & SPECS$PRPRTY_NAME == "Density"])

inner_join(tst, SPECS, by = c("GRADE"="PRDCT_NAME"))




LIMS_prop <- c("ASH"="Ash", 
               "DENS_COLUMN"="Density", 
               "GOOD CUTS"="Good Granules", 
               "% GOOD GRANULES"="Good Granules",
               "CUT_GRAN"="Granules per Gram", 
               "GOTTFERT SWELL R"="Swell Ratio", 
               "DAVENPORT SWELL R"="Swell Ratio")

SPECS$PRPRTY_NAME <- unname(LIMS_prop[SPECS$PRPRTY_NAME])


#Issues with single-sided limits: manually override:
SPECS$UCL[SPECS$PRPRTY_NAME == "Good Granules"] <- 100
SPECS$USL[SPECS$PRPRTY_NAME == "Good Granules"] <- SPECS$LCL[SPECS$PRPRTY_NAME == "Good Granules"]

SPECS$USL[SPECS$PRPRTY_NAME == "Granules per Gram"] <- NA
SPECS$LSL[SPECS$PRPRTY_NAME == "Granules per Gram"] <- NA

SPECS$USL[SPECS$PRPRTY_NAME == "Swell Ratio" &
            SPECS$PRDCT_NAME %in% c("LD1217", "LDN248", "WNC199", "XLC177")] <- 
  SPECS$LCL[SPECS$PRPRTY_NAME == "Swell Ratio" &
              SPECS$PRDCT_NAME %in% c("LD1217", "LDN248", "WNC199", "XLC177")]
