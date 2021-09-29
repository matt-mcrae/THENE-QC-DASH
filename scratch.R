library(DBI)


con <- dbConnect(odbc::odbc(), "Alkathene")

#Extract the two tables with the LIMS data of interest
LIMS.SPECS <- tbl(con, "IP_PRDCT_SPC")
LIMS.DETAIL <- tbl(con, "IP_PRDCT_SPC_DETAIL")

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
                              "CUT_GRAN", "GOTTFERT SWELL R", 
                              "DAVENPORT SWELL R")) %>% 
  collect()

#use the min of min as QA and the max of min as QC, etc
#and put the results for each group into the summary list subqry[[2]]

#Build an empty structure to hold the data (nested lists)
#Each grade-property combination will have upper and lower control and spec limits
#These can be contained in a vector length 4: [USL, UCL, LCL, LSL]

#Each grade has 5 properties of interest:
SPECS.PROP <- list("Ash" = vector(mode = "double", length = 4), 
  "Density" = vector(mode = "double", length = 4), 
  "Good Granules" = vector(mode = "double", length = 4), 
  "Granules per Gram" = vector(mode = "double", length = 4), 
  "Swell Ratio" = vector(mode = "double", length = 4))

#There are 19 grades of interest
SPECS <- vector(mode = "list", length = 19)

names(SPECS) <- c("LD0220MS", "LD1217", "LD6622", "LDD201", "LDD203",
  "LDD204", "LDD205", "LDF433", "LDH210", "LDH215", 
  "LDJ225", "LDJ226", "LDN248", "WNC199", "WRM124", 
  "XDS34", "XJF143", "XLC177", "XLF197")

for (i in 1:19){SPECS[[i]] <- SPECS.PROP}

#So a list of 19 grades... each element has a list of properties and each
#property has a vector of length 4 with the actual values from LIMS

names(SPECS[1]) #grade name
names(SPECS[[1]][1]) #Property Name
SPECS[[1]][[1]][1] #USL value

#Do a nested for loop - for GRADE, then for PROPERTY
for (j in 1:19){
  SPECS[[j]]
}
