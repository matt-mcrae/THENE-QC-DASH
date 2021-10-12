#Create an object to store product specifications by querying the LIMS database:
SPECS <- function(){
  #Create date string for the DB query (otherwise it plays up)
  st <- format(today()-28, format = "%d-%b-%Y")
  
  # Create a database connection object
  con <- dbConnect(odbc::odbc(), "Alkathene")
  
  #Assign tables with LIMS data of interest
  LIMS.SPEC <- tbl(con, "IP_PRDCT_SPC")
  LIMS.DETL <- tbl(con, "IP_PRDCT_SPC_DETAIL")
  
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
  
  #Close the database connection:
  dbDisconnect(con)
  
  #Rename PRPRTY_NAME with more print-friendly names:
  PROP <- c("ASH"="Ash", 
            "DENS_COLUMN"="Density", 
            "GOOD CUTS"="Good Granules", 
            "% GOOD GRANULES"="Good Granules",
            "CUT_GRAN"="Granules per Gram", 
            "GOTTFERT SWELL R"="Swell Ratio", 
            "DAVENPORT SWELL R"="Swell Ratio")
  
  QRY.SPEC$PRPRTY_NAME <- unname(PROP[QRY.SPEC$PRPRTY_NAME])
  
  #Create a summary table for the grade specifications (and suppress warnings about NA values)
  suppressWarnings(
    SPEC <- QRY.SPEC %>% group_by(PRDCT_NAME, PRPRTY_NAME) %>% summarise(
      "USL" = as.double(max(MAX_VALUE, na.rm = T)),
      "UCL" = as.double(min(MAX_VALUE, na.rm = T)),
      "LCL" = as.double(max(MIN_VALUE, na.rm = T)),
      "LSL" = as.double(min(MIN_VALUE, na.rm = T))
    )
  )
  
  return(SPEC)
  
}