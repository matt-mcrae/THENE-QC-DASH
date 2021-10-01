# What is 'defective' when it comes to floss?
# The USL is set at 100ppm
# But the measure is basically just a category:
# Almost None = 25ppm; Low = 50ppm; Med = 75ppm; High >=100ppm
# Could show proportions with stacked bar chart with reference lines?
# But the time-series of data seem more important...See the proportions of each
# floss amount for the past (week), faceted by BLP?

library(DBI)
library(ggplot2)
library(dplyr)
library(tibble)
library(lubridate)
library(tidyr)



st1 <- "01-SEP-2021"
# st1 <- "01-JUL-2021"


con <- dbConnect(odbc::odbc(), "Alkathene")

#Extract the two tables with the LIMS data of interest
SMPL <- tbl(con, "IP_SMPL")
RSLT <- tbl(con, "IP_TST_RSLT")

#Perform the test data query using dplyr:
qry <- inner_join(SMPL,RSLT,"SMPL_NAME") %>% 
  filter(SMPL_DT >= st1 & TMPLT_NAME == "T-BULK TANKER" & 
           PRPRTY_NAME == "FLOSS_FINES") %>% 
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

rm(con,RSLT,SMPL,st1)


# Need a mutated variable for BULK LOADING POINT (BLP)
qry$BLP <- NA
qry$BLP[substr(qry$EQ_NAME,0,2)=="EB"] <- 2
qry$BLP[substr(qry$EQ_NAME,0,2)!="EB"] <- 1


#Summarise this table by BLP
qry %>% group_by(BLP) %>% summarise(
  "Negligible" = sum(RSLT_NUMERIC_VALUE <= 25)/n(),
  "Low" = sum(RSLT_NUMERIC_VALUE == 50)/n(),
  "Moderate" = sum(RSLT_NUMERIC_VALUE == 75)/n(),
  "High" = sum(RSLT_NUMERIC_VALUE >= 100)/n())

# Summarise by date
dt <-  qry %>% group_by("Date" = round_date(SMPL_DT_TM, "day"), BLP) %>% summarise(
  "Negligible" = sum(RSLT_NUMERIC_VALUE <= 25)/n(),
  "Low" = sum(RSLT_NUMERIC_VALUE == 50)/n(),
  "Moderate" = sum(RSLT_NUMERIC_VALUE == 75)/n(),
  "High" = sum(RSLT_NUMERIC_VALUE >= 100)/n())



dt %>% ggplot(mapping = aes(Date)) +
  geom_line(aes(y=Negligible)) +
  geom_line(aes(y=Low)) +
  geom_line(aes(y=Moderate)) +
  geom_line(aes(y=High))

pivot_longer(dt,2:5) %>% ggplot(mapping = aes(Date,value, colour = name)) +
  geom_line() + scale_y_continuous(labels = scales::percent) + theme_bw()

pivot_longer(dt,2:5) %>% ggplot(mapping = aes(Date,value, fill = name)) +
  geom_col() + scale_y_continuous(labels = scales::percent) + theme_bw()


#How to use ordered factors
factor(c("good", "ok", "bad"), levels = c("good", "ok", "bad"), ordered = T)


#Turn the floss rating into an ordered factor (to make the plot nicer)
lng <-  pivot_longer(dt,3:6)

lng$name <- factor(lng$name, levels = c("High","Moderate","Low","Negligible"), ordered = T)

ggplot(lng, aes(Date,value, fill=name)) + geom_col() + 
  scale_y_continuous(labels = scales::percent) + theme_bw() +
  scale_fill_manual(values = c("red","orange","yellow","green"))

#Alternative as area (possibly looks better)
ggplot(lng, aes(Date,value, fill=name)) + geom_area() + 
  scale_y_continuous(labels = scales::percent, expand = c(0,0)) + theme_bw() +
  scale_fill_manual(values = c("red","orange","yellow","green")) +
  scale_x_datetime(expand=c(0,0)) + facet_grid(cols = vars(BLP))

#Need to do labels, titles etc. - actually, just do title on webpage!
#Need to facet by BLP? - I don't Know if this looks any good... maybe just do two charts?

#BLP1
ggplot(lng[lng$BLP==1,], aes(Date,value, fill=name)) + geom_area() + 
  scale_y_continuous(labels = scales::percent, expand = c(0,0)) + theme_bw() +
  scale_fill_manual(values = c("red","orange","yellow","green")) +
  scale_x_datetime(expand=c(0,0)) + geom_line(position = "stack") +
  labs(x=NULL, y=NULL, fill=NULL) + theme(legend.position = "bottom")

#BLP2
ggplot(lng[lng$BLP==2,], aes(Date,value, fill=name)) + geom_area() + 
  scale_y_continuous(labels = scales::percent, expand = c(0,0)) + theme_bw() +
  scale_fill_manual(values = c("red","orange","yellow","green")) +
  scale_x_datetime(expand=c(0,0)) + geom_line(position = "stack") +
  labs(x=NULL, y=NULL, fill=NULL) + theme(legend.position = "bottom")
