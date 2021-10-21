FLOSS <- function(data, ndays){
  
  st <- today() - ndays
  
  #Update date axis tick marks based on how many days are selected
  if(ndays<=7){
    datea <- 45
    datel <- "%a"
    datev <- 0.5} else if(ndays<=21){
      datea <- 45
      datel <- "%d-%b"
      datev <- 1} else{
        datea <- 90
        datel <- "%d-%b"
        datev <- 0.5}
  
  #Check if there are any data in the date range
  if(nrow(data %>% filter(DATE >= st)) > 0){
  #...and create the plot if so
  floss.plot <- data %>% filter(DATE >= st) %>%
    ggplot(aes(DATE,value,fill=category)) +
    theme_bw() +
    geom_area() +
    geom_line(position = "stack", colour = "dimgrey") +
    scale_x_datetime(date_breaks = "1 day", date_labels = datel) +
    scale_y_continuous(labels = scales::percent, expand = c(0,0)) +
    scale_fill_manual(values = c("lightcoral","lightgoldenrod","lightgreen")) +
    theme(legend.position = "top",
          legend.text = element_text(face = "bold", size = 16),
          axis.text.x = element_text(angle = datea, vjust = datev, hjust=1,
                                     face = "bold", size = 14),
          axis.text.y = element_text(face = "bold", size = 14,),
          strip.text.x = element_text(size = 16, face = "bold"),
          panel.spacing.x = unit(1.5, "lines")) +
    labs(x=NULL, y=NULL, fill=NULL) +
    facet_wrap(~EQ_NAME)
  
  #If no data, give an error message (as a plot) to the user
  } else {floss.plot <- ggplot(tibble(), aes(0,0, label="NO DATA IN DATE RANGE")) + 
    geom_text(size=10) + theme_void()}
  
  return(floss.plot)
  
}