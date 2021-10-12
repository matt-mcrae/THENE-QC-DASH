FLOSS <- function(data, ndays){
  
  st <- today() - ndays
  
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
  
  floss.plot <- data %>% filter(DATE >= st) %>%
    ggplot(aes(DATE,value,fill=category)) +
    theme_bw() +
    geom_area() +
    geom_line(position = "stack", colour = "dimgrey") +
    scale_x_datetime(date_breaks = "1 day", date_labels = datel) +
    scale_y_continuous(labels = scales::percent, expand = c(0,0)) +
    scale_fill_manual(values = c("red","orange","yellow","green")) +
    theme(legend.position = "top",
          legend.text = element_text(face = "bold", size = 12),
          axis.text.x = element_text(angle = datea, vjust = datev, hjust=1,
                                     face = "bold", size = 12),
          axis.text.y = element_text(face = "bold", size = 12),
          strip.text.x = element_text(size = 14, face = "bold")) +
    labs(x=NULL, y=NULL, fill=NULL) +
    facet_wrap(~EQ_NAME)
  
  return(suppressMessages(print(floss.plot)))
  
}