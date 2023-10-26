#DATA PREPARATION FOR THE PLOT

#date must be a vector with the temporal labels
#av must be a vector with the studied variable values for the time labels of date
#spanx and spany represent the level of smoothing for the averages and rates of change (span argument of loess() function)

data_prep <- function(date, av, spanx = 0.7, spany = 0.7) {
  
  if(!is.character(date) || !is.numeric(date)) stop("date must be numeric or character")
  if(!is.numeric(av)) stop("av must be numeric")
  if(length(date) != length(av)) stop("Date and averages must have the same length")
  
  vl <- length(av)
  cuma <- sapply(2:(vl-1), function(ca) {mean(av[(ca-1):(ca+1)])}) #averages calculation
  cha <- unlist(sapply(2:(vl-1), function(ch) {av[ch+1] - av[ch-1]})) #rate of change calculation
  ll <- 1:length(cuma)
  locuma <- loess(cuma~ll, span = spanx) #smoothing
  locha <- loess(cha~ll, span = spany) #smoothing
  
  #data.frame construction
  df_dor <- cbind.data.frame(cum_avg = predict(locuma),
                                  ch_rate = predict(locha), 
                                  date = date[-c(1,length(date))], 
                                  cum_sum = head(av, -2), 
                                  yend = c(tail(predict(locuma), n=-1), NA), 
                                  xend = c(tail(predict(locha), n=-1), NA))
  df_dor$date <- factor(df_dor$date)
  
  return(df_dor)
}

#HELICAL PLOT FUNCTION

#dor must be the data.frame object created with the function data_prep
#col must be a palette to use with scale_colour_manual() function
#y_label is the name of the y axis

helical_plot <- function(dor, col = c("#006600", "#99CC99"), y_label = "Averages") {
  
  if(!is.data.frame(dor) && !colnames(dor) %in% c("cum_avg", "ch_rate", "date", "yend", "xend")) stop("dor must be a data.frame created with the function data_prep")
  if(is.character(col) && length(col) < nrow(dor)) col <- rep(col, nrow(dor))
  
  #labels preparation
  aest <- dor %>% 
    group_by(date) %>%
    summarise(first(cum_avg), first(ch_rate), first(xend), first(yend)) %>%
    rename(cum_avg = `first(cum_avg)`, ch_rate = `first(ch_rate)`, xend = `first(xend)`, yend = `first(yend)`)
  
  #plot
  if(is.character(col)) {
  p <- ggplot(dor, aes(x = ch_rate, y = cum_avg, xend = xend, yend = yend, color = as.factor(date))) +
    geom_segment(arrow = arrow(type='closed',length=unit(0.1, "inches")), show.legend = FALSE) +
    scale_colour_manual(values = col) +
    geom_text(data = aest,
              aes(x = ch_rate, y = cum_avg, label = date),
              size = 4, 
              hjust = -0.2, 
              show.legend = FALSE, 
              col = "black", 
              check_overlap = TRUE, 
              alpha = 1, 
              fontface = "bold") +
    ylab(y_label) +
    xlab("rate of change") +
    theme_minimal() +
    theme(axis.title = element_text(size = 15),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9))
  } else {
    p <- ggplot(dor, aes(x = ch_rate, y = cum_avg, xend = xend, yend = yend, color = as.factor(date))) +
    geom_segment(arrow = arrow(type='closed',length=unit(0.1, "inches")), show.legend = FALSE) +
    scale_colour_manual(palette = col) +
    geom_text(data = aest,
              aes(x = ch_rate, y = cum_avg, label = date),
              size = 4, 
              hjust = -0.2, 
              show.legend = FALSE, 
              col = "black", 
              check_overlap = TRUE, 
              alpha = 1, 
              fontface = "bold") +
    ylab(y_label) +
    xlab("rate of change") +
    theme_minimal() +
    theme(axis.title = element_text(size = 15),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9))
  }
  
  return(p)
}


