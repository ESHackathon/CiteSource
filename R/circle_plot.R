#' Create circle plot showing overlap in duplicate records between sources
#' 
#' @description Visualise overlap across databases with a circular plot, 
#' sources are indicated by colour and the level of overlap (i.e. number 
#' of databases records are shared across) indicated by bar sequence 
#' (and label).
#' @param 
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarize rowwise mutate
#' @importFrom ggplot2 ggplot geom_bar geom_segment aes annotate theme_minimal theme element_blank geom_text
#' @importFrom grid unit
#' @return 
#' @export
#' @examples 
#' \dontrun{
#' googlesheets4::gs4_deauth()
#' data <- googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/11s6yP0F4WnQqL7z8ulcWBwknZhu-YnqHWIDq8Zd1qnU/edit#gid=0')
#' plot <- circle_plot(data)
#' plot
#' }
circle_plot <- function(data){
  
  # the following is adapted from : https://www.r-graph-gallery.com/297-circular-barplot-with-groups.html
  # Set a number of 'empty bar' to add at the end of each group
  empty_bar <- 3
  to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
  colnames(to_add) <- colnames(data)
  to_add$group <- rep(levels(data$group), each=empty_bar)
  data <- rbind(data, to_add)
  data <- data %>% arrange(group)
  data$id <- seq(1, nrow(data))
  
  # Get the name and the y position of each label
  label_data <- data
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust <- ifelse( angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle+180, angle)
  
  # prepare a data frame for base lines
  base_data <- data %>% 
    group_by(group) %>% 
    summarize(start=min(id), end=max(id)) %>% 
    rowwise() %>% 
    mutate(title=mean(c(start, end)))
  
  # prepare a data frame for grid (scales)
  grid_data <- base_data
  grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
  grid_data$start <- grid_data$start - 1
  grid_data <- grid_data[-1,]
  
  # Make the plot
  p <- ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
    
    geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
    
    geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
    ylim(-100, (round(max(data$value), digits = -2)*1.2)) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(rep(-1,4), "cm") 
    ) +
    coord_polar() + 
    geom_text(data=label_data, aes(x=id, y=value+10, label=label, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
    
    # Add base line information
    geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.2, size=0.6 , inherit.aes = FALSE) 
  
  p
  
  return(p)
  
}
