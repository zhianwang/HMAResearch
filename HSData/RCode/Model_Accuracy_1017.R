npr_accuracy <- read.csv("Result/modelaccuracy_npr_1017.csv")
totrevenue_accuracy <- read.csv("Result/modelaccuracy_totalrevenue_1017.csv")


npr_p <- unique(npr_accuracy[,c(1,5:7)])
totrev_p <- unique(totrevenue_accuracy[,c(1,5:7)])

library(ggplot2)
ggplot(npr_p, aes(x=a_Name, y=mad)) + geom_point()
ggplot(totrev_p, aes(x=a_Name, y=mad)) + geom_point()


ggplot(npr_p, aes(x=mad)) + geom_histogram() +
  geom_vline(aes(xintercept=mean(mad)),
               color="blue", linetype="dashed", size=1)

ggplot(npr_p, aes(x=mad)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 

# Stats ------------------------------------------------------------------------------------------------
median(npr_p$mad)
median(npr_p$mape)
median(npr_p$p)

median(totrev_p$mad)
median(totrev_p$mape)
median(totrev_p$p)

# Plot --------------------------------------------------------------------------------------------------
x$bins <- cut(x$rank, breaks=c(0,4,10,15), labels=c("1-4","5-10","10-15"))

#npr_p$p_bins <- cut(npr_p$p, breaks = c(0,0.8,0.9,0.95,1), labels = c("<=0.8","0.8-0.9","0.9-0.95","> 0.95"))
npr_p$p_label <- cut(npr_p$p, breaks = c(0,0.8,0.9,0.95,1), labels = c("Unqualified, precison rate<=0.8",
                                                                       "Qualified, 0.8<precison rate<0.9",
                                                                       "Good, 0.9<precison rate<0.95",
                                                                       "Excellent, precison rate>=0.95"))
levels(npr_p$p_label) <- gsub(", ", "\n", levels(npr_p$p_label))

totrev_p$p_label <- cut(totrev_p$p, breaks = c(-Inf,0.8,0.9,0.95,1), labels = c("Unqualified, precison rate<=0.8",
                                                                                "Qualified, 0.8<precison rate<0.9",
                                                                                "Good, 0.9<precison rate<0.95",
                                                                                "Excellent, precison rate>=0.95"))
levels(totrev_p$p_label) <- gsub(", ", "\n", levels(totrev_p$p_label))

nprcount <- npr_p %>% group_by(p_label) %>% summarise(count = n()) %>% mutate(Name = "NPR")
totcount <- totrev_p %>% group_by(p_label) %>% summarise(count = n()) %>% mutate(Name = "Total Revenue")
df <- npr_p %>% group_by(p_label) %>% summarise(count = n()) %>% mutate(Name = "NPR") %>%
      bind_rows(totrev_p %>% group_by(p_label) %>% summarise(count = n()) %>% mutate(Name = "Total Revenue"))

ggplot(npr_p, aes(x=p_label)) + geom_histogram(stat = "count", fill="#3e9199") #+
  scale_colour_gradient(low = "white", high = "black")

build_bar_plot(filter(df,Name == "NPR"), x_var = "p_label", y_var = "count", facet_var = "Name")

build_bar_plot(df, x_var = "p_label", y_var = "count", facet_var = "Name")

build_bar_plot <- function(
  # Adds a geom layer to a ggplot object based on user input.
  #
  # Args:
  plotdata,          # formatted data to plot
  x_var,             # Name of variable on x-axis
  y_var,             # Name of variable to plot on y-axis
  facet_var = "None",
  title = FALSE,     # True or False as to whether to show the plot title
  plot_title = NULL, # Title for the whole plot, default title is "x_var v.s. y_var"
  x_title = x_var,   # Title for x-axis, default value is the name of x variable
  y_title = y_var    # Title for y-axis, default value is the name of y variable
  # Returns:
  #   A ggplot object
){
  library(ggplot2)
  mainplot <- ggplot(data = plotdata) +
    geom_bar(aes_q(
      x = as.name(x_var),
      y = as.name(y_var),
      fill=as.name(y_var)
    ),
    stat = "identity", fill = "#3e9199", width = 0.85) +
    # add titles for x-axis and y-axis
    labs(y=as.name(y_title),x=as.name(x_title)) +
    # set numeric label attributes
    geom_text(aes_q(x = as.name(x_var),
                    y = as.name(y_var),
                    label=sapply(plotdata[,y_var],scaled_number)),
              vjust=-0.8,
              size=5,
              fontface=2,# 2(bold), 3(italic), 4(bold.italic)
              color=rgb(100,100,100, maxColorValue=255)) +
    # set theme
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "white"),
      strip.background = element_rect(fill ="#F4F4F4"),
      plot.background = element_rect(fill = "white", color="white"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(size=.1, color="gray"),
      panel.grid.minor.y = element_line(size=.1, color="lightgray"),
      axis.ticks = element_blank(),
      axis.text = element_text(size=13, face = "bold"),
      axis.text.x = element_text(margin = margin(t = -20, r = 0, b = 0, l = 0)),
      strip.text = element_text(size=15),
      axis.title.y = element_text(size = 15, face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))
      #plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5, size = 15)
    ) +
    # set y-axis label
    scale_y_continuous(
      labels = scaled_number
    )
  
  # add title if applicable
  if(!is.null(plot_title)){
    mainplot <- mainplot + ggtitle(as.name(plot_title))
  }
  # add faceting if requested
  if(facet_var != "None"){
    mainplot <- mainplot +
      facet_wrap(as.formula(paste0("~ `",facet_var, "`")))
  }
  
  # retuen the ggplot object
  return(mainplot)
}

build_bar_plot(df, x_var = "p_label", y_var = "count", facet_var = "Name",
               x_title = ' ',
               #plot_title = "Model Precision Rate Distribution",
               y_title = " # Health Systems")

scaled_number <- function(
  x # list of numbers you want to transform
){
  sapply(x, function(y){
    if(is.na(y)) return("NA")
    y_lab <- "yuge"
    if(abs(y) < 1e15) y_lab <- paste0(round(y/1e12), "T")
    if(abs(y) < 1e14) y_lab <- paste0(round(y/1e12, 1), "T")
    if(abs(y) < 1e13) y_lab <- paste0(round(y/1e12, 2), "T")
    if(abs(y) < 1e12) y_lab <- paste0(round(y/1e9), "B")
    if(abs(y) < 1e11) y_lab <- paste0(round(y/1e9, 1), "B")
    if(abs(y) < 1e10) y_lab <- paste0(round(y/1e9, 2), "B")
    if(abs(y) < 1e9) y_lab <- paste0(round(y/1e6), "M")
    if(abs(y) < 1e9) y_lab <- paste0(round(y/1e6, 1), "M")
    if(abs(y) < 1e7) y_lab <- paste0(round(y/1e6, 2), "M")
    if(abs(y) < 1e6) y_lab <- paste0(round(y/1000), "k")
    if(abs(y) < 1e5) y_lab <- as.character(round(y))
    #if(abs(y) < 1e4) y_lab <- paste0(round(y/1000, 2), "k")
    if(abs(y) < 1000) ylab <- as.character(round(y))
    if(abs(y) < 100) y_lab <- as.character(round(y,1))
    if(abs(y) < 10) y_lab <- as.character(round(y,2))
    return(y_lab)
  })
}
