

# Load necessary libraries
     
load_or_install(c("tidyverse",
                  "ggrepel", 
                  "png", # For reading PNG files
                  "grid" # For creating graphical objects
                  ))

# Create the plot background ------------
df <- expand.grid(x = 1:1000, y = 1:1000) %>%
  mutate(value = (x + y) / (2 * max(x)))  # Calculate value for coloring

midpoint <- 0.66

background <- ggplot(df, aes(x = x, y = y, fill = value)) +
  geom_raster(interpolate = TRUE) +
  #scale_fill_gradientn(colors = c("white", "gold", "#ff9900"), values = c(0, midpoint, 1)) + # Original colours (e.g. IOC consensus)
  scale_fill_gradientn(colors = c("#C6D9FF",  "#3A92FF"), values = c(0, 1)) + # FIFA colours (e.g. Serner et al 2024)
  theme_void() +
  theme(
    legend.position = "none",
    aspect.ratio = 1,
    plot.margin = margin(0),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent")
  ) +
  coord_cartesian(expand = FALSE)

# Save the background plot as a PNG file with transparent background
ggsave("03_Analysis/matrix_background.png", plot = background, width = 7, height = 7, units = "in", dpi = 600, bg = "transparent")

# Read the PNG file and create a raster grob from the image
img_grob <- rasterGrob(readPNG("03_Analysis/Results/matrix_background.png"), width = unit(1, "npc"), height = unit(1, "npc"))



# Prepare matrix data -----------



# Function for creating dataframe for matrix figure -------




create_matrix_data <- function(caselist, exposure, grouping_vars = c()) {
  
  # Set options to remove scientific notation
  options(scipen = 999)
  
  # Function to calculate confidence intervals for incidence rate
  calculate_poisson_ci <- function(injuries, exposure) {
    result <- poisson.test(injuries, T = exposure, conf.level = 0.95)
    return(data.frame(xmin = result$conf.int[1] * 1000, xmax = result$conf.int[2] * 1000))
  }
  
  # Function to calculate mean and confidence interval for time loss
  calculate_mean_ci <- function(x) {
    n <- length(x)
    mean_val <- mean(x, na.rm = TRUE)
    se <- sd(x, na.rm = TRUE) / sqrt(n)
    t_val <- qt(0.975, df = n - 1)
    ci_lower <- mean_val - t_val * se
    ci_upper <- mean_val + t_val * se
    return(data.frame(y = mean_val, 
                      ymin = ci_lower, 
                      ymax = ci_upper))
  }
  
  # Filter caselist for the outcomes of interest (e.g., injuries)
  incidence_summary <- caselist 
  
  # If grouping variables are specified, group by them; otherwise, calculate overall incidence
  if (length(grouping_vars) > 0) {
    incidence_summary <- incidence_summary %>%
      group_by(across(all_of(grouping_vars)))
  }
  
  incidence_summary <- incidence_summary %>%
    summarize(
      n_cases = n(),
      total_timeloss = sum(timeloss, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      x = (n_cases / exposure) * 1000
    ) %>%
    rowwise() %>%
    mutate(
      calculate_poisson_ci(n_cases, exposure)
    ) %>%
    ungroup()
  
  # Calculate mean time loss and CI for each group
  if (length(grouping_vars) > 0) {
    timeloss_summary <- caselist %>%
      filter(timeloss > 0) %>%
      group_by(across(all_of(grouping_vars))) %>%
      summarize(calculate_mean_ci(timeloss), .groups = "drop")
    
    incidence_summary <- incidence_summary %>%
      left_join(timeloss_summary, by = grouping_vars)
  } else {
    timeloss_summary <- caselist %>%
      filter(timeloss > 0) %>%
      summarize(calculate_mean_ci(timeloss))
    
    incidence_summary <- incidence_summary %>%
      mutate(y = timeloss_summary$y,
             ymin = timeloss_summary$ymin,
             ymax = timeloss_summary$ymax)
  }
  
  incidence_summary <- incidence_summary %>%
    mutate(burden = total_timeloss/exposure*1000) %>%
    arrange(desc(burden))
  
  # Change the name of the grouping variable to "names"
  if (length(grouping_vars) > 0) {
    incidence_summary <- incidence_summary %>%
      rename(names = !!grouping_vars[1])
  } else {
    incidence_summary <- incidence_summary %>%
      mutate(names = "Overall")
  }
  
  return(incidence_summary)
}




exposure_input <- sum(data_exposure$total)
caselist_input <- caselist %>% filter(problem_type == "Injury", timeloss_cat == "timeloss")
grouping_vars <- c("osiics_15_level_1")





matrix_data <- create_matrix_data(caselist_input , exposure_input, grouping_vars )
print(matrix_data)


matrix_data <- matrix_data %>% filter(names != "Medical")





# Set axis limits (automatic)
xlim <- max(matrix_data$x)*1.1
ylim <- max(matrix_data$y, na.rm = TRUE)*1.1



# Set axis limits (manual)
# xlim <- 14
# ylim <- 80.5

# Adjust matrix data to ensure error bars do not extend beyond axis limits
matrix_data <- matrix_data %>%
  mutate(
    xmin = pmax(xmin, 0),      # Use pmax to limit xmin to 0
    ymin = pmax(ymin, 0),      # Use pmax to limit ymin to 0
    xmax = pmin(xmax, xlim),   # Use pmin to limit xmax to xlim
    ymax = pmin(ymax, ylim)    # Use pmin to limit ymax to ylim
  ) %>%
  arrange(desc(burden)) %>%
  slice_head(n = 7)   # Keep only the top 7 rows


# Function for adding reference lines
add_reference_line <- function(value, ylim, xlim) {
  list(
    geom_function(fun = function(x) value / x, color = "black", size = 0.2, alpha = 0.3, n = 1000,
                  xlim = c(value / ylim, xlim)),
    annotate("text", x = value / ylim - 0.005 * xlim, y = 0.98 * ylim,
             label = as.character(value), size = 1.8, alpha = 0.4, hjust = 1)
  )
}

# Define reference values for lines on the plot
# values <- c(5, 10, 20, 30, 40)  # ADAPT TO DATA - These are normal for FIFA tournaments
values <- c(10, 20, 50, 100)  # ADAPT TO DATA - These are adapted to the example dataset

# Create the main plot with matrix data
matrix_plot <- ggplot(matrix_data, aes(x = x, y = y)) +
  
  annotation_custom(img_grob, xmin = 0, xmax = xlim, ymin = 0, ymax = ylim) +   # Add background image
  
  do.call(c, lapply(values, add_reference_line, ylim=ylim, xlim=xlim)) +   # Add reference lines
  
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0, colour = "black", size = 0.3) +  
  
  geom_errorbarh(aes(xmin = xmin, xmax = xmax), height = 0, colour = "black", size = 0.3) + 
  
  geom_point(fill = "white", shape = 21, size = 2.1, colour="black") +  
  
  # Data labels positioned above and to the right of the points using ggrepel for better placement 
  geom_text_repel(data=matrix_data,
                  aes(label=names),
                  size=2.3,
                  nudge_y=0.8,
                  nudge_x=0.05,
                  segment.alpha=0.5,
                  point.padding=0.3,
                  colour="black",
                  family="Open Sans") +
  
  # Format axes with specified limits and breaks  
  scale_x_continuous(expand=c(0 ,0), limits=c(0 ,xlim), breaks=seq(0 ,xlim ,by=1), name="Incidence rate (per 1000 h)") +  # ADAPT TO DATA
  scale_y_continuous(expand=c(0 ,0), limits=c(0 ,ylim), breaks=seq(0 ,ylim ,by=20), name="Average time loss (days)") +  # ADAPT TO DATA
  
  # Define theme settings for aesthetics and layout 
  theme_void() +   # Start with a void theme (no gridlines or axes)
  theme(
    plot.margin=unit(c(t=0.3,r=0.3,l=0.3,b=0.3),"cm"),   # Set margins around the plot area 
    panel.border=element_rect(colour="black", fill=NA,size=0.8),   # Add border around the plotting area 
    axis.text.x=element_text(size=8 ,colour="black" ,margin=margin(t=3)),   # Style for x-axis text 
    axis.text.y=element_text(size=8 ,colour="black" ,margin=margin(r=3)),   # Style for y-axis text 
    axis.title.x=element_text(size=8 ,colour="black" ,margin=margin(t=7)),   # Style for x-axis title 
    axis.title.y=element_text(size=8 ,colour="black" ,angle=90 ,vjust=1 ,margin=margin(r=7)),   # Style for y-axis title 
    axis.ticks.length=unit(.1,"cm"),   # Length of tick marks on axes 
    axis.ticks.x=element_line(color="black"),   # Color of ticks on x-axis 
    axis.ticks.y=element_line(color="black"),   # Color of ticks on y-axis 
    text=element_text(family="Open Sans"),   # Font family for all text elements 
    legend.position="none",   # Hide legend 
    aspect.ratio=1             # Maintain equal aspect ratio 
  )


matrix_plot # view plot


# Save the final plot as a PNG file with specified dimensions and resolution.
png("03_Analysis/Results/risk_matrix.png", width=8,height=8 ,units="cm" ,res=600)

print(matrix_plot)   # Print the plot to the PNG device 

dev.off()   # Close the PNG device to finalize and save the file.
