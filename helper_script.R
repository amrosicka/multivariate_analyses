# ------------------------------------------------------------------------#
# Load packages, functions & figures
# Authors: Anna Rosicka
# Last edited: June 2024
# ------------------------------------------------------------------------#

# Loading packages ---------------------------------------------------------
#install.packages("groundhog")
library(groundhog)

# List required packages here
packages <- c("readxl", "nestedcv", "caret", "glmnet", "splitstackshape", "tidyverse", "summarytools",
              "pagedown", "fastshap", "cowplot", "flextable", "psych", "polycor", "reshape2", "ggpubr",
              "car", "ggnewscale", "pROC", "MatchIt")

# Groudhog usage
# Groundhog installs and loads all packages in the version specified by groundhog.day.
# This might require you to restart the R session.
# Using R version 4.4.1 (2024-06-14)

groundhog.day <- "2024-07-10"
groundhog.library(packages, groundhog.day)

rm(packages, groundhog.day)

# Font installation -------------------------------------------------------
# First, in macOS Font Book, set the Default install location in Settings>Installation to "All Users".
# Next, you will need to manually install the font "Quattrocento Sans" to your computer.
# You can do so by going to https://fonts.google.com/specimen/Quattrocento+Sans and clicking "Download family".
# Then unzip the downloaded file and install the fonts "QuattrocentoSans-Regular.ttf" and "QuattrocentoSans-Bold.ttf" within by clicking on them.
# Finally, run the following:
#install.packages('extrafont')
#library(extrafont)
#font_import(pattern = "QuattrocentoSans-Regular.ttf")
#font_import(pattern = "QuattrocentoSans-Bold.ttf")


font.family <- "Quattrocento Sans" # Set font for the plots; font has to be installed manually, see helper_script.R
# font.family <- "sans" # Alternative in case you do not have Quattrocento Sans installed


# Figures settings -----------------------------------------------------------------
theme_main <- theme(
  panel.grid.major.x = element_line(color = "grey90"),
  panel.spacing = unit(1, "lines"),
  legend.justification = c(1, 0),
  legend.text = element_text(colour = "black", size = 20, family = font.family),
  legend.title = element_text(colour = "black", size = 20, face = "bold", family = font.family),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(colour = "black"),
  axis.ticks = element_line(colour = "black"),
  axis.title = element_text(size = 20, family = font.family),
  axis.text = element_text(colour = "black", size = 20, family = font.family),
  plot.background = element_rect(fill = "white"),
  plot.title = element_text(size = 25, hjust = 0.5, face = "bold", family = font.family)
)

my.palette = c("#55185D", "#ECB602", "#228B22", "#b2e061", "#447200", "#FFB5C0", "#6D8325FF", "#E5AD4FFF", "#BD5630FF"
)


## Functions -------------------------------------------------------------

# Function to calculate mode of a vector
calc_mode <- function(x) {
  t <- table(x)
  as.numeric(names(t)[which.max(t)])
}

# Average z-score across cognitive measures 
compute_avg_z <- function(data) {
  data %>%
    mutate(
      # main 3 objective outcomes only
      avg_z = (mm + srb + sra) / 3,
      # all available objective outcomes, weighted by game
      avg_z_all = ((srb + sra) / 2 + (mm + binding_cost_nameable + avgAll_Mean_proportion_of_selected_lures + avgAll_Mean_proportion_of_other_misses) / 4) / 2,
      # all available objective outcomes, not weighted
      avg_z_simple = (mm + srb)/2
    )
}

# Function to combine list of lists of data frames into a single data frame using tidyverse
combine_data_tidy <- function(list_of_lists) {
  # Use map to iterate over each sublist, unnest it, and add a list name as a column
  combined_df <- map_dfr(list_of_lists, bind_rows, .id = "dv")
  
  # Return the combined data frame
  return(combined_df)
}

