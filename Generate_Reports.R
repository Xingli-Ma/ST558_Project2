library(rmarkdown)
library(tidyverse)

# Generate report for each day below
weekdays <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
              "Saturday")

# Numbers corresponding to days; 0 = Sunday, 1 = Monday, etc.
dayNum <- c(0:6)

# Create file names
output_file <- paste0(weekdays, "Analysis")

# Create a list for each day
params = lapply(dayNum, FUN = function(x){list(day = x)})

# Put into a data frame
reports <- tibble(output_file, params)

# Generate reports
pwalk(reports, render, input = "ST558_Project2.Rmd")
