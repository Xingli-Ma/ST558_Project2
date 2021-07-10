
library(tidyverse)
library(haven)
library(knitr)
options(dplyr.print_min = 5)
options(tibble.print_min = 5)
opts_chunk$set(echo = TRUE, eval = TRUE, error = TRUE, warning = FALSE, message = FALSE, cache = TRUE)

dayData <- read_csv("day.csv")
dayData

#rmarkdown::render("ST558_Project2.Rmd", output_file = "MondayAnalysis.html",
#                  params = list(day = "Monday"))

data.frame(output_file = "MondayAnalysis.html", params = list(day = "Monday"))

# Generate report for each day below
weekdays <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
              "Saturday")

# Numbers corresponding to days; 0 = Sunday, 1 = Monday, etc.
dayNum <- c(0:6)

# Create file names
output_file <- paste0(weekdays, "Analysis.md")


#get unique days
dayIDs <- unique(dayData$weekday)

#create filenames
#output_file <- paste0(dayIDs, ".html")

#create a list for each team with just the team name parameter
params = lapply(dayIDs, FUN = function(x){list(day = x)})

#put into a data frame 
reports <- tibble(output_file, params)

reports

library(rmarkdown)
## #need to use x[[1]] to get at elements since tibble doesn't simplify
apply(reports, MARGIN = 1,
      FUN = function(x){
        render(input = "ST558_Project2.Rmd", output_file = x[[1]], params = x[[2]])
      })