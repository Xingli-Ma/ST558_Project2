```markdown
## Project 2: Creating Predictive Models and Automating Markdown Reports on Bike Sharing Data Set      

### Introduction        

The purpose of this project is to create predictive models and automate Markdown reports. The day.csv file Bike Sharing Data Set was downloaded from [UCI Machine Learning Repository website](https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset). The day.csv file includes 16 variables: instant, dteday, season, yr, mnth, holiday, weekday, workingday, weathersit, temp, atemp, hum, windspeed, casual, registered, cnt. Detailed descriptions on these variables can be found from the [data website](https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset). In our predictive models, we use cnt as response, other variables as predictors, such as season, yr, mnth, holiday, weekday, workingday, weathersit, temp, atemp, hum, windspeed. First, We filtered the data set by each day in the week, and split the day data into training and testing sets(7:3). Then, we did numerical and graphical summaries of the training data set. Then, we trained four predictive models on the training data set, and tested all four models on the testing set. We compared and selected the best model based on the smallest RMSE value. Finally, we automated the whole analysis processes to predict the number of bike users on each day of the week, and produced one report for each day in a week.                
     
**Note**: The following R packages are required to run the code to create this gitbub page.      

```{r}
require(rmarkdown)
require(knitr)
require(tidyverse)
require(dplyr)
require(ggplot2)
require(caret)
require(DT)
require(corrplot)
```        


### Data Exploration Analysis        

You can find the numerical and graphical summaries on each day of the week by clicking the link to the corresponding report.   

### Modelling, Selection, and Prediction     

Please see the detailed report for the best model that we select for each day of the week to predict the number of rental bikes.    

[Monday's Analysis](https://xingli-ma.github.io/ST558_Project2/MondayAnalysis.html)     

[Tuesday's Analysis](https://github.com/Xingli-Ma/ST558_Project2/blob/main/TuesdayAnalysis.md)     

[Wednesday's Analysis](https://github.com/Xingli-Ma/ST558_Project2/blob/main/WednesdayAnalysis.md)     

[Thursday's Analysis](https://github.com/Xingli-Ma/ST558_Project2/blob/main/ThursdayAnalysis.md)      

[Friday's Analysis](https://github.com/Xingli-Ma/ST558_Project2/blob/main/FridayAnalysis.md)     

[Saturday's Analysis](https://github.com/Xingli-Ma/ST558_Project2/blob/main/SaturdayAnalysis.md)     

[Sunday's Analysis](https://github.com/Xingli-Ma/ST558_Project2/blob/main/SundayAnalysis.md)         
```


### Reference    

https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset


