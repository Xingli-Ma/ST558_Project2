## Project 2    

### Introduction        

The purpose is to create **predictive models** and automate **Markdown reports**. The day.csv file Bike Sharing Data Set was downloaded from [UCI Machine Learning Repository website](https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset). The day.csv file include 16 columns: instant, dteday, season, yr, mnth, holiday, weekday, workingday, weathersit, temp, atemp, hum, windspeed, casual, registered, cnt. Detailed description on these variables can be find from the data website. In our predictive models, we want to use cnt as response, other variables as predictors, such as season, weekday, workingda, weathersit, temp, atemp, hum, windspeed. We will create seven predictive models, one for each day in a week, to predict the number of bike users in each day of the week. The main contents include **Exploratory Data Analysis**, **Predictive Models**, **Model Validation and Comparison**, **Prediction and Results**, and **Conclusion and Discussion**.

```markdown
The following R packages are required to run the code to create this gitbub page.      

```{r}
require(rmarkdown)
require(knitr)
require(dplyr)
require(tidyverse)
require(ggplot2)
```             
### Data Exploratory and Data Preparation/Split    

-- Data Exploratory  

-- Data Preparation/Split    

### Modelling, Selection, & Prediction  

(Monday's Analysis)[MondayAnalysis.md]
(Tuesday's Analysis)[TuesdayAnalysis.md]
(Wednesday's Analysis)[WednesdayAnalysis.md]
(Thursday's Analysis)[ThursdayAnalysis.md]
(Friday's Analysis)[FridayAnalysis.md]
(Saturday's Analysis)[SaturdayAnalysis.md]
(Sunday's Analysis)[SundayAnalysis.md]

### Conclusion and Discussion      

-- Conclusion    

-- Discussion      

```

Reference
https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset
