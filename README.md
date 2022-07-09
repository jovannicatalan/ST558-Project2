# ST558-Project2
ST 558 Group Project 2: Predictive Modeling

## Purpose Description:
The purpose of this project is to analyze the [online news popularity data set](https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity) choose some predictive variables and create some numerical and graphical summaries based on those variables. We will fit both linear and ensemble regression models and compare the results to see which model performs the best using RMSE.  
We will also use report automation to generate individual reports for each data channel from the dataset. We first would split the data by data channel and proceed from there.


## R Packages:
* tidyverse
* caret
* stringr
* ggplot2
* leaps
* readr
* randomForest
* gbm

## Analysis Links:
[Business Data Channel Report](bus.html).  
[Entertainment Data Channel Report](entertainment.html).  
[lifestyle Data Channel Report](lifestyle.html).   
[Social Media Data Channel Report](socmed.html).  
[Tech Data Channel Report](tech.html).  
[World Data Channel Report](world.html). 

## Render Code:
<code>
apply(reports, MARGIN = 1,  
      FUN = function(x){ 
        rmarkdown::render(input = "ST558-Project2.Rmd",  
               output_format = "github_document",  
               output_file = x[[1]],  
               params = x[[2]],  
               output_options = list(html_preview= FALSE)) 
      })
</code>
