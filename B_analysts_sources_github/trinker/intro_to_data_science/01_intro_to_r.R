##===========
## Intro to R
##===========

## R can be used as a calculator.  Use the operators +, -, *, / to add, 
##   subtract, multiply, and divide for a few math expressions (press `Run` OR 
##   `cntrl`/`cmd` + `enter`).









## Base R comes with a lot of useful features.  But there are add on packages
##   that can be installed to extend the capabilities of R.  Let's install the 
##   devtools & tidyverse packages. 




## Now load it via the `library()`  function.  You install once, load every time
##    you use R.
library(devtools)






## We can assign data to an object that can be accessed later.  Let's assign
##    some scalar numbers (unquoted) and strings (remember to quote them) to 
##    objects.








## We can build different data structures in R.  Two of the most useful are
##    vectors (a set of numbers) & data frames (tables).  Let's make the 
##    following and assign them to an object:


## Numeric vector:


## Character vector (made w/ strings):



## Data frame (made with equal length vectors):








## We can use the `View()` function to see a data frame spreadsheet style.  Try
##    it on the data frame you created above.






##=======================================
## Install a pre-built data set: Carnegie
##=======================================
## We will need a package that contains a data set with Carnegie classifications.
##   We have install the package from GitHub.  Run the 2 lines below to install 
##   the carnegie package.
if (!require("devtools")) install.packages("devtools"); library(devtools)
install_github('trinker/carnegie')






## Now load it via the `library()`  function.  You install once, load every time
##  you use R.







## View the `carnegie` data set that comes with the package via the `View()` 
##   function .  







## You can get help for package R objects by prefixing the object with a 
##     question mark (an alias for `help()`).  Try the following line of code.
?carnegie

## Try to get help on the plus sign (type `?+` into the console).  What happens?
##
## For reserved symbols (like arithmetic operators) we must use tick marks.  
##    These tick marks are useful any time you want to call an object that has
##    a name that would result in a parse error.





##====================
## A Taste of Plotting
##====================
## The package we will use for plotting is called `ggplot2`.  This package is
##   is based on Wilkonson's Grammar of Graphics.  We'll dive into visualization
##   deeper in future sessions but let's start the process now.  First, load
##   the ggplot2 package.






## Graphs use geometric shapes and their attributes to display variables in data
##   as a visual representation.  Stephen Few states that most questions can be
##   answered with bars, points, lines, or boxes.  ggplot2 works by starting with
##   data.  The user then adds geomteric shapes (geoms) to represent the data.
##   You have to map the variables to the attributes of the geom.  

## The basic boiler plate for a ggplot2 code chunk will be:
ggplot(data = <DATA>) + 
    <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

## What's a geom and and aesthetic?



## BARS
##
##   Let's start by looking at the distribution of states.  This is a count which  
##   is typically best shown with bars.
##
## What is are the five states with the largest number of institutions?





##  Let's try looking at the LOCALE distributions as well.





##  Let's try looking at the IPGRAD2015 distributions as well.  Any problems?
##      `coord_flip()` can be useful for when labels are long. 










## POINTS
##
## Points can be a useful way to explore bivariate relationships between 2 
## continuous variables. Let's explore some bivariate distributions between 
## SATV25 & SATM25.  Do you expect these variables to tightly correlate?  Use
## ggplot2 to build a scatterplot to explore this relationship.  
##
## Does anything look odd?  
## Are there any problems you can think of that a scatterplot with 4,665 
##   observations?  
## Why was there a warning message?


















## Let's try looking at other bivariate continuous  relationships. We'd expect 
##   FALLENR13 & FALLENR14 to correlate.  Do they?  Look at number of ROOMS and 
##   how it relates to FALLENR14.  What do you notice?  What questions does this 
##   spark?
##   Do you think there's a relationship between ROOMS and SATV25?


















## BOXES
##
## The boxplot is one way to see the distribution of continuous data.  Let's
##   look at the distribution of SATCMB25 scores.





## Let's add a group variable to make more than one boxplot (camparing is more
##  interesting).  Look at the distribution of SATCMB25 by ICLEVEL.  Is this 
## what you'd expect?




## Try the same comparison technique but comparing SATCMB25 values for CONTROL
##   groups.  Did you notice anything interesting?





## LINES
##
## Lines are powerful for showing change over time.  They also work well for 
##    summarizing raw data (e.g., a regression line).  The function `geom_smooth`
##    plots smoothed conditional means (essentially regression lines).  How can 
##    we get information about `geom_smooth`.



## Let's plot a smoothed line on the ROOMS and SATV25 bivariate continuous 
##    relationship.










## Combining Geom Layers
##
## The power of ggplot2 is in the power to combine geometric layers.  Combine 
##    the jittered points raw data with the summary line to represent the ROOMS  
##    and SATV25 bivariate continuous relationship.  Layering allows us to create
##    arbitrarily complex plots.






## Experiment and explore the data.  Create layered graphs using the tools we've 
##    used so far.





## GLOBAL VS. LOCAL PLOT SETTINGS
##
## In these plots we typed `data = carnegie, aes(x = ROOMS, y = SATV25)` multiple
##    times.  This is tedious and slows down analysis.  The `data` and `aes` can 
##    be set globally inside of `ggplot()` and this is passed to the rest of the 
##    geom layers.  Note that you can set data and aesthetics globally but still
##    override them with other data and/or aesthetics if you choose.  
##
## Let's reduce the duplicate typing in the prior code chunk with the jittered 
##    points and smoothed regression line, seen above, by setting
##    the data and aesthetics globally. 
ggplot() +
    geom_jitter(data = carnegie, aes(x = ROOMS, y = SATV25), width = 50, height = 5) +
    geom_smooth(data = carnegie, aes(x = ROOMS, y = SATV25))


