#####################################
#
# R Codes to generate cohort life table
#
#####################################


### Citation: Kendy T. Teng, Dave C. Brodbelt, Camilla Pegram, David B. Church, Dan G. ONeill. 
###           2021. Life tables of annual life expectancy and mortality for dogs in the United Kingdom. 
###           XXX


### Instruction:
# 1) Load the libraries 
# You will need to install them if you haven't 
# install.packages("rio")
# install.packages("tidyverse")
# install.packages("janitor")
library(rio) # for reading the data
library(tidyverse) # need for the functions as well
library(janitor) 

# You will have to set your working directory and put the files in there.

# 2) Load the functions in 'lifetable_func_oa'
source('lifetable_func_oa.R')

# 3) Load the data and do some descriptive
df <- import("the_dataset.xlsx")
str(df)
df %>% select_if(is.numeric) %>% map(., summary)
df %>% select_if(is.character) %>% map(., tabyl)
# Important! The column used to estimate life expectancy has to be called 'lifespan'

# 4) Simple complete cohort life table
# 4.1) Give it a try
lt <- lifetable(data = df); lt
# 4.2) Set the last year of the life table with 'top'
lt_1 <- lifetable(data = df, top = 19); lt_1

# 4.3) 'top_min' 
#      *Set the minimal number of observations for the last year (default is 11)
#      *If the last year had fewer than 'top_min', its observations will be combined 
#       with the ones in the previous year.
#      *'top_min' will override 'top' if the number of observations in the year set by 'top' 
#       is less than 'top_min'       
lt_2 <- lifetable(data = df, top_min = 200); lt_2

# 4.4) 'cell_min'
#      Set the minimal number of observations for all years (default is 3)
#      If 'cell_min' is not satisfied, it will return an error.
lt_3 <- lifetable(data = df, cell_min = 30); lt_3

# 5) Complete cohort life table with a confidence interval using empirical bootstrapping
# 5.1) Give it a try: 'size' and 'times' will have to be specified. 
#                     'size': put the number of observations
#                     'time': the times of iterations determined by you
lt_ci <- lifetable_ci(data = df, size = nrow(df), times = 10); lt_ci
# 5.2) 'top', 'top_min', and 'cell_min' can all be set as well
lt_ci_1 <- lifetable_ci(data = df, size = nrow(df), times = 10, top = 19, cell_min = 50); lt_ci_1
# 5.3) Set seed
lt_ci_2 <- lifetable_ci(data = df, size = nrow(df), times = 10, seed = 123); lt_ci_2
lt_ci_3 <- lifetable_ci(data = df, size = nrow(df), times = 10, seed = 111); lt_ci_3
# 5.4) Confidence interval: default is 95%
lt_ci_2
lt_ci_4 <- lifetable_ci(data = df, size = nrow(df), times = 10, seed = 123, ci = 0.98); lt_ci_4

# 6) Cohort life table for a subpopulation
# 6.1) Female
lt_ci_female <- lifetable_ci(data = filter(df, sex == "Female"), times = 50, seed = 1, 
                             size = filter(df, sex == "Female") %>% nrow(), top = 18)
lt_ci_female
# 6.2) Neuter female
lt_ci_nf <- lifetable_ci(data = filter(df, sex == "Female", neuter == "Neutered"), 
                         size = filter(df, sex == "Female", neuter == "Neutered") %>% nrow(),
                         times = 50, seed = 1, top_min = 50)
lt_ci_nf


### A further example
## Make a dataset
set.seed(1)
ds <- data.frame('id' = seq(500),
                 'ls' = rbeta(500, 2, 1.8)*20)

## Change the name of lifespan column into 'lifespan'
names(ds)[2] <- 'lifespan'

## Life tables
lifetable_0(data = ds)
lifetable(data = ds, cell_min = 4, top_min = 10)
lifetable(data = ds, cell_min = 5, top_min = 10)
lifetable_ci(data = ds, size = nrow(ds), times = 50, seed = 1, cell_min = 4, top_min = 20)

