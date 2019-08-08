# Vincent F. A. Meyer zu Wickern
# Course: Data Analysis and Statistics
# Description: THis is the submission for the data analysis
#              exam at the Hochschule Rhein Waal for Vincent
#              F. A. Meyer zu wickern. The impact of age, edu-
#              cation, and age on wage are examined on the 
#              basis of data from the Current Population Survey.
#             The numbering in this script is the same as for 
#             the paper 

# The table of contents is designed to match the chapter numbers of the paper
# "Impact analysis of education, age, and year on the wage of male workers in the US Mid-Atlantic region"
# to easily find the implementation for the paper here or the longer descriptions in the paper

# Table of Contents:
# 0 Preparation of the environment
# 1-2 (Left out to match paper numbering)
# 3 Distribution of variables
#   3.1 Distribution of education
#   3.2 Distribution of age
#   3.3 Distribution of years
#   3.4 Distribution of wage
# 4 Bivariate data analyses
#   4.1 Association between education and wage
#   4.2 Association between age and wage
#   4.3 Association between year and wage 
#   4.4 Association between year and education
#   4.5 Association between age and education 
#   4.6 Association between year and age
# 5 Analysis of partial correlations
#   5.1 Partial correlation between education and wage
#   5.2 Partial correlation between age and wage
#   5.3 Partial correlation between year and wage 
# 6 Regression modeling
#   6.1 Creation of a regression equation
#   6.2 Examination of regression assumptions
#   6.3 Adjustment of the regression model to meet regression assumptions
#   6.3.1 Box-Cox transformation
#   6.3.2 Regression with education as a numerical value
#   6.3.3 Robust regression .
#   6.3.4 Weighted least squares regression
#   6.4 Quantile regression

# 11-13 Techniques for Multivariate Setups 
#       --> Describing main and interaction effects
# 11) Grouped scatterplots
# 12) Partial Correlation
# 13) Multiple Regression

#--- 3. Preparation of the environment  -----------------------------

#----------------------------------------------------------
# Reset R's brain
#----------------------------------------------------------
rm(list=ls())

#----------------------------------------------------------
# Reset graphic device
#----------------------------------------------------------
while(!is.null(dev.list()))
{
  dev.off()
}

# --- Load packages if necessary ---------------------------

# ISLR. contains the necessary data

if (!require(ISLR)) install.packages('ISLR')
library(ISLR)

# tidyverse: 
# - ggplot2 (part of the tidyverse) used for visualizations
# - dplyr (part of the tidyverse) used for dataframe operations

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

# e1071: Contains multiple statistics functions

if (!require(e1071)) install.packages('e1071')
library(e1071)

# reshape2: Necessary for dataframe/tibble format changes
 
if (!require(reshape2)) install.packages('reshape2')
library(reshape2)

# lmtest: Contains functions to check autocorrelation between errors

if (!require(lmtest)) install.packages('lmtest')
library(lmtest)

# car: Contains functions to check homoskedasticity

if (!require(car)) install.packages('car')
library(car)

# caret: Advanced machine learning techniques, can identify and cope with multicollinearity

if (!require(caret)) install.packages('caret')
library(caret)

# corrplot: Used for correlation plots

if (!require(corrplot)) install.packages('corrplot')
library(corrplot)

# ppcor: Used for partial correlation

if (!require(ppcor)) install.packages('ppcor')
library(ppcor)

# MASS: Used for regression selection and robust regression

if (!require(MASS)) install.packages('MASS')
library(MASS)

# foreign: Used for robust regression (handling outliers)

if (!require(foreign)) install.packages('foreign')
library(foreign)

# quantreg: Used for quantile regression (handling homoskedasticity)

if (!require(quantreg)) install.packages('quantreg')
library(quantreg)

#--- Load sample ------------------------------------------
# This part was added as specified in the task description

attach(Wage)
assessment_dataframe <- Wage[sample(nrow(Wage), 3000), ]

# The relevant attributes year, education, wage, are filtered
assessment_dataframe <- assessment_dataframe %>% dplyr::select(year, age, education, wage)


#--- 3. Distribution of variables  -----------------------------

# Show the structure of the dataset
str(assessment_dataframe)

# Show the summary of the dataset to get a feeling of which values are contained
summary(assessment_dataframe)

# Define a function to present descriptive features of a numerical variable
show_desc_stats <- function(variable,variable_name){
  
  cat(paste(paste("Description of variable: ",variable_name)
            ,paste("Median:", median(variable), sep = "\t\t\t\t")
            ,paste("Mean: ", mean(variable), sep = "\t\t\t\t")
            ,paste("Quantile 1: ", quantile(variable,0.25), sep = "\t\t\t")
            ,paste("Quantile 3: ", quantile(variable,0.75), sep = "\t\t\t")
            ,paste("Variance: ", var(variable), sep = "\t\t\t")
            ,paste("Standard Dev.: ", sd(variable), sep = "\t\t\t")
            ,paste("Coefficient of variation: ", sd(variable) / mean(variable), sep = "\t")
            ,paste("Kurtosis: ", kurtosis(variable), sep = "\t\t\t")
            ,paste("Skewness: ", skewness(variable), sep = "\t\t\t")
            ,paste("Min:", min(variable),sep = "\t\t\t\t")
            ,paste("Max:", max(variable),sep = "\t\t\t\t")
            ,sep="\n"))
}


# -- 3.1 -  Distribution of education ---------------------------

# Create a barplot for the education of participants

ggplot(assessment_dataframe) + 
  geom_bar(mapping = aes(education), 
           stat = "count",
           color = 'cadetblue4',
           fill = 'cadetblue1') +
  ggtitle('Distribution of education levels')




# -- 3.2 - Distribution of age ---------------------------------

# Create a histogram for the age of participants with bins of size 8 from age 16 to 100

ggplot(assessment_dataframe) +
  geom_bar(mapping =aes(age),
                 color = 'cadetblue4',
                 fill = 'cadetblue1',
                 stat = 'density') + 
  ggtitle('Distribution of age')

# Present descriptive statistics of the variable

show_desc_stats(assessment_dataframe$age,'age')


# -- 3.3 - Distribution of years -------------------

# Create a histogram of the years

ggplot(assessment_dataframe) +
  geom_bar(mapping =aes(year),
           color = 'cadetblue4',
           fill = 'cadetblue1')  +
  ggtitle('Distribution of years')

# Present descriptive statistics of the variable

show_desc_stats(assessment_dataframe$year, 'year')

# To show how many records are given per year, 
# the year attribute is converted to a factor and described
# with a summary

summary(as.factor(assessment_dataframe$year))


# -- 3.4 - Distribution of wage -------------------

# Create a histogram of the wage

ggplot(assessment_dataframe) +
  geom_bar(mapping =aes(wage),
                 stat = "density",
           color = 'cadetblue4',
           fill = 'cadetblue1'
          ) +
  xlab('wage in USD 1,000') +
  ggtitle('Distribution of wage')

# Present descriptive statistics of the variable

show_desc_stats(assessment_dataframe$wage, 'wage')

# ---- 4 - Bivariate data analyses ------------------

# ---- 4.0 - Data preprocessing -------------------------

# Convert education to additional, ordinal value ---------

assessment_dataframe[,"ed_ordinal"] <- as.integer(assessment_dataframe[,"education"])


# ---- 4.1 - Correlation: Association between education and wage ---------

# Visual assessment with a boxplot diagram

boxplot(wage ~ education,data=assessment_dataframe, 
        main="Boxplot - Wage by education",
        xlab="education", 
        ylab="wage in USD 1,000",
        horizontal=FALSE)

# Visual assessment of wage distribution by education with a histogram facet grid

ggplot(assessment_dataframe, aes(wage)) +
  geom_bar(stat = "density",
           color = 'cadetblue4',
           fill = 'cadetblue1') +
  facet_grid(education ~ .) +
  xlab('wage in USD 1,000') +
  ggtitle('Facet grid histograms - Wage by education')
   
 
# Spearman's rank correlation coefficient, rho
cor(assessment_dataframe[,c("ed_ordinal","wage")], method = "spearman" )

cor.test(assessment_dataframe[,"ed_ordinal"],
         assessment_dataframe[,"wage"], 
         method = "spearman" )

# Kendall's tau rank correlation coefficient
cor(assessment_dataframe[,c("ed_ordinal","wage")], method = "kendall" )

cor.test(assessment_dataframe[,"ed_ordinal"],
         assessment_dataframe[,"wage"], 
         method = "kendall" )


# --- 4.2 - Association between age and wage ----------------

# Scatter Plot with regression line: Age -> Wage

ggplot(assessment_dataframe,mapping = aes(age, wage)) +
  geom_point() +
  geom_smooth() +
  ylab('wage in USD 1,000') +
  ggtitle('Scatter plot - age/wage') 


# Measure correlation between age and wage

cor(assessment_dataframe[,c("age","wage")], method = "pearson" )
cor(assessment_dataframe[,c("age","wage")], method = "spearman" )
cor(assessment_dataframe[,c("age","wage")], method = "kendall" )

cor.test(assessment_dataframe[,"age"],
         assessment_dataframe[,"wage"], 
         method = "pearson" )


cor.test(assessment_dataframe[,"age"],
         assessment_dataframe[,"wage"], 
         method = "spearman" )


cor.test(assessment_dataframe[,"age"],
         assessment_dataframe[,"wage"], 
         method = "kendall" )


# Scatter plot age -> wage supported by education also

ggplot(assessment_dataframe, aes(x = age, y = wage)) +
  geom_point() +
  geom_smooth(method = 'loess') +
  facet_grid(education ~ .) +
  ylab('wage in USD 1,000') +
  ggtitle('Facet grid histograms - wage by education')

# Minimum age of Advanced Degree holders in the dataset
  
min(filter(assessment_dataframe,education == '5. Advanced Degree')$age)

# --- 4.3 - Association between year and wage ----------------

# Scatter plot year to wage

ggplot(assessment_dataframe,mapping = aes(year, wage)) +
  geom_point() +
  stat_summary(color = 'cadetblue', geom = 'line') +
  ylab('wage in USD 1,000') +
  ggtitle('Scatter plot - year/wage')

# Boxplot plot year to wage

ggplot(assessment_dataframe,mapping = aes(as.factor(year), wage)) +
  geom_boxplot() +
  labs(title = 'Boxplot - year/wage', x = 'year', y = 'wage in USD 1,000') 


# Mean / median Wage development over time

# Mean wage aggregation

mean_wage_year <- assessment_dataframe %>% 
  dplyr::select(year, wage)  %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarize(mean_wage = mean(wage, na.rm = TRUE))

# Line plot: mean wage over time (solid) and trend line (dashed)

ggplot(mean_wage_year, mapping = aes(x = year, y = mean_wage)) + 
  geom_line(size = 1, linetype = 'solid') + 
  geom_smooth(method = "lm",linetype = 'dashed', se = FALSE) +
  labs(title = 'Line plot and linear regression line - year/mean wage', x = 'year', y = 'mean wage in USD 1,000')

# Correlation assessment Pearson, Spearman and Kendall

cor(assessment_dataframe[,c("year","wage")], method = "pearson" )
cor(assessment_dataframe[,c("year","wage")], method = "spearman" )
cor(assessment_dataframe[,c("year","wage")], method = "kendall" )

cor.test(assessment_dataframe[,"year"],
         assessment_dataframe[,"wage"], 
         method = "pearson" )

cor.test(assessment_dataframe[,"year"],
         assessment_dataframe[,"wage"], 
         method = "spearman" )

cor.test(assessment_dataframe[,"year"],
         assessment_dataframe[,"wage"], 
         method = "kendall" )



# ---- 4.4 - Association between year and education ---------

# Goal: Visually present the correlation between the attributes year and education
# Add education ratios into a dataframe to be able to analyze the education 
# development over time.
# First, the number of people surveyed in a year is counted.
# Second, the number of people per year with a particular education are counted 
# in joined to the same dataframe.
# Third, the number of people with a certain education in a year is divided by
# the number of all people asked in that year.
# Fourth, the columns with the ratios have to be stacked (melted with package 
# reshape2) to be able to visualize them.
# Fifth, the education development over time can be visualized.
# Sixth, the change in education ratios is given out in a table.

# 1. Count the number of people surveyed in a year

education_ratio <- assessment_dataframe %>% dplyr::count(year)

# 2. Count the number of people in a year with a particular education

education_ratio <-
  education_ratio %>% left_join(
    dplyr::rename(
      dplyr::count(
        dplyr::select(
          filter(assessment_dataframe, education == '1. < HS Grad'), 1)
        , year)
      ,count_education_level_1 = n), 
    by = 'year')

education_ratio <-
  education_ratio %>% left_join(
    dplyr::rename(
      dplyr::count(
        dplyr::select(
          filter(assessment_dataframe, education == '2. HS Grad'), 1)
        , year)
      ,count_education_level_2 = n), 
    by = 'year')

education_ratio <-
  education_ratio %>% left_join(
    dplyr::rename(
      dplyr::count(
        dplyr::select(
          filter(assessment_dataframe, education == '3. Some College'), 1)
        , year)
      ,count_education_level_3 = n), 
    by = 'year')

education_ratio <-
  education_ratio %>% left_join(
    dplyr::rename(
      dplyr::count(
        dplyr::select(
          filter(assessment_dataframe, education == '4. College Grad'), 1)
        , year)
      ,count_education_level_4 = n), 
    by = 'year')

education_ratio <-
  education_ratio %>% left_join(
    dplyr::rename(
      dplyr::count(
        dplyr::select(
          filter(assessment_dataframe, education == '5. Advanced Degree'), 1)
        , year)
      ,count_education_level_5 = n), 
    by = 'year')

# 3. Divide the number of people with a certain education by all surveyed people in that year

education_ratio <- education_ratio %>% mutate(ratio_education_level_1 = count_education_level_1 / n)
education_ratio <- education_ratio %>% mutate(ratio_education_level_2 = count_education_level_2 / n)
education_ratio <- education_ratio %>% mutate(ratio_education_level_3 = count_education_level_3 / n)
education_ratio <- education_ratio %>% mutate(ratio_education_level_4 = count_education_level_4 / n)
education_ratio <- education_ratio %>% mutate(ratio_education_level_5 = count_education_level_5 / n)

# 4. Stack (melt) the columns with the ratios to be able to visualize them.

# Melting (stacking) is necessary to show a legend in ggplot
ed_filtered <- education_ratio %>% dplyr::select(c(year,ratio_education_level_1, ratio_education_level_2, ratio_education_level_3, ratio_education_level_4, ratio_education_level_5))
education_ratio_stacked = melt(ed_filtered,id.vars = "year", 
                               measure.vars = c("ratio_education_level_1", "ratio_education_level_2", "ratio_education_level_3", "ratio_education_level_4", "ratio_education_level_5"),
                               variable.name = 'education_level',
                               value.name = 'ratio') 


education_ratio_stacked$education_level <- dplyr::recode(education_ratio_stacked$education_level, ratio_education_level_1 = "1. < HS Grad", 
                                                                                            ratio_education_level_2 = "2. HS Grad",
                                                                                              ratio_education_level_3 = "3. Some College",
                                                                                              ratio_education_level_4 = "4. College Grad",
                                                                                              ratio_education_level_5 = "5. Advanced Degree")



# 5. Visualize the education development over time.

ggplot(education_ratio_stacked, mapping = aes(x = year, y = ratio*100,color = education_level)) + 
  geom_line(size = 1, linetype = 'solid') + 
  geom_smooth(method = "lm", linetype = 'dashed', se = FALSE) +
  labs(x = 'year', y = 'ratio of workers with a particular education level in %', 
       title = 'Development of education mix over time') 
  

# 6. Show the change in education ratios in a table.
# Add table here with numbers of how the values changed from 2003 to 2009


description <- rbind(c('2003 - ratio'),c('2009 - ratio'))

education_change <- cbind(description,education_ratio %>% filter(year %in% c(2003,2009)) %>% dplyr::select(ratio_education_level_1,
                                                   ratio_education_level_2,
                                                   ratio_education_level_3,
                                                   ratio_education_level_4,
                                                   ratio_education_level_5))

education_change$description <- as.character(education_change$description)

ratio_change <- cbind('ratio change 2003 to 2009',
                      education_change[education_change['description'] == '2009 - ratio', 
                                       c('ratio_education_level_1',
                                         'ratio_education_level_2',
                                         'ratio_education_level_3', 
                                         'ratio_education_level_4',
                                         'ratio_education_level_5')] -
                        education_change[education_change['description'] == '2003 - ratio', 
                                         c('ratio_education_level_1',
                                           'ratio_education_level_2',
                                           'ratio_education_level_3', 
                                           'ratio_education_level_4',
                                           'ratio_education_level_5')])

education_levels <- c("1. < HS Grad","2. HS Grad", "3. Some College", "4. College Grad", "5. Advanced Degree")
  
colnames(ratio_change) <- c('description',education_levels)
colnames(education_change) <- c('description',education_levels)

education_change <- rbind(education_change, ratio_change)

print('Change of education ratios:')
print(education_change)

# 7. Calculate correlation of year to education

cor(assessment_dataframe[,c("year","ed_ordinal")], method = "spearman" )
cor(assessment_dataframe[,c("year","ed_ordinal")], method = "kendall" )


cor.test(assessment_dataframe[,"year"],
         assessment_dataframe[,"ed_ordinal"], 
         method = "spearman" )

cor.test(assessment_dataframe[,"year"],
         assessment_dataframe[,"ed_ordinal"], 
         method = "kendall" )


# ---- 4.5 - Association between age and education ---------


# 1. Count the number of people surveyed for an age

age_education_ratio <- assessment_dataframe %>% dplyr::count(age)


# 2. Count the number of people in a year with a particular education



age_education_ratio <-
  age_education_ratio %>% left_join(
    dplyr::rename(
      dplyr::count(
        dplyr::select(
          filter(assessment_dataframe, education == '1. < HS Grad'), 2)
        , age)
      ,count_education_level_1 = n), 
    by = 'age')

age_education_ratio <-
  age_education_ratio %>% left_join(
    dplyr::rename(
      dplyr::count(
        dplyr::select(
          filter(assessment_dataframe, education == '2. HS Grad'), 2)
        , age)
      ,count_education_level_2 = n), 
    by = 'age')

age_education_ratio <-
  age_education_ratio %>% left_join(
    dplyr::rename(
      dplyr::count(
        dplyr::select(
          filter(assessment_dataframe, education == '3. Some College'), 2)
        , age)
      ,count_education_level_3 = n), 
    by = 'age')

age_education_ratio <-
  age_education_ratio %>% left_join(
    dplyr::rename(
      dplyr::count(
        dplyr::select(
          filter(assessment_dataframe, education == '4. College Grad'), 2)
        , age)
      ,count_education_level_4 = n), 
    by = 'age')

age_education_ratio <-
  age_education_ratio %>% left_join(
    dplyr::rename(
      dplyr::count(
        dplyr::select(
          filter(assessment_dataframe, education == '5. Advanced Degree'), 2)
        , age)
      ,count_education_level_5 = n), 
    by = 'age')

# 3. Divide the number of people with a certain education by all surveyed people in that year

# replace NA values

age_education_ratio <- as.tibble(age_education_ratio)

age_education_ratio <- age_education_ratio %>% replace_na(list(count_education_level_1 = 0,
                                                               count_education_level_2 = 0,
                                                               count_education_level_3 = 0,
                                                               count_education_level_4 = 0,
                                                               count_education_level_5 = 0))

age_education_ratio <- age_education_ratio %>% mutate(ratio_education_level_1 = count_education_level_1 / n)
age_education_ratio <- age_education_ratio %>% mutate(ratio_education_level_2 = count_education_level_2 / n)
age_education_ratio <- age_education_ratio %>% mutate(ratio_education_level_3 = count_education_level_3 / n)
age_education_ratio <- age_education_ratio %>% mutate(ratio_education_level_4 = count_education_level_4 / n)
age_education_ratio <- age_education_ratio %>% mutate(ratio_education_level_5 = count_education_level_5 / n)



# 4. Stack (melt) the columns with the ratios to be able to visualize them.

# Melting (stacking) is necessary to show a legend in ggplot
age_edu_filtered <- age_education_ratio %>% dplyr::select(c(age,ratio_education_level_1, ratio_education_level_2, ratio_education_level_3, ratio_education_level_4, ratio_education_level_5))
age_education_ratio_stacked = melt(age_edu_filtered,id.vars = "age", 
                               measure.vars = c("ratio_education_level_1", "ratio_education_level_2", "ratio_education_level_3", "ratio_education_level_4", "ratio_education_level_5"),
                               variable.name = 'education_level',
                               value.name = 'ratio') 


age_education_ratio_stacked$education_level <- dplyr::recode(age_education_ratio_stacked$education_level, ratio_education_level_1 = "1. < HS Grad", 
                                                                                              ratio_education_level_2 = "2. HS Grad",
                                                                                              ratio_education_level_3 = "3. Some College",
                                                                                              ratio_education_level_4 = "4. College Grad",
                                                                                              ratio_education_level_5 = "5. Advanced Degree")



# 5. Visualize the education development for different ages

# Line plots - age/education ratio

ggplot(age_education_ratio_stacked, mapping = aes(x = age, y = ratio*100, color = education_level)) + 
  geom_line(size = 1, linetype = 'solid') +  
  labs(y = 'ratio of workers with a particular education level in %',
       title = 'Line plots for age/education ratio')

# Regression lines - age/education ratio

ggplot(age_education_ratio_stacked, mapping = aes(x = age, y = ratio*100, color = education_level)) + 
  geom_smooth(method = 'loess', linetype = 'solid', se = FALSE) + 
  labs(y = 'ratio of workers with a particular education level in %',
       title = 'Regression lines for age/education ratio')

# Regression line - age/education

ggplot(assessment_dataframe, mapping = aes(x=age, y= ed_ordinal)) +
  geom_smooth(method = 'loess') +
  labs(title = 'Regression line between age and education level',
       y = 'education level')


# 6. Calculate correlation of year to education

cor(assessment_dataframe[,c("age","ed_ordinal")], method = "spearman" )
cor(assessment_dataframe[,c("age","ed_ordinal")], method = "kendall" )


cor.test(assessment_dataframe[,"age"],
         assessment_dataframe[,"ed_ordinal"], 
         method = "spearman" )

cor.test(assessment_dataframe[,"age"],
         assessment_dataframe[,"ed_ordinal"], 
         method = "kendall" )



# --- 4.6 - Association between year and age ----------------

# Scatter plot year to age

ggplot(assessment_dataframe, mapping = aes(year, age)) +
  geom_point() +
  stat_summary(color = 'cadetblue', geom = 'line') +
  ggtitle('Scatter plot - year/age')

# Mean / median age development over time

# Mean age aggregation

mean_age_year <- assessment_dataframe %>% 
  dplyr::select(year, age)  %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarize(mean_age = mean(age, na.rm = TRUE))

# Line plot: mean age over time (solid) and trend line (dashed)

ggplot(mean_age_year, mapping = aes(x = year, y = mean_age)) + 
  geom_line(size = 1, linetype = 'solid') + 
  geom_smooth(method = "lm",linetype = 'dashed', se = FALSE) +
  labs(y = 'mean age', title = 'Line plot - year/mean age')


# Correlation assessment Pearson, Spearman and Kendall

cor(assessment_dataframe[,c("year","age")], method = "pearson" )
cor(assessment_dataframe[,c("year","age")], method = "spearman" )
cor(assessment_dataframe[,c("year","age")], method = "kendall" )

cor.test(assessment_dataframe[,"year"],
         assessment_dataframe[,"age"], 
         method = "pearson" )

cor.test(assessment_dataframe[,"year"],
         assessment_dataframe[,"age"], 
         method = "spearman" )

cor.test(assessment_dataframe[,"year"],
         assessment_dataframe[,"age"], 
         method = "kendall" )




# ---- 5 - Partial correlations ----------------


# --- 5.1 - Partial correlation between education and wage ---

ppcor::pcor.test(x=assessment_dataframe$ed_ordinal, y= assessment_dataframe$wage, 
                 z = c(assessment_dataframe$age,assessment_dataframe$year),
                 method = c("spearman"))


ppcor::pcor.test(x=assessment_dataframe$ed_ordinal, y= assessment_dataframe$wage, 
                 z = c(assessment_dataframe$age,assessment_dataframe$year),
                 method = "kendall")


# Finding: The partial correlation of education (ed_ordinal) -> wage 
# strong and significant


# --- 5.2 - Partial correlation between age and wage ---

ppcor::pcor.test(x=assessment_dataframe$age, y= assessment_dataframe$wage, 
                 z = c(assessment_dataframe$ed_ordinal,assessment_dataframe$year),
                 method = c("pearson"))

ppcor::pcor.test(x=assessment_dataframe$age, y= assessment_dataframe$wage, 
                 z = c(assessment_dataframe$ed_ordinal,assessment_dataframe$year),
                 method = c("spearman"))

ppcor::pcor.test(x=assessment_dataframe$age, y= assessment_dataframe$wage, 
                 z = c(assessment_dataframe$ed_ordinal,assessment_dataframe$year),
                 method = "kendall")


# Finding: The partial correlation of age -> wage 
# is much stronger than year -> wage and significant


# --- 5.3 - Partial correlation between year and wage ---

ppcor::pcor.test(x=assessment_dataframe$year, y= assessment_dataframe$wage, 
                 z = c(assessment_dataframe$ed_ordinal,assessment_dataframe$age),
                 method = c("pearson"))

ppcor::pcor.test(x=assessment_dataframe$year, y= assessment_dataframe$wage, 
                 z = c(assessment_dataframe$ed_ordinal,assessment_dataframe$age),
                 method = "kendall")

ppcor::pcor.test(x=assessment_dataframe$year, y= assessment_dataframe$wage, 
                 z = c(assessment_dataframe$ed_ordinal,assessment_dataframe$age),
                 method = c("spearman"))

# Finding: The partial correlation of 
# year -> wage is not strong, but significant


# ---- 6. Regression Modeling ----------------

# --- 6.1 - Creation of a regression equation ---

# with interaction effect between all independent attributes

eduAgeYearWageRegr <- lm (wage ~ education * year * age^2, data = assessment_dataframe)

summary(eduAgeYearWageRegr)

anova(eduAgeYearWageRegr)

# Result: interestingly, the interaction effect between education, year and age is significant,
#         although education and year as well as year and age is not significant

# --- 6.2 - Examination of regression assumptions ---------

# Check the conditions of regression with best model

fit=lm(wage~ed_ordinal*age^2*year,data=assessment_dataframe)
summary(fit)
plot(fit)


# 1. X and Y variables have a linear relation (linear scatter pattern)

# shown by residuals vs. fitted values

# Requirement met, the line is approx. horizontal


# 2. Errors/residuals are normally distributed

# How many records would be up to the second quantile

# Result: Requirement not met for quantiles above the second, the qqplot is not on the Q-Q-line ca. for values x > 1

# 3. Errors are independent / not autocorrelation bw errors

# Durbin-watson test for Autocorrelated/non-independence of Errors
# Ho: There is no auto-correlation bw errors (errors r independent)
  dwtest(fit)

# Result: Requirement met, Non-autocorrelation is not rejected, the errors are not autocorrelated


# 4. constant error variance - homoskedasticity of residuals or equal variance

# H0: hypothesis of constant error variance, i.e.  NO heteroscedasticity
# variance around the regression line is the same for all values of the predictor variable (X)
ncvTest(fit)

# Result: Requirement not met, homoskedasticity hypothesis is rejected


# 5. Avoid multi-collinearity bw predictors

# numerical Xs with correlation > 0.7 should be removed according to Singh (2018)

#Dropping response variable, non-numeric and non-relevant features
filter_assess = subset(assessment_dataframe, select = -c(education, age_group, wage))

#Calculating Correlation- strength of association between two variables
descrCor <- cor(filter_assess)
print(descrCor)

corrplot(descrCor)


# Result: There are no highly correlated variables in this dataset


# --- 6.3 - Adjustment of the regression term to meet regression assumptions ---------

# --- 6.3.1 - Box-Cox transformation ----------------------------------------------

# Use Box-Cox tranformation to check whether the dependent variable is not to be understood as y, but rather
# log(y), y^2 or other forms

fit=lm(wage~education*age^2*year,data=assessment_dataframe)

par(mfrow = c(1,1))

bc = boxcox(fit, lambda = seq(-3,3))
best.lam = bc$x[which(bc$y==max(bc$y))]

# best.lam close to 0, therefore log(y) as dependent variable

# Check model with log(y) as dependent variable

log_fit= lm(log(wage)~education*age^2*year,data=assessment_dataframe)
summary(log_fit)

par(mfrow = c(2,2))
plot(log_fit)


# 1. Linear relation: Not violated
# 2. Normal distribution: Not violated
# 3. No autocorrelation: Not violated
dwtest(log_fit)
# 4. Homoskedasticity: Violated
ncvTest(log_fit)
# 5. No multicollinearity (No change): Not violated 

# --- 6.3.2 - Regression with education as a numerical value ----------------


log_ed_num_fit= lm(log(wage)~ed_ordinal*age^2*year,data=assessment_dataframe)
summary(log_ed_num_fit)

plot(log_ed_num_fit)

# 1. Linear relation: Not violated
# 2. Normal distribution: Not violated
# 3. No autocorrelation: Not violated
dwtest(log_ed_num_fit)
# 4. homoskedasticity: Violated
ncvTest(log_ed_num_fit)
bptest(log_ed_num_fit)
# 5. No multicollinearity (No change): Not violated 

# --- 6.3.3 - Robust regression ---------------------------------------------

# Use robust regression to decrease the influence of outliers
# Robust linear regression with huber weights for iterated re-weighted least squares (IRLS)

fit_huber <- rlm(log(wage)~education*age^2*year,data=assessment_dataframe)
summary(fit_huber)

plot(fit_huber)

# 1. Linear relation: Not violated
# 2. Normal distribution: Partly violated
# 3. No autocorrelation: Not violated
dwtest(fit_huber)
# 4. Homoskedasticity: Violated
ncvTest(fit_huber)
bptest(fit_huber)
# 5. No multicollinearity (No change): Not violated 


# --- 6.3.4 - Weighted least squares regression -------------------------------------------------

# Applying weighted least squares regression may help to deal with heteroskedasticity

# Create regression term, which was used until now and a new regression term for the absolute values of the 
# residuals.
sd_func <- lm(abs(log_fit$residuals)~assessment_dataframe$education*assessment_dataframe$age*assessment_dataframe$year)

# Create weights with absolute sample variances from the observations
# The below formula is a standard formula to calculate these weights
# Spirce of the function: J. Perone, 2018, URL: youtube.com/watch?v=DTt0hLyRaTc
wls_weights <- 1/((sd_func$fitted.values)^2)

# Create the new regression model
wls_log_fit= lm(log(wage)~education*age^2*year,data=assessment_dataframe, weights = wls_weights)
summary(wls_log_fit)

# Plot the regression model
par(mfrow = c(2,2))
plot(wls_log_fit)

# 1. Linear relation: Not violated
# 2. Normal distribution: Not violated
# 3. No autocorrelation: Not violated (Visually tested, Durbin-Watson test not possible for weighted regression)
# 4. Homoskedasticity: Not violated
ncvTest(wls_log_fit)
# 5. No multicollinearity (No change): Not violated 

# --- 6.4 - Quantile regression ----------------------------

# Evaluating quantiles with quantile regression

# education
plot(summary(rq(log(wage)~education,data=assessment_dataframe, weights = wls_weights, tau = seq(from = 0.05, to=0.95, by = 0.05)))) 

# age
plot(summary(rq(log(wage)~age^2,data=assessment_dataframe, tau = seq(from = 0.05, to=0.95, by = 0.05)))) 

# year
plot(summary(rq(log(wage)~year,data=assessment_dataframe, tau = seq(from = 0.05, to=0.95, by = 0.05)))) 

# quantile plot for all attributes together and interaction effects
plot(summary(rq(wage~education*age*year,data=assessment_dataframe, weights = wls_weights, tau = seq(from = 0.05, to=0.95, by = 0.05))))
