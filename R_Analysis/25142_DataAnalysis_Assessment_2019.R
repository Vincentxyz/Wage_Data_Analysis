# Vincent F. A. Meyer zu Wickern
# Course: Data Analysis and Statistics


#----------------------------------------------------------
# Reset R's brain
#----------------------------------------------------------
rm(list=ls())

#----------------------------------------------------------
# Reset graphic device
# As long as there is any dev open (exept "null device") 
# close the active one!
# Caution: closes all open plots!!!!
#----------------------------------------------------------
while(!is.null(dev.list()))
{
  dev.off()
}

# --- Load packages if necessary ---------------------------

if (!require(ISLR)) install.packages('ISLR')
library(ISLR)

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

if (!require(e1071)) install.packages('e1071')
library(e1071)

if (!require(reshape2)) install.packages('reshape2')
library(reshape2)

# lmtest: Check autocorrelation between errors

if (!require(lmtest)) install.packages('lmtest')
library(lmtest)

# car: Check homoscedasticity

if (!require(car)) install.packages('car')
library(car)

# caret: Advanced machine learning techniques, can identify and cope with multicollinearity

if (!require(caret)) install.packages('caret')
library(caret)

# corrplot: Correlation plots

if (!require(corrplot)) install.packages('corrplot')
library(corrplot)

# ppcor: Used for partial correlation

if (!require(ppcor)) install.packages('ppcor')
library(ppcor)

# smatr: Used for ANCOVA analysis

if (!require(smatr)) install.packages('smatr')
library(smatr)

# MASS: Used for regression selection and robust regression

if (!require(MASS)) install.packages('MASS')
library(MASS)

# foreign: Used for robust regression (handling outliers)

if (!require(foreign)) install.packages('foreign')
library(foreign)

# quantreg: Used for quantile regression (handling homoscedasticity)

if (!require(quantreg)) install.packages('quantreg')
library(quantreg)

# ---- Create citations for packages ----------------------

# citation(package = 'ISLR')
# 
# citation(package = 'tidyverse')
# 
# citation(package = 'e1071')
# 
# citation(package = 'reshape2')
# 
# citation(package = 'lmtest')
# 
# citation(package = 'car')
# 
# citation(package = 'caret')
# 
# citation(package = 'corrplot')
# 
# citation(package = 'ppcor')
# 
# citation(package = 'smatr')
# 
# citation(package = 'MASS')
# 
# citation(package = 'foreign')
# 
# citation(package = 'quantreg')
# 
# citation(package = 'ggplot2')

#--- Load sample ------------------------------------------
# This part was added as specified in the task description
# It is commented out and the initially drawn sample is worked on

attach(Wage)
assessment_dataframe <- Wage[sample(nrow(Wage), 3000), ]
# 
# setwd("C:\Users\vince_000\OneDrive - Hochschule Rhein-Waal\Master\SS 1 - Data Analysis & Statistics\Exam_Assignment")
# 
# write.csv2(assessment_dataframe, file = "assessment_dataframe.csv", row.names = FALSE)

# assessment_dataframe <- read.csv2("assessment_dataframe.csv")
 

assessment_dataframe <- assessment_dataframe %>% dplyr::select(year, age, education, wage)


#--- 1. Descriptive statistics -----------------------------

str(assessment_dataframe)

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


## -- 1.1 Variable sample population description ------------

### -- 1.1.1 Variable description: Age -------------------------

#### Create a histogram for the age of participants with bins of size 8 from age 16 to 100

ggplot(assessment_dataframe) +
  geom_bar(mapping =aes(age),
                 color = 'cadetblue4',
                 fill = 'cadetblue1',
                 stat = 'density') + 
  ggtitle('Distribution of age')



show_desc_stats(assessment_dataframe$age,'age')



### -- 1.1.2 Variable description: Education -------------------

#### Create a barplot for the education of participants

ggplot(assessment_dataframe) + 
  geom_bar(mapping = aes(education), 
           stat = "count",
           color = 'cadetblue4',
           fill = 'cadetblue1') +
  ggtitle('Distribution of education levels')



### -- 1.1.3 Variable description: Wage -------------------

#### Create a histogram of the wage

ggplot(assessment_dataframe) +
  geom_bar(mapping =aes(wage),
                 stat = "density",
           color = 'cadetblue4',
           fill = 'cadetblue1'
          ) +
  xlab('wage in USD 1,000') +
  ggtitle('Distribution of wage')

show_desc_stats(assessment_dataframe$wage, 'wage')

### -- 1.1.4 Variable description: Year -------------------

#### Create a histogram of the years

ggplot(assessment_dataframe) +
  geom_bar(mapping =aes(year),
           color = 'cadetblue4',
           fill = 'cadetblue1')  +
  ggtitle('Distribution of years')

show_desc_stats(assessment_dataframe$year, 'year')

# To show how many records are given per year, 
# the year attribute is converted to a factor and described
# with a summary

summary(as.factor(assessment_dataframe$year))

# ---- 2 - Bivariate Data Analysis (Association / Correalation) --

# ---- 2.1 - Data preprocessing -------------------------

# ---- 2.1.1 Convert education to additional, ordinal value ---------

assessment_dataframe[,"ed_ordinal"] <- as.integer(assessment_dataframe[,"education"])

# ---- 2.2 - Compute correlation matrices for overview --

cor(dplyr::select(assessment_dataframe, c(year, age, ed_ordinal, wage)), method = 'pearson')

cor(dplyr::select(assessment_dataframe, c(year, age, ed_ordinal, wage)), method = 'spearman')

cor(dplyr::select(assessment_dataframe, c(year, age, ed_ordinal, wage)), method = 'kendall')


# ---- 2.3 - Evaluate correlation between attributes ----

# ---- 2.3.1 - Correlation: education -> Wage ---------

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
  xlab('wage in USD 1,000')
  ggtitle('Facet grid histograms - wage by education')
   

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


# --- 2.3.2 - Correlation: age -> wage ----------------

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

# --- 2.3.1.3 - Correlation: year -> wage ----------------

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

# Median wage aggregation

median_wage_year <- assessment_dataframe %>% 
  dplyr::select(year, wage)  %>% 
  group_by(year) %>% 
  summarize(median_wage = median(wage, na.rm = TRUE))

# Line plot: median wage over time (solid) and trend line (dashed)

ggplot(median_wage_year, mapping = aes(x = year, y = median_wage)) + 
  geom_line(size = 1, linetype = 'solid') + 
  geom_smooth(method = "lm",linetype = 'dashed', se = FALSE)

# ---- 2.3.4 - Correlation: year -> education ---------

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


# ---- 2.3.5 - Correlation: age -> education ---------


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



# --- 2.3.6 - Correlation: year -> age ----------------

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




# ---- 2.4 Partial correlations ----------------


# --- 2.4.1 Partial correlation education -> wage ---

ppcor::pcor.test(x=assessment_dataframe$ed_ordinal, y= assessment_dataframe$wage, 
                 z = c(assessment_dataframe$age,assessment_dataframe$year),
                 method = c("spearman"))


ppcor::pcor.test(x=assessment_dataframe$ed_ordinal, y= assessment_dataframe$wage, 
                 z = c(assessment_dataframe$age,assessment_dataframe$year),
                 method = "kendall")


# Finding: The partial correlation of education (ed_ordinal) -> wage 
# strong and significant


# --- 2.4.2 Partial correlation age -> wage ---

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


# --- 2.4.3 Partial correlation year -> wage ---

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


# ---- 3. Regression Models ----------------

# ---- 3.1 Simple linear regression --------

# --- 3.1.1 S.l. regression education -> wage ----

simpleLinearEduWageRegr <- lm(wage ~ education, data = assessment_dataframe)

summary(simpleLinearEduWageRegr)


ggplot(assessment_dataframe,aes(ed_ordinal,wage))+
  geom_point() +
  geom_smooth(method='lm') +
  xlab('education level')

plot(summary(rq(wage ~ education, data=assessment_dataframe, tau = seq(from = 0.05, to=0.95, by = 0.05)))) 



# --- 3.1.2 S.l. regression age -> wage ----

simpleLinearAgeWageRegr <- lm(wage ~ age, data = assessment_dataframe)

summary(simpleLinearAgeWageRegr)


ggplot(assessment_dataframe,aes(age,wage))+
  geom_point() +
  geom_smooth()

plot(summary(rq(wage ~ age^2, data=assessment_dataframe, tau = seq(from = 0.05, to=0.95, by = 0.05)))) 


# --- 3.1.3 S.l. regression year -> wage ----

simpleLinearYearWageRegr <- lm(wage ~ year, data = assessment_dataframe)

summary(simpleLinearYearWageRegr)


ggplot(assessment_dataframe,aes(year,wage))+
  geom_point() +
  geom_smooth(method='lm')

plot(summary(rq(wage ~ year, data=assessment_dataframe, tau = seq(from = 0.05, to=0.95, by = 0.05)))) 


# --- 3.1.4 S.l. regression year -> education ----

simpleLinearYearEduRegr <- lm(ed_ordinal ~ year, data = assessment_dataframe)

summary(simpleLinearYearEduRegr)


ggplot(assessment_dataframe,aes(year,ed_ordinal))+
  geom_point() +
  geom_smooth(method='lm')

# --- 3.1.5 S.l. regression age -> education ----

simpleLinearAgeEduRegr <- lm(ed_ordinal ~ age, data = assessment_dataframe)

summary(simpleLinearAgeEduRegr)


ggplot(assessment_dataframe,aes(age,ed_ordinal))+
  geom_point() +
  geom_smooth(method='lm')

# --- 3.1.6 S.l. regression year -> age --------

simpleLinearYearAgeRegr <- lm(age ~ year, data = assessment_dataframe)

summary(simpleLinearYearAgeRegr)


ggplot(assessment_dataframe,aes(year,age))+
  geom_point() +
  geom_smooth(method='lm')

plot(summary(rq(age ~ year, data=assessment_dataframe, tau = seq(from = 0.05, to=0.95, by = 0.05)))) 


# --- 3.2 - Multiple Linear Regression ---------

# --- 3.2.1 M. l. regression education, year -> wage ---

# without interaction effect

eduYearWageRegr <- lm (wage ~ education + year, data = assessment_dataframe)

summary(eduYearWageRegr)

# with interaction effect

eduYearWageRegr <- lm (wage ~ education * year, data = assessment_dataframe)

summary(eduYearWageRegr)

anova(eduYearWageRegr)

# Result: Interaction between education and year is not significant

# --- 3.2.2 M. l. regression age, year -> wage ---

# without interaction effect

ageYearWageRegr <- lm (wage ~ age + year, data = assessment_dataframe)

summary(ageYearWageRegr)

# with interaction effect

ageYearWageRegr <- lm (wage ~ age * year, data = assessment_dataframe)

summary(ageYearWageRegr)

anova(ageYearWageRegr)

# Result: Interaction between age and year is not significant

# --- 3.2.3 M. l. regression age, education -> wage ---

# without interaction effect

ageEduWageRegr <- lm (wage ~ age + education, data = assessment_dataframe)

summary(ageEduWageRegr)

# with interaction effect

ageEduWageRegr <- lm (wage ~ age * education, data = assessment_dataframe)

summary(ageEduWageRegr)

anova(ageEduWageRegr)

# Result: Interaction between age and education is significant

# --- 3.2.4 M. l. regression education, year, age -> wage ---

# without interaction effect

eduAgeYearWageRegr <- lm (wage ~ education + age^2 + year, data = assessment_dataframe)

summary(eduAgeYearWageRegr)

# with interaction effect between education and age

eduAgeYearWageRegr <- lm (wage ~ education * age^2 + year, data = assessment_dataframe)

summary(eduAgeYearWageRegr)

# with interaction effect between education and year

eduAgeYearWageRegr <- lm (wage ~ education * year + age, data = assessment_dataframe)

summary(eduAgeYearWageRegr)

# with interaction effect between age and year

eduAgeYearWageRegr <- lm (wage ~ education + year * age, data = assessment_dataframe)

summary(eduAgeYearWageRegr)

# with interaction effect between all independent attributes

eduAgeYearWageRegr <- lm (wage ~ education * year * age^2, data = assessment_dataframe)

summary(eduAgeYearWageRegr)

anova(eduAgeYearWageRegr)

# Result: interestingly, the interaction effect between education, year and age is significant,
#         although education and year as well as year and age is not significant

# --- 3.3 - Multiple Linear Regression - Assumption check ---------

# Check the conditions of regression with best model

fit=lm(wage~ed_ordinal*age^2*year,data=assessment_dataframe)
summary(fit)
plot(fit)


# 1. X and Y variables have a linear relation (linear scatter pattern)

# shown by residuals vs. fitted values

# Requirement met, the line is approx. horizontal


# 2. Errors/residuals are normally distributed

# How many records would be up to the second quantile

# Result: Requirement not met for quantiles above the second, the qqplot is not on the qqline ca. for values x > 1

# 3. Errors are independent / not autocorrelation bw errors

# Durbin-watson test for Autocorrelated/non-independence of Errors
# Ho: There is no auto-correlation bw errors (errors r independent)
  dwtest(fit)

# Result: Requirement met, Non-autocorrelation is not rejected, the errors are not autocorrelated


# 4. constant error variance - Homoscedasticity of residuals or equal variance

# H0: hypothesis of constant error variance, i.e.  NO heteroscedasticity
# variance around the regression line is the same for all values of the predictor variable (X)
ncvTest(fit)

# Result: Requirement not met, homoscedasticity hypothesis is rejected


# 5. Avoid multi-collinearity bw predictors

# numerical Xs with correlation > 0.7 should be removed according to Singh (2018)

#Dropping response variable, non-numeric and non-relevant features
filter_assess = subset(assessment_dataframe, select = -c(education, age_group, wage))

#Calculating Correlation- strength of association between two variables
descrCor <- cor(filter_assess)
print(descrCor)

corrplot(descrCor)


# Result: There are no highly correlated variables in this dataset


# --- 3.4 - Multiple Linear Regression - Correction of data to meet regression conditions ---------



# --------------------- Correcting violation of regression condictions ----

# --- 3.4.1 - Correcting homoscedasticity ----------------------------------------------

# Use boxcox to check whether the dependent variable is not to be understood as y, but rather
# log(y), y^2 or other forms

fit=lm(wage~ed_ordinal*age^2*year,data=assessment_dataframe)

par(mfrow = c(1,1))

bc = boxcox(fit, lambda = seq(-3,3))
best.lam = bc$x[which(bc$y==max(bc$y))]

# best.lam close to 0, therefore log(y) as dependent variable

# Check model with log(y) as dependent variable

fit= lm(log(wage)~education*age^2*year,data=assessment_dataframe)
summary(fit)

par(mfrow = c(2,2))
plot(fit)


# --- 3.4.3 - Evaluating quantiles with quantile regression----------------------------

plot(summary(rq(log(wage)~education*age*year,data=assessment_dataframe, tau = seq(from = 0.05, to=0.95, by = 0.05)))) 

par(mfrow = c(2, 2))
plot(qrego)



# Answer: It is not entirely normal as seen in the qqplots, it has two modes

# --- 3.4.1 - Correcting the normality of residuals ----------------------------------------------


# Check the histogram of the response variable: Is it normal?

ggplot(assessment_dataframe) +
  geom_bar(mapping =aes(wage),
           stat = "density",
           color = 'cadetblue4',
           fill = 'cadetblue1',
  ) 


# --- 3.4.1.2 Use robust regression to decrease the influence of outliers

# Robust linear regression with huber weights for iterated re-weighted least squares (IRLS)

rr.huber <- rlm(log(wage)~education*age*year,data=assessment_dataframe)
summary(rr.huber)


par(mfrow = c(1,1))
plot(rr.huber)


# Check again the conditions of regression with best model


# 1. X and Y variables have a linear relation (linear scatter pattern)

# Result: Requirement met, Residuals vs. Fitted plot is a straight horizontal line

# 2. Errors/residuals are normally distributed

# Result: Requirement not entirely met, the qqplot is not on the qqline ca. for values x > 2

# 3. Errors are independent / not autocorrelation bw errors

# Durbin-watson test for Autocorrelated/non-independence of Errors
#Ho: There is no auto-correlation bw errors (errors r independent)
dwtest(rr.huber)

# Result: Requirement not met, autocorrelation is not rejected, the errors are not autocorrelated


# 4. constant error variance - Homoscedasticity of residuals or equal variance

# H0: hypothesis of constant error variance, i.e.  NO heteroscedasticity
# variance around the regression line is the same for all values of the predictor variable (X)
ncvTest(rr.huber)
 

# Result: Requirement not met, homoscedasticity hypothesis is rejected. However, this may 
# be due to the large amount of data, visually the tendency is not strong


# 5. Avoid multi-collinearity bw predictors


#Dropping response variable, non-numeric and non-relevant features
filter_assess = subset(assessment_dataframe, dplyr::select = -c(education, age_group, wage))

#Calculating Correlation- strength of association between two variables
descrCor <- cor(filter_assess)
print(descrCor)

# Result: Correlation is very weak, multicollinearity is not problematic

# Entire result: Normality of dependent attribute could be improved, but is still not normal
#               for large values




# --- 3.5 - Selection of a regression model------------------------------------------

##examine all models
step(lm(wage~year+age+education,data=assessment_dataframe),direction="both")
step(lm(log(wage)~year+age+education,data=assessment_dataframe),direction="both")


# no other better regression model is presented

#install.packages('relaimpo')
library(relaimpo)
