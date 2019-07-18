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

#--- Load sample ------------------------------------------
# This part was added as specified in the task description
# It is commented out and the initially drawn sample is worked on



require(ISLR)
library(ISLR)
attach(Wage)
assessment_dataframe <- Wage[sample(nrow(Wage), 3000), ]
# 
# setwd("C:\Users\vince_000\OneDrive - Hochschule Rhein-Waal\Master\SS 1 - Data Analysis & Statistics\Exam_Assignment")
# 
# write.csv2(assessment_dataframe, file = "assessment_dataframe.csv", row.names = FALSE)

# assessment_dataframe <- read.csv2("assessment_dataframe.csv")

require(tidyverse)
library(tidyverse)
 

assessment_dataframe <- assessment_dataframe %>% select(year, age, education, wage)


#--- 1. Descriptive statistics -----------------------------

require(e1071)
library(e1071)

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
            ,sep="\n"))
}


## -- 1.1 Variable sample population description ------------

### -- 1.1.1 Variable description: Age -------------------------

#### Create a histogram for the age of participants with bins of size 8 from age 16 to 100

ggplot(assessment_dataframe) +
  geom_histogram(mapping =aes(age), 
                 breaks = seq(from = 16, to = 80, by = 8),
                 color = 'cadetblue4',
                 fill = 'cadetblue1',
                 stat = 'density')



ggplot(assessment_dataframe) +
  geom_boxplot(mapping =aes(x="", y=age),
               color = 'cadetblue4',
               fill = 'cadetblue1')

show_desc_stats(assessment_dataframe$age,'age')



### -- 1.1.2 Variable description: Education -------------------

#### Create a barplot for the education of participants

ggplot(assessment_dataframe) + 
  geom_bar(mapping = aes(education), 
           stat = "count",
           color = 'cadetblue4',
           fill = 'cadetblue1')



### -- 1.1.3 Variable description: Wage -------------------

#### Create a histogram of the wage

ggplot(assessment_dataframe) +
  geom_histogram(mapping =aes(wage),
                 stat = "density",
           color = 'cadetblue4',
           fill = 'cadetblue1',
           breaks = c(25, 50,75,100,125,150,175,200,250,300,350)
          ) 

show_desc_stats(assessment_dataframe$wage, 'wage')


### -- 1.1.4 Variable description: Year -------------------

#### Create a histogram of the years

ggplot(assessment_dataframe) +
  geom_bar(mapping =aes(year),
           color = 'cadetblue4',
           fill = 'cadetblue1') 


# ---- 2 - Bivariate Data (Association / Correalation) --

# ---- 2.1 - Data preprocessing -------------------------

# ---- 2.1.1 Convert education to ordinal value ---------

ed <- assessment_dataframe[,"education"]

ed_ordinal <- as.integer(ed)

assessment_dataframe[,"ed_ordinal"] <- ed_ordinal

# ---- 2.2 - Compute correlation matrices for overview --

cor(select(assessment_dataframe, c(year, age, ed_ordinal, wage)), method = 'pearson')

cor(select(assessment_dataframe, c(year, age, ed_ordinal, wage)), method = 'spearman')

cor(select(assessment_dataframe, c(year, age, ed_ordinal, wage)), method = 'kendall')


# ---- 2.3 - Evaluate correlation between attributes ----

# ---- 2.3.1 - Correlation on wage ----------------------

# ---- 2.3.1.1 - Correlation: education -> Wage ---------

# Visual assessment with a boxplot diagram

boxplot(wage ~ education,data=assessment_dataframe, 
        main="Wage by Education",
        xlab="education", 
        ylab="wage",
        horizontal=FALSE)


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

# Discrete Quantitative to continuous - Pearson's correlation

# --- 2.3.1.2 - Correlation: age -> wage ----------------

ggplot(assessment_dataframe[sample(nrow(assessment_dataframe),300),]) +
  geom_point(mapping = aes(age, wage))

### With education also

ggplot(assessment_dataframe[assessment_dataframe[,"education"] == '1. < HS Grad',]) +
  geom_point(mapping = aes(x = age, y = wage, color = education))


ggplot(assessment_dataframe[assessment_dataframe[,"education"] == '2. HS Grad',]) +
  geom_point(mapping = aes(x = age, y = wage, color = education))


ggplot(assessment_dataframe[assessment_dataframe[,"education"] == '3. Some College',]) +
  geom_point(mapping = aes(x = age, y = wage, color = education))


ggplot(assessment_dataframe[assessment_dataframe[,"education"] == '4. College Grad',]) +
  geom_point(mapping = aes(x = age, y = wage, color = education))


ggplot(assessment_dataframe[assessment_dataframe[,"education"] == '5. Advanced Degree',]) +
  geom_point(mapping = aes(x = age, y = wage, color = education))


# Linear Correlation Coefficient or Pearson Product Moment Correlation Coefficient 
# Parametric statistic, requires interval data for both variables
cor(assessment_dataframe[,c("age","wage")], method = "pearson" )
cor(assessment_dataframe[,c("age","wage")], method = "spearman" )
cor(assessment_dataframe[,c("age","wage")], method = "rho" )

# --- 2.3.1.3 - Correlation: year -> wage ----------------

# Scatter plot year to wage
# sampling of 300 random points so that points can be made out

ggplot(assessment_dataframe[sample(nrow(assessment_dataframe),300),]) +
  geom_point(mapping = aes(year, wage))

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
                      select(year, wage)  %>% 
                      group_by(year) %>% 
                      summarize(mean_wage = mean(wage, na.rm = TRUE))

# Line plot: mean wage over time (solid) and trend line (dashed)

ggplot(mean_wage_year, mapping = aes(x = year, y = mean_wage)) + 
  geom_line(size = 1, linetype = 'solid') + 
  geom_smooth(method = "lm",linetype = 'dashed', se = FALSE)

# Median wage aggregation

median_wage_year <- assessment_dataframe %>% 
  select(year, wage)  %>% 
  group_by(year) %>% 
  summarize(median_wage = median(wage, na.rm = TRUE))

# Line plot: median wage over time (solid) and trend line (dashed)

ggplot(median_wage_year, mapping = aes(x = year, y = median_wage)) + 
  geom_line(size = 1, linetype = 'solid') + 
  geom_smooth(method = "lm",linetype = 'dashed', se = FALSE)

# ---- 2.3.2 - Correlation on education -----------------

# ---- 2.3.2.1 - Correlation: time -> education ---------


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

education_ratio <- assessment_dataframe %>% count(year)

# 2. Count the number of people in a year with a particular education

education_ratio <-
  education_ratio %>% left_join(
    rename(
      count(
        select(
          filter(assessment_dataframe, education == '1. < HS Grad'), 1)
        , year)
      ,count_education_level_1 = n), 
    by = 'year')

education_ratio <-
  education_ratio %>% left_join(
    rename(
      count(
        select(
          filter(assessment_dataframe, education == '2. HS Grad'), 1)
        , year)
      ,count_education_level_2 = n), 
    by = 'year')

education_ratio <-
  education_ratio %>% left_join(
    rename(
      count(
        select(
          filter(assessment_dataframe, education == '3. Some College'), 1)
        , year)
      ,count_education_level_3 = n), 
    by = 'year')

education_ratio <-
  education_ratio %>% left_join(
    rename(
      count(
        select(
          filter(assessment_dataframe, education == '4. College Grad'), 1)
        , year)
      ,count_education_level_4 = n), 
    by = 'year')

education_ratio <-
  education_ratio %>% left_join(
    rename(
      count(
        select(
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

require(reshape2)
library(reshape2)

# Melting (stacking) is necessary to show a legend in ggplot
ed_filtered <- education_ratio %>% select(c(year,ratio_education_level_1, ratio_education_level_2, ratio_education_level_3, ratio_education_level_4, ratio_education_level_5))
education_ratio_stacked = melt(ed_filtered,id.vars = "year", 
                               measure.vars = c("ratio_education_level_1", "ratio_education_level_2", "ratio_education_level_3", "ratio_education_level_4", "ratio_education_level_5"),
                               variable.name = 'education_level',
                               value.name = 'ratio') 


education_ratio_stacked$education_level <- revalue(education_ratio_stacked$education_level, c("ratio_education_level_1" = "1. < HS Grad", 
                                                                                              "ratio_education_level_2" = "2. HS Grad",
                                                                                              "ratio_education_level_3" = "3. Some College",
                                                                                              "ratio_education_level_3" = "4. College Grad",
                                                                                              "ratio_education_level_3" = "5. Advanced Degree"))



education_ratio_stacked = stack(education_ratio,select = c(ratio_education_level_1, ratio_education_level_2, ratio_education_level_3, ratio_education_level_4, ratio_education_level_5)) 
education_ratio_stacked['year'] <- education_ratio_stacked %>% left_join(select(education_ratio,))

education_ratio_stacked[education_ratio_stacked[,'ind'] == 'ratio_education_level_1']

# 5. Visualize the education development over time.

ggplot(education_ratio_stacked, mapping = aes(x = year, y = ratio)) + 
  geom_line(size = 1, mapping = aes(color = education_level), linetype = 'solid') + 
  geom_smooth(method = "lm", mapping = aes(color = education_level),linetype = 'dashed', se = FALSE)
  

# 6. Show the change in education ratios in a table.
# Add table here with numbers of how the values changed from 2003 to 2009


description <- rbind(c('2003 - ratio'),c('2009 - ratio'))

education_change <- cbind(description,education_ratio %>% filter(year %in% c(2003,2009)) %>% select(ratio_education_level_1,
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


# ---- 2.3.2.1 - Correlation: age -> education ---------


# 1. Count the number of people surveyed in for an age

assessment_dataframe$age

education_age_ratio <- table(assessment_dataframe$age)
education_age_ratio <- assessment_dataframe %>% count(ed_ordinal)

colnames(assessment_dataframe)

# 2. Count the number of people in a year with a particular education

education_ratio <-
  education_ratio %>% left_join(
    rename(
      count(
        select(
          filter(assessment_dataframe, education == '1. < HS Grad'), 1)
        , year)
      ,count_education_level_1 = n), 
    by = 'year')

education_ratio <-
  education_ratio %>% left_join(
    rename(
      count(
        select(
          filter(assessment_dataframe, education == '2. HS Grad'), 1)
        , year)
      ,count_education_level_2 = n), 
    by = 'year')

education_ratio <-
  education_ratio %>% left_join(
    rename(
      count(
        select(
          filter(assessment_dataframe, education == '3. Some College'), 1)
        , year)
      ,count_education_level_3 = n), 
    by = 'year')

education_ratio <-
  education_ratio %>% left_join(
    rename(
      count(
        select(
          filter(assessment_dataframe, education == '4. College Grad'), 1)
        , year)
      ,count_education_level_4 = n), 
    by = 'year')

education_ratio <-
  education_ratio %>% left_join(
    rename(
      count(
        select(
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

# x. Calculate correlation of year to education

cor(assessment_dataframe[,c("age","ed_ordinal")], method = "spearman" )
cor(assessment_dataframe[,c("age","ed_ordinal")], method = "kendall" )


cor.test(assessment_dataframe[,"age"],
         assessment_dataframe[,"ed_ordinal"], 
         method = "spearman" )

cor.test(assessment_dataframe[,"age"]
         assessment_dataframe[,"ed_ordinal"], 
         method = "kendall" )

