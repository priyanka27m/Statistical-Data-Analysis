---
title: "Technical Report"
output:
  rmdformats::downcute:
    self_contained: true
    thumbnails: true
    lightbox: true
    gallery: false
    highlight: tango
---

```{r setup, echo=FALSE}
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(warning = FALSE)
```

```{r, Libraries,include=FALSE}

install.packages("rlang")

#library(tidyverse)

library(readxl)

library(flextable)

library(skimr)

library(janitor)

library(AMR)

library(stats)

library(knitr)

library(rmarkdown)

library(readr)

library("data.table")

#library(ggplot2)

library(survey)

library(tidyr)

library(broom)

#library(GGally)

library(ggpubr)

library(reshape2)

library(RColorBrewer)

library(gridExtra)

library(dplyr)

library(missForest)

library(cluster)

library(factoextra)

library(e1071)

library(psych)

library(corrplot)

library(glmnet)

```

<style>
      p {
        text-align: justify;
      }
</style>


<h1 style="text-align:center; font-weight:bold; font-style:italic;"> An exploratory study on Household Finance and Consumption of Italian households </h1>

<h2 style="text-align:center; font-style:italic;"> 

</h2>

<h3 style="text-align:center;"> 
By <br>
</h3>


## Introduction

The Household Finance and Consumption Survey data from European survey data of Banca D'Italia provides a comprehensive view of household balance sheets and related economic and demographic variables. It contains a vast array of variables related to household financial information, demographics, and assets. Each row of the dataset represents a value based on households' earnings, and the variables described in the report refer to different groups of households based on demographic or economic characteristics.

The dataset includes information on household weight, age of the reference person, number of household members, type of household, real estate properties' value, income from various sources such as self-employment and pensions, as well as information on net worth and debt. Additionally, it provides detailed information on the financial and non-financial assets, liabilities, income, and consumption of households in Italy.

The data was collected by sampling households from each region of the country, making it a representative dataset. With a broad range of variables related to household finances, including employment status, income, savings, debt, property, assets, and consumption patterns, this dataset can be a valuable resource for researchers and policymakers analyzing household finances in Italy.

```{r, Image2, echo=FALSE}
knitr::include_graphics("D:/1. Team 14 Project/HCFS.png")
```

Dataset Link : <a href="https://www.bancaditalia.it/statistiche/tematiche/indagini-famiglie-imprese/bilanci-famiglie/dati-indagine-europea/index.html">Household Finance and Consumption Survey</a>

<h3> Objective </h3>

The objective of this exploratory analysis on the dataset is to gain a better understanding of the financial and non-financial characteristics of households in Italy. This analysis will aim to identify trends in household income, wealth, debt, and consumption, as well as explore the relationships between these variables and demographic and economic characteristics of households. By analyzing the dataset, we hope to generate insights about household finance and consumption in Italy.


<h3> Workflow </h3>

```{r, Image, echo=FALSE}
knitr::include_graphics("D:/1. Team 14 Project/Pipeline.png")
```

We divided the pipeline of Statistical Data analysis into different sprints like in agile methodology to achieve desired results. The following are the tasks that were done as part of the each sprint.

```{r, Sprint, echo=FALSE}

Pipeline <- read_excel("D:/1. Team 14 Project/Hypothesis.xlsx", sheet = "Sprint")
ft_gen <- flextable(Pipeline)
ft_gen <- fontsize(ft_gen, size = 10)
ft_gen <- align(ft_gen, align = "left")
ft_gen <- set_caption(ft_gen, "Sprints")
ft_gen

```

The dataset comprises several CSV files, some of which contain non-core and core variables. Among them, we focused on two CSV files, D1.csv (127 columns) and H1.csv (920 columns), which together had over 1000 columns in total. However, as many of the columns in D1 file were derived from H1 file, we narrowed our focus to approximately 20 columns in H1 file that had data about expenditure. To determine which variables to select and which statistical questions to ask, we brainstormed various possibilities based on the available columns. 

```{r, Importing_DataSet, echo=FALSE}

dunclean <- read.csv("d1.csv")

#head(dunclean)

dcsv <- read.csv("d1s.csv")

hunclean <- read.csv("h1.csv")

#head(hunclean)

hcsv <- fread("h1.csv",
                select = c("ID"  , "HB0600", "HB2300" , "HB4300" , "HB4400" , "HB4500" , "HB4600" , "HB4700" , "HB4710" , "HC0300" , "HC0330" , "HC0340" , "HC1300" , "HD1200" , "HD1210" , "HD1800" , "HI0100" , "HI0200" , "HI0210" , "HI0220"))


hcfs <-merge(dcsv,hcsv,by = "ID")

```

The raw sample data of the files D1 and H1 from the survey is as follows,
```{r, echo=FALSE}
# create a flextable object with the head of the dataset
ftd <- flextable(head(dunclean))

ftd

# create a flextable object with the head of the dataset
fth <- flextable(head(hunclean))

fth
```

```{r Write_To_csv, include=FALSE}

#setwd("D:/1. Team 14 Project")

#write.csv(hcfs, "D:/1. Team 14 Project/hcfs_updated.csv", row.names=FALSE)
```

The following table lists outs the variables that were considered for this study and a short description about them derived from the meta data of the dataset.

```{r, About_Variables, echo=FALSE}

# Read the excel file
df_desc <- read_excel("D:/1. Team 14 Project/Variables_Table.xlsx", sheet = "Desc")

# convert to flextable
ft_desc <- flextable(df_desc)

# set table properties
ft_desc <- fontsize(ft_desc, size = 10)
ft_desc <- align(ft_desc, align = "left")

# print table
ft_desc

```

# Data Cleaning and Transformation

## Variable Recoding 

Filtering the required columns from the dataset files provided us a dataset with necessary columns that define the income and expenditure of the households. Since the data was raw, recoding and renaming was performed for most of the columns. Each row in the dataset represents a household and as there were no rows with many missing values data cleaning did not take much effort apart from skipping columns that do not have much relevant information to be considered.

```{r, Variable_Recoding}
# Identify numeric columns
numeric_cols <- sapply(hcfs, is.numeric)

# Replace NAs with 0 in numeric columns
hcfs[numeric_cols][is.na(hcfs[numeric_cols])] <- 0

hcfs <- hcfs %>% 
  rename(Gender = DHGENDERH1, Age = DHAGEH1, Education_Level = DHEDUH1) %>%
  filter(Gender %in% c(1,2)) %>% 
  mutate(Gender = recode(Gender, `1` = "Male", `2` ="Female")) %>% 
  mutate(Age = age_groups(Age, split_at = c(35, 45, 55, 65, 75), na.rm = FALSE)) %>% 
  mutate(Education_Level = recode(Education_Level, 
                                  `0` = "No formal education",
                                  `1` = "Primary education",
                                  `2` = "Lower secondary",
                                  `3` = "Upper secondary",
                                  `4` = "Post-secondary",
                                  `5` = "First stage tertiary",
                                  `6` = "Second stage tertiary"))

hcfs <- hcfs %>% 
  rename(Employment_status = DHEMPH1) %>% 
  mutate(Employment_status = recode(Employment_status, 
                                  `1` = "Employee",
                                  `2` = "Self-employed",
                                  `3` = "Unemployed",
                                  `4` = "Retired",
                                  `5` = "Other"))

hcfs <- hcfs %>% 
  rename(
      Number_of_Household_Members = DH0001, 
      Number_of_Household_Members_in_Employment = DH0004,   
      Household_Type =  DHHTYPE,        
      Value_of_Household_Vehicles = DA1130,       
      Valuables = DA1131,  
      Deposits = DA2101,
      Mutual_Funds = DA2102,     
      Bonds = DA2103,           
      Employee_Income = DI1100,     
      Self_Employment_income = DI1200,      
      Rental_Income = DI1300,
      Has_Rental_Income = DI1300i,
      Financial_assets_Income = DI1400,    
      Pension_Income =  DI1500,
      Total_Real_Assets =  DA1000,   
      Total_Financial_Assets = DA2100, 
      Has_Real_Assets = DA1000i,    
      Has_Financial_Assets = DA2100i,    
      Has_Vehicles = DA1130i,  
      Has_Valuables = DA1131i,   
      Value_of_Self_employment_Businesses = DA1140i,      
      Has_Real_Estate_Wealth = DA1400i,     
      Has_Deposits = DA2101i,   
      Has_Mutual_Funds = DA2102i, 
      Has_Bonds = DA2103i,    
      Has_Shares = DA2105i,     
      Has_Debt = DL1000i,
      Housing_Status = DHHST,       
      Has_Employee_Income = DI1100i,       
      Has_Self_Employee_Income = DI1200i,   
      Has_Financial_assets_Income =   DI1400i,      
      Has_Income_From_Pensions = DI1500i,   
      Income_From_Other_Sources =   DI1800,     
      Has_Income_From_Other_Sources =  DI1800i,   
      Credit_Card_Debt = DL1220,  
      Has_Credit_Card_Debt =  DL1220i,       
      Way_Of_Acquring_Property = HB0600,         
      Monthly_Amount_Paid_As_Rent = HB2300,     
      Ownership_of_Cars = HB4300,     
      Total_Value_of_Cars = HB4400,   
      Has_Other_Vehicles = HB4500, 
      Value_Of_Other_Vehicles =   HB4600,    
      Ownership_Of_Other_Valuables = HB4700,     
      Value_Of_Other_Valuables = HB4710,                 
      Household_Has_a_Credit_Card = HC0300,    
      Has_Private_Loans = HC0330,          
      No_of_PrivateLoans = HC0340,   
      Has_Applied_for_Loan_Credit = HC1300,      
      Household_Owns_Saving_accounts = HD1200,  
      Value_of_Saving_Accounts = HD1210,      
      Investment_Attitudes = HD1800,            
      Amount_spent_on_Food_at_Home = HI0100,       
      Amount_Spent_on_Food_Outside_Home = HI0200,      
      AMount_Spent_on_Utilities = HI0210,       
      Amount_Spent_on_Consumer_Goods_Services = HI0220,
      Total_Gross_Income=DI2000
  )

hcfs <- hcfs %>%
    mutate(Household_Owns_Saving_accounts = recode(Household_Owns_Saving_accounts, `1` = "Yes", `2` ="No", .default = "No")) %>%
    mutate(Ownership_of_Cars = recode(Ownership_of_Cars, `1` = "Yes", `2` ="No", .default = "No")) %>%
    mutate(Ownership_Of_Other_Valuables = recode(Ownership_Of_Other_Valuables, `1` = "Yes", `2` ="No", .default = "No")) %>%
    mutate(Has_Real_Assets = recode(Has_Real_Assets, `1` = "Yes", `2` ="No", .default = "No")) %>%
    mutate(Has_Financial_Assets = recode(Has_Financial_Assets, `1` = "Yes", `2` ="No", .default = "No")) %>%
    mutate(Has_Vehicles = recode(Has_Vehicles, `1` = "Yes", `2` ="No", .default = "No")) %>%
    mutate(Has_Valuables = recode(Has_Valuables, `1` = "Yes", `2` ="No", .default = "No")) %>%
    mutate(Has_Real_Estate_Wealth = recode(Has_Real_Estate_Wealth, `1` = "Yes", `2` ="No", .default = "No")) %>%
    mutate(Has_Deposits = recode(Has_Deposits, `1` = "Yes", `2` ="No", .default = "No")) %>%
    mutate(Has_Mutual_Funds = recode(Has_Mutual_Funds, `1` = "Yes", `2` ="No", .default = "No")) %>%
    mutate(Has_Bonds = recode(Has_Bonds, `1` = "Yes", `2` ="No", .default = "No")) %>%
    mutate(Has_Shares = recode(Has_Shares, `1` = "Yes", `2` ="No", .default = "No")) %>%
    mutate(Has_Debt = recode(Has_Debt, `1` = "Yes", `2` ="No", .default = "No")) %>%
    mutate(Has_Employee_Income = recode(Has_Employee_Income, `1` = "Yes", `2` ="No", .default = "No")) %>%
    mutate(Has_Self_Employee_Income = recode(Has_Self_Employee_Income, `1` = "Yes", `2` ="No", .default = "No")) %>%
    mutate(Has_Financial_assets_Income = recode(Has_Financial_assets_Income, `1` = "Yes", `2` ="No", .default = "No")) %>%
    mutate(Has_Income_From_Pensions = recode(Has_Income_From_Pensions, `1` = "Yes", `2` ="No", .default = "No")) %>%
    mutate(Has_Income_From_Other_Sources = recode(Has_Income_From_Other_Sources, `1` = "Yes", `2` ="No", .default = "No")) %>%
    mutate(Has_Credit_Card_Debt = recode(Has_Credit_Card_Debt, `1` = "Yes", `2` ="No", .default = "No")) %>%
    mutate(Has_Other_Vehicles = recode(Has_Other_Vehicles, `1` = "Yes", `2` ="No", .default = "No")) %>%
    mutate(Household_Has_a_Credit_Card = recode(Household_Has_a_Credit_Card, `1` = "Yes", `2` ="No", .default = "No")) %>%
    mutate(Has_Private_Loans = recode(Has_Private_Loans, `1` = "Yes", `2` ="No", .default = "No")) %>%
    mutate(Has_Applied_for_Loan_Credit = recode(Has_Applied_for_Loan_Credit, `1` = "Yes", `2` ="No", .default = "No")) %>% 
    mutate(Has_Rental_Income = recode(Has_Rental_Income, `1` = "Yes", `2` ="No", .default = "No"))

hcfs <- hcfs %>%
  mutate(Way_Of_Acquring_Property = recode(Way_Of_Acquring_Property, 
                                  `1` = "Purchased",
                                  `2` = "Own construction",
                                  `3` = "Inherited",
                                  `4` = "Gift",
                                  .default = "Inherited"))


hcfs <- hcfs %>%
  mutate(Housing_Status = recode(Housing_Status, 
                                  `1` = "Owner",
                                  `2` = "Owner with mortgage",
                                  `3` = "Renter"))


hcfs <- hcfs %>%
  mutate(Investment_Attitudes = recode(Investment_Attitudes, 
                                  `1` = "Take substantial financial risks",
                                  `2` = "Take above average financial risks",
                                  `3` = "Take average financial risks",
                                  `4` = "Not willing to take any financial risk"))


hcfs <- hcfs %>%
  mutate(Household_Type = recode(Household_Type, 
                                  `51` = "One adult, younger than 65 years",
                                  `52` = "One adult, 65 years and over",
                                  `6` = "Two adults younger than 65 years",
                                  `7` = "Two adults, at least one aged 65 years and over",
                                  `8` = "Three or more adults",
                                  `9` = "Single parent with dependent children",
                                  `10` = "Two adults with one dependent child",
                                  `11` = "Two adults with two dependent children",
                                  `12` = "Two adults with three or more dependent children",
                                  `13` = "Three or more adults with dependent children"))



```

## Outlier detection

To detect outliers in the dataset the boxplot of the major metric variables were considered and the outlier values were replaced by mean values.

```{r Outlier_detection, echo=FALSE}

#-------------------------------------------------------------------------------
#summary(hcfs$Total_Gross_Income)

before <- ggplot(hcfs, aes(x = Total_Gross_Income)) +
  geom_boxplot(fill = "#69b3a2") +
  labs(x = "Total Gross Income of the respondents")

## Recoding the outlier with mean value
mean_Income <- mean(hcfs$Total_Gross_Income)

hcfs$Total_Gross_Income <- ifelse(hcfs$Total_Gross_Income > 50000, mean_Income, hcfs$Total_Gross_Income)

## Replotting the boxplot
#summary(hcfs$Total_Gross_Income)

after <- ggplot(hcfs, aes(x = Total_Gross_Income)) +
  geom_boxplot(fill = "#69b3a2") +
  labs(x = "Total Gross Income of the respondents Cleaned")

grid.arrange(before,after, ncol= 2)

#-------------------------------------------------------------------------------

#summary(hcfs$Credit_Card_Debt)

before <- ggplot(hcfs, aes(x = Credit_Card_Debt)) +
  geom_boxplot(fill = "#69b3a2") +
  labs(x = "Credit_Card_Debt of the respondents")

mean_debt <- mean(hcfs$Credit_Card_Debt)

hcfs$Credit_Card_Debt <- ifelse(hcfs$Credit_Card_Debt > 10000, mean_debt, hcfs$Credit_Card_Debt)

#summary(hcfs$Credit_Card_Debt)

after <- ggplot(hcfs, aes(x = Credit_Card_Debt)) +
  geom_boxplot(fill = "#69b3a2") +
  labs(x = "Credit_Card_Debt of the respondents Cleaned")

grid.arrange(before,after, ncol= 2)

#-------------------------------------------------------------------------------

#summary(hcfs$Value_of_Saving_Accounts)

before <- ggplot(hcfs, aes(x = Value_of_Saving_Accounts)) +
  geom_boxplot(fill = "#69b3a2") +
  labs(x = "Value_of_Saving_Accounts of the respondents")

mean_balance <- mean(hcfs$Value_of_Saving_Accounts)

hcfs$Value_of_Saving_Accounts <- ifelse(hcfs$Value_of_Saving_Accounts > 10000, mean_balance, hcfs$Value_of_Saving_Accounts)

#summary(hcfs$Value_of_Saving_Accounts)

after <- ggplot(hcfs, aes(x = Value_of_Saving_Accounts)) +
  geom_boxplot(fill = "#69b3a2") +
  labs(x = "Value_of_Saving_Accounts of the respondents Cleaned")

grid.arrange(before,after, ncol= 2)

#-------------------------------------------------------------------------------

```

# Structure and Summaries

The dataset has 8156 rows with 62 columns after cleaning and recoding. The structure and the distribution of values from the cleaned data is as follows,

```{r, Summary, echo=FALSE}

# create a flextable object with the head of the dataset
ft <- flextable(head(hcfs))

ft

str(hcfs)
#summary(hcfs)

skim(hcfs)

#names(hcfs)

```

# Cross tabulations

To get a quick overview of the distribution of the data and to identify any patterns or relationships that may exist, the following cross tabulations were drawn and the results have been discussed accordingly.

```{r, echo=FALSE}
hcfs %>%
  tabyl(Age, Gender) %>% 
  adorn_totals(where = "col") %>% 
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting() %>% 
  adorn_ns(position = "front") %>% 
  adorn_title(
    row_name = "Age Category",
    col_name = "Gender",
    placement = "combined") %>% # this is necessary to print as image
  flextable::flextable() %>%    # convert to pretty image
  flextable::autofit()  
```

<p style="text-align:center;"> Table 1 : Age vs Gender <p>

The table displays the number and percentage of individuals by age and gender category ranging from 0-34 to 75+ years. It can be seen that the respondents are majorly Males and of age group 55-64.

```{r, echo=FALSE}
hcfs %>%
  tabyl(Education_Level, Gender) %>% 
  adorn_totals(where = "col") %>% 
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting() %>% 
  adorn_ns(position = "front") %>% 
  adorn_title(
    row_name = "Level of Education",
    col_name = "Gender",
    placement = "combined") %>% # this is necessary to print as image
  flextable::flextable() %>%    # convert to pretty image
  flextable::autofit()

```

<p style="text-align:center;"> Table 2 : Education level vs Gender <p>

The table displays the distribution of education levels among females and males. We observe that the majority of females have completed primary education (34.8%), followed by lower secondary (22.2%), while the majority of males have completed upper secondary (33.7%), followed by lower secondary (32.1%).

```{r, echo=FALSE}
hcfs %>%
  tabyl(Employment_status, Gender) %>% 
  adorn_totals(where = "col") %>% 
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting() %>% 
  adorn_ns(position = "front") %>% 
  adorn_title(
    row_name = "Employment_status",
    col_name = "Gender",
    placement = "combined") %>% 
  flextable::flextable() %>%    
  flextable::autofit()
```

<p style="text-align:center;"> Table 3 : Employment status vs Gender <p>

The above table presents the distribution of employment status among females and males in the population being studied. The results indicate that a higher proportion of males are employed compared to females (38.2% vs. 33.3%). On the other hand, a higher proportion of females are retired compared to males (30.9% vs. 43.0%). Additionally, a small proportion of both males and females are self-employed or unemployed.

```{r, echo=FALSE}
hcfs %>%
  tabyl(Education_Level, Employment_status) %>%
  adorn_totals(where = "col") %>% 
  adorn_title(
    row_name = "Education_Level",
    col_name = "Employment_status",
    placement = "combined") %>% 
  flextable::flextable() %>%    
  flextable::autofit() 
```

<p style="text-align:center;"> Table 4 : Education Level vs Employment Status <p>

The table presents the cross-tabulation between Education Level and Employment Status. The highest count of individuals falls in the Education Level category of Lower Secondary (2,329) and Employment Status category of Employee (2,635). The lowest count of individuals falls in the Education Level category of First Stage Tertiary and Employment Status category of Unemployed (13). The highest count of individuals in Education Level category of First Stage Tertiary is employed in the Self-Employed category (181), while the highest count of individuals in Education Level category of Lower Secondary and Primary Education are employed in the Employee category (939 and 1,319, respectively).

```{r, echo=FALSE}
hcfs %>%
  tabyl(Investment_Attitudes, Gender) %>% 
  adorn_totals(where = "col") %>% 
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting() %>% 
  adorn_ns(position = "front") %>% 
  adorn_title(
    row_name = "Investment Attitude",
    col_name = "Gender",
    placement = "combined") %>% 
  flextable::flextable() %>%    
  flextable::autofit()
```

<p style="text-align:center;"> Table 5 : Investment Attitude by Gender <p>

The Cross tabulation presents the distribution of Investment Attitude by Gender. The majority of the respondents, both female (66.9%) and male (54.8%), were not willing to take any financial risk. A small percentage of respondents, both female (8.5%) and male (12.3%), were willing to take above-average financial risks. The percentage of females who were willing to take average financial risks (24.4%) was slightly higher than males (32.0%). Finally, a negligible percentage of respondents, both female (0.2%) and male (0.9%), were willing to take substantial financial risks. Overall, the results suggest that both female and male respondents were generally risk-averse regarding their investment attitude.

```{r, echo=FALSE}
hcfs %>%
  tabyl(Number_of_Household_Members, Number_of_Household_Members_in_Employment) %>% 
  adorn_title(
    row_name = "Number of Household Members",
    col_name = "Number of Household Members in Employment",
    placement = "combined") %>% # this is necessary to print as image
  flextable::flextable() %>%    # convert to pretty image
  flextable::autofit() 
```

<p style="text-align:center;"> Table 6 : Frequency of households by the number of household members vs number of household members in employment <p>

The table represents the frequency of households by the number of household members and the number of household members in employment. The majority of households have no members in employment, and this is more common among females. As the number of household members in employment increases, the frequency of households decreases. The highest frequency of households is observed in the category of one household member with one household member in employment (662), followed by two household members with two household members in employment (537).

```{r, echo=FALSE}
hcfs %>%
  group_by(Gender) %>%
    summarise(
              N = n(),
              Employement = mean(Employee_Income, na.rm = TRUE), 
              Self = mean(Self_Employment_income, na.rm = TRUE),
              Rental = mean(Rental_Income, na.rm = TRUE),
              Financial = mean(Financial_assets_Income, na.rm = TRUE),
              Pension = mean(Pension_Income, na.rm = TRUE),
              Total_Gross_Income = mean(Total_Gross_Income, na.rm = TRUE)) %>%
  flextable::flextable() %>%    # convert to pretty image
  flextable::autofit()
```

<p style="text-align:center;"> Table 7 : Gender and their Mean Income <p>

The table represents the income from different sources of the households based on gender. There are 2,903 households with a female head and 5,253 households with a male head. On average, male-headed households have higher total gross income than female-headed households. Male-headed households also have higher average income from self-employment, rental, financial, and pension sources. The difference in total gross income between male and female-headed households may be due to various factors such as differences in education, work experience, and job opportunities. However, without further analysis, it is difficult to draw any definite conclusions.

```{r, echo=FALSE}
hcfs %>%
  group_by(Gender) %>%
    summarise(
              N = n(),
              Food = mean(Amount_spent_on_Food_at_Home, na.rm = TRUE), 
              Consumer_Goods = mean(Amount_Spent_on_Consumer_Goods_Services, na.rm = TRUE),
              Utilities = mean(AMount_Spent_on_Utilities, na.rm = TRUE)) %>%
  flextable::flextable() %>%    # convert to pretty image
  flextable::autofit()
```

<p style="text-align:center;"> Table 8 : Gender and their expenditure <p>

The  table presents the average expenditures on food, consumer goods, and utilities for females and males. From the table, we can see that males have higher average expenditures in all categories than females. Specifically, males spend on average 482.8249 more on food, 1,341.658 more on consumer goods, and 183.1321 more on utilities than females.


```{r, echo=FALSE}
hcfs %>%
  group_by(Household_Type) %>%
    summarise(
              N = n(),
              Food = mean(Amount_spent_on_Food_at_Home, na.rm = TRUE), 
              Consumer_Goods = mean(Amount_Spent_on_Consumer_Goods_Services, na.rm = TRUE),
              Utilities = mean(AMount_Spent_on_Utilities, na.rm = TRUE)) %>%
  flextable::flextable() %>%    # convert to pretty image
  flextable::autofit()
```

<p style="text-align:center;"> Table 9 : Household Type and their expenditure <p>

The cross-tabulation result presented in the table shows the distribution of Household_Type in the hcfs dataset with respect to three expenditure categories: Food, Consumer_Goods, and Utilities. The table also provides the number of observations (N) in each category. From the results, we can observe that the category with the largest number of observations is "Two adults, at least one aged 65 years and over" with 1,744 observations, while the category with the smallest number of observations is "Two adults with three or more dependent children" with only 198 observations.

```{r, echo=FALSE}
hcfs %>%
  group_by(Gender, Investment_Attitudes) %>%
    summarise(
              N = n(),
              Employement = mean(Employee_Income, na.rm = TRUE), 
              Self = mean(Self_Employment_income, na.rm = TRUE),
              Rental = mean(Rental_Income, na.rm = TRUE),
              Financial = mean(Financial_assets_Income, na.rm = TRUE),
              Pension = mean(Pension_Income, na.rm = TRUE),
              Total_Gross_Income = mean(Total_Gross_Income, na.rm = TRUE)) %>%
  flextable::flextable() %>%    # convert to pretty image
  flextable::autofit()
```

<p style="text-align:center;"> Table 10 : Investment Attitude with Gender and their mean Income <p>

The table presents investment attitudes and total gross income of males and females in four categories of investment attitudes (Not willing to take any financial risk, Take above average financial risks, Take average financial risks, and Take substantial financial risks). From the table, it can be seen that:

Males tend to have a higher total gross income than females across all categories of investment attitudes. Both males and females who are willing to take above average or substantial financial risks tend to have higher total gross income than those who are not willing to take any financial risks. Males tend to have higher income in the categories of Take above average financial risks and Take substantial financial risks, while females tend to have higher income in the category of Take average financial risks.

```{r, echo=FALSE}
hcfs %>%
  tabyl(Housing_Status, Gender) %>% 
  adorn_totals(where = "col") %>% 
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting() %>% 
  adorn_ns(position = "front") %>% 
  adorn_title(
    row_name = "Housing_Status",
    col_name = "Gender",
    placement = "combined") %>% 
  flextable::flextable() %>%    
  flextable::autofit()
```

<p style="text-align:center;"> Table 11 : Gender and Housing status <p>

From the cross tabulation of gender and housing status, it can be inferred that the majority of respondents were owners (64.1%) followed by renters (28%) and those with a mortgage (7.9%). Females were more likely to be renters (32.4%) compared to males (25.5%) and males were more likely to be owners (65.4%) compared to females (61.8%). When looking at investment attitudes, a larger percentage of females were not willing to take any financial risks (56.9%) compared to males (52.1%). However, a larger percentage of males were willing to take above-average financial risks (21.9%) compared to females (10.8%).

```{r, echo=FALSE}
hcfs %>%
  tabyl(Has_Private_Loans, Education_Level) %>% 
  adorn_totals(where = "col") %>% 
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting() %>% 
  adorn_ns(position = "front") %>% 
  adorn_title(
    row_name = "Has_Private_Loans",
    col_name = "Education_Level",
    placement = "combined") %>% 
  flextable::flextable() %>%    
  flextable::autofit()
```

<p style="text-align:center;"> Table 12 : Private loans by Education Level <p>

The table represents the distribution of individuals based on their education level and whether they have private loans. It can be observed that a vast majority of individuals with different education levels do not have private loans. The highest percentage of individuals without private loans was observed among those with a first stage tertiary education (97.9%). On the other hand, the highest percentage of individuals with private loans was observed among those with lower secondary education (4.3%). 

```{r, echo=FALSE}
hcfs %>%
  group_by(Gender, Has_Credit_Card_Debt) %>%
    summarise(
              N = n(),
              Debt = mean(Credit_Card_Debt, na.rm = TRUE)) %>%
  flextable::flextable() %>%    # convert to pretty image
  flextable::autofit()
```

<p style="text-align:center;"> Table 13 : Who has credit card debit? <p>

This table shows the relationship between gender and having credit card debt, as well as the amount of debt for those who have it. Among females, 2,891 have no credit card debt, while 12 have an average debt of 1,155.833. Among males, 5,201 have no credit card debt, while 52 have an average debt of 1,669.542.

```{r, echo=FALSE}
hcfs %>%
  group_by(Gender, Education_Level, Age) %>%
    summarise(
              N = n(),
              Savings = mean(Value_of_Saving_Accounts, na.rm = TRUE), 
              Vehicles = mean(Value_of_Household_Vehicles, na.rm = TRUE),
              Business = mean(Value_of_Self_employment_Businesses, na.rm = TRUE)) %>%
  flextable::flextable() %>%    # convert to pretty image
  flextable::autofit()

```

<p style="text-align:center;"> Table 14 : Who has savings, vehicles, and business ownership? <p>

The table displays the summary statistics for savings, vehicles, and business ownership for different demographic groups, including gender, education level, and age. In general, females tend to save less and own fewer vehicles and businesses compared to males. Additionally, individuals with higher education levels tend to have more savings and own more vehicles and businesses compared to those with lower education levels. Finally, older individuals tend to have more savings and own more vehicles and businesses compared to younger individuals.

# Data Visualization

## Graphs

The following graphs for the data were drawn to visualize the dataset and obtain insights.

```{r Graphs, echo=FALSE}

ggplot(hcfs, aes(x = Education_Level, fill = Has_Employee_Income)) +
  geom_bar(position = "dodge", alpha = 0.7) +
  labs(title = "Education Level vs. Has Employment Income", x = "Education Level", y = "Has Employment Income") +
  scale_fill_manual(values = c("#3366CC", "#DC3912"), name = "Has Employment Income") +
  theme_minimal()

ggplot(data = hcfs) +
  geom_count(mapping = aes(x = Education_Level, y = Age))

ggplot(hcfs, aes(x = Employment_status)) +
  geom_bar(aes(fill = ..count..), color = "black") +
  scale_fill_gradient(low = "pink", high = "blue") +
  theme_minimal() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), 
            color = "black", size = 3, vjust = -0.5) +
  labs(x = "Employment status", y = "Count", 
       title = "Counts of Employment status") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5))

ggplot(hcfs, aes(x = Housing_Status)) +
  geom_bar(aes(fill = ..count..), color = "black") +
  scale_fill_gradient(low = "yellow", high = "pink") +
  theme_minimal() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), 
            color = "black", size = 3, vjust = -0.5) +
  labs(x = "Housing_Status", y = "Count", 
       title = "Counts of Housing status") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5))

ggplot(hcfs, aes(x = Investment_Attitudes)) +
  geom_bar(aes(fill = ..count..), color = "black") +
  scale_fill_gradient(low = "green", high = "pink") +
  theme_minimal() +
  coord_flip()+
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), 
            color = "black", size = 3, vjust = -0.5) +
  labs(x = "Investment_Attitudes", y = "Count", 
       title = "Counts of Investment_Attitudes") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5))

ggplot(hcfs, aes(x = Age, y = Value_of_Saving_Accounts, color = Gender, fill = Gender)) + 
  geom_jitter(position = position_jitter(width = 0.2, height = 0), alpha = 0.5) +
  labs(x = "Age (years)", y = "Value of Saving Accounts", color = "Gender", fill = "Gender") +
  theme_bw()

ggplot(hcfs, aes(x = Age, y = Total_Gross_Income, color = Gender, fill = Gender)) + 
  geom_jitter(position = position_jitter(width = 0.2, height = 0), alpha = 0.5) +
  labs(x = "Age (years)", y = "Total_Gross_Income", color = "Gender", fill = "Gender") +
  theme_bw()

ggplot(hcfs, aes(x = Age, y = Credit_Card_Debt, color = Gender, fill = Gender)) + 
  geom_jitter(position = position_jitter(width = 0.2, height = 0), alpha = 0.5) +
  labs(x = "Age (years)", y = "Credit_Card_Debt", color = "Gender", fill = "Gender") +
  theme_bw()

ggplot(hcfs, aes(x=Amount_Spent_on_Consumer_Goods_Services)) + 
geom_histogram(binwidth=50,aes(fill=Gender))+
facet_grid(Gender ~ .)+
labs(x="Amount_Spent_on_Consumer_Goods_Services",
         y="Density", 
       title="Distribution of Gender on Amount_Spent_on_Consumer_Goods_Services")+  
theme_bw()+
theme(plot.title = element_text(size=22)
      ,axis.text.x= element_text(size=15),
       axis.text.y= element_text(size=15),
        axis.title=element_text(size=18))

```

## Q-Q Plots

To obtain a series of QQ (Quantile-Quantile) plots for the data we considered different subsets of data. Subset 1 includes income-related variables, subset 2 includes variables related to household expenses, and subset 3 includes variables related to household assets. For each subset, a loop is used to create multiple QQ plots, one for each variable in the subset. The resulting QQ plots were used to check whether the data follow a normal distribution.

```{r, QQ Plot, echo=FALSE}

hcfs_subset1 <- hcfs[, c("Total_Gross_Income", "Employee_Income",
                        "Self_Employment_income", "Financial_assets_Income", 
                        "Rental_Income", "Pension_Income", "Credit_Card_Debt",
                        "Income_From_Other_Sources")]


  

hcfs_subset2 <- hcfs[, c("AMount_Spent_on_Utilities", 
                        "Amount_Spent_on_Consumer_Goods_Services","Amount_spent_on_Food_at_Home",
                  "Amount_Spent_on_Food_Outside_Home","Monthly_Amount_Paid_As_Rent")]


hcfs_subset3 <- hcfs[, c("Value_of_Household_Vehicles",
"Valuables",
"Deposits",
"Mutual_Funds",
"Bonds",
"Value_of_Self_employment_Businesses",
"Total_Real_Assets",
"Total_Value_of_Cars",
"Value_Of_Other_Vehicles",
"Value_Of_Other_Valuables"
)]

# Subset 1 QQ plots
par(mfrow=c(3,3))
for(i in 1:8) {
  qqnorm(hcfs_subset1[,i], main = names(hcfs_subset1)[i])
  qqline(hcfs_subset1[,i])
}

# Subset 2 QQ plots
par(mfrow=c(2,3))
for(i in 1:5) {
  qqnorm(hcfs_subset2[,i], main = names(hcfs_subset2)[i])
  qqline(hcfs_subset2[,i])
}

# Subset 3 QQ plots
par(mfrow=c(2,5))
for(i in 1:10) {
  qqnorm(hcfs_subset3[,i], main = names(hcfs_subset3)[i])
  qqline(hcfs_subset3[,i])
}

# # Create a data frame with the variables of interest
# vars <- c("Total_Gross_Income", "AMount_Spent_on_Utilities", 
#           "Amount_Spent_on_Consumer_Goods_Services", "Credit_Card_Debt")
# 
# df <- hcfs[, vars]
# 
# # Create a list of QQ plots for each variable
# qqplot_list <- lapply(vars, function(var) {
#   ggplot(df, aes(sample = .data[[var]])) +
#     geom_qq() +
#     stat_qq_line() +
#     labs(title = paste("QQ plot for", var), x = "Theoretical Quantiles", y = "Sample Quantiles") +
#     scale_x_continuous(labels = scales::comma) + 
#     theme_bw()
# })
# 
# # Arrange the plots in a grid
# do.call(grid.arrange, c(qqplot_list, ncol = 2))

```

## Tiles

The ggplot graphs, display the distribution of a different set of variables. The graphs display the relationship between two variables with a color-coded tile. The color of the tile represents the value of a third variable Total_Gross_Income, Amount_Spent_on_Consumer_Goods_Services, or Investment_Attitudes

```{r Data_Distribution, echo=FALSE}

ggplot(hcfs, aes(Age,Employment_status , fill=Total_Gross_Income)) + 
  geom_tile()+
  scale_fill_distiller(palette = "RdPu") +
labs(y="Employment_status",
         x="Age", 
       title="Distribution of Age & Employment_status with Total_Gross_Income)")+  
theme_bw()+
theme(plot.title = element_text(size=16)
      ,axis.text.x= element_text(size=12),
       axis.text.y= element_text(size=12),
        axis.title=element_text(size=12))

#-------------------------------------------------------------------------------

ggplot(hcfs, aes(Age,Employment_status , fill=Amount_Spent_on_Consumer_Goods_Services)) + 
  geom_tile()+
  scale_fill_distiller(palette = "PuOr") +
labs(y="Employment_status",
         x="Age", 
       title="Distribution of Age & Employment_status with Amount_Spent_on_Consumer_Goods_Services)")+  
theme_bw()+
theme(plot.title = element_text(size=16)
      ,axis.text.x= element_text(size=12),
       axis.text.y= element_text(size=12),
        axis.title=element_text(size=12))


#-------------------------------------------------------------------------------

ggplot(hcfs, aes(Education_Level,Employment_status , fill=Total_Gross_Income)) + 
  geom_tile()+
  scale_fill_distiller(palette = "YlOrRd") +
labs(y="Employment_status",
         x="Education_Level", 
       title="Distribution of Education_Level & Employment_status with Total_Gross_Income)")+  
theme_bw()+
theme(plot.title = element_text(size=16)
      ,axis.text.x= element_text(size=12),
       axis.text.y= element_text(size=12),
        axis.title=element_text(size=12))


#-------------------------------------------------------------------------------

ggplot(hcfs, aes(Education_Level,Investment_Attitudes , fill=Total_Gross_Income)) + 
  geom_tile()+
  scale_fill_distiller(palette = "BuGn") +
labs(y="Investment_Attitudes",
         x="Education_Level", 
       title="Distribution of Education_Level & Investment_Attitudes with Total_Gross_Income)")+  
theme_bw()+
theme(plot.title = element_text(size=16)
      ,axis.text.x= element_text(size=12),
       axis.text.y= element_text(size=12),
        axis.title=element_text(size=12))


#-------------------------------------------------------------------------------

ggplot(hcfs, aes(Gender,Housing_Status , fill=Total_Gross_Income)) + 
  geom_tile()+
  scale_fill_distiller(palette = "RdYlBu") +
labs(y="Housing_Status",
         x="Gender", 
       title="Distribution of Gender & Housing_Status with Total_Gross_Income)")+  
theme_bw()+
theme(plot.title = element_text(size=16)
      ,axis.text.x= element_text(size=12),
       axis.text.y= element_text(size=12),
        axis.title=element_text(size=12))


#-------------------------------------------------------------------------------

```


## Pie Charts

```{r}

# Create a table of education levels
edu_table <- table(hcfs$Education_Level)

# Plot a pie chart with the frequency count and percentage labels
pie(edu_table, main = "Education Level", labels = paste(names(edu_table), " (", round(100*edu_table/sum(edu_table),1), "%)", sep = ""))


# Create a table of education levels
edu_table <- table(hcfs$Gender)

# Plot a pie chart with the frequency count and percentage labels
pie(edu_table, main = "Education Level", labels = paste(names(edu_table), " (", round(100*edu_table/sum(edu_table),1), "%)", sep = ""))


# Create a table of education levels
edu_table <- table(hcfs$Investment_Attitudes)

# Plot a pie chart with the frequency count and percentage labels
pie(edu_table, main = "Education Level", labels = paste(names(edu_table), " (", round(100*edu_table/sum(edu_table),1), "%)", sep = ""))


# Create a table of education levels
edu_table <- table(hcfs$Employment_status)

# Plot a pie chart with the frequency count and percentage labels
pie(edu_table, main = "Education Level", labels = paste(names(edu_table), " (", round(100*edu_table/sum(edu_table),1), "%)", sep = ""))

```

## Skewness

To understand how the data is distributed, the shape and center we have computed the skew values of the major metric columns and plotted the following histograms to visualize them.

```{r, Skewness, echo=FALSE}

cols <- c("Total_Gross_Income","AMount_Spent_on_Utilities", "Amount_Spent_on_Consumer_Goods_Services",
          "Employee_Income", "Self_Employment_income", "Financial_assets_Income", "Value_of_Self_employment_Businesses", "Pension_Income", "Amount_spent_on_Food_at_Home",
          "Rental_Income", "Credit_Card_Debt", "Value_of_Saving_Accounts", "Income_From_Other_Sources")

# Compute skewness for the selected columns
skew <- apply(hcfs[cols], 2, skewness)

# Print the skewness values
print(skew)

# Plot a histogram for the variables with skewness values
ggplot(data = gather(hcfs[cols]), aes(x = value)) +
  geom_histogram(fill = "lightblue") +
  facet_wrap(~ key, scales = "free_x") +
  ggtitle("Distribution of variables with skewness values") +
  xlab("Value") +
  ylab("Frequency")

# ------------------------------------------------------------------------------

skew_TGI <- skewness(hcfs$Total_Gross_Income)

ggplot(data.frame(x = hcfs$Total_Gross_Income), aes(x)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "lightblue", col = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(hcfs$Total_Gross_Income), sd = sd(hcfs$Total_Gross_Income)), color = "red") +
  labs(x = "Gross Income", y = "Density") +
  ggtitle(paste0("Skewness value = ", round(skew_TGI, 2)))

# ------------------------------------------------------------------------------

skew_ASU <- skewness(hcfs$AMount_Spent_on_Utilities)

ggplot(data.frame(x = hcfs$AMount_Spent_on_Utilities), aes(x)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "pink", col = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(hcfs$AMount_Spent_on_Utilities), sd = sd(hcfs$AMount_Spent_on_Utilities)), color = "red") +
  labs(x = "AMount_Spent_on_Utilities", y = "Density") +
  ggtitle(paste0("Skewness value = ", round(skew_ASU, 2)))

# ------------------------------------------------------------------------------

skew_ASgs <- skewness(hcfs$Amount_Spent_on_Consumer_Goods_Services)

ggplot(data.frame(x = hcfs$Amount_Spent_on_Consumer_Goods_Services), aes(x)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "violet", col = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(hcfs$Amount_Spent_on_Consumer_Goods_Services), sd = sd(hcfs$Amount_Spent_on_Consumer_Goods_Services)), color = "red") +
  labs(x = "Amount_Spent_on_Consumer_Goods_Services", y = "Density") +
  ggtitle(paste0("Skewness value = ", round(skew_ASgs, 2)))

# ------------------------------------------------------------------------------

skew_ASF <- skewness(hcfs$Amount_spent_on_Food_at_Home)

ggplot(data.frame(x = hcfs$Amount_spent_on_Food_at_Home), aes(x)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "yellow", col = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(hcfs$Amount_spent_on_Food_at_Home), sd = sd(hcfs$Amount_spent_on_Food_at_Home)), color = "red") +
  labs(x = "Amount_spent_on_Food_at_Home", y = "Density") +
  ggtitle(paste0("Skewness value = ", round(skew_ASF, 2)))

# ------------------------------------------------------------------------------

skew_Ei <- skewness(hcfs$Employee_Income)

ggplot(data.frame(x = hcfs$Employee_Income), aes(x)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "blue", col = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(hcfs$Employee_Income), sd = sd(hcfs$Employee_Income)), color = "red") +
  labs(x = "Employee_Income", y = "Density") +
  ggtitle(paste0("Skewness value = ", round(skew_Ei, 2)))


# ------------------------------------------------------------------------------

skew_SEi <- skewness(hcfs$Self_Employment_income)

ggplot(data.frame(x = hcfs$Self_Employment_income), aes(x)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "green", col = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(hcfs$Self_Employment_income), sd = sd(hcfs$Self_Employment_income)), color = "red") +
  labs(x = "Self_Employment_income", y = "Density") +
  ggtitle(paste0("Skewness value = ", round(skew_SEi, 2)))


# ------------------------------------------------------------------------------

skew_FAi <- skewness(hcfs$Financial_assets_Income)

ggplot(data.frame(x = hcfs$Financial_assets_Income), aes(x)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "brown", col = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(hcfs$Financial_assets_Income), sd = sd(hcfs$Financial_assets_Income)), color = "red") +
  labs(x = "Financial_assets_Income", y = "Density") +
  ggtitle(paste0("Skewness value = ", round(skew_FAi, 2)))


# ------------------------------------------------------------------------------

skew_Ri <- skewness(hcfs$Rental_Income)

ggplot(data.frame(x = hcfs$Rental_Income), aes(x)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "orange", col = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(hcfs$Rental_Income), sd = sd(hcfs$Rental_Income)), color = "red") +
  labs(x = "Rental_Income", y = "Density") +
  ggtitle(paste0("Skewness value = ", round(skew_Ri, 2)))

# ------------------------------------------------------------------------------

skew_Pi <- skewness(hcfs$Pension_Income)

ggplot(data.frame(x = hcfs$Pension_Income), aes(x)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "purple", col = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(hcfs$Pension_Income), sd = sd(hcfs$Pension_Income)), color = "red") +
  labs(x = "Pension_Income", y = "Density") +
  ggtitle(paste0("Skewness value = ", round(skew_Pi, 2)))

# ------------------------------------------------------------------------------

skew_iOS <- skewness(hcfs$Income_From_Other_Sources)

ggplot(data.frame(x = hcfs$Income_From_Other_Sources), aes(x)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "grey", col = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(hcfs$Income_From_Other_Sources), sd = sd(hcfs$Income_From_Other_Sources)), color = "red") +
  labs(x = "Income_From_Other_Sources", y = "Density") +
  ggtitle(paste0("Skewness value = ", round(skew_iOS, 2)))

# ------------------------------------------------------------------------------

skew_SB <- skewness(hcfs$Value_of_Self_employment_Businesses)

ggplot(data.frame(x = hcfs$Value_of_Self_employment_Businesses), aes(x)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "lightgreen", col = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(hcfs$Value_of_Self_employment_Businesses), sd = sd(hcfs$Value_of_Self_employment_Businesses)), color = "red") +
  labs(x = "Value_of_Self_employment_Businesses", y = "Density") +
  ggtitle(paste0("Skewness value = ", round(skew_SB, 2)))

# ------------------------------------------------------------------------------

skew_SA <- skewness(hcfs$Value_of_Saving_Accounts)

ggplot(data.frame(x = hcfs$Value_of_Saving_Accounts), aes(x)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "lightyellow", col = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(hcfs$Value_of_Saving_Accounts), sd = sd(hcfs$Value_of_Saving_Accounts)), color = "red") +
  labs(x = "Value_of_Saving_Accounts", y = "Density") +
  ggtitle(paste0("Skewness value = ", round(skew_SA, 2)))

# ------------------------------------------------------------------------------

skew_CD <- skewness(hcfs$Credit_Card_Debt)

ggplot(data.frame(x = hcfs$Credit_Card_Debt), aes(x)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "lightpink", col = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(hcfs$Credit_Card_Debt), sd = sd(hcfs$Credit_Card_Debt)), color = "red") +
  labs(x = "Credit_Card_Debt", y = "Density") +
  ggtitle(paste0("Skewness value = ", round(skew_CD, 2)))

# ------------------------------------------------------------------------------

```

# Correlation

The following set of scatter plots show the pairwise relationships between the numeric variables in the dataset. Considering three subsets of the dataset that include different combinations of variables related to income, expenses, and assets the plots visually identify patterns and correlations.

```{r, scatterPlots, echo=FALSE}

hcfs_subset1 <- hcfs[, c("Total_Gross_Income", "Employee_Income",
                        "Self_Employment_income", "Financial_assets_Income", 
                        "Rental_Income", "Pension_Income", "Credit_Card_Debt",
                        "Income_From_Other_Sources")]


  

hcfs_subset2 <- hcfs[, c("AMount_Spent_on_Utilities", 
                        "Amount_Spent_on_Consumer_Goods_Services","Amount_spent_on_Food_at_Home",
                  "Amount_Spent_on_Food_Outside_Home","Monthly_Amount_Paid_As_Rent")]


hcfs_subset3 <- hcfs[, c("Value_of_Household_Vehicles",
"Valuables",
"Deposits",
"Mutual_Funds",
"Bonds",
"Value_of_Self_employment_Businesses",
"Total_Real_Assets",
"Total_Value_of_Cars",
"Value_Of_Other_Vehicles",
"Value_Of_Other_Valuables"
)]

# Draw scatter plots of numeric columns against each other as a grid

ggpairs(hcfs_subset1)

ggpairs(hcfs_subset2)

ggpairs(hcfs_subset3)
```

To further visualize the relationship between the variables correlation matrix, and correlation plot using the corrplot package are drawn.

```{r Correlation, echo=FALSE}

# Select only the numeric variables from hcfs
hcfs_numeric <- hcfs %>% 
  select_if(is.numeric)

#summary(hcfs_numeric)

hcfs_cor <- cor(hcfs_numeric)

hcfs_cor_melted <- melt(hcfs_cor)

ggplot(melt(hcfs_cor), aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradientn(colours=rev(brewer.pal(11, "RdYlBu"))) +
  labs(title="HCFS Dataset Correlation Matrix", x="Variables", y="Variables") +
  theme(plot.title = element_text(hjust = 0.5))

# ggpairs(hcfs_numeric, title="correlogram with ggpairs()") 
```

```{r, echo=FALSE}
#-------------------------------------------------------------------------------

hcfs_subset2 <- hcfs[, c("Age", "Total_Gross_Income", "AMount_Spent_on_Utilities", 
                        "Amount_Spent_on_Consumer_Goods_Services", "Employee_Income",
                        "Self_Employment_income", "Financial_assets_Income", 
                        "Rental_Income", "Pension_Income", "Credit_Card_Debt",
                        "Value_of_Saving_Accounts")]


hcfs_subset2$Age <- as.factor(hcfs_subset2$Age)

# Compute the correlation matrix
cor_matrix <- cor(hcfs_subset2[,2:11], use = "pairwise.complete.obs")
cor_matrix_df <- as.data.frame(cor_matrix)

kable(cor_matrix_df)

# Plot the correlation matrix
corrplot(cor_matrix, method = "color", type = "lower", 
         order = "hclust", tl.col = "black", 
         tl.srt = 30, tl.cex = 0.6, cl.pos = "n", cl.length = 2.5, 
         col = colorRampPalette(c("white", "purple"))(100))

#-------------------------------------------------------------------------------
```

The correlation matrix shows the pairwise correlations between the variables in the dataset. The correlation coefficient ranges from -1 to 1, where a value of 1 indicates a perfect positive correlation, a value of 0 indicates no correlation, and a value of -1 indicates a perfect negative correlation.

- The variables Total Gross Income and Amount Spent on Consumer Goods and Services have a moderate positive correlation 0.56, while the variables Amount Spent on Utilities and Amount Spent on Consumer Goods and Services have a moderate positive correlation 0.46.

- The variables Employee Income and Pension Income have a weak positive correlation with Total Gross Income 0.36 and 0.27, respectively, while Self Employment Income and Financial Assets Income have a weak positive correlation with Total Gross Income 0.23 and 0.15, respectively.

- The variables Credit Card Debt and Value of Saving Accounts have weak positive correlations with Total Gross Income 0.04 and 0.09, respectively.

```{r, echo=FALSE}
hcfs_subset3 <- hcfs[, c("Value_of_Household_Vehicles",
"Valuables",
"Deposits",
"Mutual_Funds",
"Bonds",
"Value_of_Self_employment_Businesses",
"Total_Real_Assets",
"Income_From_Other_Sources",
"Monthly_Amount_Paid_As_Rent",
"Total_Value_of_Cars",
"Value_Of_Other_Vehicles",
"Value_Of_Other_Valuables",
"No_of_PrivateLoans",
"Amount_spent_on_Food_at_Home",
"Amount_Spent_on_Food_Outside_Home")]


# Compute the correlation matrix
cor_matrix <- cor(hcfs_subset3, use = "pairwise.complete.obs")

cor_matrix_df <- as.data.frame(cor_matrix)

kable(cor_matrix_df)


# Compute the correlation matrix
cor_matrix <- cor(hcfs_subset3, use = "pairwise.complete.obs")

# Plot the correlation matrix
corrplot(cor_matrix, method = "color", type = "lower", 
         order = "hclust", tl.col = "black", 
         tl.srt = 30, tl.cex = 0.6, cl.pos = "n", cl.length = 2.5, 
         col = colorRampPalette(c("white", "blue"))(100))


```

The correlation matrix suggests that the Total Real Assets have a strong positive correlation with the Value of Self-employment Businesses (0.27), Valuables (0.49), and Total Value of Cars (0.32). The Monthly Amount Paid as Rent has a negative correlation with Total Real Assets (-0.22) and Value of Household Vehicles (-0.09).

The other variables have low or moderate correlations with each other. For example, Income from Other Sources has a very low correlation with most other variables, while No of Private Loans has a moderate positive correlation with Valuables (0.23).

# Hypothesis Testing

## Hypothesis Statements

To perform Hypothesis testing on the dataset the following set of questions were considered to test whether there is a significant difference between two groups or whether there is a significant relationship between two variables. Based on previous computations of data distribution and skew values the parametric and non parametric tests were selected.

```{r, Hypothesis_Statements, echo=FALSE}

Gen_hyp <- read_excel("D:/1. Team 14 Project/Hypothesis.xlsx", sheet = "Gender")
ft_gen <- flextable(Gen_hyp)
ft_gen <- fontsize(ft_gen, size = 10)
ft_gen <- align(ft_gen, align = "left")
ft_gen <- set_caption(ft_gen, "Hypothesis Statements of Independent Variable Gender")
ft_gen

age_hyp <- read_excel("D:/1. Team 14 Project/Hypothesis.xlsx", sheet = "Age")
ft_age <- flextable(age_hyp)
ft_age <- fontsize(ft_age, size = 10)
ft_age <- align(ft_age, align = "left")
ft_age <- set_caption(ft_age, "Hypothesis Statements of Independent Variable Age")
ft_age

Educ_hyp <- read_excel("D:/1. Team 14 Project/Hypothesis.xlsx", sheet = "Educ")
ft_edu <- flextable(Educ_hyp)
ft_edu <- fontsize(ft_edu, size = 10)
ft_edu <- align(ft_edu, align = "left")
ft_edu <- set_caption(ft_edu, "Hypothesis Statements of Independent Variable Education Level")
ft_edu
```

## T-tests

The following box plots show how the means of the metric variables for male and female.

```{r, Gender vs Metric Plots, echo=FALSE}

g11 <- ggplot(hcfs, aes(x = Gender, y = Total_Gross_Income)) +
  geom_boxplot(fill = "#69b3a2") +
  labs(x = "Gender", y = "Total Gross Income")
  
g12 <- ggplot(hcfs, aes(x = Gender, y = AMount_Spent_on_Utilities)) +
  geom_boxplot(fill = "#69b3a2") +
  labs(x = "Gender", y = "AMount Spent on Utilities") 

g13 <- ggplot(hcfs, aes(x = Gender, y = Amount_Spent_on_Consumer_Goods_Services)) +
  geom_boxplot(fill = "#69b3a2") +
  labs(x = "Gender", y = "Amount Spent on Consumer Goods Services") 

grid.arrange(g11,g12,g13, ncol = 3)

g14 <- ggplot(hcfs, aes(x = Gender, y = Employee_Income)) +
  geom_boxplot(fill = "#69b3a2") +
  labs(x = "Gender", y = "Employee Income") 

g15 <- ggplot(hcfs, aes(x = Gender, y = Self_Employment_income)) +
  geom_boxplot(fill = "#69b3a2") +
  labs(x = "Gender", y = "Self Employment Income") 

g16 <- ggplot(hcfs, aes(x = Gender, y = Financial_assets_Income)) +
 geom_boxplot(fill = "#69b3a2") +
  labs(x = "Gender", y = "Financial assets Income") 

g17 <- ggplot(hcfs, aes(x = Gender, y = Rental_Income)) +
  geom_boxplot(fill = "#69b3a2") +
  labs(x = "Gender", y = "Rental Income") 

grid.arrange(g14,g15,g16,g17, ncol = 4)

g18 <- ggplot(hcfs, aes(x = Gender, y = Credit_Card_Debt)) +
  geom_boxplot(fill = "#69b3a2") +
  labs(x = "Gender", y = "Credit Card Debt") 

g19 <- ggplot(hcfs, aes(x = Gender, y = Value_of_Saving_Accounts)) +
  geom_boxplot(fill = "#69b3a2") +
  labs(x = "Gender", y = "Balance in Savings Account") 

grid.arrange(g18,g19, ncol = 2)

```

```{r, T-test}
t.test(Total_Gross_Income ~ Gender, data = hcfs)
```

  We conducted a Welch Two Sample t-test to determine if there is a difference in the Total Gross Income between males and females. The test was performed with a significance level of 0.05. The test showed that the t-value was -23.467 with a degrees of freedom (df) of 5521.1 and a p-value of less than 2.2e-16, which is much smaller than the significance level. This indicates strong evidence against the null hypothesis and suggests that there is a statistically significant difference in the means of Total Gross Income between males and females. The 95 percent confidence interval for the difference in means ranged from -6866.277 to -5807.520. The sample mean for females was 20823.63 and for males it was 27160.53.

## Non-Parametric Tests with Gender

```{r, Mann Whitney U test}

hcfs_subset <- hcfs[, c("AMount_Spent_on_Utilities", "Amount_Spent_on_Consumer_Goods_Services", "Employee_Income", "Self_Employment_income", "Financial_assets_Income", "Value_of_Self_employment_Businesses", "Pension_Income", "Amount_spent_on_Food_at_Home", "Rental_Income", "Credit_Card_Debt", "Value_of_Saving_Accounts", "Income_From_Other_Sources", "Gender")]

cols_of_interest <- c("AMount_Spent_on_Utilities", "Amount_Spent_on_Consumer_Goods_Services", "Employee_Income",
                      "Self_Employment_income", "Financial_assets_Income", "Value_of_Self_employment_Businesses",
                      "Pension_Income", "Amount_spent_on_Food_at_Home", "Rental_Income", "Credit_Card_Debt",
                      "Value_of_Saving_Accounts", "Income_From_Other_Sources")

# Perform Wilcoxon-Mann-Whitney test for each column
for(col in cols_of_interest) {
  test_res <- hcfs_subset %>%
    wilcox.test(formula = as.formula(paste(col, "~ Gender")), data = .)
  
  print(paste0("Column: ", col))
  print(test_res)
}

```

  Each test is a Wilcoxon rank sum test with continuity correction that is used to compare two groups: male and female, for each variable. The null hypothesis is that there is no difference between the two groups and the alternative hypothesis is that there is a difference between the two groups.For all variables except "Pension Income", the p-value is less than 0.05, which means that we reject the null hypothesis and conclude that there is a significant difference between the two groups for these variables. The p-values for "Pension Income", "Value of Saving Accounts" and "Income From Other Sources" are greater than 0.05, which means that we fail to reject the null hypothesis and conclude that there is no significant difference between the two groups for these variables. Numeric values for the test statistic (W) and p-value are given for each variable.

  The test statistic measures the difference between the median values of the two groups, and the p-value represents the probability of obtaining the observed test statistic, or one more extreme, assuming the null hypothesis is true. In conclusion, the results of the Wilcoxon rank sum tests suggest that there is a significant difference between male and female for most of the variables except "Pension Income", "Value of Saving Accounts" and "Income From Other Sources".

  These results suggest that gender is a significant factor in determining the differences in the variables such as "Amount Spent on Utilities", "Amount Spent on Consumer Goods and Services", "Employee Income", "Self Employment Income", "Financial Assets Income", "Value of Self Employment Businesses", "Amount Spent on Food at Home", "Rental Income" and "Credit Card Debt".
  
```{r, Test Results1, echo=FALSE}

Gen_hypR <- read_excel("D:/1. Team 14 Project/Hypothesis.xlsx", sheet = "Gen")
ft_gen <- flextable(Gen_hypR)
ft_gen <- fontsize(ft_gen, size = 10)
ft_gen <- align(ft_gen, align = "left")
ft_gen <- set_caption(ft_gen, "Hypothesis Test Results")
ft_gen

```

## ANOVA

Before performing the ANOVA test the following plots were drawn,

```{r ANOVA_plots, echo=FALSE}

ggplot(hcfs, aes(x = Age, y = Total_Gross_Income)) +
  geom_boxplot() +
  labs(x = "Age Group", y = "Total Gross Income") +
  ggtitle("Box Plot of Age vs Total Gross Income")

ggplot(hcfs, aes(x = Education_Level, y = Total_Gross_Income)) +
  geom_boxplot() +
  labs(x = "Education Level", y = "Total Gross Income") +
  ggtitle("Box Plot of Education Level vs Total Gross Income")

```

```{r ANOVA_Age}

fit <- lm(Total_Gross_Income ~ Age, data = hcfs)

anova(fit)

```

  This analysis presents an ANOVA table for the response variable Total_Gross_Income, which is being analyzed in terms of the Age group. The null hypothesis in this test is that there is no significant difference in the mean Total_Gross_Income across the different age groups, and the alternative hypothesis is that there is a significant difference in the mean Total_Gross_Income across at least one age group. The table shows that there are 5 degrees of freedom for Age and 8150 degrees of freedom for Residuals. The sum of squares for Age is 7.0898e+10, while the sum of squares for Residuals is 1.0566e+12. The mean sum of squares for Age is 1.4180e+10, while the mean sum of squares for Residuals is 1.2964e+08. The F-statistic for this test is 109.38, which has a p-value less than 2.2e-16, indicating that there is significant evidence to reject the null hypothesis. Therefore, we can conclude that there is a significant difference in the mean Total_Gross_Income across at least one age group. The age group variable is a significant predictor of the Total_Gross_Income, and we can reject the null hypothesis.

```{r, Anova_Education_level}

fit <- lm(Total_Gross_Income ~ Education_Level, data = hcfs)
anova(fit)

```

Above we have analysis of variance table for the response variable Total_Gross_Income, where the data has been divided by Education_Level. The table shows the results of the one-way ANOVA, which tests whether there are any statistically significant differences in the mean Total_Gross_Income between the different Education_Level groups.

The table reports two degrees of freedom (df) values: Df for Education_Level and Df for Residuals. The Sum Sq column displays the sum of squares for each source of variation, while the Mean Sq column displays the mean sum of squares for each source of variation. The F-value and its associated p-value (Pr(>F)) test whether there is a significant difference between groups, with a lower p-value indicating a greater likelihood that the differences are statistically significant. In this case, the p-value is less than 0.001 (< 2.2e-16), which indicates strong evidence that there are statistically significant differences between the mean Total_Gross_Income for the different Education_Level groups. Therefore, we reject the null hypothesis that there is no difference in mean Total_Gross_Income between the groups.

## Non-Parametric Tests with Age

```{r, Kruskall Wallis Age}

hcfs_selected <- hcfs %>% select(Age, AMount_Spent_on_Utilities, Amount_Spent_on_Consumer_Goods_Services,
                                  Employee_Income, Self_Employment_income, Financial_assets_Income,
                                  Value_of_Self_employment_Businesses, Amount_spent_on_Food_at_Home, Credit_Card_Debt,
                                  Value_of_Saving_Accounts, Income_From_Other_Sources)

for (col in 2:ncol(hcfs_selected)) {
  kw_result <- kruskal.test(as.formula(paste(colnames(hcfs_selected)[col], "~", "Age")), data = hcfs_selected)
  print(paste("Column:", colnames(hcfs_selected)[col]))
  print(kw_result)
}

```

The Kruskal-Wallis rank sum test was performed on 10 different columns of data categorized by age groups. The test was used to determine whether there were statistically significant differences between the medians of each age group for each variable. For the column "Amount_Spent_on_Utilities", the Kruskal-Wallis chi-squared value was 131.27 with 5 degrees of freedom and a p-value of less than 2.2e-16, indicating strong evidence of a significant difference in median amount spent on utilities across age groups. Similarly, for the remaining nine columns, the Kruskal-Wallis test yielded chi-squared values and p-values that strongly suggested significant differences in median values across age groups. Based on these results, we reject the null hypothesis that there are no differences in median values across age groups for each variable, and conclude that age is a significant factor in determining the median value for each variable.

## Non-Parametric Tests with Education level

```{r, Kruskall Wallis Educ}

hcfs_selected <- hcfs %>% select(Education_Level, AMount_Spent_on_Utilities, Amount_Spent_on_Consumer_Goods_Services,
                                  Employee_Income, Self_Employment_income, Financial_assets_Income,
                                  Value_of_Self_employment_Businesses, Pension_Income,
                                  Amount_spent_on_Food_at_Home, Credit_Card_Debt,
                                  Value_of_Saving_Accounts, Income_From_Other_Sources)


for (col in 2:ncol(hcfs_selected)) {
  kw_result <- kruskal.test(as.formula(paste(colnames(hcfs_selected)[col], "~", "Education_Level")), data = hcfs_selected)
  print(paste("Column:", colnames(hcfs_selected)[col]))
  print(kw_result)
}

```

  The test results can be used to evaluate whether there is a statistically significant difference between the education levels in terms of the amount spent on utilities, amount spent on consumer goods and services, employee income, self-employment income, financial assets income, value of self-employment businesses, pension income, amount spent on food at home, credit card debt, value of savings accounts, and income from other sources. For all columns, the p-value is less than 0.05, indicating that there is a statistically significant difference between the education levels with respect to the amount spent on utilities, amount spent on consumer goods and services, employee income, self-employment income, financial assets income, value of self-employment businesses, pension income, amount spent on food at home, credit card debt, value of savings accounts, and income from other sources.

  Therefore, we reject the null hypothesis that there is no significant difference between the education levels with respect to the amount spent on utilities, amount spent on consumer goods and services, employee income, self-employment income, financial assets income, value of self-employment businesses, pension income, amount spent on food at home, credit card debt, value of savings accounts, and income from other sources. The alternative hypothesis is that there is a significant difference between the education levels for these variables.

 For example, for the column "Amount_Spent_on_Utilities," the Kruskal-Wallis chi-squared value is 453.25, with 3 degrees of freedom and a p-value of less than 2.2e-16, which is less than the significance level of 0.05. Thus, we reject the null hypothesis.

```{r, Test Results, echo=FALSE}

age_hyp <- read_excel("D:/1. Team 14 Project/Hypothesis.xlsx", sheet = "AgeR")
ft_age <- flextable(age_hyp)
ft_age <- fontsize(ft_age, size = 10)
ft_age <- align(ft_age, align = "left")
ft_age <- set_caption(ft_age, "Hypothesis Test Results - Age")
ft_age

Educ_hyp <- read_excel("D:/1. Team 14 Project/Hypothesis.xlsx", sheet = "EducR")
ft_edu <- flextable(Educ_hyp)
ft_edu <- fontsize(ft_edu, size = 10)
ft_edu <- align(ft_edu, align = "left")
ft_edu <- set_caption(ft_edu, "Hypothesis Test Results - Education Level")
ft_edu

```

## Chi-Square

To determine if there is significant association between gender and the categorical variables in our dataset we performed chi square test on selected subset of dataset with 12 different HAS variables. 

The following Hypothesis statements are considered,

- Null hypothesis (H0): There is no association between gender and any of the listed categorical variables related to assets and financial status.

- Alternative hypothesis (HA): There is an association between gender and at least one of the listed categorical variables related to assets and financial status.. 

The variables are listed in the first column and are of character data type as yes or no responses. The test statistic value and p-value are provided for each variable.

```{r Chi-Square, echo=FALSE}

cat_vars <- c("Has_Real_Assets", "Has_Financial_Assets", "Has_Vehicles", "Has_Valuables", 
              "Has_Real_Estate_Wealth", "Has_Deposits", "Has_Mutual_Funds", "Has_Bonds", 
              "Has_Shares", "Has_Debt", "Has_Credit_Card_Debt", "Has_Private_Loans", 
              "Has_Applied_for_Loan_Credit")

results <- data.frame()

# looping through each categorical variable and performing chi-square test
for (var in cat_vars) {
  # create a contingency table of gender vs the current variable
  tab <- table(hcfs$Gender, hcfs[[var]])
  # perform chi-square test and store the result in a data frame
  res <- data.frame(variable = var, test = "Chi-square test", 
                    statistic = chisq.test(tab)$statistic, 
                    p.value = chisq.test(tab)$p.value)
  # append the result to the results data frame
  results <- rbind(results, res)
}

# print the results
print(results)

```

From the results, we can see that for most of the variables, the p-value is less than 0.05, which is the commonly used significance level. This means that we can reject the null hypothesis and conclude that there is a significant association between gender and the listed variables. However, for the variable Has_Valuables and Has_Private_Loans the p-value is greater than 0.05, which means that we cannot reject the null hypothesis and conclude that there is no significant association between gender and Has_Valuables or Has_Private_Loans.

```{r, Test Results3, echo=FALSE}

chi_hyp <- read_excel("D:/1. Team 14 Project/Hypothesis.xlsx", sheet = "Chi")
ft_chi <- flextable(chi_hyp)
ft_chi <- fontsize(ft_chi, size = 10)
ft_chi <- align(ft_chi, align = "left")
ft_chi <- set_caption(ft_chi, "Chi Square Test Results")
ft_chi

```

# Regression

## Linear Regression

To check how does total gross income affect the amount spent on consumer goods and services we considered a linear regression model assuming that there is linear relationship between the variables we performed the test and obtained the following result.

```{r Linear_Regression, echo=FALSE}

# Perform linear regression
reg_model <- lm(Amount_Spent_on_Consumer_Goods_Services ~ Total_Gross_Income, data = hcfs)

# Print summary of the model
summary(reg_model)

ggplot(hcfs, aes(x = Total_Gross_Income, y = Amount_Spent_on_Consumer_Goods_Services)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Linear Regression Plot") +
  xlab("Total Gross Income") +
  ylab("Amount Spent on Consumer Goods and Services")

```

A linear regression analysis was performed to examine the influence of the variable Total_Gross_Income on the variable Amount_Spent_on_Consumer_Goods_Services.

The regression model showed that the variable Total_Gross_Income explained 31.91% of the variance from the variable Amount_Spent_on_Consumer_Goods_Services. An ANOVA was used to test whether this value was significantly different from zero. Using the present sample, it was found that the effect was significantly different from zero, F=3821.04, p = <.001, R2 = 0.32.

The following regression model is obtained, 

Amount_Spent_on_Consumer_Goods_Services = 377.79 +0.03 · Total_Gross_Income

When all independent variables are zero, the value of the variable Amount_Spent_on_Consumer_Goods_Services is 377.79. If the value of the variable Total_Gross_Income changes by one unit, the value of the variable Amount_Spent_on_Consumer_Goods_Services changes by 0.03.

The standardized coefficients beta are independent of the measured variable and are always between -1 and 1. The larger the amount of beta, the greater the contribution of the respective independent variable to explain the dependent variable Amount_Spent_on_Consumer_Goods_Services . In this model, the variable Total_Gross_Income has the greatest influence on the variable Amount_Spent_on_Consumer_Goods_Services.

A linear regression analysis was performed to examine the influence of the variable Value_of_Saving_Accounts on the variable Amount_Spent_on_Consumer_Goods_Services.

```{r Linear_Regression2, echo=FALSE}

# Perform linear regression
reg_model <- lm(Amount_Spent_on_Consumer_Goods_Services ~ Value_of_Saving_Accounts, data = hcfs)

# Print summary of the model
summary(reg_model)

ggplot(hcfs, aes(x = Value_of_Saving_Accounts, y = Amount_Spent_on_Consumer_Goods_Services)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Linear Regression Plot") +
  xlab("Value_of_Saving_Accounts") +
  ylab("Amount Spent on Consumer Goods and Services")

```

The regression model showed that the variable Value_of_Saving_Accounts explained 0.63% of the variance from the variable Amount_Spent_on_Consumer_Goods_Services. An ANOVA was used to test whether this value was significantly different from zero. Using the present sample, it was found that the effect was significantly different from zero, F=51.74, p = <.001, R2 = 0.01.

The following regression model is obtained,

Amount_Spent_on_Consumer_Goods_Services = 1204.98 +0.03 · Value_of_Saving_Accounts

When all independent variables are zero, the value of the variable Amount_Spent_on_Consumer_Goods_Services is 1204.98.
If the value of the variable Value_of_Saving_Accounts changes by one unit, the value of the variable Amount_Spent_on_Consumer_Goods_Services changes by 0.03. In this model, the variable Value_of_Saving_Accounts has the greatest influence on the variable Amount_Spent_on_Consumer_Goods_Services.

## Multiple Linear Regression

A multiple linear regression analysis was performed to examine the influence of the variables Total_Gross_Income and Value_of_Saving_Accounts on the variable Total_Real_Assets.

```{r Multi_Linear Education_Level, echo=FALSE}

# Subset the data to relevant variables
hcfs_subset <- hcfs[, c("Total_Real_Assets", "Total_Gross_Income", "Value_of_Saving_Accounts")]

# Check for missing data
sum(is.na(hcfs_subset))

# Remove rows with missing data
hcfs_subset <- na.omit(hcfs_subset)

# Perform multiple linear regression
model <- lm(Total_Real_Assets ~ Total_Gross_Income + Value_of_Saving_Accounts, data = hcfs_subset)

# View the model summary
summary(model)
```

The regression model showed that the variables Total_Gross_Income and Value_of_Saving_Accounts explained 8.3% of the variance from the variable Total_Real_Assets. An ANOVA was used to test whether this value was significantly different from zero. Using the present sample, it was found that the effect was significantly different from zero, F=369.14, p = <.001, R2 = 0.08.

The following regression model is obtained,

Total_Real_Assets = 9570.2 +8.42 · Total_Gross_Income +2.07 · Value_of_Saving_Accounts

When all independent variables are zero, the value of the variable Total_Real_Assets is 9570.2. If the value of the variable Total_Gross_Income changes by one unit, the value of the variable Total_Real_Assets changes by 8.42. If the value of the variable Value_of_Saving_Accounts changes by one unit, the value of the variable Total_Real_Assets changes by 2.07. In this model, the variable Total_Gross_Income has the greatest influence on the variable Total_Real_Assets.

A multiple linear regression analysis was performed to examine the influence of the variables Employee_Income, Self_Employment_income, Rental_Income, Financial_assets_Income and Pension_Income on the variable Value_of_Household_Vehicles.

```{r Multi_Linear2, echo=FALSE}

# Subset the data to relevant variables
hcfs_subset <- hcfs[, c("Value_of_Household_Vehicles", "Employee_Income", "Rental_Income", "Self_Employment_income", "Financial_assets_Income","Pension_Income")]

# Check for missing data
sum(is.na(hcfs_subset))

# Remove rows with missing data
hcfs_subset <- na.omit(hcfs_subset)

# Perform multiple linear regression
model <- lm(Value_of_Household_Vehicles  ~ Employee_Income  + Self_Employment_income + Rental_Income  + Financial_assets_Income + Pension_Income , data = hcfs_subset)

# View the model summary
summary(model)
```

The regression model showed that the variables Employee_Income, Self_Employment_income, Rental_Income, Financial_assets_Income and Pension_Income explained 22.92% of the variance from the variable Value_of_Household_Vehicles. An ANOVA was used to test whether this value was significantly different from zero. Using the present sample, it was found that the effect was significantly different from zero, F=484.68, p = <.001, R2 = 0.23.

The following regression model is obtained,

Value_of_Household_Vehicles = 1975.53 +0.16 · Employee_Income +0.14 · Self_Employment_income +0.03 · Rental_Income +0.34 · Financial_assets_Income +0.09 · Pension_Income

- When all independent variables are zero, the value of the variable Value_of_Household_Vehicles is 1975.53.

- If the value of the variable Employee_Income changes by one unit, the value of the variable Value_of_Household_Vehicles changes by 0.16.

- If the value of the variable Self_Employment_income changes by one unit, the value of the variable Value_of_Household_Vehicles changes by 0.14.

- If the value of the variable Rental_Income changes by one unit, the value of the variable Value_of_Household_Vehicles changes by 0.03.

- If the value of the variable Financial_assets_Income changes by one unit, the value of the variable Value_of_Household_Vehicles changes by 0.34.

- If the value of the variable Pension_Income changes by one unit, the value of the variable Value_of_Household_Vehicles changes by 0.09.

In this model, the variable Employee_Income has the greatest influence on the variable Value_of_Household_Vehicles.

## Logistic Regression

To perform Logistic Regression we considered the following questions,

1. What is the relationship between age, education level, employment status and the likelihood of having credit card debt?
2. Can likelihood of having mutual funds be predicted based on Gender, Education level and other variables?

```{r, echo=FALSE}
#-----------------------------------------------------

# Subset the data for relevant columns
hcfs_subset <- hcfs[, c("Education_Level", "Employment_status", "Has_Credit_Card_Debt")]

hcfs_subset <- hcfs_subset %>%
  mutate(Has_Credit_Card_Debt = recode(Has_Credit_Card_Debt, "Yes" = 1, "No" = 0, .default = 0))

# Fit a logistic regression model
logit_model <- glm(Has_Credit_Card_Debt ~ Education_Level + Employment_status, data=hcfs_subset, family=binomial)

# Summarize the model
summary(logit_model)

```
The logistic regression model includes Education_Level and Employment_status as predictors of whether a person has credit card debt. The coefficients show the direction and magnitude of the effect of each predictor on the outcome variable.

The intercept coefficient is -3.8523, which is the log-odds of having credit card debt when all predictors are zero.

The Education_Level coefficients show that compared to having a tertiary education level, having a lower secondary or primary education level is associated with a lower log-odds of having credit card debt. The coefficient for upper secondary education level is not statistically significant.

The Employment_status coefficients show that compared to being employed full-time, being retired is associated with a lower log-odds of having credit card debt, while being self-employed or unemployed is not significantly associated with credit card debt. The coefficient for "other" employment status is not statistically significant.

The deviance residuals indicate that the model fits the data reasonably well, and the AIC is 722.28, which suggests that the model is a good fit.


```{r}

# Subset the data for relevant columns
hcfs_subset <- hcfs[, c("Education_Level", "Employment_status", "Has_Applied_for_Loan_Credit", "Housing_Status")]

hcfs_subset <- hcfs_subset %>%
  mutate(Has_Applied_for_Loan_Credit = recode(Has_Applied_for_Loan_Credit, "Yes" = 1, "No" = 0, .default = 0))

# Fit a logistic regression model
logit_model <- glm(Has_Applied_for_Loan_Credit  ~ Education_Level + Employment_status + Housing_Status, data=hcfs_subset, family=binomial)

# Summarize the model
summary(logit_model)

```
The logistic regression model assesses the association between the probability of having applied for a loan credit and the independent variables education level, employment status, and housing status. The coefficients and standard errors of the model indicate the direction and strength of the relationship between the dependent variable and independent variables.

The p-values associated with the coefficients of the independent variables show that education level, employment status, and housing status are all significant predictors of having applied for a loan credit. Among the education levels, those with lower secondary education are more likely to apply for loan credit compared to those with primary education. Similarly, among employment status, those who are self-employed and those who have other employment status are more likely to apply for loan credit compared to those who are unemployed. Among housing status, those who own a house with a mortgage and those who rent are more likely to apply for loan credit compared to those who live in other housing arrangements.

The null and residual deviance and AIC show that the model provides a good fit to the data. The number of Fisher Scoring iterations indicates the number of iterations required to fit the model to the data.


```{r}

# Subset the data for relevant columns
hcfs_subset <- hcfs[, c("Education_Level", "Employment_status", "Has_Mutual_Funds", "Gender", "Has_Real_Assets")]

hcfs_subset <- hcfs_subset %>%
  mutate(Has_Mutual_Funds = recode(Has_Mutual_Funds, "Yes" = 1, "No" = 0, .default = 0))

# Fit a logistic regression model
logit_model <- glm(Has_Mutual_Funds  ~ Education_Level + Employment_status + Has_Real_Assets + Gender, data=hcfs_subset, family=binomial)

# Summarize the model
summary(logit_model)

library(ggplot2)

# Create a data frame with predictor values
newdata <- expand.grid(
  Education_Level = unique(hcfs_subset$Education_Level),
  Employment_status = unique(hcfs_subset$Employment_status),
  Gender = unique(hcfs_subset$Gender),
  Has_Real_Assets = unique(hcfs_subset$Has_Real_Assets)
)

# Add predicted probabilities to data frame
newdata$prob <- predict(logit_model, newdata, type="response")

# Plot the logistic regression curve
ggplot(newdata, aes(x=prob, color=Employment_status)) +
  geom_density() +
  xlab("Predicted Probability of Having Mutual Funds") +
  ylab("Density") +
  ggtitle("Logistic Regression Curve")


```

The logistic regression model tests the association between the binary response variable "Has_Mutual_Funds" and the predictor variables "Education_Level," "Employment_status," "Has_Real_Assets," and "Gender." The model's deviance residuals indicate that the model fits the data well.

The coefficients of the model reveal that individuals with lower education levels are less likely to have mutual funds, with estimates of -1.61 for "Lower secondary," -2.05 for "Primary education," and -0.70 for "Upper secondary" education levels. Retired individuals and those who are self-employed are more likely to have mutual funds, with estimates of 0.33 and 0.59, respectively, while individuals in other employment status categories are less likely to have mutual funds.

Moreover, individuals with real assets are more likely to have mutual funds, with an estimate of 2.04, and male individuals are more likely to have mutual funds than female individuals, with an estimate of 0.34. The significance codes reveal that all coefficients are statistically significant, except for "Employment_statusUnemployed" and "Has_Real_AssetsYes" at the 0.05 significance level.

The null and residual deviances of the model suggest that the model explains a substantial amount of the variation in the data. The Akaike information criterion (AIC) value of 3421.3 indicates that this model is better than other candidate models with higher AIC values.

```{r, echo=FALSE}
#-----------------------------------------------------

# # Subset the data
# hcfs_subset <- hcfs[, c("Has_Mutual_Funds", "Total_Gross_Income", "Age", "Gender", 
#                         "Education_Level", "Has_Financial_Assets", "Has_Valuables", 
#                         "Has_Real_Estate_Wealth", "Has_Deposits")]
# 
# hcfs_subset <- hcfs_subset %>%
#   mutate(Has_Mutual_Funds = recode(Has_Mutual_Funds, "Yes" = 1, "No" = 0, .default = 0)) %>%
#   mutate(Has_Financial_Assets = recode(Has_Financial_Assets, "Yes" = 1, "No" = 0, .default = 0)) %>%
#   mutate(Has_Valuables = recode(Has_Valuables, "Yes" = 1, "No" = 0, .default = 0)) %>%
#   mutate(Has_Real_Estate_Wealth = recode(Has_Real_Estate_Wealth, "Yes" = 1, "No" = 0, .default = 0)) %>%
#   mutate(Has_Deposits = recode(Has_Deposits, "Yes" = 1, "No" = 0, .default = 0))
# 
# # Convert categorical variables to factors with reference levels
# hcfs_subset$Gender <- factor(hcfs_subset$Gender, levels=c("Female", "Male"), ordered=TRUE)
# 
# hcfs_subset$Education_Level <- factor(hcfs_subset$Education_Level, ordered=TRUE)
# 
# # Fit the logistic regression model
# model <- glm(Has_Mutual_Funds ~ Total_Gross_Income + Age + Gender + Education_Level + 
#                   Has_Financial_Assets + Has_Valuables + Has_Real_Estate_Wealth + Has_Deposits,
#               data=hcfs_subset, family=binomial)
# 
# # Summarize the model
# summary(model)

# The results show the output of a logistic regression analysis. The model aimed to examine the association between the binary outcome variable, Has_Mutual_Funds, and various predictors such as Total_Gross_Income, Age, Gender, Education_Level, Has_Financial_Assets, Has_Valuables, Has_Real_Estate_Wealth, and Has_Deposits.
# 
# The coefficients of the model can be interpreted as follows:
# 
# - Holding all other variables constant, for every one unit increase in Total_Gross_Income, the odds of having mutual funds increase by a factor of exp(0.00004809) = 1.000048, or about 0.005%.
# 
# - For Age, the coefficients are presented in polynomial form, with the L, Q, C, and higher-order coefficients indicating different powers of Age. The L coefficient corresponds to linear age effects, the Q coefficient to quadratic effects, and so on. It appears that there is a significant linear age effect, as the Age.L coefficient is positive and significant.
# 
# - Females have a lower odds of having mutual funds compared to males, as indicated by the positive and significant Gender.L coefficient.
# 
# - Compared to the reference level of Education_Level, individuals with higher education levels are more likely to have mutual funds. The Education_Level.L and Education_Level.Q coefficients are both significant, indicating a linear and quadratic effect of education level, respectively.
# 
# - The variables Has_Financial_Assets and Has_Deposits do not seem to be significant predictors of having mutual funds, as their coefficients are not significant.
# 
# - The null and residual deviances are also shown, with a difference of 453.1 degrees of freedom between them. This suggests that the model is an improvement over the null model and fits the data well. The AIC value of 3293.7 indicates that this model has a better fit than other models with different sets of predictors.

```

# Principal Component Analysis

To reduce the dimensionality of the data by identifying the most important variables, which can be used to represent the data, Principal Component Analysis of the numeric columns in the dataset was performed.

```{r, PCA, echo=FALSE}

# # select only the columns with numeric data
# hcfs_num <- hcfs[, sapply(hcfs, is.numeric)]
# 
# hcfs_num <- hcfs_num[,-c(1,2,3,4)]
# 
# names(hcfs_num)
# 
# # scale the data
# hcfs_scaled <- scale(hcfs_num)
# 
# # perform PCA
# hcfs_pca <- prcomp(hcfs_scaled, center = TRUE, scale. = TRUE)
# 
# # view summary of the PCA results
# summary(hcfs_pca)
# 
# # plot the PCA results
# biplot(hcfs_pca)


# The result shows the principal components analysis (PCA) for a dataset consisting of 26 variables related to the financial situation of households. The PCA reduces the dimensionality of the dataset by identifying the most important patterns or relationships among the variables. The result indicates that the first 10 principal components explain more than 75% of the variance in the data, with the first component explaining the largest proportion of variance (20.55%). The biplot shows the relationships among the variables and the principal components, where the direction and length of the arrows indicate the strength and direction of the relationship between the variables and the principal components.

```

```{r}
# Select only the numeric variables from hcfs
hcfs_numeric <- hcfs %>% 
  select_if(is.numeric)

#str(hcfs_numeric)

keep1<-subset(hcfs_numeric, select = 3:ncol(hcfs_numeric))

# # scale the data
# hcfs_scaled <- scale(keep1)
# 
# # perform PCA
# hcfs_pca <- prcomp(hcfs_scaled, center = TRUE, scale. = TRUE)
# 
# # view summary of the PCA results
# summary(hcfs_pca)
# 
# # plot the PCA results
# biplot(hcfs_pca)

# Principal Components Analysis creating 15 principal components (i.e. artificial variables)

cat("Doing PCA\n")

# change the number “15” in the code below this line if you want to adjust the number of principal components to be created from your data
pc <- principal(keep1, nfactors=min(ncol(keep1),15), rotate="varimax") #rotated
print(summary(pc)) # print the variance accounted for by each principal component
print(loadings(pc)) # pc loadings for each observed variable
print(pc$values)

# create scree plot (to help decide how many PCs to keep)
plot(pc$values,type="l",main="", xlab="# factors", ylab="Eigenvalue") # scree plot

```

The test of the hypothesis that 15 factors are sufficient shows that the degrees of freedom for the model is 40 and the objective function was 41.33. The number of observations was 8156, and the Chi-Square value was 336272.5 with prob <0, which indicates that the model is a good fit for the dataset.

The loadings table provides information about the correlation between each variable in the dataset and each of the principal components. The values in the table represent the factor loadings, which indicate the strength and direction of the relationship between the variables and the components. The higher the absolute value of the factor loading, the stronger the relationship between the variable and the component.The retained components are listed in the columns, and the variables are listed in the rows. The values in the table represent the correlation between the variable and the component, with values close to 1 or -1 indicating a strong correlation.

For example, the variable "Value_of_Household_Vehicles" has a loading of 0.27 on the first principal component (RC1), indicating a moderate positive correlation. The variable "Valuables" has a loading of 0.98 on RC2, indicating a strong positive correlation. The variable "Credit_Card_Debt" has a loading of 1 on RC4, indicating a perfect correlation. The variable "Amount_Spent_on_Food_Outside_Home" has a loading of -0.53 on RC15, indicating a moderate negative correlation.

The table also shows the eigenvalues and the proportion and cumulative proportion of variance explained by each component. The eigenvalues indicate the amount of variance in the data that is explained by each component. The proportion and cumulative proportion of variance explained indicate the proportion of total variance in the data that is explained by each component and the cumulative proportion of variance explained by each successive component. For example, the first principal component (RC1) explains 12.2% of the total variance in the data, while the first two components (RC1 and RC2) together explain 23% of the total variance.

The root mean square of the residuals (RMSA) is 0.04, which is low, indicating that the model has a good fit to the data.

Overall, the output suggests that 15 components are sufficient to explain the variability in the dataset. The loadings table shows that several variables are strongly correlated with the first few components, indicating that these components capture important information in the data.

# Decision Trees

To identify the important factors that determine whether a person has private loans, deposits, real estate wealth, vehicles, financial assets, debt, or real assets we used decision trees. 

We converted the categorical variables to factors and considered a subset of data to include variables, such as gender, age, employment status, education level, total gross income, and whether the household owns saving accounts or has credit card debt.

We then fit a decision tree model using the rpart() function to predict whether an individual has private loans, deposits, real estate wealth, vehicles, financial assets, debt, or real assets based on all available predictors, and then plotted the decision tree using the rpart.plot() function to visualize the important predictors that influence the target variable.

```{r, Decision Tree}


library(rpart)
library(rpart.plot)

#Subset the data
hcfs_subset <- hcfs[, c("Gender", "Age", "Employment_status", "Education_Level", 
                        "Total_Gross_Income", "Has_Debt", "Household_Owns_Saving_accounts", 
                        "Has_Credit_Card_Debt",
                        "Has_Mutual_Funds", "Has_Shares", "Has_Bonds","Has_Real_Assets", "Has_Financial_Assets", "Has_Vehicles", "Has_Valuables", 
              "Has_Real_Estate_Wealth", "Has_Deposits",
              "Has_Private_Loans", 
              "Has_Applied_for_Loan_Credit")]

# Convert categorical variables to factors
hcfs_subset$Gender <- as.factor(hcfs_subset$Gender)
hcfs_subset$Employment_status <- as.factor(hcfs_subset$Employment_status)
hcfs_subset$Education_Level <- as.factor(hcfs_subset$Education_Level)
hcfs_subset$Has_Debt <- as.factor(hcfs_subset$Has_Debt)
hcfs_subset$Household_Owns_Saving_accounts <- as.factor(hcfs_subset$Household_Owns_Saving_accounts)
hcfs_subset$Has_Credit_Card_Debt <- as.factor(hcfs_subset$Has_Credit_Card_Debt)
hcfs_subset$Has_Mutual_Funds <- as.factor(hcfs_subset$Has_Mutual_Funds)
hcfs_subset$Has_Shares <- as.factor(hcfs_subset$Has_Shares)
hcfs_subset$Has_Bonds <- as.factor(hcfs_subset$Has_Bonds)
hcfs_subset$Has_Real_Assets <- as.factor(hcfs_subset$Has_Real_Assets)
hcfs_subset$Has_Financial_Assets <- as.factor(hcfs_subset$Has_Financial_Assets)
hcfs_subset$Has_Vehicles <- as.factor(hcfs_subset$Has_Vehicles)
hcfs_subset$Has_Real_Estate_Wealth <- as.factor(hcfs_subset$Has_Real_Estate_Wealth)
hcfs_subset$Has_Deposits <- as.factor(hcfs_subset$Has_Deposits)
hcfs_subset$Has_Private_Loans <- as.factor(hcfs_subset$Has_Private_Loans)
hcfs_subset$Has_Applied_for_Loan_Credit <- as.factor(hcfs_subset$Has_Applied_for_Loan_Credit)

```

```{r, DecTree, echo=FALSE}
# Fit a decision tree model
tree_model <- rpart(Has_Private_Loans ~ ., data = hcfs_subset, method = "class")

# Plot the decision tree
rpart.plot(tree_model)


# Fit a decision tree model
tree_model <- rpart(Has_Real_Estate_Wealth ~ ., data = hcfs_subset, method = "class")

# Plot the decision tree
rpart.plot(tree_model)

# Fit a decision tree model
tree_model <- rpart(Has_Vehicles ~ ., data = hcfs_subset, method = "class")

# Plot the decision tree
rpart.plot(tree_model)

# Fit a decision tree model
tree_model <- rpart(Has_Debt ~ ., data = hcfs_subset, method = "class")

# Plot the decision tree
rpart.plot(tree_model)

# Fit a decision tree model
tree_model <- rpart(Has_Real_Assets ~ ., data = hcfs_subset, method = "class")

# Plot the decision tree
rpart.plot(tree_model)
```
