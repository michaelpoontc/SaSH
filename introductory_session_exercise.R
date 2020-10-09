# This R script is written for introductory session with SasH group

# File organisation is vital when handling large datasets
# Old habits die hard, so you shoudl start with the best habits
# Always work within a project space so relative paths can be made

# For this short session, you should already have a directory / folder for SaSH
# You should create a RProject from RStudio, saving it in the same directory / folder
# The data file downloaded from the email should also be saved in this place


# Loading libraries and installing packages if required ==========================================
if(!require(plyr)) install.packages("plyr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(here)) install.packages("here", repos = "http://cran.us.r-project.org")
if(!require(gtsummary)) install.packages("gtsummary", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(survival)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(survminer)) install.packages("readxl", repos = "http://cran.us.r-project.org")


library(plyr)
library(tidyverse)
library(here)
library(gtsummary)
library(readxl)
library(survival)
library(survminer)


# Loading in data from excel spreadsheet =========================================================
# You will need to change the file name to the actual file name you have!
sash_raw <- read_excel("synthetic_sash_data.xlsx")

# Viewing your data ==============================================================================
# There are many ways to do this
# We discussed using View() and head() to look at the data
# The names() function allows you to quickly view the columns / variables in your dataframe
# You can also use the symbol '$' to take out a particular column from a dataframe
#     like this: dataframe$column
# Try these

# View(sash_raw)
head(sash_raw)

# What command do you use to know what columns there are? Try it out!



# Can you type in codes that show you what data is contained in a column called "hyponatraemia"?




# Data types =====================================================================================
# As discussed, a large part of data cleaning is about organising your data to allow for 
# the analyses that you have planned. This also applies to making sure that the data types
# are fit for purpose. Remember the class() function tells you what data type the selected
# variable / column is. Here is an example:

class(sash_raw$weight) # This is a numeric variable!

# How about the variable 'mortality_day'



# Let's change it to a numeric variable
# Here is how I would change a variable into a numeric variable
sash_raw$weight <- as.numeric(sash_raw$weight)

# Can you adapt the code so that you change 'mortality_day' into a numeric variable?





# Factors / categorical variable =================================================================
# Categorical variables are very common and tidying this type of data requires a little
# more thoughts because of the levels within a category. In general, the factor() function 
# help us turning a variable into a factor. Let's have a look at the variable / column "sex"

sash_raw$sex
class(sash_raw$sex)

# We know that the variable 'sex' contains "Female" and "Male", and it should really be a
# factor rather than a character variable. So we need to change that to tidy the data.

sash_raw$sex <- factor(sash_raw$sex,
                       levels = c("Female", "Male"))

# The factor() function allows you to also label the levels with names of your choice
# so that you can have a more meaningful / interpretable variable.
sash_raw$sex <- factor(sash_raw$sex,
                       levels = c("Female", "Male"),
                       labels = c("F", "M"))

# Have a look at the variable wfns - it is a character variable at the moment. Can you
# write some R codes to turn it into a factor variables with labels I to V?

sash_raw$wfns
class(sash_raw$wfns)
sash_raw$wfns <- factor(sash_raw$wfns, 
                        levels = c("I : GCS 15, no motor deficit",
                                   "II : GCS 13-14 without deficit",
                                   "III : GCS 13-14 with focal neurological deficit",
                                   "IV : GCS 7-12, with or without deficit",
                                   "V : GCS <7 , with or without deficit"),
                        labels = c("I", "II", "III", "IV", "V"))

# Can you check that you have made the changes you think you've made?
sash_raw$wfns

# Changing or generating variables ===============================================================
# Often we generate a new variable based on existing variables like age-group, BMI categories,
# severity of hyponatraemia. There is a handy way to do that using the mutate() function.

# If I want to group WFNS grades into a binary variable, I could write this:
sash_raw <- sash_raw %>% 
  mutate(wfns_cat = case_when(wfns == "I" ~ 0,
                              wfns == "II" ~ 0,
                              wfns == "III" ~ 1,
                              wfns == "IV" ~ 1,
                              wfns == "V" ~ 1))

# If I want to be even more fancy to change this into a factor variable, I would type:
sash_raw <- sash_raw %>% 
  mutate(wfns_cat = case_when(wfns == "I" ~ 0,
                              wfns == "II" ~ 0,
                              wfns == "III" ~ 1,
                              wfns == "IV" ~ 1,
                              wfns == "V" ~ 1)) %>%
  mutate(wfns_cat = factor(wfns_cat))

# Can you generate a new variable called 'agegroup' with three levels: age <50, age 50-69, and 70+?
# You will need to use some operators to specify the conditions within case_when() function.




# Filtering & subsetting dataset =================================================================
# Sometimes we are only interested in a subset of our dataset or the part that is "clean"
# This can easily be done with filter() and select() function
# Remember the pipe function (%>%) that lets you combine functions one after another
# Also use operators to help

# This is an example

sash_raw %>% filter(`Institute Abbreviation` == "EDI") %>%
  select(age, sex)

# Could you filter data from patients from "EDI" and "GLA" who are females, and
# select only agegroup, sex, and mortality as the variables?




# Mock dataset ===================================================================================
# You should create a new dataset called sash_trim with only the following variables:
# Record_ID, age, agegroup, sex, mortality_day, mortality, LOS_total, hyponatraemia



# Now examine this dataframe for any problems with data types


# The max() function outputs the maximum value but if there is missing data, you need to 
# specify na.rm = T to remove the missing data. For example:

max(sash_trim$age, na.rm = T)

# Also, we need to check for consistency across the dataset. In this case, the length
# of hospital stay "LOS_total" should be the same as the time to death "mortality_day".
# Can you find out whether there are such cases?




# For the purposes of this session, we will turn a blind eye to this problem above.
# You have noticed that the variable 'mortality' is a character variable, but we
# need it to be a variable only containin 1 and 0, turning "Yes" to 1 and "No" to 0. 
# Also we need to have one variable containing the follow up time. In this setting,
# we will take the mortality_day value if the patient has died, and take the LOS_total 
# value if the patient did not die. So let's try to do these two things to complete
# the cleaning process. 
# (1) Change 'mortality' variable to 1 and 0
# (2) New variable called futime specified above





# Initial approach ===============================================================================
# Table one is usually the summary statistics of the dataset, and it is useful in knowing
# what these are. 

sash_trim %>% select(agegroup, sex, hyponatraemia, mortality, futime) %>%
  tbl_summary(by = hyponatraemia) %>% add_p()

# Can you alter the codes so that you have a table stratified by mortality status?





# As part of the univariate analysis, sometimes it is useful to visualise the survival function
# stratified by the group of interest. For example, below are codes to plot survival curves
# by sex. 

fit <- survfit(Surv(futime, mortality) ~ sex, data = sash_trim)
ggsurvplot(fit, conf.int = TRUE, pval = TRUE, data = sash_trim,
           risk.table = TRUE)

# Can you copy and change the codes so that you visualise survival function stratified by 
# hyponatraemia?





# Getting ahead of ourselves =====================================================================

cox_fit <- coxph(Surv(futime, mortality) ~ agegroup + sex + hyponatraemia, data = sash_trim)
tbl_regression(cox_fit, exponentiate = TRUE)




