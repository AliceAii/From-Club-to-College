
##1.Set up environment ----
rm(list=ls())
#Load Packages 
library(haven)
library(tidyverse)
library(dplyr)
library(forcats)
library(skimr)
library(flextable)
library(table1)
library(knitr)
library(patchwork)
library(car)
library(naniar)
library(mice)

#use this restricted dataset to link SCH_ID
u13student <- read_sav("/Users/aishuhan/Desktop/UCLA PhD/Dataset_IES_HERI/IES Restricted HSLS 2009-2013/Data/SPSS data files/u13student.sav")

school <- read_sav("/Users/aishuhan/Desktop/UCLA PhD/Dataset_IES_HERI/IES Restricted HSLS 2009-2013/Data/SPSS data files/School.sav")

#this dataset has college level variables, we can find some to merge in the current dataset later
pets_data <- read_dta("/Users/aishuhan/Desktop/UCLA PhD/Dataset_IES_HERI/HSLS Public Data 2009-2021/hsls_17_student_pets_sr_v1_0.dta")

#set up the current dataset we use
load("/Users/aishuhan/Desktop/UCLA PhD/Dataset_IES_HERI/HSLS 2009-2013 Public Use Data ICPSR_36423/DS0002 Student Level Data/36423-0002-Data.rda")
hsls_stu_raw <- da36423.0002
remove(da36423.0002)

#############clean data################
stu_data <- u13student %>%
  dplyr::select(
    STU_ID, SCH_ID, W3W1W2STU, #index and weights # W1SCHOOL in school-level data
    S2SCLUB, S2SGROUP, #Treatment
    X2SCIID, S3FIELD_STEM, #DV
    X1SCIID, X1STU30OCC_STEM1, #pre-IV
    
    X1CONTROL, A1MSAFTERSCH, A1MSMENTOR, 
    X1FREELUNCH, A1FREELUNCH, X1SCHWHITE, A1WHITESTU, X1SCHASIAN, A1AP,  #
    C1APCOURSE, C1ENRICHMENT, C1SCHOLARSHP, C1PURSUE,
    X1REGION,  X1LOCALE, #school-level IVs
    
    X1RACE, X1SEX, X1SESQ5, X1TXMSCR, X1SCIINT, X1STUEDEXPCT, X1SCHOOLBEL, X1SCHOOLENG,
    S1SMUSEUM, S1SBOOKS, S1WEBINFO,
    S1ALG1M09, S1ALG2M09, S1GEOM09, S1SFALL09, S1TALKFUTURE) #student-level IVs

skim(stu_data)

table(stu_data$S2SCLUB == 1 | stu_data$S2SGROUP == 1 )

# Join with W1SCHOOL from the school dataset
stu_data <- stu_data %>%
  left_join(dplyr::select(school, SCH_ID, W1SCHOOL), by = "SCH_ID") #add school weight

print(colSums(is.na(stu_data[c("X1FREELUNCH", "A1FREELUNCH", "X1SCHWHITE", "A1WHITESTU", "A1AP")])))  #percentage
print(colSums(is.na(stu_data[c("C1APCOURSE", "C1ENRICHMENT", "C1SCHOLARSHP", "C1PURSUE")])))  #binary


#prepare each variable
stu_data <- stu_data %>% 
  mutate(
    STU_ID = as.character(haven::as_factor(STU_ID)),
    SCH_ID = as.character(haven::as_factor(SCH_ID)),
    
    #create Treatment variable (science club | science group between 9th and 11th)
    sciclubgroup = case_when(S2SCLUB == 1 | S2SGROUP == 1 ~ 1, 
                             S2SCLUB == 0 & S2SGROUP == 0 ~ 0,
                             TRUE ~ NA_real_),
    sciclubgroup =factor(sciclubgroup, levels = c(0, 1), labels = c("Control", "Treatment")),
    
    #clean DVs - 11th grade sciid
    x2sciid = as.numeric(X2SCIID),
    x2sciid = ifelse(x2sciid %in% c(-8, -9), NA, x2sciid),
    
    #clean DVs - STEM major aspiration - 0 = non-STEM major, not decide
    #Item legitimate skip/NA, Unit non-response, Missing is coded as NA
    collegeSTEMmajorasp = ifelse(S3FIELD_STEM %in% c(-7, -8, -9), NA, S3FIELD_STEM),
    collegeSTEMmajorasp = if_else(S3FIELD_STEM == 1, 1, 0),
    collegeSTEMmajorasp = factor(collegeSTEMmajorasp, levels = c(0, 1), labels = c("No", "Yes")),
    
    #clean pre-IVs - 9th grade sciid
    x1sciid = as.numeric(X1SCIID),
    x1sciid = ifelse(x1sciid %in% c(-8, -9), NA, x1sciid),
    
    #clean pre-IVs - 9th grade STEM occupation aspiraitons
    stemoccasp9th = case_when(X1STU30OCC_STEM1 %in% 1:6 ~ 1,
                              X1STU30OCC_STEM1 == 0 ~ 0,
                              TRUE ~ NA_real_),
    stemoccasp9th = factor(stemoccasp9th, levels = c(0, 1), labels = c("non-STEM", "STEM")),
    
    #school-level IVs
      #schoole locale/urbancity, reference = Rural
      x1locale = case_when(X1LOCALE == 4 ~ 0,  # Rural
                           X1LOCALE == 1 ~ 1,  # City
                           X1LOCALE == 2 ~ 2,  # Suburb
                           X1LOCALE == 3 ~ 3,  # Town
                           TRUE ~ NA_real_),
      x1locale = factor(x1locale, levels = 0:3, labels = c("Rural", "City", "Suburb", "Town")),
      
      #school control
      x1control = case_when(X1CONTROL %in% c(2, 3) ~ 0,  # Catholic or other private
                            X1CONTROL == 1 ~ 1,  # Public
                            TRUE ~ NA_real_),
      x1control = factor(x1control, levels = 0:1, labels = c("Catholic or other private", "Public")),
    
      #school region
      x1region = case_when(X1REGION == 1 ~ 0,  # Northeast
                           X1REGION == 2 ~ 1,  # Midwest
                           X1REGION == 3 ~ 2,  # South
                           X1REGION == 4 ~ 3,  # West
                           TRUE ~ NA_real_),
      x1region = factor(x1region, levels = 0:3, labels = c("Northeast", "Midwest", "South", "West")),
      
      #sponsors a math or science after-school program
      a1mspdintrst = case_when(A1MSAFTERSCH == 0 ~ 0,  #No
                               A1MSAFTERSCH == 1 ~ 1,  #Yes
                               TRUE ~ NA_real_),
      a1mspdintrst = factor(a1mspdintrst, levels = 0:1, labels = c("No", "Yes")),
      
      #pairs students with mentors in math or science
      a1msmentor = case_when(A1MSMENTOR == 0 ~ 0,  #No
                             A1MSMENTOR == 1 ~ 1,  #Yes
                             TRUE ~ NA_real_),
      a1msmentor = factor(a1msmentor, levels = 0:1, labels = c("No", "Yes")),
    
     
    #student-level IVs - Background
      #school belonging and school engaging
      x1schoolbel = ifelse(as.numeric(X1SCHOOLBEL)%in% c(-8, -9), NA, as.numeric(X1SCHOOLBEL)),
      x1schooleng = ifelse(as.numeric(X1SCHOOLENG)%in% c(-8, -9), NA, as.numeric(X1SCHOOLENG)),
  
      #race, gender, ses
      x1race = case_when(
        X1RACE == 8 ~ 0,                  # White
        X1RACE == 2 ~ 1,                  # Asian
        X1RACE == 3 ~ 2,                  # Black
        X1RACE %in% c(4, 5) ~ 3,          # Latino
        X1RACE == 6 ~ 4,                  # Multiracial
        X1RACE == 1 ~ 5,                  # Native American
        X1RACE %in% c(0, 7) ~ 6,          # Other
        TRUE ~ NA_real_),
      x1race = factor(x1race,
                      levels = 0:6,
                      labels = c("White", "Asian", "Black", "Latino",
                                 "Multiracial", "Native American", "Other")),
      x1sex = case_when(X1SEX == 1 ~ 0,  # Male
                        X1SEX == 2 ~ 1,  # Female
                        TRUE ~ NA_real_),
      x1sex = factor(x1sex, levels = c(0, 1), labels = c("Male", "Female")),

      x1sesq5 = case_when(X1SESQ5 == 1 ~ 0,  # First quintile
                          X1SESQ5 == 2 ~ 1,
                          X1SESQ5 == 3 ~ 2,
                          X1SESQ5 == 4 ~ 3,
                          X1SESQ5 == 5 ~ 4,
                          TRUE ~ NA_real_),
      x1sesq5 = factor(x1sesq5, levels = 0:4, labels = c("First quintile (lowest)", "Second quintile",
                                "Third quintile", "Fourth quintile", "Fifth quintile (highest)")),
      
      #baseline math achievement
      x1txmscr = ifelse(as.numeric(X1TXMSCR)%in% c(-8, -9), NA, as.numeric(X1TXMSCR)),
    
      #baseline science class interest
      x1sciint = ifelse(as.numeric(X1SCIINT)%in% c(-8, -9), NA, as.numeric(X1SCIINT)),
    
       #Recode X1STUEDEXPCT into 5 categories, reference = Uncertain
      x1stuedexpct = case_when(
                               X1STUEDEXPCT %in% c(1, 2) ~ 0,   # High school
                               X1STUEDEXPCT %in% 3:6 ~ 1,       # Bachelor
                               X1STUEDEXPCT %in% 7:8 ~ 2,       # Master
                               X1STUEDEXPCT %in% 9:10 ~ 3,      # PhD
                               X1STUEDEXPCT == 11 ~ 4,          # Uncertain
                               TRUE ~ NA_real_),
      x1stuedexpct = factor(x1stuedexpct, levels = 0:4, labels = c("High school", "Bachelor", "Master", "PhD", "Uncertain")),
    
    #student-level IVs - Formal and Informal Education
      #formal education: take algebra I, algebra II, geometry, and take science course 
      s1alg1m09 = case_when(S1ALG1M09 == 0 ~ 0,  #No
                            S1ALG1M09 == 1 ~ 1,  #Yes
                           TRUE ~ NA_real_),
      s1alg1m09 = factor(s1alg1m09, levels = 0:1, labels = c("No", "Yes")),
    
      s1alg2m09 = case_when(S1ALG2M09 == 0 ~ 0,  #No
                            S1ALG2M09 == 1 ~ 1,  #Yes
                            TRUE ~ NA_real_),
      s1alg2m09 = factor(s1alg2m09, levels = 0:1, labels = c("No", "Yes")),
    
      s1tgeom09 = case_when(S1GEOM09 == 0 ~ 0,  #No
                            S1GEOM09 == 1 ~ 1,  #Yes
                            TRUE ~ NA_real_),
      s1tgeom09 = factor(s1tgeom09, levels = 0:1, labels = c("No", "Yes")),
    
      s1sfall09 = case_when(S1SFALL09 == 0 ~ 0,  #No
                            S1SFALL09 == 1 ~ 1,  #Yes
                          TRUE ~ NA_real_),
      s1sfall09 = factor(s1sfall09, levels = 0:1, labels = c("No", "Yes")),
      
      #informal education: numeric, science museum, science book, computer technology
      s1smuseum = ifelse(as.numeric(S1SMUSEUM)%in% c(-8, -9), NA, as.numeric(S1SMUSEUM)),
      s1sbooks = ifelse(as.numeric(S1SBOOKS)%in% c(-8, -9), NA, as.numeric(S1SBOOKS)),
      s1webinfo = ifelse(as.numeric(S1WEBINFO)%in% c(-8, -9), NA, as.numeric(S1WEBINFO)))


stu_data <- stu_data %>%
  mutate(
    #school percentage of free lunch students
    x1freelunch = case_when(X1FREELUNCH %in% c(0, 1, 2) ~ 0, # Low Free Lunch Percentage (<20%)
                            X1FREELUNCH %in% c(3, 4, 5) ~ 1, # Medium Free Lunch Percentage (20-50%)
                            X1FREELUNCH %in% c(7, 8, 9, 10, 11) ~ 2, # High Free Lunch Percentage (50-100%)
                            X1FREELUNCH %in% c(-8, -9) ~ NA_real_, 
                            TRUE ~ NA_real_),
    x1freelunch = factor(x1freelunch, levels = 0:2,
                         labels = c("Low (<20%)", 
                                    "Medium (20-50%)", 
                                    "High (50-100%)")),
    
    #school percentage of student of color
    a1whitestu = case_when(A1WHITESTU %in% c(-5, -8, -9) ~ NA_real_, # Treat suppressed, non-response, missing as NA
                           TRUE ~ A1WHITESTU), # Keep valid percentages as they are
    a1stucolor_perc = 100 - a1whitestu,
    
    a1stucolor = case_when(a1stucolor_perc  >= 0 & a1stucolor_perc  < 10 ~ 0,   # Very Low % Students of Color (0-10%)
                           a1stucolor_perc  >= 10 & a1stucolor_perc  < 40 ~ 1,  # Low % Students of Color (10-40%)
                           a1stucolor_perc  >= 40 & a1stucolor_perc  < 70 ~ 2,  # Medium % Students of Color (40-70%)
                           a1stucolor_perc  >= 70 & a1stucolor_perc  <= 100 ~ 3, # High % Students of Color (70-100%)
                           TRUE ~ NA_real_),
    a1stucolor = factor(a1stucolor, levels = 0:3,
                        labels = c("Very Low (<10%)",
                                   "Low (10-40%)",
                                   "Medium (40-70%)",
                                   "High (70-100%)")),
    
    #school percentage of students who enrolled in AP classes
    a1ap = case_when(is.na(A1AP) ~ NA_real_, # Keep existing NAs as NA
                    A1AP < 0 | A1AP > 100 ~ NA_real_, # Treat out-of-range values as NA
                    TRUE ~ A1AP), # Keep valid percentages (0-100)

    x1ap = case_when(a1ap >= 0 & a1ap < 10 ~ 0, # Low AP Enrollment
                     a1ap >= 10 & a1ap < 30 ~ 1, # Medium AP Enrollment
                     a1ap >= 30 & a1ap <= 100 ~ 2, # High AP Enrollment
                     TRUE ~ NA_real_),
    x1ap = factor(x1ap, levels = 0:2,
                  labels = c("Low (<20%)",
                             "Medium (20-40%)",
                             "High (40-100%)")),
    
    #school supports HS students with scholarships for events/programs/class
    c1scholarship = case_when(C1SCHOLARSHP == 0 ~ 0,  #No
                 C1SCHOLARSHP == 1 ~ 1,  #Yes
                 TRUE ~ NA_real_),
    c1scholarship = factor(c1scholarship, levels = 0:1, labels = c("No", "Yes")),

    #school has program to encourage underrepresented student in math/science
    c1pursue = case_when(C1PURSUE == 0 ~ 0,  #No
                         C1PURSUE == 1 ~ 1,  #Yes
                              TRUE ~ NA_real_),
    c1pursue = factor(c1pursue, levels = 0:1, labels = c("No", "Yes"))
  )

table(stu_data$a1stucolor)

ggplot(stu_data, aes(x = A1AP)) +
  geom_histogram(binwidth = 5, fill = 'steelblue', color = 'black', alpha = 0.7) + 
  theme_minimal()

skim(stu_data)

table(stu_data$collegeSTEMmajorasp)

#sum(stu_data$W3W1W2STU > 0, na.rm = TRUE)
#[1] 16044


#Remove students and schools if there's no one who reports participating in a science club
schools_with_sciclub <- stu_data %>%
  group_by(SCH_ID) %>% 
  summarise(
    has_sciclub = any(sciclubgroup == "Treatment", na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  filter(has_sciclub) %>%            # keep only TRUE rows
  pull(SCH_ID) 

stu_data <- stu_data %>%
  filter(SCH_ID %in% schools_with_sciclub)
                          
#Filter data to outcome 1 Science Identity is not NA, to W3W1W2STU>0 and sciclubgroup is not NA
stu_data.filter.sciid <- stu_data %>% 
  filter(W3W1W2STU>0 & !is.na(sciclubgroup) & !is.na(x2sciid))

#Filter data to outcome 2 STEM Major is not NA, to W3W1W2STU>0 and sciclubgroup is not NA
stu_data.filter.major <- stu_data %>% 
  filter(W3W1W2STU>0 & !is.na(sciclubgroup) & !is.na(collegeSTEMmajorasp))



##2. Prepare Full Dataset ----
#2.a check missingness
#Visual summary of missingness
#vis_miss(stu_data.filter)

# check average missing rate
## for sciid sample
missing_rates.sciid <- colMeans(is.na(stu_data.filter.sciid))

missing_df.sciid <- data.frame(
  variable = names(missing_rates.sciid),
  missing_rate = missing_rates.sciid
) %>%
  arrange(desc(missing_rates.sciid))
View(missing_df.sciid)

mean(missing_rates.sciid)

## for stemmajor sample
missing_rates.stemmajor <- colMeans(is.na(stu_data.filter.major))

missing_df.stemmajor <- data.frame(
  variable = names(missing_rates.stemmajor),
  missing_rate = missing_rates.stemmajor
) %>%
  arrange(desc(missing_rates.stemmajor))
View(missing_df.stemmajor)

mean(missing_rates.stemmajor)


#Missingness pattern is consistent with Missing At Random (MAR) — it varies by observed characteristics but does not appear systematically concentrated by rows (individuals) or completely random.


# impute single value using stochastic imputation. conduct chained equation using mice

######For Sciid Imputation#######

skim(stu_data.filter.sciid)

stu_data.clean.sciid <- stu_data.filter.sciid %>% 
         dplyr::select(STU_ID, SCH_ID, W3W1W2STU, W1SCHOOL,
                       sciclubgroup, #treatment
                       x2sciid,  #DVs
                       x1sciid, stemoccasp9th, #school-level IV
                       x1control, x1freelunch, a1stucolor, x1ap,
                       X1FREELUNCH, a1stucolor_perc, A1AP,
                       a1mspdintrst, a1msmentor, c1scholarship, c1pursue, 
                       x1locale, x1region, #school-level IV
                       x1schoolbel, x1schooleng, 
                       x1race, x1sex, x1sesq5, x1txmscr, x1sciint, x1stuedexpct, #student-level IV background
                       s1alg1m09, s1alg2m09, s1tgeom09, s1sfall09, #student-level IV formal edu
                       s1smuseum, s1sbooks, s1webinfo #student-level IV informal edu
                       )

stu_data.clean.sciid <- stu_data.clean.sciid %>%
  mutate(across(where(is.labelled), ~ if (is.numeric(.) | is.integer(.)) as.numeric(.) else as_factor(.)))

#Using multiple imputation by chained equations (MICE). impute single value. 

#Step1: Create the method vector for mice()
#automatically create methods for all variables
method.sciid <- make.method(stu_data.clean.sciid)

#manually assign based on variable type
method.sciid[c("x1sciid", "x1schoolbel", "x1schooleng", "x1sciint", "x1txmscr", "X1FREELUNCH", "a1stucolor_perc", "A1AP")] <- "norm" # For continuous numeric variables
method.sciid[c("stemoccasp9th", "c1scholarship", "c1pursue", "a1mspdintrst", "a1msmentor", "s1alg1m09", "s1alg2m09", "s1tgeom09", "s1sfall09")] <- "logreg" # For binary or factor variables
method.sciid[c("x1race", "x1sex", "x1sesq5", "x1stuedexpct", "x1locale", "x1control", "x1region", "x1freelunch", "a1stucolor", "x1ap")] <- "polyreg" # For categorical variables with >2 levels
method.sciid[c("s1smuseum", "s1sbooks", "s1webinfo")] <- "pmm" # For ordinal or mixed type data, often a good robust choice

#do not impute IDs or weights
method.sciid[c("STU_ID", "SCH_ID", "W3W1W2STU", "W1SCHOOL", "x2sciid", "sciclubgroup")] <- ""

#Step2: Run mice() with seed for replication
set.seed(123)

stu_data.impute.sciid <- mice(stu_data.clean.sciid, method = method.sciid, m = 1, seed = 123)
stu_data.imputed.sciid <- complete(stu_data.impute.sciid, 1)

skim(stu_data.imputed.sciid)

#save it
setwd("/Users/aishuhan/Desktop/hsls_sciclub")
#saveRDS(stu_data.imputed.sciid, "hslssicclub_datasciid2.rds")


######For STEM Major Imputation#######

stu_data.clean.stemmajor <- stu_data.filter.major%>% 
  dplyr::select(STU_ID, SCH_ID, W3W1W2STU, W1SCHOOL,
                sciclubgroup, #treatment
                collegeSTEMmajorasp,  #DVs
                x1sciid, stemoccasp9th, #school-level IV
                x1control, x1freelunch, a1stucolor, x1ap,
                X1FREELUNCH, a1stucolor_perc, A1AP,
                a1mspdintrst, a1msmentor, c1scholarship, c1pursue, 
                x1locale, x1region, #school-level IV
                x1schoolbel, x1schooleng, 
                x1race, x1sex, x1sesq5, x1txmscr, x1sciint, x1stuedexpct, #student-level IV background
                s1alg1m09, s1alg2m09, s1tgeom09, s1sfall09, #student-level IV formal edu
                s1smuseum, s1sbooks, s1webinfo #student-level IV informal edu
  )

stu_data.clean.stemmajor <- stu_data.clean.stemmajor %>%
  mutate(across(where(is.labelled), ~ if (is.numeric(.) | is.integer(.)) as.numeric(.) else as.factor(.)))

#Using multiple imputation by chained equations (MICE). impute single value. 

#Step1: Create the method vector for mice()
#automatically create methods for all variables
method.stemmajor <- make.method(stu_data.clean.stemmajor)

#manually assign based on variable type
method.stemmajor[c("x1sciid", "x1schoolbel", "x1schooleng", "x1sciint", "x1txmscr", "X1FREELUNCH", "a1stucolor_perc", "A1AP")] <- "norm" # For continuous numeric variables
method.stemmajor[c("stemoccasp9th", "c1scholarship", "c1pursue", "a1mspdintrst", "a1msmentor", "s1alg1m09", "s1alg2m09", "s1tgeom09", "s1sfall09")] <- "logreg" # For binary or factor variables
method.stemmajor[c("x1race", "x1sex", "x1sesq5", "x1stuedexpct", "x1locale", "x1control", "x1region", "x1freelunch", "a1stucolor", "x1ap")] <- "polyreg" # For categorical variables with >2 levels
method.stemmajor[c("s1smuseum", "s1sbooks", "s1webinfo")] <- "pmm" # For ordinal or mixed type data, often a good robust choice

#do not impute IDs or weights
method.stemmajor[c("STU_ID", "SCH_ID", "W3W1W2STU", "W1SCHOOL", "collegeSTEMmajorasp", "sciclubgroup")] <- ""


#Step2: Run mice() with seed for replication
set.seed(123)
stu_data.impute.stemmajor <- mice(stu_data.clean.stemmajor, method = method.stemmajor, m = 1, seed = 123)
stu_data.imputed.stemmajor <- complete(stu_data.impute.stemmajor, 1)

skim(stu_data.imputed.stemmajor)

#save it
setwd("/Users/aishuhan/Desktop/hsls_sciclub")
#saveRDS(stu_data.imputed.stemmajor, "hslssicclub_datastemmajor2.rds")







