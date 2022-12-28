# load environment 
library(foreign)  
library(dplyr) 
library(tidyr)   
library(Hmisc)  
library(readxl) 
  
# import excel files from local drive 
adsl <- read_excel("D:/R/adsl.xls") 

# clean ADSL  

# check type of variables
str(adsl) 
 
# check factors available of character variables
as.factor(adsl$USUBJID)
as.factor(adsl$RACE)
as.factor(adsl$Ethnic) 
as.factor(adsl$Country)
as.factor(adsl$Trt01p)
as.factor(adsl$Trt01a)
as.factor(adsl$BORRESP)
as.factor(adsl$NEREASN)
as.factor(adsl$ITTFL)
# 100 USUBJID check, 7 race, 2 ethnic, 2 contries
# 2 levels of TRT01p, CMP123 and Placebo 
# 2 levels of TRT01a, CMP123 and Placebo, missing value found
# 5 levels of BORRESP
# 3 levels of NEREASN, value missing only when BORRESP ne to NE, variabels shows reason of NE

# align sex variables to M and F 
as.factor(adsl$SEX)
# we have factor levels f F  female Female M Male, so rename to F and M only
adsl$SEX[adsl$SEX == "f"] <- "F"
adsl$SEX[adsl$SEX == "female"] <- "F"
adsl$SEX[adsl$SEX == "Female"] <- "F"
adsl$SEX[adsl$SEX == "Male"] <- "M"


# draw timeline for date variables
ggplot(adsl, aes(randdt,USUBJID)) + geom_point()
ggplot(adsl, aes(trtstdtc,USUBJID)) + geom_point()
# with the warning message provided by geom_point default setting we found 5 missing data for randdt
# and 3 missing value for trtstdtc, also based on the visualized point plot 
# we see 2 outliers value in trtstdtc datasets, one is 1900-1-15 another is 1980-1-15
# obviously those two data are recorded or derived incorrectly

# we remove these two observations out and report as data issues, since
# it is easy to find out the study start date and end date in SAP
# here we assume it is 2018-01-01
require(lubridate)

startdate <- as.POSIXct("2018-01-01")

adsl_1 <- adsl[adsl$trtstdtc > startdate,]

# also we remove observations with missing actual treatment records
adsl_clean <- adsl_1[complete.cases(adsl_1$trtstdtc),]

# and we also subset all removed observations and report to primary programmer
adsl_report <- adsl[adsl$trtstdtc < startdate,]


# Derivations and Analysis
# create ITTFL and SAFFL, ITTFL exists, so create SAFFL only
# Y if ittfl = Y and trtstdtc ne missing. N otherwise
adsl_clean$SAFFL[adsl_clean$ITTFL=="Y" & is.na(adsl_clean$trtstdtc)== FALSE] <- "Y"

# get a warning message but variable is successfully created

# derive age group variables
adsl_clean$agegr1[adsl_clean$Age<65] <-"<65" 
adsl_clean$agegr1[adsl_clean$Age>=65] <- ">=65"
adsl_clean$agegr2[adsl_clean$Age<45] <- "<45"
adsl_clean$agegr2[adsl_clean$Age>=45 & adsl_clean$Age<65] <- "45-65"
adsl_clean$agegr2[adsl_clean$Age>=65] <- ">=65"

# select only ITTFL = Y records for analysis
adsla <- adsl_clean[adsl_clean$ITTFL == "Y",]


######### summary: so based on our previous work, we have # of subjects without randomization
######### equal to 5(2 of 5 with unreasonable trtstdtc), we put all 5 in adsl_report dataset
######### we use adsl_a as our analysis dataset. Also I want to point out that we have 2 
######### missing trt01a observations, in this case we do analysis based on itt population
######### so I did not exclude these two, but they should still be reported.


### Generating a summary table
library(magrittr)
library(qwraps2)

options(qwraps2_markup = "markdown")

summary <-
  list("AGE Group1 CATEGORIZATION (%)" =
         list("< 65" = ~ qwraps2::n_perc0(.data$agegr1 == "<65"),
              ">= 65" = ~ qwraps2::n_perc0(.data$agegr1 == ">=65")),
       "AGE Group2 CATEGORIZATION (%)" =
         list("< 45" = ~ qwraps2::n_perc0(.data$agegr2 == "<45"),
              "45 - 65" = ~ qwraps2::n_perc0(.data$agegr2 == "45-65"),
              ">= 65" = ~ qwraps2::n_perc0(.data$agegr2 == ">=65")),
       "GENDER (%)" =
         list("MALE" = ~ qwraps2::n_perc0(.data$SEX == "M"),
              "FEMALE" = ~ qwraps2::n_perc0(.data$SEX == "F")),
       "RACE (%)" =
         list("American Indian or Alaska Native" = ~ qwraps2::n_perc0(.data$RACE == "American Indian or Alaska Native"),
              "Asian" = ~ qwraps2::n_perc0(.data$RACE == "Asian"),
              "Black" = ~ qwraps2::n_perc0(.data$RACE == "Black"),
              "White" = ~ qwraps2::n_perc0(.data$RACE == "White"),
              "Native Hawaiian or Pacific Islander" = ~ qwraps2::n_perc0(.data$RACE == "Native Hawaiian or Pacific Islander"),
              "Multiracial" = ~ qwraps2::n_perc0(.data$RACE == "Multiracial"), 
              "Other" = ~ qwraps2::n_perc0(.data$RACE == "Other")),
       "ETHNICITY (%)" =
         list("MALE" = ~ qwraps2::n_perc0(.data$Ethnic == "Hispanic"),
              "FEMALE" = ~ qwraps2::n_perc0(.data$Ethnic == "Non-Hispanic")),
       "AGE" =
         list("MEAN" = ~ sprintf(mean(.data$Age),fmt = '%#.2f'),
              "MEDIAN" = ~ median(.data$Age),
              "MIN" = ~ min(.data$Age),
              "MAX" = ~ max(.data$Age),
              "STANDARD DEVIATION" = ~sprintf(sd(.data$Age), fmt = '%#.2f' )),
       "PLATELET COUNT" =
         list("MEAN" = ~ sprintf(mean(.data$Pltcnt),fmt = '%#.6f'),
              "MEDIAN" = ~ median(.data$Pltcnt),
              "MIN" = ~ min(.data$Pltcnt),
              "MAX" = ~ max(.data$Pltcnt),
              "STANDARD DEVIATION" = ~sprintf(sd(.data$Pltcnt), fmt = '%#.6f' ))
       )

total <- summary_table(adsla, summary)
total

by_trt01p <- summary_table(dplyr::group_by(adsla, Trt01p), summary)
by_trt01p

whole <- cbind(by_trt01p,total)
whole

print(whole,
      rtitle = "Demographic and Baseline Characteristics Summary",
      cnames = c("CMP123", "Placebo","Total"))


# Done



###########################
######### ADTTE############

# Create KM Plot
library(survival)
library(survminer)

KM <- survfit(Surv(aval,cnsr) ~ trt01a, data = adtte)
ggsurvplot(KM,  
           data = adtte,
           size = 0.5,
           pval = T,
           xlab = "Time(in days)",
           break.time.by = 5,
           risk.table = T,
           risk.table.col = "strata",
           legend.labs = 
             c("CMP135","Placebo"),
           risk.table.height = 0.25,
           ggthem = theme_bw())

# Create waterfall plot, it doesnt seem meaningful to draw a waterfall plot for this certain
# adtte dataset because we dont have response and baseline data. So I just create a plot that
# shows aval changes stratified by trt arm. 

## Plot based on adsl data could show most common kind of wf plot, with change in % of response as x axis
## Concepts are same so we can redo it anytime with almost same process and code.
str(adtte)
adtte$trt01a <- as.factor(adtte$trt01a)

library(ggplot2)

adtte_1 <- adtte[order(-adtte$aval),]
adtte_1$usubjid <- factor(adtte_1$usubjid, levels = adtte_1$usubjid)

WF <- ggplot(adtte_1, aes(usubjid, aval, fill = trt01a, color = trt01a))+
  scale_fill_discrete(name="TREATMENT ARM")+ scale_color_discrete(guide="none")+
  labs(list(title = "waterfall plot for pfs in days"), x=NULL, y = "change in days") +
  theme_classic() %+replace%
  theme(axis.line.x = element_blank(), axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(face="bold",angle=90)) +
  coord_cartesian(ylim = c(-10,35)) +
  geom_bar(stat="identity", width=0.7,position = position_dodge(width=0.4))

WF
