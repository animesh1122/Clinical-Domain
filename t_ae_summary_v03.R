#------------------------------
#Package: tidyverse, kableExtra, rtf
#Goal: Create AE Table using R Base and  
#      tidyverse
#------------------------------
##############################
# Step: 0 - Load libraries
##############################
library(tidyverse)
library(kableExtra)
library(rtf)
##############################
# Step: 1 - Get Data into R
##############################
#Set working Directory
setwd("F:/Learning/R/R_Working_Directory")
adae <- read.csv(file = "F:/Learning/R/R in Clinical Programming/Project 1_AE/adae.csv", 
                 fileEncoding="UTF-8-BOM", 
                 blank.lines.skip = TRUE,
                 header = TRUE)

############################## 
# Step: 2 - Selection of records
##############################
# Select safety population and keep only
# variables needed.
#->>Using Base R Function subset.
adae2 <- subset(adae, SAFFL=="Y", 
                select = c(USUBJID, AEBODSYS, AEDECOD, AETOXGR, TRTA, TRTAN)) 

#Sort Data 
adae3 <- adae2[order(adae2$AEBODSYS, adae2$AEDECOD, -adae2$AETOXGR),]

#Select Highest Toxicity Grade AEs per
# ::A:: Any AE - Toxicity Grade 
any_ae <- adae3 %>%
  group_by(USUBJID, TRTA) %>%
  arrange(USUBJID, TRTA, -AETOXGR)%>%
  slice_head(n=1)

 
# ::B:: Any AEBODSYS AE - Toxicity Grade
any_bodsys <- adae3 %>%
  group_by(USUBJID, TRTA, AEBODSYS) %>%
  arrange(USUBJID, TRTA,AEBODSYS, -AETOXGR)%>%
  slice_head(n=1)

# ::C:: Any AEBODSYS-AEDECOD AE - Toxicity Grade
any_decod <- adae3 %>%
  group_by(USUBJID, TRTA, AEBODSYS, AEDECOD) %>%
  arrange(USUBJID, TRTA,AEBODSYS, AEDECOD, -AETOXGR)%>%
  slice_head(n=1)

##############################
# Step: 3 - Summary Statistics
##############################
#Get Big N
###############################
bign <- table(TRTA=any_ae$TRTAN)
bign[1]

#::A:: Get Freq and Calculate % - ANY AE
any_ae1 <- any_ae %>%
  group_by(TRTA,TRTAN)%>%
  summarise(n=n(),.groups = "keep") %>%
  mutate( pct = if_else(TRTAN == '1', n*100/bign[1], n*100/bign[2] ),
          ORD1=0,
          ORD2=0, 
          txt="Total Subjects with an Event",
          AEBODSYS="Total Subjects with an Event",
          AEDECOD = " " 
  )

#::B:: Get Freq and Calculate % - ANY AEBODSYS
any_bodsys1 <- any_bodsys %>%
  group_by(TRTA,TRTAN, AEBODSYS)%>%
  summarise(n=n(),.groups = "keep") %>%
  mutate( pct = if_else(TRTAN == '1', n*100/bign[1], n*100/bign[2] ),
          ORD1=1,
          ORD2=0, 
          ORD3=9999,
          txt=AEBODSYS, 
          AEDECOD = " " )
# To Use for Ordering ;
any_bodsys_ord <- any_bodsys1 %>%
  group_by(AEBODSYS ) %>%
  summarise(ORD2=sum(ORD2), .groups ="keep")
#------------------------------------
                                    
#::C:: Get Freq and Calculate % - ANY AEBODSYS-AEDECOD
any_decod1 <- any_decod %>%
  group_by(TRTA,TRTAN, AEBODSYS, AEDECOD )%>%
  summarise(n=n(),.groups = "keep") %>%
  mutate( pct = if_else(TRTAN == '1', n*100/bign[1], n*100/bign[2] ),
          ORD1=1,
          ORD2=9999,
          ORD3=n, 
          txt=paste("&nbsp;&nbsp;&nbsp;", AEDECOD, sep = "&nbsp;"), )

##############################
# Step: 4 - Data Arrangement 
##############################

#--------------------------------
# Stack the all data - Any AE + AEBODSYS + AEDECOD
#--------------------------------
all_ae <- rbind(any_ae1,any_bodsys1,
                           any_decod1)
#Sorting - Ordering 
all_ae1 <- all_ae[order(all_ae$TRTA, all_ae$ORD1,all_ae$AEBODSYS, -all_ae$ORD2, -all_ae$ORD3),]

#Create Freq (PCT) variable
all_ae2 <- all_ae1 %>% 
  mutate(var1 = paste (n,"(",format(round(pct,digits = 2),nsmall = 2),")"),
         TRTA = ifelse(TRTA=="Active Drug A", "DrugA","Placebo"))


#--------------------------------
# Transpose Data 
#--------------------------------
t_ae <- all_ae2 %>%
  ungroup()%>%
  select(-TRTAN,-n,-pct, -AEDECOD, -ORD3) %>%
  spread(TRTA, var1) %>%
  mutate(DrugA= ifelse(is.na(DrugA),0, DrugA), Placebo = ifelse(is.na(Placebo),0, Placebo) )%>%
  arrange(ORD1, AEBODSYS, ORD2)
# Drop unnecessary variables
t_ae1 <- subset(t_ae, select=c(-ORD1, -ORD2, -AEBODSYS))

##############################
# Step: 5 - Reporting 
##############################

#-------------------
#Approach 1: html
#-------------------

#-----Alignment of Title----
t1<- paste(rep('&nbsp;',30,),collapse = " ")
t2<- paste(rep('&nbsp;',33,),collapse = " ")

title1 <- "Adverse Event Summary <br/>"
title2<- "Safety Population<br/>"
titleX <- paste(t1,title1,t2,title2, collapse = '')

#Using KTable or kble
kbl(x = t_ae1, 
    col.names = c(" ", "Drug A", "Placebo" ), 
    caption = titleX, 
    escape = FALSE , longtable = T) %>%
  kable_paper("striped", full_width = F) %>%
  add_header_above(c(" " = 3))%>%
  footnote(general =c("AE is calculated at highest toxicity grade.", "Subject is counted once in each PT.")) %>%
  column_spec(1,width="12cm" ) %>%
  column_spec(2,width ="3cm" ) %>%
  column_spec(3,width = "3cm",popover = "Test" ) %>%
  save_kable(file = "table1.html", self_contained = T)

#-------------------
#Approach 2: rtf
#-------------------

rtf<- RTF("t_ae_summary.rtf",
          width=8.5,
          height=11,
          font.size=10,
          omi=c(1,1,1))
addHeader(rtf, 
          title= "Title 1: Summary AE", 
          subtitle = "Title 2: Safety Population",
          font.size=12
)

addTable(rtf,
         as.data.frame(t_ae1),
         font.size=10,
         row.names=FALSE,
         NA.string="-",
         col.widths=c(4,1,1),
         header.col.justify= c("L","J","J") )

done(rtf)
#************************DONE***

#--------End of Code------------




