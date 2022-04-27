install.packages("r2rtf")
library(dplyr) # Manipulate data
library(tidyr) # Manipulate data
library(r2rtf) # Reporting in RTF format


r2rtf_adae %>%
  select(USUBJID, TRTA, AEDECOD) %>%
  head(10)

r2rtf_adsl %>%
  select(USUBJID,STUDYID,TRTSDT) %>%
  head(4)

tbl1 <- r2rtf_adae %>%
  count(TRTA,AEDECOD) %>%
  pivot_wider(names_from = TRTA,values_from = n,values_fill = 0)

tbl1 %>% head(5)

# First Table
head(tbl1) %>% 
# tbl1 %>%  /for whole dataset 
  rtf_body() %>%
  rtf_encode() %>%
  write_rtf("D:/R/R_clinical/TFL_output/ae_1.rtf")


# Second Table to adjust Width of column
head(tbl1) %>% 
  rtf_body(col_rel_width = c(3,2,2,2)) %>%
  rtf_encode() %>%
  write_rtf("D:/R/R_clinical/TFL_output/ae_2.rtf")
  

# Third Table to adjust Width of column header 
head(tbl1) %>% 
  rtf_colheader(colheader = "Adverse Events | Placebo | Xanomeline High Dose | Xanomeline Low Dose",
                col_rel_width = c(3,2,2,2)) %>%
  rtf_body(col_rel_width = c(3,2,2,2)) %>%
  rtf_encode() %>%
  write_rtf("D:/R/R_clinical/TFL_output/ae_3.rtf")


# Fourth Table for text justification(left,right,center) 
head(tbl1) %>% 
  rtf_colheader(colheader = "Adverse Events | Placebo | Xanomeline High Dose | Xanomeline Low Dose",
                col_rel_width = c(3,2,2,2)) %>%
  rtf_body(col_rel_width = c(3,2,2,2), text_justification = c("l","c","c","c")) %>%
  rtf_encode() %>%
  write_rtf("D:/R/R_clinical/TFL_output/ae_4.rtf")


# Final Table with multiple column header 
head(tbl1) %>% 
  rtf_colheader(colheader = " | TREATMENT",
                col_rel_width=c(3,6)) %>%
  
  rtf_colheader(colheader = "Adverse Events | Placebo | Xanomeline High Dose | Xanomeline Low Dose",
                border_top = c("", "single", "single", "single"),
                col_rel_width = c(3,2,2,2)) %>%
  
  rtf_body(col_rel_width = c(3,2,2,2), text_justification = c("l","c","c","c")) %>%
  
  rtf_encode() %>%
  write_rtf("D:/R/R_clinical/TFL_output/ae_final.rtf")

