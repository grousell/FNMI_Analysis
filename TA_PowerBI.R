# Tuition Agreement Students Annual Report -------------------------------

library(tidyverse)
library(readxl)
library(stringr)

# School Master List -----------------------------------------------------

SecondaryMaster <- read_csv ("C:/Users/grousell/OneDrive - Grand Erie DSB/PowerBI/MasterData/Sec_School_Master.csv")

# 2016-2017 Student Detail ------------------------------------------------

df <- read_csv ("C:/Users/grousell/OneDrive - Grand Erie DSB/PowerBI/MasterData/StudentDetail_20162017.csv") %>%
  #filter (month == 3) %>%
  filter (tuition == "Six Nations") %>%
  select (MIDENT = SchoolCode,
          SchoolName, 
          OEN = StudentID,
          GivenName, 
          Surname,
          Grade = GradeCode,
          Gender = SexCode,
          Exceptiona,
          SelfID,
          tuition,
          month) %>%
  mutate (Grade_Sort = recode (Grade,
                          "9" = "01_9",
                          "10" = "02_10",
                          "11" = "03_11",
                          "12" = "04_12"),
          Grade = recode (Grade,
                          "9" = "Grade 9",
                          "10" = "Grade 10",
                          "11" = "Grade 11",
                          "12" = "Grade 12"))

SixNationALL <- df %>%
  select (MIDENT, OEN, GivenName, Surname, SelfID, tuition, Grade) %>%
  mutate (dup = duplicated (OEN)) %>%
  filter (dup == "FALSE") %>%
  select (-dup)

# Semster 1 RC ------------------------------------------------------------

sem1 <- read_csv("C:/Users/grousell/OneDrive - Grand Erie DSB/Report Card/Data/Secondary/20162017_SecondaryRC_T1.csv") %>%
  #filter ( OEN %in% oens) %>%
  mutate (semester = "1",
          OEN = parse_number(OEN)) 
  

sem1Credits <- sem1 %>%
  select (MIDENT = SCHOOLID,
          OEN,
          Name = `Student Name`,
          Course = `Course Number`,
          PotHours = `Potential Credit Hrs`,
          Earned = `Earned Credit Hrs`,
          Final,
          semester) %>%
  group_by (OEN) %>%
  summarise (`Potential Credit Hours` = sum (PotHours, na.rm = TRUE),
             `Earned Credits`  = sum (Earned, na.rm = TRUE),
             Percent = round ((`Earned Credits` / `Potential Credit Hours`) * 100,1)) %>%
  mutate (OEN = parse_number(OEN))

# Semster 2 RC ------------------------------------------------------------

sem2 <- read_excel("C:/Users/grousell/OneDrive - Grand Erie DSB/Report Card/Data/Secondary/Greg Rousell - 30 June 2017 - 20162017_SecondaryRC_T2.xlsx",
                   sheet = "20162017_SecondaryRC_T2") %>%
  mutate (semester = "2",
          OEN = parse_number(OEN))

sem2Credits <- sem2 %>%
  select (MIDENT = SCHOOLID,
          OEN,
          Name = `Student Name`,
          Course = `Course Number`,
          PotHours = `Potential Credit Hrs`,
          Earned = `Earned Credit Hrs`,
          Final,
          semester) %>%
  mutate (PotHours = parse_number(PotHours),
          Earned = parse_number(Earned)) %>%
  group_by (OEN) %>%
  summarise (`Potential Credit Hours` = sum (PotHours, na.rm = TRUE),
             `Earned Credits`  = sum (Earned, na.rm = TRUE),
             Percent = round ((`Earned Credits` / `Potential Credit Hours`) * 100,1)) %>%
  mutate (OEN = parse_number(OEN))

# SixNation - Sem 1 ------------------------------------------------------------

SixNation_Sem1 <- df %>%
  filter (month == 10) %>%
  left_join(sem1Credits, by = c("OEN")) %>%
  mutate (AllCredits = ifelse (Percent > 99, 1, 0),
          Grade = recode (Grade,
                          "9" = "Grade 9",
                          "10" = "Grade 10",
                          "11" = "Grade 11",
                          "12" = "Grade 12"),
          Grade_Sort = recode(Grade, 
                              "Grade 9" = "01_9",
                              "Grade 10" = "02_10",
                              "Grade 11" = "03_11",
                              "Grade 12" = "04_12")) %>%
  mutate (TEMP = str_detect(Exceptiona, ", Mult Except"),
          SpecEd = ifelse (TEMP == 1, "Mult Except", Exceptiona)) %>%
  select (-SchoolName, -Exceptiona, -TEMP)

# SixNation - Sem 2 ------------------------------------------------------------
SixNation_Sem2 <- df %>%
  filter (month == 3) %>%
  left_join(sem2Credits, by = c("OEN")) %>%
  mutate (AllCredits = ifelse (Percent > 99, 1, 0),
          Grade = recode (Grade,
                          "9" = "Grade 9",
                          "10" = "Grade 10",
                          "11" = "Grade 11",
                          "12" = "Grade 12"),
          Grade_Sort = recode(Grade, 
                              "9" = "01_9",
                              "10" = "02_10",
                              "11" = "03_11",
                              "12" = "04_12")) %>%
  mutate (TEMP = str_detect(Exceptiona, ", Mult Except"),
          SpecEd = ifelse (TEMP == 1, "Mult Except", Exceptiona)) %>%
  select (-SchoolName, -Grade, -Exceptiona, -TEMP)

# Courses -----------------------------------------------------------------

codes <- c("NBE3C", "NBE3E", "NBV3E", "NAC1O", "NAC2O", "NDG4M", "NDW4M", "LNMAO", "LNMBO", "LNAAO", "LNABO")

Courses <- sem1 %>%
  select (MIDENT = SCHOOLID, 
          OEN,
          `Course Number`,
          semester) %>%
  rbind (sem2 %>%
           select (MIDENT = SCHOOLID, 
                   OEN,
                   `Course Number`,
                   semester)
         ) %>%
  mutate (Course = substr (`Course Number`, 1, 5)) %>%
  filter (Course %in% codes) %>%
  left_join(SixNationALL %>%
              select (OEN, tuition),
            by = c ("OEN"))


