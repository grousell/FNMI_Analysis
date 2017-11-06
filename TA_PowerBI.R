# Tuition Agreement Students Annual Report -------------------------------

# Load Packages -----------------------------------------------------------

library(tidyverse)
library(readxl)
library(stringr)
library(scales)

# Load Functions ----------------------------------------------------------

na_missing <- function (x) {
  x[is.na(x)] <- "Missing"
  return(x)
}

substrRight <- function(x, n){
  sapply(x, function(xx)
    substr(xx, (nchar(xx)-n+1), nchar(xx))
  )
}


# ggplot theme and colours ------------------------------------------------------------
# For custom board colours

gedsbGreen <- "#59AD46"
gedsbBlue <- "#04559F"
gedsbGreen2 <- "#8CE079"
gedsbBlue2 <- "#51A2EC"

# Colour Palette - GEDSB Colours for charts

palette3 <- c("#59AD46", "#173B32", "#04559F")
palette4 <- c("grey60","#04559F","#59AD46", "#A6FA93")
palette5 <- c("#04559F", "#1E6FB9", "#3788D2", "#51A2Ec", "#6ABBFF")
palette5 <- c("#6ABBFF", "#51A2Ec", "#3788D2",  "#1E6FB9","#04559F" )

theme_update(
  plot.margin= unit(c(0.25,0.25,0.25,0.25), "cm"),
  plot.title = element_text (colour="black", size=16, hjust = 0.5),
  
  panel.background = element_rect(fill="NA"),
  panel.border = element_blank(),
  panel.spacing = unit(1, "lines"),
  
  panel.grid.major.y = element_line(colour="grey90"),
  panel.grid.minor.y = element_line(colour="NA"),
  panel.grid.major.x = element_line(colour="NA"),
  panel.grid.minor.x = element_line(colour="NA"),
  
  axis.text.y = element_text (colour="black", size=12, hjust=1),
  axis.title.y = element_text (colour="black", size=14, angle=90),
  
  axis.text.x = element_text (colour="black", size=12,angle=0),
  axis.title.x = element_text (colour="black", size=14),
  
  axis.ticks = element_blank(),
  
  legend.text = element_text (colour="black", size = 14),
  legend.position = ("right"),
  legend.title = element_blank(),
  legend.key = element_blank()
)

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
  select (MIDENT, OEN, GivenName, Surname, SelfID, tuition, Grade, Grade_Sort, Exceptiona) %>%
  mutate (dup = duplicated (OEN),
          Year = "2016-2017") %>%
  filter (dup == "FALSE") %>%
  mutate (TEMP = str_detect(Exceptiona, ", Mult Except"),
          SpecEd = ifelse (TEMP == 1, "Mult Except", Exceptiona)) %>%
  select (-dup, -TEMP, -Exceptiona)

# Semster 1 RC ------------------------------------------------------------

sem1 <- read_csv("C:/Users/grousell/OneDrive - Grand Erie DSB/Report Card/Data/Secondary/20162017_SecondaryRC_T1.csv") %>%
  #filter ( OEN %in% oens) %>%
  mutate (semester = "1",
          OEN = parse_number(OEN)) %>%
  filter (!is.na (`Potential Credit Hrs`)) 
  
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
  summarise (PotHours = sum (PotHours, na.rm = TRUE),
             Earned  = sum (Earned, na.rm = TRUE),
             Percent = round ((Earned / PotHours) * 100,1)) %>%
  mutate (OEN = parse_number(OEN),
          `Credits Attempted` = ifelse (PotHours < 1.9, "Less than 2", 
                                        ifelse (PotHours > 1.9 & PotHours <3.9, "Two to Three", "Four or More")),
          CreditsAttempted_SORT = recode (`Credits Attempted`,
                                          "Less than 2" = "01_Less2",
                                          "Two to Three" = "02_2to3",
                                          "Four or More" = "03_FourMore"))

# Semster 2 RC ------------------------------------------------------------

sem2 <- read_excel("C:/Users/grousell/OneDrive - Grand Erie DSB/Report Card/Data/Secondary/Greg Rousell - 30 June 2017 - 20162017_SecondaryRC_T2.xlsx",
                   sheet = "20162017_SecondaryRC_T2") %>%
  mutate (semester = "2",
          OEN = parse_number(OEN)) %>%
  filter (!is.na (`Potential Credit Hrs`)) 

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
  summarise (PotHours = sum (PotHours, na.rm = TRUE),
             Earned  = sum (Earned, na.rm = TRUE),
             Percent = round ((Earned / PotHours) * 100,1)) %>%
  mutate (OEN = parse_number(OEN),
          `Credits Attempted` = ifelse (PotHours < 3.9, "Less than 4", 
                                        ifelse (PotHours > 3.9 & PotHours <5.9, "Four to Five", 
                                                ifelse (PotHours > 5.9 & PotHours < 8, "Six to Seven", 
                                                        "Eight or More"))),
          CreditsAttempted_SORT = recode (`Credits Attempted`,
                                          "Less than 4" = "01_Less4",
                                          "Four to Five" = "02_FourFive",
                                          "Six to Seven" = "03_SixSeven",
                                          "Eight or More" = "04_EightMore"))

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
          SpecEd = ifelse (TEMP == 1, "Mult Except", Exceptiona),
          PotHours_R = ifelse (PotHours == 0, "0", 
                               ifelse (PotHours >0 & PotHours <1.5, "1", 
                                       ifelse (PotHours >1.4 & PotHours <2.4, "2", 
                                               ifelse (PotHours >2.4 & PotHours <3.4, "3", 
                                                       ifelse (PotHours >3.4 & PotHours <4.4, "4", 
                                                               ifelse (PotHours >4.4 & PotHours <5.4, "5", 
                                                                       ifelse (PotHours >5.4 & PotHours <6.4, "6",
                                                                               ifelse (PotHours >6.4 & PotHours <7.4, "7", "8+")
                                                                               )
                                                                       )
                                                               )
                                                       )
                                               )
                                       )
                               )) %>%
  select (-SchoolName, -Exceptiona, -TEMP) %>%
  filter (!is.na(PotHours) )


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
                              "Grade 9" = "01_9",
                              "Grade 10" = "02_10",
                              "Grade 11" = "03_11",
                              "Grade 12" = "04_12")) %>%
  mutate (TEMP = str_detect(Exceptiona, ", Mult Except"),
          SpecEd = ifelse (TEMP == 1, "Mult Except", Exceptiona),
          PotHours_R = ifelse (PotHours == 0, "0", 
                               ifelse (PotHours >0 & PotHours <1.5, "1", 
                                       ifelse (PotHours >1.4 & PotHours <2.4, "2", 
                                               ifelse (PotHours >2.4 & PotHours <3.4, "3", 
                                                       ifelse (PotHours >3.4 & PotHours <4.4, "4", 
                                                               ifelse (PotHours >4.4 & PotHours <5.4, "5", 
                                                                       ifelse (PotHours >5.4 & PotHours <6.4, "6",
                                                                               ifelse (PotHours >6.4 & PotHours <7.4, "7", "8+")
                                                                       )
                                                               )
                                                       )
                                               )
                                       )
                               )
          )) %>%
  select (-SchoolName, -Exceptiona, -TEMP) %>%
  filter (!is.na(PotHours) )

# Courses -----------------------------------------------------------------

codes <- c("NBE3C", "NBE3E", "NBV3E", "NAC1O", "NAC2O", "NDG4M", "NDW4M", "LNMAO", "LNMBO", "LNAAO", "LNABO")

falseifNA <- function(x){
  ifelse(is.na(x), FALSE, x)
}

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
            by = c ("OEN")) %>%
  mutate (Course = recode (Course, 
                           "NAC1O" = "NAC1O Expressing Aboriginal Cultures",
                           "NAC2O" = "NAC2O Aboriginal People in Canada",
                           "NDA3M" = "NDA3M Current Aboriginal Issues in Canada",
                           "NBE3C" = "NBE3C English: Contemporary Aboriginal Voices",
                           "NBE3E" = "NBE3E English: Contemporary Aboriginal Voices",
                           "NBV3E" = "NBV3E Aboriginal Beliefs, Values and Aspirations",
                           "NDG4M" = "NDG4M Aboriginal Governance: Emerging Directions", 
                           "NDW4M" = "NDW4M Issues of Indigenous Peoples in a Global Context",
                           "LNMAO" = "LNMAO Mohawk Language, Level 1",
                           "LNMBO" = "LNMBO Mohawk Language, Level 2",
                           "LNAAO" = "LNAAO Cayuga Language, Level 1",
                           "LNABO" = "LNABO Cayuga Language, Level 2")) %>%
  mutate (tuition = falseifNA(tuition)) %>%
  mutate (tuition = ifelse (tuition == "Six Nations", "Six Nations", "Non Six Nations"),
          tuition_R = recode (tuition, 
                              "Six Nations" = "01_SixNations",
                              "Non Six Nations" = "02_NonSixNations"))






# Add DOB to recode for Grade 12+ -----------------------------------------

dob <- read_csv ("C:/Users/grousell/OneDrive - Grand Erie DSB/PowerBI/MasterData/StudentDetail_20162017.csv") %>%
  #filter (month == 3) %>%
  filter (tuition == "Six Nations") %>%
  select (OEN = StudentID, 
          Birthdate,
          GradeCode,
          Gender = SexCode) %>%
  distinct (OEN, 
            Birthdate,
            GradeCode,
            Gender) %>%
  mutate (BirthYear = substrRight(Birthdate, 4))

# Credit Accumulation -----------------------------------------------------
credits <- read_csv ("C:/Users/grousell/OneDrive - Grand Erie DSB/Student Success/Credit Accumulation/CreditAccumulation 2016-2017.csv") %>%
  filter (`Tuition Agreement` == "Government of Canada") %>%
  mutate (MIDENT = recode (School, 
                           "Brantford Collegiate Institute and V.S."="896187",
                           "Cayuga Secondary School"="899046",
                           "Delhi District Secondary School"="903728",
                           "Dunnville Secondary School"="906069",
                           "Grand Erie Learning Alternatives"="891304",
                           "Grand Erie Learning Alternatives - Adult"="891304",
                           "Grand Erie Learning Alternatives - ILC"="891304",
                           "Grand Erie Learning Alternatives - Night School"="891304",
                           "Grand Erie Learning Alternatives - PSW"="891304",
                           "Grand Erie Learning Alternatives - Summer School"="891304",
                           "Hagersville Secondary School"="915033",
                           "McKinnon Park Secondary School"="898007",
                           "North Park Collegiate and V.S."="930245",
                           "Paris District High School"="933490",
                           "Pauline Johnson Collegiate and V.S."="934402",
                           "Simcoe Composite School"="941557",
                           "Tollgate Technological Skills Centre"="916412",
                           "Valley Heights Secondary School"="949035",
                           "Waterford District High School"="950785")) %>%
  select (MIDENT, 
          OEN, 
          Grade,
          Gender,
          `Tuition Agreement`,
          `Attempted To Date`,
          `Achieved To Date`) %>%
  mutate (Gender = ifelse (Gender == "F", "Female", "Male")) %>%
  left_join (dob, by = c("OEN"))


# Recode Grade for 12+ ----------------------------------------------------

credits <- credits %>%
  mutate (temp = BirthYear, 
          temp = ifelse (BirthYear == 2003, "9",
                            ifelse (BirthYear == 2002, "9",
                                    ifelse (BirthYear == 2001, "10",
                                            ifelse (BirthYear == 2000, "11",
                                                    ifelse (BirthYear == 1999, "12", 
                                                            ifelse (BirthYear < 1999, "12+", "NO DATA")))))),
          temp = na_missing(temp),
          Grade_R = ifelse (temp == "Missing", Grade, temp)) %>%
  select(-temp, -Birthdate, -BirthYear)


