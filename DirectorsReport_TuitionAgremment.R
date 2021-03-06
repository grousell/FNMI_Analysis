
# Tuittion Agreement Students Annual Report -------------------------------


source ("https://raw.githubusercontent.com/grousell/Misc/master/R/preamble.R")

df <- read_csv ("C:/Users/grousell/OneDrive - Grand Erie DSB/PowerBI/MasterData/StudentDetail_20162017.csv") %>%
  #filter (month == 3) %>%
  filter (tuition == "Six Nations")

sem1 <- read_csv("C:/Users/grousell/OneDrive - Grand Erie DSB/Report Card/Data/Secondary/20162017_SecondaryRC_T1.csv") %>%
  #filter ( OEN %in% oens) %>%
  mutate (semester = "1") %>%
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

sem2 <- read_excel("C:/Users/grousell/OneDrive - Grand Erie DSB/Report Card/Data/Secondary/Greg Rousell - 30 June 2017 - 20162017_SecondaryRC_T2.xlsx",
                   sheet = "20162017_SecondaryRC_T2") %>%
  #filter ( OEN %in% oens) %>%
  mutate (semester = "2")%>%
  select (MIDENT = SCHOOLID,
          OEN,
          Name = `Student Name`,
          Course = `Course Number`,
          PotHours = `Potential Credit Hrs`,
          Earned = `Earned Credit Hrs`,
          Final,
          semester) %>%
  mutate (PotHours = parse_number (PotHours),
          Earned = parse_number(Earned)) %>%
  group_by (OEN) %>%
  summarise (`Potential Credit Hours` = sum (PotHours, na.rm = TRUE),
             `Earned Credits`  = sum (Earned, na.rm = TRUE),
             Percent = round ((`Earned Credits` / `Potential Credit Hours`) * 100,1)) %>%
  mutate (OEN = parse_number(OEN))

# df2 <- rbind (sem1, sem2) %>%
#   mutate (PotHours = parse_number(PotHours),
#           Earned = parse_number(Earned) )

# df2 <- df2 %>%
#   group_by (OEN) %>%
#   summarise (`Potential Credit Hours` = sum (PotHours, na.rm = TRUE),
#              `Earned Credits`  = sum (Earned, na.rm = TRUE),
#              Percent = round ((`Earned Credits` / `Potential Credit Hours`) * 100,1)) %>%
#   mutate (OEN = parse_number(OEN))

SixNation_Sem1 <- df %>%
  filter (month == 10) %>%
  left_join(sem1, by = c("StudentID" = "OEN"))

SixNation_Sem2 <- df %>%
  filter (month == 3) %>%
  left_join(sem2, by = c("StudentID" = "OEN"))

data.frame(table (SixNation_Sem2$`Potential Credit Hours`))
