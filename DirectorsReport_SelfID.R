
# 2016-2017 Data for Self ID Directors Report -----------------------------

# Preamble
source ("https://raw.githubusercontent.com/grousell/Misc/master/R/preamble.R")
# Recode MIDENT to Region
source ("https://raw.githubusercontent.com/grousell/Misc/master/R/RecodeMident.R")

library(readxl)

na_NonFNMI <- function (x) {
  x[is.na(x)] <- "Non FNMI"
  return(x)
}


df <- read_excel("C:/Users/grousell/OneDrive - Grand Erie DSB/Student Data/SIS Data/Greg Rousell - 27 September 2017 - All Students with Self ID.XLSX",
               sheet = "Export for GR") %>%
  rename (FNMI = `Aboriginal Type Name`,
          Grade = `Grade Level`) %>%
  filter (Mident != 645367) %>%
  filter (Mident != 992268) %>%
  filter (Mident != 995712) %>%
  mutate (Region = recode.mident(Mident),
          Panel = recode.panel(Mident),
          FNMI_R = ifelse (FNMI == "First Nations", "FNMI", 
                         ifelse(FNMI == "Inuit", "FNMI",
                                ifelse (FNMI == "Métis", "FNMI", "Non FNMI"))))


df$FNMI_R <- na_NonFNMI(df$FNMI_R)

table (df$FNMI_R)
# Check that recoding worked
# table (df$TEST, df$Mident)

table1 <- df %>%
  filter (FNMI_R == "FNMI") %>%
  group_by (Panel, Region) %>%
  summarise (n = length(OEN))

SelfID_ByGrade <- df %>%
  filter (FNMI_R == "FNMI") %>%
  group_by (Grade) %>%
  summarise (n = length(OEN)) %>%
  mutate (Grade = recode (Grade, 
                          "JK" = "JK",
                          "SK" = "SK",
                          "1" = "Grade 1",
                          "2" = "Grade 2",
                          "3" = "Grade 3",
                          "4" = "Grade 4",
                          "5" = "Grade 5",
                          "6" = "Grade 6",
                          "7" = "Grade 7",
                          "8" = "Grade 8",
                          "9" = "Grade 9",
                          "10" = "Grade 10",
                          "11" = "Grade 11",
                          "12" = "Grade 12"),
          Grade = factor (Grade, levels = c("JK",
                                            "SK",
                                            "Grade 1",
                                            "Grade 2",
                                            "Grade 3",
                                            "Grade 4",
                                            "Grade 5",
                                            "Grade 6",
                                            "Grade 7",
                                            "Grade 8",
                                            "Grade 9",
                                            "Grade 10",
                                            "Grade 11",
                                            "Grade 12"))) 


plot1 <- SelfID_ByGrade %>%
  ggplot (aes (x = Grade, y = n)) + 
  geom_bar (stat = "identity", fill = gedsbBlue) +
  coord_flip() + 
  geom_text(aes(label = n),
            hjust = -0.5) + 
  labs (title = "Self-Identified Indigenous Students\nBy Grade",
        y = "Number of Students",
        x = "")+
  scale_y_continuous(limits = c(0,350)) +
  theme (panel.grid.major.y = element_line(colour="NA"),
         panel.grid.major.x = element_line(colour="grey90"),
         plot.title = element_text (hjust = 0.5))



