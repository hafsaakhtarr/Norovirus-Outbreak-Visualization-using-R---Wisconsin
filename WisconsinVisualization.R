#Wisconsin Analysis

#installing packages
#install.packages("tidyverse")

library("tidyverse")
library("ggplot2")
library("dplyr")
library("tidyr")

testfilter <- read.csv("data/NORS_20251029.csv")
wi_2019 <- testfilter %>%
  filter(State == "Wisconsin", 
         Year == 2019,
         grepl("Norovirus", Etiology, ignore.case = TRUE))

cat("Total number of norovirus outbreaks in Wisconsin 2019:", nrow(wi_2019), "\n")
cat("Total illnesses:", sum(wi_2019$Illnesses, na.rm = TRUE), "\n")
cat("Total hospitalizations:", sum(wi_2019$Hospitalizations, na.rm = TRUE), "\n")
cat("Total deaths:", sum(wi_2019$Deaths, na.rm = TRUE), "\n\n")

monthly_outbreaks <- wi_2019 %>%
  group_by(Month) %>%
  summarise(
    Outbreaks = n(),
    Total_Illnesses = sum(Illnesses, na.rm = TRUE),
    Total_Hospitalizations = sum(Hospitalizations, na.rm = TRUE)
  )

# month labels
month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
monthly_outbreaks$Month_Label <- month_labels[monthly_outbreaks$Month]

# Outbreaks by Month
ggplot(monthly_outbreaks, aes(x = factor(Month), y = Outbreaks)) +
  geom_bar(stat = "identity", fill = "#2E86AB", alpha = 0.8) +
  geom_text(aes(label = Outbreaks), vjust = -0.5, size = 3.5) +
  scale_x_discrete(labels = month_labels) +
  labs(
    title = "Norovirus Outbreaks by Month in Wisconsin (2019)",
    x = "Month",
    y = "Number of Outbreaks"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

ggplot(monthly_outbreaks, aes(x = factor(Month), y = Total_Illnesses)) +
  geom_bar(stat = "identity", fill = "#A23B72", alpha = 0.8) +
  geom_text(aes(label = Total_Illnesses), vjust = -0.5, size = 3.5) +
  scale_x_discrete(labels = month_labels) +
  labs(
    title = "Total Norovirus Illnesses by Month in Wisconsin (2019)",
    x = "Month",
    y = "Number of Illnesses"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

setting_summary <- wi_2019 %>%
  group_by(Setting) %>%
  summarise(
    Outbreaks = n(),
    Total_Illnesses = sum(Illnesses, na.rm = TRUE)
  ) %>%
  arrange(desc(Outbreaks))


setting_summary$Setting_Short <- gsub("Long-term care/nursing home/assisted living facility", 
                                      "Long-term care", 
                                      setting_summary$Setting)

ggplot(setting_summary, aes(x = reorder(Setting_Short, Outbreaks), y = Outbreaks)) +
  geom_bar(stat = "identity", fill = "#F18F01", alpha = 0.8) +
  geom_text(aes(label = Outbreaks), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(
    title = "Norovirus Outbreaks by Setting in Wisconsin (2019)",
    x = "Setting",
    y = "Number of Outbreaks"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.y = element_text(size = 9)
  )

mode_summary <- wi_2019 %>%
  group_by(Primary.Mode) %>%
  summarise(
    Outbreaks = n(),
    Total_Illnesses = sum(Illnesses, na.rm = TRUE)
  ) %>%
  arrange(desc(Outbreaks))

ggplot(mode_summary, aes(x = reorder(Primary.Mode, Outbreaks), y = Outbreaks)) +
  geom_bar(stat = "identity", fill = "#BC4B51", alpha = 0.8) +
  geom_text(aes(label = Outbreaks), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(
    title = "Norovirus Outbreaks by Transmission Mode in Wisconsin (2019)",
    x = "Transmission Mode",
    y = "Number of Outbreaks"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )
cat("\nSUMMARY STATISTICS \n\n")
cat("Monthly Breakdown:\n")
print(monthly_outbreaks)

