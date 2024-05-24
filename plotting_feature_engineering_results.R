library(tidyverse)

data <- read.csv("~/Master/15hp_project/mGPS-master/mGPS-master/Metasub/Feature_engineering_test - Blad2.csv")
meta_abundance <- read.csv("C:/Users/andre/OneDrive/Documents/Master/15hp_project/mGPS-master/mGPS-master/Data/Data/Metasub/meta_taxa_read.csv")
#Replace all commas 
data <- data %>% 
  mutate_all(~ gsub(",", ".", .))

data$Min_distance <- as.double(data$Min_distance)
data$Mean_distance <- as.double(data$Mean_distance)
data$Max_distance <- as.double(data$Max_distance)
data$Median <- as.double(data$Median)
data$Class_accuracy <- as.double(data$Class_accuracy)
data$Dist.100km <- as.double(data$Dist.100km)
data$Dist.250km <- as.double(data$Dist.250km)
data$Dist.500km <- as.double(data$Dist.500km)
data$Dist.1000km <- as.double(data$Dist.1000km)
data$Dist.1500km <- as.double(data$Dist.1500km)
data$Feat_sel <- as.factor(data$Feat_sel)
data_long <- pivot_longer(
  data,
  cols = c("Min_distance", "Dist.100km", "Dist.250km", "Dist.500km", "Dist.1000km", "Dist.1500km", "Class_accuracy", "Mean_distance", "Median", "Max_distance"),
  names_to = "Metric",
  values_to = "Value"
)      

data_long <- data_long %>%
  mutate(Value = ifelse(Metric != 'Max_distance', Value * 12, Value))

ggplot(data_long, aes(x = Metric, y = Value, fill = interaction(Feat_sel, Test))) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  labs(
    title = "Comparison of Values by Metric, Test, and Feat_sel",
    x = "Metric",
    y = "Value",
    fill = "Feat_sel & Test"
  ) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data_long, aes(x = Metric, y = Value, col = interaction(Feat_sel, Test), group = interaction(Feat_sel, Test))) + 
  geom_line(aes(linetype = Feat_sel), size = 1) + 
  geom_point(size = 3) + 
  theme_minimal() +
  labs(
    title = "Comparison of Values by Metric, Test, and Feat_sel",
    x = "Metric",
    y = "Value",
    color = "Feat_sel & Test",
    linetype = "Feat_sel"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data_long, aes(x = Metric, y = Value, col = Test, group = Test)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  facet_wrap(~Feat_sel)

ggplot(data_long, aes(x = Test, y = Value)) +
  geom_boxplot(aes(fill = Test)) +
  geom_point(size = 0.9) +
  facet_grid(Feat_sel ~ Metric) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.title.x = element_text(size = 20),  # Increase font size for x-axis title
        axis.title.y = element_text(size = 20),  # Increase font size for y-axis title
        axis.text.y = element_text(size = 15),    # Increase font size for y-axis tick labels
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        strip.text = element_text(size = 10)
        ) +
  labs(fill = "Feature Set")


ggplot(meta_abundance, aes(x = continent)) +
  geom_bar(aes(fill = continent)) +
  scale_fill_manual(values = c("east_asia" = "#ffffcc", "europe" = "#a1dab4", "middle_east" = "#41b6c4", 
                                "north_america" = "#2c7fb8", "oceania" = "#253494", "south_america" = "#081d58", "sub_saharan_africa" = "darkred"))


ggplot(meta_abundance) +
  geom_density(aes(x = longitude, fill = "longitude"), alpha = 0.5) +
  geom_density(aes(x = latitude, fill = "latitude"), alpha = 0.5) +
  scale_fill_manual(values = c("latitude" = "#2c7fb8", "longitude" = "#ffffcc"))


data_long <- data_long %>%
  mutate(Value = ifelse(Metric != 'Max_distance', Value / 12, Value))


#Differences BORUTA vs RFE

#SPECIES
t_test_result <- t.test(Value ~ Feat_sel, data = data_long[data_long$Test == "Species" & data_long$Metric == "Min_distance", ])
t_test_result <- t.test(Value ~ Feat_sel, data = data_long[data_long$Test == "Species" & data_long$Metric == "Max_distance", ])
t_test_result <- t.test(Value ~ Feat_sel, data = data_long[data_long$Test == "Species" & data_long$Metric == "Dist.100km", ])
t_test_result <- t.test(Value ~ Feat_sel, data = data_long[data_long$Test == "Species" & data_long$Metric == "Dist.250km", ])
t_test_result <- t.test(Value ~ Feat_sel, data = data_long[data_long$Test == "Species" & data_long$Metric == "Dist.500km", ])
t_test_result <- t.test(Value ~ Feat_sel, data = data_long[data_long$Test == "Species" & data_long$Metric == "Dist.1000km", ])
t_test_result <- t.test(Value ~ Feat_sel, data = data_long[data_long$Test == "Species" & data_long$Metric == "Dist.1500km", ])
t_test_result <- t.test(Value ~ Feat_sel, data = data_long[data_long$Test == "Species" & data_long$Metric == "Class_accuracy", ])

# PHYLA
t_test_result <- t.test(Value ~ Feat_sel, data = data_long[data_long$Test == "Phyla" & data_long$Metric == "Min_distance", ])
t_test_result <- t.test(Value ~ Feat_sel, data = data_long[data_long$Test == "Phyla" & data_long$Metric == "Max_distance", ])
t_test_result <- t.test(Value ~ Feat_sel, data = data_long[data_long$Test == "Phyla" & data_long$Metric == "Dist.100km", ])
t_test_result <- t.test(Value ~ Feat_sel, data = data_long[data_long$Test == "Phyla" & data_long$Metric == "Dist.250km", ])
t_test_result <- t.test(Value ~ Feat_sel, data = data_long[data_long$Test == "Phyla" & data_long$Metric == "Dist.500km", ])
t_test_result <- t.test(Value ~ Feat_sel, data = data_long[data_long$Test == "Phyla" & data_long$Metric == "Dist.1000km", ])
t_test_result <- t.test(Value ~ Feat_sel, data = data_long[data_long$Test == "Phyla" & data_long$Metric == "Dist.1500km", ])
t_test_result <- t.test(Value ~ Feat_sel, data = data_long[data_long$Test == "Phyla" & data_long$Metric == "Class_accuracy", ])

# COMBINED
t_test_result <- t.test(Value ~ Feat_sel, data = data_long[data_long$Test == "Combined" & data_long$Metric == "Min_distance", ])
t_test_result <- t.test(Value ~ Feat_sel, data = data_long[data_long$Test == "Combined" & data_long$Metric == "Max_distance", ])
t_test_result <- t.test(Value ~ Feat_sel, data = data_long[data_long$Test == "Combined" & data_long$Metric == "Dist.100km", ])
t_test_result <- t.test(Value ~ Feat_sel, data = data_long[data_long$Test == "Combined" & data_long$Metric == "Dist.250km", ])
t_test_result <- t.test(Value ~ Feat_sel, data = data_long[data_long$Test == "Combined" & data_long$Metric == "Dist.500km", ])
t_test_result <- t.test(Value ~ Feat_sel, data = data_long[data_long$Test == "Combined" & data_long$Metric == "Dist.1000km", ])
t_test_result <- t.test(Value ~ Feat_sel, data = data_long[data_long$Test == "Combined" & data_long$Metric == "Dist.1500km", ])
t_test_result <- t.test(Value ~ Feat_sel, data = data_long[data_long$Test == "Combined" & data_long$Metric == "Class_accuracy", ])

# Subset the data
subset_data <- data_long[data_long$Feat_sel == "BORUTA" & data_long$Metric == "Min_distance", ]
subset_data <- data_long[data_long$Feat_sel == "BORUTA" & data_long$Metric == "Max_distance", ]
subset_data <- data_long[data_long$Feat_sel == "BORUTA" & data_long$Metric == "Dist.100km", ]
subset_data <- data_long[data_long$Feat_sel == "BORUTA" & data_long$Metric == "Dist.250km", ]
subset_data <- data_long[data_long$Feat_sel == "BORUTA" & data_long$Metric == "Dist.500km", ]
subset_data <- data_long[data_long$Feat_sel == "BORUTA" & data_long$Metric == "Dist.1000km", ]
subset_data <- data_long[data_long$Feat_sel == "BORUTA" & data_long$Metric == "Dist.1500km", ]
subset_data <- data_long[data_long$Feat_sel == "BORUTA" & data_long$Metric == "Class_accuracy", ]


# Fit the ANOVA model
anova_result <- aov(Value ~ Test, data = subset_data)

# Read the CSV file into a vector
yali <- read.csv("metasub_global_git.csv")
yali_vector <- yali$taxa
yali_vector <- trimws(yali_vector)

# Read the text file into a vector
my_gits <- scan("my_gits.txt", what = "", sep = ",")
my_gits <- trimws(my_gits)
# Find the matches
matches <- intersect(my_gits, yali_vector)

# Count the number of matches
num_matches <- length(matches)

# Print the number of matches
print(num_matches)

# Print the matches (optional)
print(matches)

