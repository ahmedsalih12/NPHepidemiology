
library(ggplot2) 
library(tidyverse) 
library(viridis) 
install.packages("viridisLite")
library(tidyr)  
install.packages("hrbrthemes") 
library(hrbrthemes) 
library(ggplot2)
library(RColorBrewer)
library(paletteer)
library(readxl) 
library(ggthemes) 

NPHageediting <- read_excel("Downloads/NPHageediting.xlsx")


NPHageediting2 <- NPHageediting %>%
  pivot_longer(cols = c("0-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+", "Total"),
               names_to = "age",
               values_to = "Diagnosis")    
NPHageediting2_filtered <- NPHageediting2 %>%
  filter(age != "Total")


ggplot(NPHageediting2_filtered, aes(x = Year, y = Diagnosis, fill = age)) +
  geom_area(colour = "black", size = 0.2, alpha = 0.7) + 
  scale_fill_viridis(discrete = TRUE) +
  theme_minimal() +
  labs(y = "Number of NPH diagnosis",
       title = "Age-stratified number of diagnoses of Normal Pressure Hydrocephalus (G91.2)",
       fill = "Age group") +  # Renaming the legend title for fill aesthetic
  theme(plot.title = element_text(hjust = 0.5, size = 15, margin = margin(b = 25)))

# RESOLUTION FOR DOWNLOAD: 1100, 710

NPHageediting2 %>%
  ggplot(aes(x=Year, y=Diagnosis, group=age, fill=age)) +
  geom_area() +
  theme(legend.position="none") +
  ggtitle("Age-stratified number of diagnoses of Normal Pressure Hydrocephalus (G91.2)") +
  theme(
    legend.position="none",
    panel.spacing = unit(2, "lines"),
    strip.text.x = element_text(size=9),
    plot.title = element_text(size=14),
    axis.text.x = element_text(size = 8, angle = 90)
  ) +
  facet_wrap(~age)



library(openxlsx)
t_tests <- pairwise.t.test(NPHageadjusted$Diagnosis, NPHageadjusted$age,
                           p.adjust.method = "bonferroni")  
excel_file <- "pairwise_t_test_results.xlsx"


 
# LINEAR AND LOG PLOT of age adjusted 

NPHageediting <- read_excel("Downloads/NPHageadjusted.xlsx")
 
linear_plot_diagnosis <- NPHageadjusted %>%
  ggplot(aes(x = as.factor(Year), y = age_adjusted, group = age)) +
  geom_line(data = subset(NPHageadjusted, age != "Total"),
            aes(color = age), linewidth = 1) +
  geom_line(data = subset(NPHageadjusted, age == "Total"),
            linetype = "dashed", color = "black", linewidth = 1) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  theme_classic() +
  labs(title = "Linear Scale",
       x = "Year",
       y = "Age-adjusted incidences per 100,000 population",
       color = "Age Group",
       shape = "Age Group") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "right",
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 10)) 

 
log_plot_diagnosis <- ggplot(NPHageadjusted, aes(x = as.factor(Year), y = age_adjusted, group = age)) +
  geom_line(data = subset(NPHageadjusted, age != "Total"),
            aes(color = age), linewidth = 1) +
  geom_line(data = subset(NPHageadjusted, age == "Total"),
            linetype = "dashed", color = "black", linewidth = 1) +
  scale_y_log10() + 
  scale_color_brewer(type = "qual", palette = "Set1") +
  theme_classic() +
  labs(title = "Log Scale",
       x = "Year",
       y = "Log10(Age-adjusted incidences per 100,000 population)",
       color = "Age Group",
       shape = "Age Group") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "right",
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 10)) +
  expand_limits(y = c(1, 100))  # Ensure that the y-axis includes 1 to 100

log_plot_diagnosis
# Arrange the plots side by side 
library(gridExtra)
grid.arrange(linear_plot_diagnosis, log_plot_diagnosis, ncol = 2) 


#find highest incidence risers per age group


# Define function to calculate changes
calculate_changes <- function(data, age_group) 
  
  # Filter data for the specific age group
  data_age_group <- NPHageadjusted %>%
    filter(age == age_group)
  
  # Get values for 2005 and 2021
  value_2005 <- NPHageadjusted$age_adjusted[NPHageadjusted$Year == 2005]
  value_2021 <- NPHageadjusted$age_adjusted[NPHageadjusted$Year == 2022]
  
  # Calculate absolute and relative change
  absolute_change <- value_2021 - value_2005
  relative_change <- (absolute_change / value_2005) * 100
  
  # Return a data frame with the results
results <- (data.frame(Age = NPHageadjusted$age,
                    Incidence_2005 = value_2005,
                    Incidence_2021 = value_2021,
                    Absolute_Change = absolute_change,  
                    Relative_Change = relative_change))  

results <- unique(results) 
results <- results %>%
  filter(!grepl("Total", Age))


# Print the age groups with the top 3 absolute changes
top_absolute_changes <- results %>%
  arrange(desc(Absolute_Change)) %>%
  head(3)

print("Top 3 age groups with the highest absolute changes:")
print(top_absolute_changes)

# Print the age groups with the top 3 relative changes
top_relative_changes <- results %>%
  arrange(desc(Relative_Change)) %>%
  head(3)

print("Top 3 age groups with the highest relative changes:")
print(top_relative_changes)


# Assuming you have a data frame named 'GermanSpondyHeat' with columns 'Age' and 'Year' for age groups and years, and 'Incidence' for incidence

# Perform two-way ANOVA
resultanova <- aov(age_adjusted ~ as.factor(age) + as.factor(Year), data = NPHageadjusted)

# Check ANOVA table
summary(resultanova)

# Perform post-hoc tests for pairwise comparisons (Tukey's test) for age groups
tukey_age <- TukeyHSD(resultanova, "as.factor(age)")
tukey_age

# Perform post-hoc tests for pairwise comparisons (Tukey's test) for years
tukey_year <- TukeyHSD(resultanova, "as.factor(Year)")
tukey_year
 


age_data_list <- split(NPHageadjusted, NPHageadjusted$age)

linear_regression_age <- function(age_data) {
  lm(Diagnosis ~ Year, data = age_data)
}
 
# Apply linear regression to each age group using lapply
linear_regression_results <- lapply(age_data_list, linear_regression_age)

summary_list <- lapply(linear_regression_results, summary)
print(summary_list)










G91_2surgery <- read_excel("Downloads/G91_2surgery.xlsx")

SURIGICAL INTERVENTIONS 


NPHsurg <- G91_2surgery %>%
  pivot_longer(cols = c("Total", "CSF shunt", "Revision and removal of CSF drains", "Cranioplasty", "Duraplasty", "Others", "Incision on the cerebrospinal fluid system", "Intracranial measuring probe"),
               names_to = "procedure",
               values_to = "number")   
NPHsurg_filtered <- NPHsurg %>% 
  select(procedure, number, Year)  



procedure_colors <- c("CSF shunt" = "#E41A1C",  # Red
                      "Revision and removal of CSF drains" = "#377EB8",  # Blue
                      "Incision on the cerebrospinal fluid system" = "#4DAF4A",  # Green
                      "Intracranial measuring probe" = "#FF7F00",  # Orange
                      "Others" = "#F781BF",  # Pink
                      "Cranioplasty" = "#A65628",  # Brown
                      "Duraplasty" = "#984EA3",  # Purple
                      "Total" = "black")  # Black for total

# Create the plot
overall_plotsurg <- NPHsurg_filtered %>%
  ggplot(aes(x = as.factor(Year), y = number, group = procedure)) +
  geom_line(data = subset(NPHsurg_filtered, procedure != "Total"),
            aes(color = procedure), linewidth = 1) +
  geom_line(data = subset(NPHsurg_filtered, procedure == "Total"),
            linetype = "dashed", color = "black", linewidth = 1) +
  scale_color_manual(values = procedure_colors,  # Use manual colors
                     breaks = c("CSF shunt", "Revision and removal of CSF drains", "Incision on the cerebrospinal fluid system", "Intracranial measuring probe", "Cranioplasty", "Duraplasty", "Others", "Total")) +  # Specify legend order
  labs(title = "Number of diagnoses of Normal Pressure Hydrocephalus with surgical interventions (G19.2)",
       x = "Year",
       y = "Number of diagnoses",
       color = "Surgical Intervention",
       shape = "Surgical Intervention") +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", margin = margin(b = 30)),
    plot.subtitle = element_text(face = "italic"),
    legend.position="bottom",
    legend.title = element_text(face = "bold", size = 12),
    legend.direction = "horizontal",
    legend.key.size = unit(1.5, "cm"),
    legend.text = element_text(size = 12),
    legend.spacing.y = unit(-0.2, "cm"),
    legend.box = "horizontal",
    panel.grid.major = element_line(colour = "grey", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(2, 2, 2, 2, "cm") # adjust the numbers as needed
  )

# Print the plot
print(overall_plotsurg)
# Print the plot





NPH_shunt <- G91_2surgery %>%
  pivot_longer(cols = c("Ventriculoatrial", "Ventriculoperitoneal", "Cisternoperitoneal", "Telemetric shunt monitor", "Other shunt"),
               names_to = "procedure",
               values_to = "number")   
NPHshunt_filtered <- NPH_shunt %>%
  select(procedure, number, Year)

# Reorder the legend
procedure_order <- c("Ventriculoperitoneal", "Ventriculoatrial", "Telemetric shunt monitor","Cisternoperitoneal", "Other shunt")

# Create the plot
plotshunt <- NPHshunt_filtered %>%
  ggplot(aes(x = as.factor(Year), y = number, group = procedure)) +
  geom_line(aes(color = factor(procedure, levels = procedure_order)), linewidth = 1) +
  scale_color_brewer(type = "qual", palette = "Set1") + 
  scale_y_log10() + 
  labs(x = "Year", y = "Number of diagnoses", color = "Surgical Intervention", linetype = "Surgical Intervention",
       title = "Number of diagnoses of Normal Pressure Hydrocephalus with CSF shunts (G19.2)") +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", margin = margin(b = 30)),
    plot.subtitle = element_text(face = "italic"),
    legend.position="bottom",
    legend.title = element_text(face = "bold", size = 13),
    legend.direction = "horizontal",
    legend.key.size = unit(1.5, "cm"),
    legend.text = element_text(size = 12),
    legend.spacing.y = unit(-0.2, "cm"),
    legend.box = "horizontal",
    panel.grid.major = element_line(colour = "grey", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(2, 2, 2, 2, "cm") # adjust the numbers as needed
  )

# Print the plot
print(plotshunt)


NPHincision <- G91_2surgery %>%
  pivot_longer(cols = c("External drainage", "ventricular-resvoir", "stoma"),
               names_to = "procedure",
               values_to = "number")    

NPHincision_filtered <- NPHincision %>%
  mutate(procedure = case_when(
    procedure == "External drainage" ~ "CSF external drainage",
    procedure == "ventricular-resvoir" ~ "Ventricular reservoir insertion",
    procedure == "stoma" ~ "Endoscopic stomies",
    TRUE ~ procedure
  )) %>%
  select(procedure, number, Year)
 

procedure_order2 <- c("Ventricular reservoir insertion", "CSF external drainage", "Endoscopic stomies")

linear_plotincision <- NPHincision_filtered %>%
  ggplot(aes(x = as.factor(Year), y = number, group = procedure)) +
  geom_line(aes(color = factor(procedure, levels = procedure_order2)), linewidth = 1) +
  scale_color_brewer(type = "qual", palette = "Set1") + 
  labs(x = "Year", y = "Number of diagnoses", color = "Surgical Intervention", linetype = "Surgical Intervention",
       title = "Number of diagnoses of Normal Pressure Hydrocephalus with cerebrospinal fluid system incisions (G19.2)") +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", margin = margin(b = 30)),
    plot.subtitle = element_text(face = "italic"),
    legend.position="bottom",
    legend.title = element_text(face = "bold", size = 13),
    legend.direction = "horizontal",
    legend.key.size = unit(1.5, "cm"),
    legend.text = element_text(size = 12),
    legend.spacing.y = unit(-0.2, "cm"),
    legend.box = "horizontal",
    panel.grid.major = element_line(colour = "grey", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(2, 2, 2, 2, "cm") # adjust the numbers as needed
  )














