##bring the 4 datasets back together and make plots encompassing them all
# merge null_distributions
null_distribution_df_2017$Year<-2017
null_distribution_df_2018$Year<-2018
null_distribution_df_2019$Year<-2019
null_distribution_df_2020$Year<-2020

combined_null_distribution_df <- bind_rows(null_distribution_df_2017, 
                                           null_distribution_df_2018, 
                                           null_distribution_df_2019,
                                           null_distribution_df_2020)

# merge results
results_2017$Year<-2017
results_2018$Year<-2018
results_2019$Year<-2019
results_2020$Year<-2020

combined_results<-rbind(results_2017,
                        results_2018,
                        results_2019,
                        results_2020)


# Ensure Year is treated as a factor
combined_null_distribution_df$Year <- as.factor(combined_null_distribution_df$Year)
combined_results$Year <- as.factor(combined_results$Year)

# PLOT


#without years listed
ggplot(combined_null_distribution_df, aes(x = Year, y = Schoeners_D, fill = Year)) +
  geom_boxplot(width = 0.7, alpha = 0.5) + 
  geom_point(data = combined_results, aes(x = Year, y = Schoeners_D, color = Year), size = 3) +  
  scale_fill_manual(values = c("2017" = "red", "2018" = "blue", "2019" = "green", "2020" = "purple")) +  
  scale_color_manual(values = c("2017" = "red", "2018" = "blue", "2019" = "green", "2020" = "purple")) +  
  facet_wrap(~ID, ncol = 1, strip.position = "left", scales = "fixed") +  
  theme_minimal() +
  labs(title = "Observed vs. Null Distribution of Schoener's D across Years",
       y = "Schoener's D",
       x = "") +
  theme(
    strip.text.y.left = element_text(angle = 0, hjust = 1, size = 14),  # Larger ID text
    plot.title = element_text(size = 18, face = "bold",hjust=0.5),  # Larger and bold graph title
    legend.text = element_text(size = 12),  # Larger legend text
    axis.title.x = element_text(size = 12, hjust = 0.5),  # Center x-axis title
    legend.title = element_text(size = 14, face = "bold"),  # Larger and bold legend title
    panel.spacing = unit(0.1, "lines"),  
    strip.background = element_blank(),  
    strip.placement = "outside",  
    panel.border = element_rect(color = "grey80", fill = NA, size = 0.5),  
    panel.grid = element_blank()  ,
    axis.text.y=element_blank()
  ) +
  coord_flip()

#with years listed
ggplot(combined_null_distribution_df, aes(x = Year, y = Schoeners_D, fill = Year)) +
  geom_boxplot(width = 0.7, alpha = 0.5) + 
  geom_point(data = combined_results, aes(x = Year, y = Schoeners_D, color = Year), size = 3) +  
  scale_fill_manual(values = c("2017" = "red", "2018" = "blue", "2019" = "green", "2020" = "purple")) +  
  scale_color_manual(values = c("2017" = "red", "2018" = "blue", "2019" = "green", "2020" = "purple")) +  
  facet_wrap(~ID, ncol = 1, strip.position = "left", scales = "fixed") +  
  theme_minimal() +
  labs(title = "Observed vs. Null Distribution of Schoener's D across Years",
       y = "Schoener's D",
       x = "") +
  theme(
    strip.text.y.left = element_text(angle = 0, hjust = 1, size = 14),  # Larger ID text
    plot.title = element_text(size = 18, face = "bold",hjust=0.5),  # Larger and bold graph title
    legend.text = element_text(size = 12),  # Larger legend text
    axis.title.x = element_text(size = 12, hjust = 0.5),  # Center x-axis title
    legend.title = element_text(size = 14, face = "bold"),  # Larger and bold legend title
    panel.spacing = unit(0.1, "lines"),  
    strip.background = element_blank(),  
    strip.placement = "outside",  
    panel.border = element_rect(color = "grey80", fill = NA, size = 0.5),  
    panel.grid = element_blank()  
  ) +
  coord_flip()

#make a plot of niche trackers
NicheTrackers<-read.csv("C:/Users/floey/OneDrive - University of Glasgow/MScR/Data_Analysis/Habitat Selection/Plots/HabSel of niche trackers/Habitat Selection of niche trackers_nobigCIs.csv")

# Create the plot
#lots of info so have to separate by season
breeding_season <- NicheTrackers %>% filter(Season == "Breeding")
ggplot(breeding_season, aes(x = Gull_and_Year, y = Selection, color = LC_cat)) +
  geom_point(position = position_dodge(width = 0.5)) + # Points with dodging for clarity
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.2, 
                position = position_dodge(width = 0.5)) + # Error bars with dodging
  labs(title = "Selection by Gull and Year (Breeding Season)",
       x = "Gull and Year",
       y = "Selection",
       color = "Land Cover Category") + # Labels
  theme_minimal() +# Minimal theme
  theme(plot.title = element_text(hjust = 0.5)) #centre the title

#now just for wintering
wintering_season <- NicheTrackers %>% filter(Season == "Wintering")
ggplot(wintering_season, aes(x = Gull_and_Year, y = Selection, color = LC_cat)) +
  geom_point(position = position_dodge(width = 0.5)) + # Points with dodging for clarity
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.2, 
                position = position_dodge(width = 0.5)) + # Error bars with dodging
  labs(title = "Selection by Gull and Year (Wintering Season)",
       x = "Gull and Year",
       y = "Selection",
       color = "Land Cover Category") + # Labels
  theme_minimal() + # Minimal theme
  theme(plot.title = element_text(hjust = 0.5)) #centre the title


