#print summary of data
Lana_data %>% 
  summary()

#view distribution of energy
ggplot(Lana_data, aes(energy))+
  geom_histogram(bins=40, color="black", fill="lavender")+
  labs(title="Distrubution of Energy")+
  xlab("Energy")+
  ylab("Count")

#boxplot of energy by each album
Lana_data %>% 
ggplot(aes(x = energy, y = album_name, fill=album_name)) +
  geom_boxplot() +
  labs(
    title = "Box Plot of Energy by Album",
    y = "Album", 
    x = "Energy") +
  scale_fill_manual(values = c("pink", "brown", "lavender", "magenta", "cyan", "aquamarine", "aliceblue"))+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Correlation matrix
Lana_numeric <- Lana_data %>%
  select_if(is.numeric)
Lana_cor <- cor(Lana_numeric)
Lana_corrplot <- corrplot(Lana_cor, method = "circle", addCoef.col = 1, number.cex = 0.7)

#scatterplot of energy vs loudness
Lana_data %>% 
  ggplot(aes(x=energy, y=loudness)) + 
  geom_jitter(width = 0.5, size = 1) +
  geom_smooth(method = "lm", se =F, col="purple") +
  labs(title = "Energy vs. Loudness")

#scatterplot of energy vs acousticness
Lana_data %>% 
  ggplot(aes(x=energy, y=acousticness)) + 
  geom_jitter(width = 0.5, size = 1) +
  geom_smooth(method = "lm", se =F, col="purple") +
  labs(title = "Energy vs. Acousticness")