HDI_data <- read.csv("https://statsnotebook.io/blog/data_management/example_data/HDI_countries.csv")

HDI_data %>%
  ggplot(aes(y = HDI, x = GDP)) +
  geom_jitter(alpha = 0.6, na.rm = TRUE)+
  scale_fill_brewer(palette = "Set2")+
  scale_color_brewer(palette = "Set2")+
  theme_bw(base_family = "sans")+
  ggtitle("Scatterplot of Gross domestic product and Human development index")+
  xlab("Gross domestic product (GDP, 2018 US$)")+
  ylab("Human development index")+
  theme(legend.position = "bottom")

HDI_data %>%
  drop_na(Continent, Pop) %>%
  ggplot(aes(y = HDI, x = GDP, size = Pop)) +
  geom_jitter(alpha = 0.5, aes(color = Continent), na.rm = TRUE)+
  scale_size(range = c(0.1, 8))+
  scale_fill_brewer(palette = "Set2")+
  scale_color_brewer(palette = "Set2")+
  theme_bw(base_family = "sans")+
  ggtitle("Scatterplot of Gross domestic product and Human development index")+
  xlab("Gross domestic product (GDP, 2018 US$)")+
  ylab("Human development index")+
  labs(color = "Continent", fill = "Continent")+
  labs(size = "Population (millions)")+
  theme(legend.position = "bottom")

HDI_data %>%
  drop_na(Continent, Pop) %>%
  ggplot(aes(y = HDI, x = GDP, size = Pop)) +
  geom_jitter(alpha = 0.5, aes(color = Continent), na.rm = TRUE)+
  scale_size(range = c(0.3, 15))+
  scale_fill_brewer(palette = "Set2")+
  scale_color_brewer(palette = "Set2")+
  theme_bw(base_family = "sans")+
  ggtitle("Scatterplot of Gross domestic product and Human development index")+
  xlab("Gross domestic product (GDP, 2018 US$)")+
  ylab("Human development index")+
  labs(color = "Continent", fill = "Continent")+
  labs(size = "Population (millions)")+
  theme(legend.position = "bottom")

HDI_data %>%
  drop_na(Continent) %>%
  ggplot(aes(y = HDI, x = Schooling)) +
  geom_jitter(alpha = 0.6, aes(color = Continent), na.rm = TRUE)+
  geom_smooth(method = "lm", se = TRUE, level = 0.95, na.rm = TRUE, show.legend = FALSE)+
  scale_fill_brewer(palette = "Set2")+
  scale_color_brewer(palette = "Set2")+
  theme_bw(base_family = "sans")+
  ggtitle("Scatterplot of Years of schooling and Human development index")+
  xlab("Years of schooling")+
  ylab("Human development index")+
  labs(color = "Continent", fill = "Continent")+
  theme(legend.position = "bottom")

HDI_data %>%
  drop_na(Continent) %>%
  ggplot(aes(y = HDI, x = Schooling, color = Continent)) +
  geom_jitter(alpha = 0.6, na.rm = TRUE)+
  geom_smooth(method = "lm", se = TRUE, level = 0.95, na.rm = TRUE, show.legend = FALSE)+
  scale_fill_brewer(palette = "Set2")+
  scale_color_brewer(palette = "Set2")+
  theme_bw(base_family = "sans")+
  ggtitle("Scatterplot of Years of schooling and Human development index")+
  xlab("Years of schooling")+
  ylab("Human development index")+
  labs(color = "Continent", fill = "Continent")+
  labs(size = "Population (millions)")+
  theme(legend.position = "bottom")

HDI_data %>%
  drop_na(Continent) %>%
  ggplot(aes(y = HDI, x = Schooling, color = Continent)) +
  geom_jitter(alpha = 0.6, na.rm = TRUE)+
  geom_smooth(method = "lm", se = TRUE, level = 0.95, na.rm = TRUE, show.legend = FALSE)+
  scale_fill_brewer(palette = "Set2")+
  scale_color_brewer(palette = "Set2")+
  theme_bw(base_family = "sans")+
  ggtitle("Scatterplot of Years of schooling and Human development index")+
  xlab("Years of schooling")+
  ylab("Human development index")+
  labs(color = "Continent", fill = "Continent")+
  labs(size = "Population (millions)")+
  theme(legend.position = "bottom") +
  facet_wrap( ~ Continent)