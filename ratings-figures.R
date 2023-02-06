library(tidyverse)
library(reshape2)
library(RColorBrewer)

# read in data
ratings <- read.csv("Reproducibility_Ratings.csv", 
                    sep = ",", na.strings = c("", "NA"))

# transpose
ratings_t <- as.data.frame(t(ratings))

# change column and row names to first column and row
colnames(ratings_t) <- ratings_t[1, ]
rownames(ratings_t) <- ratings_t[, 1]

# remove superfluous first row
ratings_t <- ratings_t[-1, ]

# subset group and individual ratings
ratings_individual <- ratings_t[seq(1, nrow(ratings_t), 2), ]
ratings_group <- ratings_t[seq(2, nrow(ratings_t), 2), ]

# wide to long format
ratings_individual <- melt(ratings_individual, id.vars = "Name")
ratings_group <- melt(ratings_group, id.vars = "Name")

# ratings values as factor
ratings_individual$value <- factor(ratings_individual$value)
ratings_group$value <- factor(ratings_group$value)

# vector for 'anonymising' articles
articles <- c("Article 1", "Article 2", "Article 3", 
              "Article 4", "Article 5", "Article 6", "Article 7", 
              "Article 8", "Article 9", "Article 10", "Article 11", 
              "Article 12", "Article 13", "Article 14")

# create stacked bar plot for individual ratings
ratings_individual %>% 
  filter(!is.na(value)) %>% 
  ggplot(aes(x = fct_rev(Name),fill = value)) + 
  geom_bar(stat = "count") + 
  coord_flip() + 
  scale_x_discrete(labels = rev(articles)) + 
  labs(x = NULL, y = NULL) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_manual(values = brewer.pal(5, "Blues"), 
                    name = "Reproducibility Rating", breaks = 5:1, 
                    labels = c("Not at all", "Mostly not", "Partially", 
                               "Essentially", "Exactly")) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA), 
        plot.background = element_rect(fill = "transparent", colour = NA))

ggsave("individual.png")

# create stacked bar plot for group ratings
ratings_group %>% 
  filter(!is.na(value)) %>% 
  ggplot(aes(x = fct_rev(Name), fill = value)) + 
  geom_bar(stat = "count") + 
  coord_flip() + 
  scale_x_discrete(labels = rev(articles)) + 
  labs(x = NULL, y = NULL) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_manual(values = brewer.pal(5, "Blues"), 
                    name = "Reproducibility Rating", breaks = 5:1, 
                    labels = c("Not at all", "Mostly not", "Partially", 
                               "Essentially", "Exactly")) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = "transparent", colour = NA), 
        plot.background = element_rect(fill = "transparent", colour = NA))

ggsave("group.png")
