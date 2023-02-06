library(tidyverse)
library(knitr)
library(kableExtra)

# read in data
ratings <- read.csv("Reproducibility_Ratings.csv",
                    sep = ",",
                    na.strings = c("", "NA"))

# transpose
ratings_t <- as.data.frame(t(ratings))

# change column names and row names to first column and row
colnames(ratings_t) <- ratings_t[1,]
rownames(ratings_t) <- ratings_t[, 1]

# remove superfluous first column and row
ratings_t <- ratings_t[-1,]
ratings_t <- ratings_t[,-1]

# clean up row names
row.names(ratings_t) <- gsub(" ", "_", row.names(ratings_t))

# sum ratings up into "anonymous" rater 1, rater 2, rater 3, rater 4, rater 5
rb <- ratings_t %>%
  unite(newcol, everything(), na.rm = TRUE) %>%
  separate(newcol,
           into = c("rater_1", "rater_2", "rater_3", "rater_4", "rater_5"))

# convert data frame to numeric
rb <- mutate_all(rb, function(x)
  as.numeric(as.character(x)))

# subset group and individual ratings
ratings_individual <- rb[seq(1, nrow(rb), 2),]
ratings_group <- rb[seq(2, nrow(rb), 2),]

# Add median ratings for individual and group ratings

# median function 
modefun <- function(x) {
  tabresult <- tabulate(x)
  moderesult <- which(tabresult == max(tabresult))
  if (sum(tabresult == max(tabresult)) > 1)
    moderesult <- " "
  return(moderesult)
}

# add median to individual/initial and group/summary ratings 
ratings_individual$median_individual <-
  apply(as.matrix(ratings_individual), 1, modefun)
ratings_group$median_group <-
  apply(as.matrix(ratings_group), 1, modefun)

# change numerical ratings to text ratings individual/initial
ratings_individual_text <- ratings_individual %>% mutate(across(
  .cols = everything(),
  .fns = ~ case_when(
    .x == 1 ~ "Exactly",
    (.x > 1) & (. < 2.5) ~ "Essentially",
    (.x >= 2.5) & (. < 3.5) ~ "Partially",
    (.x >= 3.5) & (. < 4.5) ~ "Mostly not",
    (.x >= 4.5) ~ "Not at all",
    TRUE ~ as.character(.x)
  )
))

# change numerical ratings to text ratings group/summary
ratings_group_text <- ratings_group %>% mutate(across(
  .cols = everything(),
  .fns = ~ case_when(
    .x == 1 ~ "Exactly",
    (.x > 1) & (. < 2.5) ~ "Essentially",
    (.x >= 2.5) & (. < 3.5) ~ "Partially",
    (.x >= 3.5) & (. < 4.5) ~ "Mostly not",
    (.x >= 4.5) ~ "Not at all",
    TRUE ~ as.character(.x)
  )
))



# Create text tables

# change rownames to reflect random labelling, group/summary ratings
rownames(ratings_group_text) <-
  c(
    "Article 106",
    "Article 112",
    "Article 105",
    "Article 114",
    "Article 110",
    "Article 108",
    "Article 102",
    "Article 104",
    "Article 111",
    "Article 103",
    "Article 113",
    "Article 101",
    "Article 109",
    "Article 107"
  )

# change rownames to reflect random labelling, individual/initial ratings
rownames(ratings_individual_text) <-
  c(
    "Article 106",
    "Article 112",
    "Article 105",
    "Article 114",
    "Article 110",
    "Article 108",
    "Article 102",
    "Article 104",
    "Article 111",
    "Article 103",
    "Article 113",
    "Article 101",
    "Article 109",
    "Article 107"
  )


# order
ratings_group_text <-
  ratings_group_text[order(row.names(ratings_group_text)),]

ratings_individual_text <-
  ratings_individual_text[order(row.names(ratings_individual_text)),]

# missing mode issue fix
ratings_group_text$median_group[14] <-
  "Partially" # there is no mode here (so otherwise shows NA), no majority agreement for "Essentially"

# Create group/summary ratings table
tableGroup <- knitr::kable(
  ratings_group_text,
  format = "latex",
  booktabs = TRUE,
  escape = FALSE,
  col.names = c("Rater 1", "Rater 2", "Rater 3", "Rater 4", "Rater 5", "Modal Rating"),
)

# Create individual/initial ratings table
tableIndividual <- knitr::kable(
  ratings_individual_text,
  format = "latex",
  booktabs = TRUE,
  escape = FALSE,
  col.names = c("Rater 1", "Rater 2", "Rater 3", "Rater 4", "Rater 5", "Modal Rating")
)

# Save pretty tables
kableExtra::save_kable(tableGroup, "Group_Table.png")
kableExtra::save_kable(tableIndividual, "Individual_Table.png")