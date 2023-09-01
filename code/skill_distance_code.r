#Packages Used
library("readxl")
library("stringr")
library("dplyr")

#Set working directory and read files
setwd("C:/Users/chews/Downloads")
my_data <- read_excel("Skills.xlsx")

#Change the column names for easier coding by removing spaces
names(my_data)<-str_replace_all(names(my_data), c(" " = "." , "," = "" ))

#Keeping the level measures 
my_data <- my_data %>%
  filter(Scale.Name == 'Level')

#Use to identify unique occupations in the Skills Dataset
#to compute skills distance
unique_titles <- my_data %>%
  distinct(Title)

#Create a new dataframe that stores all the skill
#distance combinations
#First.Occupation and Second.Occupation
#indicates that given a job, what are the skill distance
#to the other jobs
df_skill_distance <- data.frame(matrix(ncol = 3, nrow = 0))
columns <- c("First.Occupation", "Second.Occupation", "Skill.Distance")
colnames(df_skill_distance) <- columns

#Execute the loop to find the skill distance
for (first in 1:nrow(unique_titles)){
  first_occ <- my_data %>% 
    filter(Title == unique_titles$Title[first]) 
  for(second in 1:nrow(unique_titles)){
    second_occ <- my_data %>%
      filter(Title == unique_titles$Title[second]) 
    skill_dist <- sqrt(sum((first_occ$Data.Value - second_occ$Data.Value)^2))
    df_skill_distance[nrow(df_skill_distance)+1,] <- c(first_occ$Title[first],second_occ$Title[first],skill_dist)
  }
}


