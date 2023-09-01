library(ONETr)
library(haven)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textplots)
library(quanteda.textstats)
library(readxl)
library(lexicon)
library(dplyr)
library(foreign)

#Setting up working directory and importing data(File on alternate titles and all countries)
#data - Reading from collapsed_allcountries_foranalysis
#job_title - All alternative job title from ONET
setwd("C:/Users/chews/OneDrive - National University of Singapore/Documents/NUS/2223 Sem 1/EC3551")
data <- read_dta("collapsed_allcountries_foranalysis.dta")
job_title <- read_excel("Alternate Titles.xlsx")

#   saved_data - All entries in collapsed_all_countries_foranalysis that does not have a proper jobname
#and is kept aside to be merge back in the future
#   data - The main dataframe that contains valid jobname that we are going to be working with
invalid_jobname_data <- data[(is.na(data$jobname) | data$jobname==""|data$jobname=="#N/A"|data$jobname=="[no label]"|data$jobname=="0"), ]
data <- data[!(is.na(data$jobname) | data$jobname==""|data$jobname=="#N/A"|data$jobname=="[no label]"|data$jobname=="0"), ]

##Creating a corpus aka A list of all words in jobname column 
##in collapsed_allcountries_foranalysis.dta
corpus <- corpus(data, text_field = 'jobname')

##Creates a list of tokens that is 
##going to be used to for NLP prediction later
corpus_tokens <- corpus |>
  tokens(remove_punct = T, remove_numbers = T, remove_symbols = T) |> 
  tokens_tolower() |>                                                   ## normalize
  tokens_remove(stopwords('en')) |>                                     ## remove stopwords (English)
  tokens_replace(pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma)## lemmanization


##Creates a Document Feature Matrix(DFM) that identify the key 
##root words for each row of jobtitle in collapsed_allcountries_foranalysis.dta
##that is going to be used for NLP prediction later
dfm <- dfm(corpus_tokens)




##As above, except that this is for the Alternate title file in Onet
alt_titles <- corpus(job_title, text_field = 'Alternate Title')
alt_titles2 <- alt_titles |>
  tokens(remove_punct = T, remove_numbers = T, remove_symbols = T) |> 
  tokens_tolower() |>                                                   ## normalize
  tokens_remove(stopwords('en')) |>                                     ## remove stopwords (English)
  tokens_replace(pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma)## lemmanization
alt_titles_dfm <- dfm(alt_titles2)



#Create a Naive Bayes model that is going to be used to predict the 
#Title in Onet data based on the jobtitle column in collapsed_allcountries_foranalysis.dta
nb_model <-  textmodel_nb(alt_titles_dfm, alt_titles_dfm$Title)

#Create a new category in collapsed_allcountries_foranalysis.dta 
#named predicted_categories that predicts the Title in Onet data
#based on the jobname column 
#  Following which, we merge saved_data back into data
data$predicted_categories <- predict(nb_model,newdata = dfm,type = "class",force = TRUE)
data <- dplyr::bind_rows(data, invalid_jobname_data)

#Save the resulting file
write.csv(data, "C:/Users/chews/Downloads/NEA/collapsed_allcountries_foranalysis_update.csv")
