# Author: Camila Hennessey
# Feb 7, 2024

# Libraries
library(ggplot2)
library(ggthemes)
library(tm)
library(dplyr)       
library(tidyr)      
library(stringr)     
library(wordcloud)   
library(RColorBrewer) 
library(wordcloud2)  
library(qdapRegex)   
library(text2vec)    
library(cld2)      
library(cld3)     
library(tidytext)

# wd
setwd("~/Google Drive/MBAN/Visualizing and Analyzing with R/Visualizing and Analyzing with R/personalFiles")

# custom functions
# Options & Functions
Sys.setlocale('LC_ALL','C')

tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url)) 
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# load data
studentBios <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing_Analyzing_Data_with_R/main/Cases/A3_NLP/Student%20Ambassador%20Bios/final_student_data.csv')

# Add stop word

customStopwords <- c(stopwords('english'), 
                     'hult', 'university', 'business', 
                     'name', 'also', 'can', 
                     'everyone', 'part', 'new', 
                     'student', 'international', 'global', 
                     'experience','able', 'ability', 'very', 'many', 'much', 'own', 
                     'such', 'thing', 'things', 'various',
                     'study', 'studies', 'field', 'major', 'career', 'goal', 'goals', 
                     'aspiration', 'aspirations',
                     'skill', 'skills', 'talent', 'talents', 'interest', 'interests', 
                     'hobby', 'hobbies', 'activity', 'activities',
                     'work', 'working', 'worked', 'learn', 'learning', 'learned', 'education', 
                     'educational', 'project', 'projects', 'made', 'people', 'world', 'hello',
                     'school', 'campus', 'currently', 'love', 'one', 'different','london',
                     'will', 'get', 'years', 'im', 'time', 'like', 'feel',
                     'free', 'best', 'mba', 'help', 'club', 'clubs', 'around', 
                     'questions', 'ambassador', 'know', 'marketing', 
                     'community', 'degree', 'management', 'program', 'life', 'day', 'dubai', 
                     'bachelors', 'enjoy', 'want', 'ask', 'students', 'masters', 
                     'come', 'boston', 'studying', 'enjoy', 'Italy', 'well', 'lot', 'far', 
                     'day', 'year', 'india', 'professors', 'finance', 'helped', 'italy',
                     'really', 'ive', 'came', 'think', 'opportunities', 'meeting',
                     'coming', 'city', 'really', 'first', 'decided', 'born', 'anything',
                     'truly', 'say', 'president', 'personally', 'originally', 'make',
                     'background', 'member', 'music', 'social', 'professional', 'happy', 
                     'diverse', 'helps', 'wanted', 'apply', 'three', 'comfort', 'apply', 'long', 
                     'else','choice', 'decision', 'economics', 'looking', 'see', 'chose', 'place', 'hsa',
                     'fashion', 'contact', 'level', 'brand', 'become', 'answer', 'involved', 'zone', 
                     'sales', 'class', 'ever', 'mexico', 'hutls', 'san francisco', 'end', 'met', 'affairs',
                     'chat', 'looking', 'give', 'cant', 'design', 'everything', 'found', 'body', 'find', 'head',
                     'true', 'two', 'along', 'right', 'add', 'couldnt', 'gain', 'undergraduate', 'offer', 
                     'wait', 'better', 'media', 'film', 'design', 'dual', 'etc', 'dont', 'germany', 'still',
                     'living', 'recently', 'improve', 'creating', 'several', 'allowed', 'decisions',
                     'just', 'team', 'choose', 'every', 'job', 'ago', 'home', 'taking', 'role', 'always', 'lived', 
                     'several', 'states', 'done', 'art', 'especially', 'msc', 'forward', 'foods', 'prior', 'important',
                     'reach', 'comes', 'globe', 'addition', 'companies', 'soon', 'several',
                     'open', 'analytics', 'general', 'perfect', 'reason', 'mib', 'pursuing',
                     'majoring', 'makes', 'way', 'definitely', 'association', 'bba', 'interested', 'hope',
                     'reason', 'grown', 'half', 'past', 'exploring', 'football', 'graduated', 'achieve', 'back',
                     'join', 'led', 'hey', 'explore', 'another', 'covid', 'current', 'already', 'another',
                     'set', 'fulltime', 'ways', 'top', 'traveling', 'rather', 'going', 'explore', 'house', 'investing', 
                     'talk', 'understand', 'corporate', 'look', 'challenge', 'person', 'france', 'colombia', 'cofounder', 
                     'glad', 'keep', 'advice', 'aside', 'sports', 'entire', 'hults', 'executive', 'allowing', 
                     'practical', 'strong', 'guys', 'connect', 'start', 'even', 'months', 'editing', 'brazilian', 
                     'women', 'tank', 'cultural', 'thats', 'never', 'executive', 'despite', 'master', 'develop',
                     'abroad', 'classes', 'constantly', 'backgrounds', 'throughout', 'incredible', 'active',
                     'network', 'networking', 'something'
                     )


# create a corpus
studentCorpus <- VCorpus(VectorSource(studentBios$bio))


# clean the corpus
studentCorpus <- cleanCorpus(studentCorpus,customStopwords)

# Examine one corpus
content(studentCorpus[[1]])

# document term metrix (documents are rows)
studentDTM <- DocumentTermMatrix(studentCorpus)
studentDTM <- as.matrix(studentDTM)

# Examine the dimentions
dim(studentDTM)


# words frequency matrix
studentFreq <- colSums(studentDTM)
studentFreq <- data.frame(word=names(studentFreq),
                          frequency=studentFreq, 
                          row.names = NULL)

# Examine a portion of the WFH to make sure we built it correctly
head(studentFreq,10)

# Correcting the approach for consistency

# Apply threshold >= 10 for both visualizations
topWords <- subset(studentFreq, frequency >= 10) 
topWords <- topWords[order(topWords$frequency, decreasing=TRUE),]  # Correct the order

# Now, use `topWords` for both the bar chart and word cloud to ensure consistency

# Bar chart
ggplot(topWords, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='hotpink') + 
  coord_flip() + theme_gdocs() +
  geom_text(aes(label=frequency), hjust=-0.1, size=5.0, color="white")

# Word Cloud
wordcloud(words = topWords$word, freq = topWords$frequency, min.freq = 10,
          max.words=75,  # Reduced max.words to limit the number of words displayed
          random.order=FALSE, rot.per=0.25,  # Adjusted rotation percentage to have fewer vertical words
          scale=c(3, 0.5),  # Adjusted scale to have a wider size range, preventing cut-offs
          colors=brewer.pal(8, "Dark2"))


# Create a corpus from the 'interests' column
studentInterestsCorpus <- VCorpus(VectorSource(studentBios$interests))

customStopwordsInterests <- c(stopwords('english'), 
                     'hult', 'university', 'business', 'things', 'student',
                     'soon', 'new', 'name', 'also', 'born', 'chat', 'everyone',
                     'know', 'italy', 'mexico', 'interests', 'include', 'india', 'hey',
                     'international', 'masters', 'like', 'london', 'lets', 'hello', 'free', 'food', 
                     'hello', 'include', 'impact', 'feel', 'boston', 'also', 'love', 'everyone', 
                     'social', 'degree', 'chat', 'campus', 'life', 'cooking', 'travel', 'sports', 'music',
                     'indian', 'reading', 'passionate', 'people'
                      )

# Clean the corpus
studentInterestsCorpus <- cleanCorpus(studentInterestsCorpus, customStopwordsInterests)

# Examine one corpus item to ensure cleaning is effective
content(studentInterestsCorpus[[1]])

# Create Document-Term Matrix for the 'interests' corpus
studentInterestsDTM <- DocumentTermMatrix(studentInterestsCorpus)
studentInterestsDTM <- as.matrix(studentInterestsDTM)

# Examine the dimensions of the DTM to understand its structure
dim(studentInterestsDTM)

# Calculate words frequency matrix from the DTM
studentInterestsFreq <- colSums(studentInterestsDTM)
studentInterestsFreq <- data.frame(word = names(studentInterestsFreq), frequency = studentInterestsFreq, row.names = NULL)

# Examine a portion of the word frequency matrix
head(studentInterestsFreq, 20)

# Apply threshold for visualization (e.g., frequency >= 5)
topInterestsWords <- subset(studentInterestsFreq, frequency >= 5)
topInterestsWords <- topInterestsWords[order(topInterestsWords$frequency, decreasing = TRUE),]

# Bar chart visualization for the 'interests' column
ggplot(topInterestsWords, aes(x = word, y = frequency)) + 
  geom_bar(stat = "identity", fill = 'lightgreen') + 
  coord_flip() + theme_gdocs() +
  geom_text(aes(label = frequency), hjust = -0.1, size = 5.0, color = "white")

# Word Cloud for the 'interests' column, ensuring the min.freq matches the threshold used above
wordcloud(words = topInterestsWords$word, freq = topInterestsWords$frequency, min.freq = 5, # Adjust min.freq to match the threshold
          max.words = 75, random.order = FALSE, rot.per = 0.25, 
          scale = c(3, 0.5), colors = brewer.pal(8, "Dark2"))

# Calculate frequencies of each program
programFreqs <- table(studentBios$programTitle)

# Convert to dataframe for sorting and filtering
programFreqsDF <- as.data.frame(programFreqs)

# Create bar plot
ggplot(programFreqsDF, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  theme_minimal() +
  labs(title = "Frequency of Each Program", x = "Program Title", y = "Frequency") +
  coord_flip()  # Flip coordinates for horizontal bars

# Name the columns appropriately
names(programFreqsDF) <- c("Program", "Frequency")

# Sort by frequency and select the top 5
topProgramsDF <- programFreqsDF[order(-programFreqsDF$Frequency), ][1:5,]

# Enhanced bar plot for the top 5 programs
ggplot(topProgramsDF, aes(x = reorder(Program, Frequency), y = Frequency, fill = Program)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  # Remove legend since labels are self-explanatory
  geom_text(aes(label = Frequency), vjust = -0.3, size = 3.5) +  # Add frequency labels above bars
  scale_fill_brewer(palette = "Blues") +  # Use a color palette that's visually appealing
  coord_flip() +  # For horizontal bars
  theme_minimal() +
  theme(axis.title.x = element_blank(),  # Remove the X axis title for cleaner look
        axis.text.x = element_text(size = 12),  # Adjust text size for readability
        axis.text.y = element_text(size = 12)) +
  labs(title = "Top 5 Programs by Frequency", y = "Frequency")  # Adjust titles and labels as needed

# Sort by frequency and select the top 3
topProgramsDF <- programFreqsDF[order(-programFreqsDF$Frequency), ][1:3,]

# Enhanced bar plot for the top 3 programs
ggplot(topProgramsDF, aes(x = reorder(Program, Frequency), y = Frequency, fill = Program)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  # Remove legend since labels are self-explanatory
  geom_text(aes(label = Frequency), vjust = -0.3, size = 3.5) +  # Add frequency labels above bars
  scale_fill_brewer(palette = "Blues") +  # Use a color palette that's visually appealing
  coord_flip() +  # For horizontal bars
  theme_minimal() +
  theme(axis.title.x = element_blank(),  # Remove the X axis title for cleaner look
        axis.text.x = element_text(size = 12),  # Adjust text size for readability
        axis.text.y = element_text(size = 12)) +
  labs(title = "Top 3 Programs by Frequency", y = "Frequency")  # Adjust titles and labels as needed

# Calculate frequencies of each campus
campusFreqs <- table(studentBios$campus)

# Convert to dataframe for ggplot
campusFreqsDF <- as.data.frame(campusFreqs)

# Name the columns appropriately
names(campusFreqsDF) <- c("Campus", "Frequency")

# Optionally, sort by frequency if you want to visualize top N campuses
campusFreqsDF <- campusFreqsDF[order(-campusFreqsDF$Frequency),]

ggplot(campusFreqsDF, aes(x = reorder(Campus, -Frequency), y = Frequency, fill = Campus)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  # Using bars to represent frequencies
  geom_text(aes(label = Frequency), nudge_y = 5, size = 3.5) +  # Adjusting for text labels above bars
  scale_fill_brewer(palette = "Blues") +  # Coloring bars
  theme_minimal() +  # Minimal theme for a cleaner look
  theme(
    axis.title.x = element_blank(),  # Cleaner look by removing the x-axis title
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Improve readability of x-axis labels
    axis.text.y = element_text(size = 12)  # Ensuring y-axis text size is readable
  ) +
  labs(title = "Frequency of Each Campus", y = "Frequency", x = "")  # Labelling the graph

# Contingency table for Gender and Campuses
genderCampusTable <- table(studentBios$namSorGender.likelyGender, studentBios$campus)

# View the contingency table
genderCampusTable

# Contingency table for Gender and Programs
genderProgramTable <- table(studentBios$namSorGender.likelyGender, studentBios$programTitle)

# View the contingency table
genderProgramTable

# Convert the contingency table to a dataframe for plotting
genderCampusDF <- as.data.frame(genderCampusTable)

# Plotting
ggplot(genderCampusDF, aes(x = Var2, y = Freq, fill = Var1)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Pastel1", name = "Gender") +
  labs(title = "Gender Distribution Across Campuses", x = "Campus", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Convert the contingency table to a dataframe for plotting
genderProgramDF <- as.data.frame(genderProgramTable)

# Plotting
ggplot(genderProgramDF, aes(x = Var2, y = Freq, fill = Var1)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Pastel2", name = "Gender") +
  labs(title = "Gender Distribution Across Programs", x = "Program", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Convert corpus to a dataframe
corpus_df <- data.frame(text = sapply(studentCorpus, as.character), stringsAsFactors = FALSE)

# Tokenize words
tidy_bios <- corpus_df %>%
  unnest_tokens(word, text)

# Join with sentiment lexicon
sentiment_bios <- tidy_bios %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)

# Summarize sentiment
sentiment_summary <- sentiment_bios %>%
  group_by(sentiment) %>%
  summarize(n = sum(n))

# Print summary
print(sentiment_summary)

# Assuming sentiment_summary is already calculated as shown previously

# Visualize sentiment distribution
ggplot(sentiment_summary, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("positive" = "lightgreen", "negative" = "#FFC0CB")) +
  theme_minimal() +
  labs(title = "Sentiment Analysis of Student Bios",
       x = "Sentiment",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Improve x-axis label readability
        legend.title = element_blank())  # Remove legend title

# Adjusting the corpus to dataframe conversion to retain group identifiers
tidy_bios <- studentBios %>%
  select(bio, programTitle, campus, namSorGender.likelyGender) %>% # Select relevant columns
  unnest_tokens(word, bio) %>%
  anti_join(stop_words) %>%
  filter(!word %in% customStopwords) %>%
  mutate(document = row_number())

# Proceed with TF-IDF calculation
tf_idf <- tidy_bios %>%
  count(programTitle, word, sort = TRUE) %>% # Count words within programs
  bind_tf_idf(word, programTitle, n)

# Explore top TF-IDF scores for each program
top_tf_idf <- tf_idf %>%
  group_by(programTitle) %>%
  top_n(5, tf_idf) %>%
  ungroup() %>%
  arrange(programTitle, desc(tf_idf))

print(top_tf_idf)


# Assuming 'studentBios' is loaded and 'customStopwords' includes relevant stopwords


# Prepare the interests text data
interests_text <- studentBios %>%
  select(interests) %>%
  unnest_tokens(word, interests) %>%
  filter(!word %in% customStopwords) %>%
  mutate(document = row_number())

# Join with sentiment lexicon
sentiment_interests <- interests_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)

# Summarize sentiment
sentiment_summary_interests <- sentiment_interests %>%
  group_by(sentiment) %>%
  summarize(n = sum(n))

# Print summary
print(sentiment_summary_interests)

# Visualize sentiment distribution for "interests"
ggplot(sentiment_summary_interests, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("positive" = "lightgreen", "negative" = "#FFC0CB")) +
  theme_minimal() +
  labs(title = "Sentiment Analysis of Student Interests",
       x = "Sentiment",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())


# Unnest tokens for 'bio' or 'interests' and join with sentiment lexicon
sentiment_analysis <- studentBios %>%
  select(bio, programTitle, campus, namSorGender.likelyGender) %>% # Select relevant columns for analysis
  unnest_tokens(word, bio) %>% # Use 'interests' instead of 'bio' if analyzing interests
  inner_join(get_sentiments("bing")) %>%
  count(programTitle, campus, namSorGender.likelyGender, sentiment, sort = TRUE) %>%
  group_by(programTitle, campus, namSorGender.likelyGender) %>%
  summarize(
    total = n(),
    positive = sum(sentiment == "positive"),
    negative = sum(sentiment == "negative"),
    sentiment_score = (positive - negative) / total
  )

# View the sentiment analysis summary
print(sentiment_analysis)

# Plot sentiment scores across programs
ggplot(sentiment_analysis, aes(x = reorder(programTitle, sentiment_score), y = sentiment_score, fill = programTitle)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Sentiment Score by Program", x = "", y = "Sentiment Score") +
  theme_minimal() +
  theme(legend.position = "none")

# Plot sentiment scores across campuses
ggplot(sentiment_analysis, aes(x = reorder(campus, sentiment_score), y = sentiment_score, fill = campus)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Sentiment Score by Campus", x = "", y = "Sentiment Score") +
  theme_minimal() +
  theme(legend.position = "none")

# Plot sentiment scores across genders
ggplot(sentiment_analysis, aes(x = reorder(namSorGender.likelyGender, sentiment_score), y = sentiment_score, fill = namSorGender.likelyGender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Sentiment Score by Gender", x = "", y = "Sentiment Score") +
  theme_minimal() +
  theme(legend.position = "none")

