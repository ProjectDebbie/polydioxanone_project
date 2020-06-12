library(tidyr)
library(tidytext)
library(dplyr)
library(pluralize)
library(ggplot2)

# in the dataframe, create and one column containing the titles and abstracts of each record
pdo$text <- paste(pdo$Title, pdo$Abstracts)

# tokenize, remove stop words
t <- df %>% group_by(PMID) %>% unnest_tokens(word, text) %>% anti_join(stop_words) %>% ungroup()

# Singularize and remove words shorter than 3 characters
t$word <- singularize(t$word)
t <- subset(t, nchar(as.character(word)) >= 3)

# count word total, total per year and join period of publication
df <- df %>% replace(is.na(.), 0)
total <- df %>% count (word, sort = TRUE)
df <- df %>% group_by(Year) %>% count(word, sort = TRUE)
df2 <- left_join(df, total, by = 'word')
df$period <- ifelse(df$Year > 2000, "post-2000", "pre-2000")

# plot change over years for a subset of word count (eg: 125-150)
p = ggplot(data= subset(df5, total < 125 & total > 100), aes(x=Year, y=n, group=word)) + geom_line(aes(color=word))+ geom_point(aes(color=word))+ geom_text(aes(label=word), hjust=0, vjust=0)+ ggtitle('word frequency between 100-125'
plot(p)

