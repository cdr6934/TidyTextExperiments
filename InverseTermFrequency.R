## Inverse Term Frequency 

library(dplyr)
library(janeaustenr)
library(tidytext)
library(ggplot2)


#Generates every word that occurs in a book 
book_words <- austen_books()%>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE) %>%
  ungroup()


# generates a total count of words for each book 
total_words <- book_words %>% group_by(book) %>% summarize(total = sum(n))
book_words <- left_join(book_words, total_words)
book_words


#Plots a histogram of all the words and their occurances in each of the books 
ggplot(book_words, aes(n/total, fill = book )) +
  geom_histogram(show.legend =FALSE) + 
  xlim(NA, 0.0009) + 
  facet_wrap(~book, ncol = 2, scales = "free_y")


#generating the inverse document frequency
book_words <- book_words %>% 
  bind_tf_idf(word, book, n)

book_words

#The inverse document frequency is very low for words that occur in many of the documents in a collection; this is how this approach decreases the weight for common words. 
# The inverse document frequency will be a higher number for words that occur in fewer of 
# the documents in the collection. 


# Proper nouns, names that are in fact important in these novels. None of them occur 
# in all of the novels, and they are important characteristic words for each text
book_words %>% 
select(-total) %>%
  arrange(desc(tf_idf))


# Sometimes we can also just look at them for one book 
book_words %>% 
  filter(book == "Pride & Prejudice") %>%
  select(-total) %>%
  arrange(desc(tf_idf))