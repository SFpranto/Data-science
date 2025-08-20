
list.of.packages <- c("rvest", "xml2", "httr", "dplyr", "tm", "stringr",
                      "topicmodels", "tidytext", "ggplot2", "wordcloud")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load libraries
library(rvest)
library(xml2)
library(httr)
library(dplyr)
library(tm)
library(stringr)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(wordcloud)

save_path <- "C:/final project"

main_url <- "https://www.bbc.com/sport"
main_page <- read_html(main_url)


raw_links <- html_nodes(main_page, "a.gs-c-promo-heading") %>%
  html_attr("href") %>%
  unique()

article_links <- paste0("https://www.bbc.com/sport", raw_links[grepl("^/sport/articles/", raw_links)])
article_links <- head(article_links, 30)

spell_corrections <- c("secureed" = "secured", "dramattic" = "dramatic", "aganst" = "against",
                       "amzing" = "amazing", "twistz" = "twists", "turnz" = "turns", "deligted" = "delighted")
correct_spelling <- function(text) {
  for (wrong in names(spell_corrections)) {
    text <- gsub(wrong, spell_corrections[[wrong]], text)
  }
  return(text)
}
simple_stem <- function(tokens) {
  gsub("(ing|ed|ly|s)$", "", tokens)
}

results <- list()
stop_words <- stopwords("en")

for (url in article_links) {
  webpage <- try(read_html(url), silent = TRUE)
  if (inherits(webpage, "try-error")) next
  
  title_node <- html_node(webpage, '.ssrcss-1pxtyyf-StyledHeading')
  title <- if (!is.na(title_node)) html_text(title_node) else NA_character_
  
  paragraphs <- html_nodes(webpage, 'p')
  text <- paste(html_text(paragraphs), collapse = " ")
  
  raw_text <- text

  clean_text <- tolower(raw_text)
  clean_text <- removePunctuation(clean_text)
  clean_text <- removeNumbers(clean_text)
  clean_text <- stripWhitespace(clean_text)
  clean_text <- correct_spelling(clean_text)
  
  tokens <- unlist(strsplit(clean_text, "\\s+"))
  tokens <- tokens[!(tokens %in% stop_words)]
  stemmed_tokens <- simple_stem(tokens)
  
  results[[length(results) + 1]] <- data.frame(
    URL = url,
    Title = title,
    RawText = raw_text,
    CleanText = clean_text,
    Tokens = paste(shQuote(tokens), collapse = " "),
    StemmedTokens = paste(shQuote(stemmed_tokens), collapse = " "),
    stringsAsFactors = FALSE
  )
}

final_data <- bind_rows(results)
raw_data <- final_data %>% select(URL, Title, RawText)
cleaned_data <- final_data %>% select(URL, Title, CleanText, Tokens, StemmedTokens)

write.csv(raw_data, file = file.path(save_path, "bbc_football_raw_data.csv"), row.names = FALSE)
write.csv(cleaned_data, file = file.path(save_path, "bbc_football_cleaned_data.csv"), row.names = FALSE)


View(raw_data)
View(cleaned_data)

cat("\n--- 🔹 Raw Data (Before Cleaning) ---\n")
print(head(raw_data))

cat("\n--- 🔸 Cleaned Data (After Cleaning) ---\n")
print(head(cleaned_data))


corpus <- VCorpus(VectorSource(cleaned_data$CleanText))
dtm <- DocumentTermMatrix(corpus, control = list(
  wordLengths = c(3, Inf), removePunctuation = TRUE,
  removeNumbers = TRUE, stopwords = TRUE
))
dtm <- removeSparseTerms(dtm, 0.99)

num_topics <- 4
lda_model <- LDA(dtm, k = num_topics, control = list(seed = 1234))


top_terms <- terms(lda_model, 10)
print(top_terms)

topics <- tidy(lda_model, matrix = "beta")
top_terms_plot <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

if (!"reorder_within" %in% ls("package:tidytext")) {
  tidytext::reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
    new_x <- paste(x, within, sep = sep)
    stats::reorder(new_x, by, FUN = fun)
  }
}
if (!"scale_x_reordered" %in% ls("package:tidytext")) {
  tidytext::scale_x_reordered <- function(..., sep = "___") {
    ggplot2::scale_x_discrete(labels = function(x) gsub(sep, "\n", x), ...)
  }
}

ggplot(top_terms_plot, aes(reorder_within(term, beta, topic), beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top Terms per Topic", x = "Terms", y = "Beta")

for (i in 1:num_topics) {
  topic_terms <- topics %>%
    filter(topic == i) %>%
    arrange(desc(beta)) %>%
    head(40)
  
  wordcloud(words = topic_terms$term, freq = topic_terms$beta, max.words = 40,
            colors = brewer.pal(8, "Dark2"))
}


library(dplyr)
library(ggplot2)
library(tidytext)

top_terms_plot <- topics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  mutate(topic = paste("Topic", topic),
         term = reorder_within(term, beta, topic))


ggplot(top_terms_plot, aes(term, beta, fill = factor(topic))) + 
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = "Term Probability (Beta)", 
       title = "Top Terms in Each Topic",
       subtitle = "LDA Topic Modeling Results") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        strip.text = element_text(size = 12, face = "bold"))