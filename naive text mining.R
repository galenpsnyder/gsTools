stopwords <- c(
  "a", "an", "and", "another",
  "be", "been", "before", "but", "by",
  "can", "could",
  "did", "do", "does",
  "get", "give", "go", "gone",
  "for", "from",
  "in", "is", "it",
  "none", "not",
  "of",
  "so",
  "than", "the", "then",
  "what", "when", "where", "who", "why"
)

parse_text <- function(text) {
  text <- gsub("[[:punct:]]", " ", text)
  text <- tolower(text)
  text <- unlist(strsplit(text, "\\s+"))
  unique(text)
}

remove_stopwords <- function(text) {
  text[!text %in% stopwords]
}

tidy_match_text_to_corpus <- function(text, corpus) {
  ii <- length(text)
  out <- 0
  
  for(i in seq_len(ii)) {
    test <- corpus == text[i]
    out <- out + sum(test, na.rm = TRUE)
  }
  
  out
}

tidy_match_text_to_corpus_deep <- function(text, corpus, thresh = 0.6) {
  ii <- length(text)
  jj <- length(corpus)
  out <- 0
  # comp_mat <- matrix(
  #   nrow = ii, 
  #   ncol = jj, 
  #   dimnames = list(text, corpus)
  # )
  
  t1 <- strsplit(text, "")
  t2 <- strsplit(corpus, "")
  
  for(i in seq_len(ii)) {
    t1_i <- t1[[i]]
    l_t1_i <- length(t1_i)
    for(j in seq_len(jj)) {
      t2_j <- t2[[j]]
      l_t2_j <- length(t2_j)
      test <- suppressWarnings(t1_i == t2_j)
      com <- match(FALSE, test, nomatch = min(l_t1_i, l_t2_j) + 1) - 1
      # comp_mat[i, j] <- as.numeric(com / max(l_t1_i, l_t2_j) >= thresh)
      out <- out + as.numeric(com / max(l_t1_i, l_t2_j) >= thresh)
    }
  }
  out
}

tidy_text_similarity <- function(text, corpus, deep = FALSE, thresh = 0.6) {
  ii <- length(corpus)
  out <- numeric(length = ii)
  parsed_text <- remove_stopwords(parse_text(text))

  for(i in seq_len(ii)) {
    parsed_corpus <- remove_stopwords(parse_text(corpus[i]))
    if(deep) {
      out[i] <- tidy_match_text_to_corpus_deep(parsed_text, parsed_corpus, thresh)
    } else {
      out[i] <- tidy_match_text_to_corpus(parsed_text, parsed_corpus)
    }
  }
  
  out
}

library(tidyverse)
test_text <- "ability to compare visual patterns or identify a visual pattern among distracting patterns"
corpus_data <- readRDS("corpus_data.Rds")

corpus_data %>%
  mutate(text_sim = tidy_text_similarity(test_text, description, deep = FALSE)) %>%
  arrange(desc(text_sim)) %>%
  view()

corpus_data %>%
  mutate(text_sim = tidy_text_similarity(test_text, description, deep = TRUE)) %>%
  arrange(desc(text_sim)) %>%
  view()
