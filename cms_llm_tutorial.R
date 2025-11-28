setwd("C:/Users/Kanat/Desktop")

install.packages(c("readr", "dplyr", "stringr", "tidytext"))

library(readr)
library(dplyr)
library(stringr)
library(tidytext)
comments <- read_csv("cms_comments.csv")
install.packages("readr")
library(readr)
comments <- read_csv("cms_comments.csv")
library(readr)
comments <- read_csv(file.choose())
names(comments)
head(comments, 3)
library(readr)
library(dplyr)
library(stringr)
library(tidytext)
comments_clean <- comments %>%
  select(
    comment_id   = `Document ID`,   # колонка с ID
    comment_text = Comment          # колонка с текстом комментария
  ) %>%
  filter(!is.na(comment_text), comment_text != "")
head(comments_clean, 3)
head(comments_clean$comment_text, 5)
# Разбиваем текст комментариев на отдельные слова
tokens <- comments_clean %>%
  unnest_tokens(word, comment_text)

# Загружаем лексикон Bing (positive/negative слова)
bing_lex <- get_sentiments("bing")

# Считаем позитивные/негативные слова по каждому комменту
sentiment_scores <- tokens %>%
  inner_join(bing_lex, by = "word") %>%
  count(comment_id, sentiment) %>%
  tidyr::pivot_wider(
    names_from  = sentiment,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(sentiment_score = positive - negative)

# Смотрим результаты
sentiment_scores
comments <- read_csv("cms_comments.csv")


summary(sentiment_scores$sentiment_score)

library(ggplot2)

ggplot(sentiment_scores, aes(x = sentiment_score)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(
    title = "Distribution of Sentiment Scores for CMS-2023-0144 Comments",
    x = "Sentiment Score (positive - negative)",
    y = "Count of Comments"
  )

# Сохранить как PNG
ggsave(
  filename = "sentiment_histogram.png",  # имя файла
  plot     = p,                          # какой график сохраняем
  width    = 8,                          # ширина в дюймах
  height   = 5,                          # высота
  dpi      = 300                         # качество
)

ggsave(
  filename = "sentiment_histogram.png",
  plot     = p,
  width    = 8,
  height   = 5,
  dpi      = 300
)

ggsave(
  filename = "C:/Users/User/Desktop/sentiment_histogram.png",
  plot     = p,
  width    = 8,
  height   = 5,
  dpi      = 300
)
p <- ggplot(...)
library(ggplot2)
p <- ggplot(sentiment_scores, aes(x = sentiment_score)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(
    title = "Distribution of Sentiment Scores for CMS-2023-0144 Comments",
    x = "Sentiment Score (positive - negative)",
    y = "Count of Comments"
  )
p

ggsave(
  filename = "C:/Users/User/Desktop/sentiment_histogram.png",
  plot     = p,
  width    = 8,
  height   = 5,
  dpi      = 300
)

library(dplyr)

top20_positive <- sentiment_scores %>%
  arrange(desc(sentiment_score)) %>%     # сортируем по убыванию
  slice_head(n = 20) %>%                 # берём первые 20
  left_join(comments_clean, by = "comment_id")   # добавляем текст

# посмотреть в RStudio
View(top20_positive)

# сохранить в CSV (опционально)
write.csv(top20_positive, "top20_positive_comments.csv", row.names = FALSE)



top20_negative <- sentiment_scores %>%
  arrange(sentiment_score) %>%           # сортируем по возрастанию (от -∞ к +∞)
  slice_head(n = 20) %>%                 # первые 20 = самые негативные
  left_join(comments_clean, by = "comment_id")   # добавляем текст

# посмотреть в RStudio
View(top20_negative)

# сохранить в CSV (опционально)
write.csv(top20_negative, "top20_negative_comments.csv", row.names = FALSE)
library(dplyr)

sentiment_scores_grouped <- sentiment_scores %>%
  mutate(
    sentiment_group = case_when(
      sentiment_score > 0  ~ "Positive",
      sentiment_score < 0  ~ "Negative",
      TRUE                 ~ "Neutral"
    )
  )
table(sentiment_scores_grouped$sentiment_group)

comparison_table <- sentiment_scores_grouped %>%
  group_by(sentiment_group) %>%
  summarise(
    n           = n(),
    perc        = n() / nrow(sentiment_scores_grouped) * 100,
    mean_score  = mean(sentiment_score),
    median_score= median(sentiment_score),
    min_score   = min(sentiment_score),
    max_score   = max(sentiment_score),
    .groups = "drop"
  )

comparison_table
comparison_pos_neg <- sentiment_scores_grouped %>%
  filter(sentiment_group != "Neutral") %>%
  group_by(sentiment_group) %>%
  summarise(
    n           = n(),
    perc        = n() / nrow(sentiment_scores_grouped) * 100,
    mean_score  = mean(sentiment_score),
    median_score= median(sentiment_score),
    min_score   = min(sentiment_score),
    max_score   = max(sentiment_score),
    .groups = "drop"
  )

comparison_pos_neg

write.csv(comparison_table, "sentiment_group_summary.csv", row.names = FALSE)

write.csv(comparison_table,
          "C:/Users/User/Desktop/sentiment_group_summary.csv",
          row.names = FALSE)

library(dplyr)

set.seed(123)
sample_comments <- comments_clean %>%
  sample_n(20)   # берём 20 случайных комментариев

head(sample_comments$comment_text, 3)

install.packages("httr")
install.packages("jsonlite")

library(httr)
library(jsonlite)
library(dplyr)


classify_comment_llm <- function(text) {
  
  # короткий, чёткий промпт
  prompt_text <- paste0(
    "You are classifying public comments on a proposed CMS regulation (CMS-2023-0144).\n",
    "Read the following comment and classify the stance toward the regulation.\n",
    "Return exactly one of the following labels:\n",
    "- Support\n",
    "- Oppose\n",
    "- Neutral\n\n",
    "Comment:\n\"",
    text,
    "\"\n\n",
    "Answer with only one word: Support, Oppose, or Neutral."
  )
  
  # ЗАМЕНИ На URL и модель своего провайдера LLM (пример для OpenAI)
  res <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(
      Authorization = paste("Bearer", "YOUR_API_KEY_HERE"),
      "Content-Type" = "application/json"
    ),
    body = toJSON(list(
      model = "gpt-4.1-mini",  # или другую подходящую модель
      messages = list(
        list(role = "system", content = "You are a helpful assistant for text classification."),
        list(role = "user",   content = prompt_text)
      ),
      temperature = 0
    ), auto_unbox = TRUE)
  )
  
  # обработка результата
  if (status_code(res) != 200) {
    warning("API request failed with status: ", status_code(res))
    return(NA_character_)
  }
  
  res_content <- content(res, as = "text", encoding = "UTF-8")
  res_json    <- fromJSON(res_content)
  
  raw_answer <- res_json$choices[[1]]$message$content
  
  # почистим пробелы, переводы строк и обрежем до одного слова
  clean_answer <- raw_answer %>%
    stringr::str_trim() %>%
    stringr::str_to_title()
  
  # на всякий случай нормализуем
  if (clean_answer %in% c("Support", "Oppose", "Neutral")) {
    return(clean_answer)
  } else {
    return(NA_character_)
  }
}
# добавим новую колонку llm_stance
sample_comments_llm <- sample_comments %>%
  mutate(
    llm_stance = sapply(comment_text, classify_comment_llm)
  )

sample_comments_llm

write.csv(sample_comments_llm, "sample_comments_llm_stance.csv", row.names = FALSE)

write.csv(
  sample_comments_llm,
  "C:/Users/User/Desktop/sample_comments_llm_stance.csv",
  row.names = FALSE
)

