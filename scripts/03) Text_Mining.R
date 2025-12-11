#### ---- SSIP YU 03) Text Mining ----

# ---- Quick Cleanup ----
rm(list = ls(all=TRUE)) 
if(dev.cur() > 1) dev.off()
cat("\014") 

# ---- 1: Data Structures ----
x <- c(1, 2, 3, 4, 5, 6); y <- c(2, 4, 6, 8, 10, 12) 
z <- x*y
z
matrix(z, nrow = 2, ncol = 3) 
matrix(z, nrow = 2, ncol = 3, byrow = T) 

gender <- c("m", "f", "m", "f", "f")
name <- c("Sugii", "Nagai", "Sugino", "Yamamoto", "Ueda")
building <- c("fgss", "edu", "edu", "fgss", "edu")
floor <- c(3, 1, 2, 2, 1)
df <- data.frame(gender, name, building, floor)
df

df$gender

mylist <- list(
  colors = c("red", "blue", "green"),
  num = c(1, 3, 5, 6, 8, 8, 9, 10),
  data = df
)

mylist
mylist$data
mylist$data$name

building[2]

fruits <- c(banana = 230, grape = 560, strawberry = 450)
fruits

length(fruits) 
names(fruits) 
fruits[2]
fruits["banana"]
fruits[[3]]
fruits[3] 

mylist$data
mylist["data"] 
mylist$data$name[3]

# ---- 2: Pipes ----
mylist$data$gender |> table()

rnorm(5000, mean = 0, sd = 1) |>
  matrix(ncol = 2) |>
  hist()

rnorm_vec <- rnorm(5000, mean = 0, sd = 1)
rnorm_mat <- matrix(rnorm_vec, ncol = 2)
hist(rnorm_mat)

# ----3: Text Mining ----

## ---- Libraries ----
install.packages("devtools")
devtools::install_github("IshidaMotohiro/RMeCab")

library(RMeCab)
library(dplyr)
library(ggplot2)

## ---- Sample Sentence (but it's Japanese) ----
sample_sentence <- "今日はRでテキストマイニングの入門を行います。RとMeCabを使うと、日本語の文章も分析できます。"

morph_result <- RMeCabC(sample_sentence)
morph_result
unlist(morph_result)

## ---- Frequency Chart ----
### Create sample_text.txt and copy the contents of sample_sentence into it
freq_raw <- RMeCabFreq("sample_text.txt")
head(freq_raw)

### ---- Frequency Chart for nouns, verbs and adjectives ----
content_pos <- c("名詞", "動詞", "形容詞")

freq_content <- freq_raw |>
  filter(
    Freq >= 2,
    Info1 %in% content_pos
  ) |>
  arrange(desc(Freq), Term)

freq_content

head(freq_content, 20)

## ---- Visualizing the top 20 words ----
top_n <- 20

freq_top <- freq_content |>
  slice_max(order_by = Freq, n = top_n)

ggplot(
  freq_top, aes(x =reorder(Term, Freq), y = Freq)) +
  geom_col() + 
  coord_flip() + 
  labs(
    title = "Top 20 Most Frequently Seen Words in Sample Text",
    x     = "Words",
    y     = "Frequency"
  ) +
  theme_bw(base_family = "sans")
  
# ---- Freestyle ----
# Make a self-introduction and put it into my_text.txt
freq_raw_me <- RMeCabFreq("my_text.txt")

### ---- Frequency Chart for nouns, verbs and adjectives ----
content_pos <- c("名詞", "動詞", "形容詞")

freq_content_me <- freq_raw_me |>
  filter(
    Freq >= 2,
    Info1 %in% content_pos
  ) |>
  arrange(desc(Freq), Term)

freq_content_me

head(freq_content_me, 20)

## ---- Visualizing the top 20 words ----
top_n <- 20

freq_top_me <- freq_content_me |>
  slice_max(order_by = Freq, n = top_n)

ggplot(
  freq_top_me, aes(x =reorder(Term, Freq), y = Freq)) +
  geom_col() + 
  coord_flip() + 
  labs(
    title = "Top 20 Most Frequently Seen Words in My Text",
    x     = "Words",
    y     = "Frequency"
  ) +
  theme_bw(base_family = "sans")








