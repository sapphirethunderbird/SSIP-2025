#### ---- SSIP YU 02) Chocolate ----


# ---- Quick Cleanup ----
rm(list = ls(all=TRUE)) 
if(dev.cur() > 1) dev.off()
cat("\014") 

# ---- 1: Math and English Scores ----
sugaku <- c(60, 75, 90, 85, 70)
eigo <- c(65, 70, 80, 90, 60)
df01 <- data.frame(sugaku, eigo)
df01

heikin <- (sugaku + eigo) / 2
heikin
df02 <- cbind(df01, heikin)
df02

hist(df02$heikin)

x_bar = NULL
for ( i in sugaku) {
  x <- sugaku - mean(sugaku)
  x_bar <- c(x)
}
x_bar

y_bar = NULL
for ( j in eigo) {
  y <- eigo - mean(eigo)
  y_bar <- c(y)
}
y_bar

for ( k in x_bar) {
  z <- x_bar * y_bar
  z_vec <- c(z)
}
z

rxy <- sum(z)
rxy

x_bunsan <- NULL
for ( l in x_bar) {
  bunsan <- x_bar^2
  x_bunsan <- c(bunsan)
}
x_bunsan
rx <- sum(x_bunsan)

y_bunsan <- NULL
for ( m in y_bar) {
  bunsan <- y_bar^2
  y_bunsan <- c(bunsan)
}
y_bunsan
ry <- sum(y_bunsan)

r <- rxy / sqrt((rx * ry))
r

plot(x = df02$sugaku,
     y = df02$eigo,
     pch = 19, cex = 0.8, frame = FALSE, col = "red",
     xlab = "Math", ylab = "English")

# ---- 2: Student Interviews ----
teachers <- c("Sakaguchi", "Yamamoto", "Sugii", "Adachi", "Arimoto")
gender <- c("m", "f", "m", "m", "m")
building <- c("fgss", "fgss", "fgss", "education", "education")
floor <- c(1, 2, 3, 1, 3)
fgss_df <- data.frame(teachers, gender, building, floor)
# What interview?

# ---- 3: Welcome to the Farm ----
faostat02 <- read.csv("faostat_practice_02.csv")
faostat2 <- subset(faostat02, complete.cases(faostat02))
faostat2

faostat2_jp <- subset(faostat2, faostat2$Area=="Japan")
faostat2_jp
items <- unique(faostat2_jp$Item)
items
length(items)

faostat2_no <- subset(faostat2, faostat2$Area=="Norway")
faostat2_no_ch <- subset(faostat2_no, faostat2_no$Item=="Cherries")
write.csv(faostat2_no_ch, "faostat2_no_ch.csv")
library(psych)
describe(faostat2_no_ch)

# ---- 4: Arts and Crafts ----
faostat02 <- read.csv("faostat_practice_02.csv")
faostat2 <- subset(faostat02, complete.cases(faostat02))
faostat2
summary(faostat2)

# Histogram 
faostat2_ap <- subset(faostat2, faostat2$Item=="Apples")
faostat2_ap
hist(faostat2_ap$Value_Yield,
     col = "red",
     main = "Apple Yield (Faostat)",
     xlab = "Apple Value Yield",
     ylab = "Frequency",)

# Boxplot
# Normal Boxplot
boxplot(Value_Yield~Item, data=faostat2,
        col = "red")

# Nice Boxplot
library(ggplot2)
ggplot(faostat2,aes(x=Item,y=Value_Yield,fill=Item)) + geom_boxplot()

# Scatterplot and Connected Scatterplot
ggplot(faostat2_ap, aes(x=Year, y=Value_Yield, color=Area)) + geom_point()

library(tidyverse)
faostat2_ap %>%
  ggplot( aes(x=Year, y=Value_Yield, group=Area, color=Area)) +
  geom_line() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  ggtitle("A spaghetti chart of apple yield per country")

faostat2_ap %>%
  ggplot( aes(x=Year, y=Value_Yield, group=Area, fill=Area)) +
  geom_area() +
  ggtitle("Yield of apples from 1961~2023") +
  facet_wrap(~Area)

# ---- 5: Test Results ----
score <- c(82, 70, NA, 89, 100, 3, 85, 78, NA, 91, 500)
score_valid = NULL
for (i in score){
  tensu <- ifelse(score > 100, NA, score)
  score_valid <- c(tensu)
}
score_valid <- score_valid[!is.na(score_valid)]
score_valid

ave <- mean(score_valid)
stan <- sd(score_valid)
z_scores <- (score_valid - ave) / stan
z_scores
score_clean <- score_valid[abs(z_scores) <= 3]
score_clean

ave_clean <- mean(score_clean)
med_clean <- median(score_clean)
sd_clean <- sd(score_clean)
clean_stats <- c(ave_clean, med_clean, sd_clean)
clean_stats_names <- c("average", "median", "standard distribution")
clean_stats_df <- data.frame(clean_stats_names, clean_stats)
clean_stats_df

hist(score_clean,
     col = "green",
     main = "Exam Results for Highest Scoring Students",
     xlab = "Score",
     ylab = "Students",)

boxplot(score_clean,
        col = "green",
        main = "Exam Results for Highest Scoring Students")

# ---- 6: High Score ----
data_names <- c("A", "B", "C", "D", "E", "F")
data <- c(75, 89, 63, "missing", 92, 81)
data_df <- data.frame(data_names, data)
data_df$data <- as.numeric(data_df$data)
clean_df <- data_df[!is.na(data_df$data), ]

ave_score <- mean(clean_df$data)
high_scores <- subset(clean_df, data > ave_score)
high_scores

library(ggplot2)
ggplot(high_scores, aes(x=data_names, y=data, color=data_names)) + 
  geom_point(size = 3.5) + 
  geom_hline(yintercept = mean(high_scores$data), color = "red")

# ---- 7: Exam Season ----
student1 <- read.csv("student_sample01.csv")
student2 <- read.csv("student_sample02.csv")

students <- merge(student1, student2, by = "id")
students

student_ave <- (students$score_math + students$score_science) / 2
student_ave
df_avg <- cbind(students, student_ave)

library(ggplot2)
ggplot(df_avg, aes(x=major, y=student_ave)) + 
  geom_bar(stat = "identity") +
  coord_flip()

df_high <- df_avg[which(student_ave >= 80),] # Nobody has an average of 80 or higher

df_high <- df_avg[which(student_ave >= 50),] # 50 gives us something to work with though

# ---- 8: Student DB ----
raw <- c(
  "ID:001|Name=Yamada|Dept=Economics|Score=82",
  "ID:002|Name=Suzuki|Dept=Sociology|Score=missing",
  "ID:003|Name=Kato|Dept=Law|Score=75",
  "ID:004|Name=Tanaka|Dept=Economics|Score=91"
)
library(tidyverse)
split_data <- tibble(raw_text = raw) %>%
  separate(raw_text, into = c("ID", "Name", "Dept", "Score"), sep = "\\|") %>%
  mutate(across(everything(), ~str_remove(.x, "^.*[:=]"))) %>%
  mutate(Score = na_if(Score, "missing"),
         Score = as.numeric(Score))

split_data
split_data$Dept <- as.factor(split_data$Dept)
split_data
eco <- subset(split_data, split_data$Dept=="Economics")
soc <- subset(split_data, split_data$Dept=="Sociology")
law <- subset(split_data, split_data$Dept=="Law")
eco_med <- median(eco$Score)
eco_med
soc_med <- median(soc$Score)
soc_med
law_med <- median(law$Score)
law_med

library(ggplot2)
ggplot(split_data,aes(x=Dept,y=Score,fill=Dept)) + geom_boxplot()


# ---- 9: Welcome to the Farm Part 2 ----
df_crop <- read.csv("faostat_practice03.csv")

df_crop <- subset(df_crop, complete.cases(df_crop))
country <- c("Japan", "USA", "France")
crop <- c("Wheat", "Rice")

df_crop_jp <- subset(df_crop, df_crop$Country=="Japan")
df_crop_us <- subset(df_crop, df_crop$Country=="USA")
df_crop_fr <- subset(df_crop, df_crop$Country=="France")

jp_wheat <- subset(df_crop_jp, df_crop_jp$Crop=="Wheat")
jp_rice <- subset(df_crop_jp, df_crop_jp$Crop=="Rice")

us_wheat <- subset(df_crop_us, df_crop_us$Crop=="Wheat")
us_rice <- subset(df_crop_us, df_crop_us$Crop=="Rice")

fr_wheat <- subset(df_crop_fr, df_crop_fr$Crop=="Wheat")
fr_rice <- subset(df_crop_fr, df_crop_fr$Crop=="Rice")

mean(jp_wheat$Production)
mean(jp_rice$Production)

mean(us_wheat$Production)
mean(us_rice$Production)

mean(fr_wheat$Production)
mean(fr_rice$Production)

wheat <- c(mean(jp_wheat$Production), 
           mean(us_wheat$Production), 
           mean(fr_wheat$Production))
rice <- c(mean(jp_rice$Production),
          mean(us_rice$Production),
          mean(fr_rice$Production))

df_summary <- data.frame(country, wheat, rice)

library(tidyverse)
library(ggplot2)
df_crop_jp %>%
  ggplot( aes(x=Year, y=Production, group=Crop, fill=Crop)) +
  geom_area() +
  ggtitle("Production of wheat and rice in Japan") +
  facet_wrap(~Crop)

df_crop_us %>%
  ggplot( aes(x=Year, y=Production, group=Crop, fill=Crop)) +
  geom_area() +
  ggtitle("Production of wheat and rice in USA") +
  facet_wrap(~Crop)

df_crop_fr %>%
  ggplot( aes(x=Year, y=Production, group=Crop, fill=Crop)) +
  geom_area() +
  ggtitle("Production of wheat and rice in France") +
  facet_wrap(~Crop)

library(dplyr)
df_jp <- df_crop_jp %>%
  group_by(Crop) %>%                      
  mutate(                                 
    pct_change = (Production - lag(Production)) / lag(Production) * 100
  )
df_jp
library(ggplot2)
df_jp %>%
  ggplot( aes(x=Year, y=pct_change, group=Crop, fill=Crop)) +
  geom_area() +
  ggtitle("Production of wheat and rice in Japan") +
  facet_wrap(~Crop)

# ---- 10: Chocolate ----
# For those on Mac
par(family = "HiraKakuProN-W3")

set.seed(2738291)
required_packages <- c(
  "readxl",       # for reading excel files
  
  "tidyverse",    # dplyr・ggplot2 in the same package
  "RColorBrewer", # nice color palettes 
  "GGally",       # for making life easier
  
  "psych",        # the GOAT of psychological statistics
  "skimr",        # TLDR plz
  "rcompanion",   # more statistics
  "effectsize",   # even more statistics
  
  # データ可視化の拡張
  "ggplot2",      # for nice graphs
  "corrplot",     # for heatmaps
  "corrtable",    # for making nice tables with heatmap data
  "ggExtra",      # for adding histograms to scatterplots
  "gridExtra",    # put multiple ggplot graphs in one pane
  "ggbiplot",     # PCA biplot with ggplot
  "ggfortify",    # making PCA and cluster graphs easilt
  
  "FactoMineR",   # for PCA and MCA
  "factoextra",   # for visualizing FactoMineR results
  
  "cluster",      # for clustering
  "NbClust"       # for deterimining number of clusters
)

## --- 10-1: Package Installation ----------
# If you are dealing with a proxy:
##proxy_set <- "<YOUR PROXY ADDRESS AND PORT>"
##Sys.setenv(
##  http_proxy  = proxy_set,
##  ftp_proxy   = proxy_set,
##  https_proxy = proxy_set
##)

install_and_load <- function(pkgs) {
  for (pkg in pkgs) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    } else {
      library(pkg, character.only = TRUE)
    }
  }
}

install_and_load(required_packages)


# ---- 10-2: Reading Data ----
data_path  <- "2025_環境と心理_ChocolateData.xlsx"
sheet_name <- "Original"

choco_raw <- readxl::read_excel(
  path      = data_path,
  sheet     = sheet_name,
  col_types = "text"  # いったん全部文字列として読み込む（後で型変換）
)

str(choco_raw)
head(choco_raw)

colnames(choco_raw)

# ---- 10-3: Sorting Data ----
meta_cols <- c("ID", "Date", "Timestamp", "ChocoType") # setting the colnames as our metadata
image_cols <- setdiff(colnames(choco_raw), meta_cols) 

# - ID needs to be an integer
# - ChocoType needs to be a factor
# - The experience needs to be a factor（Make Levels: Doesn't Fit=0, Fits=1）

choco <- choco_raw |>
  dplyr::mutate(
    ID        = as.integer(ID),
    ChocoType = factor(ChocoType)
  ) |>
  dplyr::mutate_at(
    .vars = dplyr::all_of(image_cols),
    .funs = ~ factor(
      trimws(as.character(.)),
      levels = c("当てはまらない", "当てはまる")
    )
  )
str(choco)

# ---- 10-4: Making binary (0 or 1) ----
choco_bin <- choco |>
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(image_cols),
      ~ ifelse(trimws(as.character(.x)) == "当てはまる", 1L, 0L), #Using 1L, 0L will treat them as integers
      .names = "{.col}_bin"
    )
  )
colnames(choco_bin)
str(choco_bin[, paste0(image_cols[1], "_bin"), drop = FALSE])

# ---- 10-5: Descriptive Statistics ----
choco_type_summary <- choco |>
  dplyr::count(ChocoType) |>
  dplyr::mutate(
    prop      = n / sum(n),
    ChocoType = forcats::fct_reorder(ChocoType, n)
  ) |>
  dplyr::arrange(desc(n))             

choco_type_summary

overall_prop_tidy |>
  ggplot2::ggplot(ggplot2::aes(x = Item, y = Prop)) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "各イメージ語の「当てはまる」割合（全体）",
    x     = "イメージ語",
    y     = "当てはまる割合"
  )


taste_summary_by_timestamp <- choco_bin |>
  dplyr::group_by(Timestamp) |>
  dplyr::summarise(
    dplyr::across(
      .cols = dplyr::all_of(image_bin_cols),
      .fns  = ~ mean(as.numeric(.x), na.rm = TRUE)
    ),
    n = dplyr::n(),          # そのチョコ種類のサンプル数
    .groups = "drop"
  ) |>
  # 2) ワイド → ロングに変換（Item: イメージ語名, Prop: 割合）
  tidyr::pivot_longer(
    cols      = dplyr::all_of(image_bin_cols),
    names_to  = "Item",
    values_to = "Prop"
  ) |>
  dplyr::mutate(
    Item = stringr::str_remove(Item, "_bin$")  # 列名から "_bin" を外す
  )

taste_summary_by_timestamp

taste_summary_by_date <- choco_bin |>
  dplyr::group_by(Date) |>
  dplyr::summarise(
    dplyr::across(
      .cols = dplyr::all_of(image_bin_cols),
      .fns  = ~ mean(as.numeric(.x), na.rm = TRUE)
    ),
    n = dplyr::n(),          # そのチョコ種類のサンプル数
    .groups = "drop"
  ) |>
  # 2) ワイド → ロングに変換（Item: イメージ語名, Prop: 割合）
  tidyr::pivot_longer(
    cols      = dplyr::all_of(image_bin_cols),
    names_to  = "Item",
    values_to = "Prop"
  ) |>
  dplyr::mutate(
    Item = stringr::str_remove(Item, "_bin$")  # 列名から "_bin" を外す
  )

taste_summary_by_date
