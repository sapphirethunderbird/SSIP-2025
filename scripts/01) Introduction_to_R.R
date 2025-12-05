#### ---- SSIP YU 01) R: A gentle introduction ----


# ---- Gonna do some math ----
1 + 1
65 - 5
32*3
10/2
(2*4)/3(1+4*5) #This will throw an error
(2*4)/3*(1+4*5)

# Boolean time
10000000 > 10000001

234 >= 233
234 < 232
12 == 12
"Osugi" == "MuraiP"
"Sugino" != "Sugii"

# SEMICOLONS!!!!!!!!
x <- 20; y <- 15; z <- x+y; w <- z-45 
(w+x)*z/10+(x*y)

getwd()
x <- c(1, 2, 3, 4, 5, 6)
mean(x)
?mean()

# To dataframe or to matrix
x <- c(1, 2, 3, 4, 5, 6); y <- c(2, 4, 6, 8, 10, 12)
z <- x*y
z
matrix(z, nrow = 2, ncol = 3)
matrix(z, nrow = 2, ncol = 3, byrow = T)

gender <- c("m", "f", "m", "f", "f")
name <-  c("Sugii", "Nagai", "Sugino", "Yamamoto", "Ueda")
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

# Indexing
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

# Plumbing
mylist$data$gender |> table()
rnorm(5000, mean = 0, sd = 1) |>
  matrix(ncol = 2) |>
  hist()
rnorm_vec <- rnorm(5000, mean = 0, sd = 1)  
rnorm_mat <- matrix(rnorm_vec, ncol = 2)
hist(rnorm_mat)

# Statistics lol
kokugo <- c(70, 53, 64, 82, 48) 
sugaku <- c(51, 49, 86, 88, 71)
heikin <- (kokugo + sugaku) / 2
heikin
mean(heikin)

# Height, Weight, Age
hayashi <- c(170, 60, 35)
sugino <- c(173, 70, 23)
ehara <- c(175, 85, 42)

hayashi[1:3]
data_hwa1 <- cbind(hayashi, sugino, ehara)
data_hwa2 <- t(data_hwa1) #Swapping rows and columns
colnames(data_hwa2) <-  c("height", "weight", "age")
data_hwa3 <- as.data.frame(data_hwa2)
data_hwa3

str(data_hwa2)
str(data_hwa3)

data_hwa2[1, 3]
data_hwa3[1, 3]
data_hwa3$height
mean(data_hwa3$height)
mean(data_hwa3$weight)

# Objects
objects()
rm(list = ls(all=TRUE)) #Destruction
cat("\014")
if(dev.cur()>1) dev.off()

# Snippets
vec <- NULL
for (i in 1:5) {
  temp <- i
  vec <- c(vec, temp)
}
vec

# ---- Let's go to the library ----
install.packages("psych", dependencies = T) #Gimme all you got
library(psych)
require(psych)

rm(list = ls(all=TRUE)) 
if(dev.cur() > 1) dev.off()
cat("\014") 

outinfo1 <- "r_practice"
outinfo2 <- Sys.Date()
set.seed(123456789)

d01 <- read.csv("faostat_practice_01.csv", header = T, row.names = 1)
View(d01)
head(d01, n = 10)
tail(d01, n = 10)
str(d01)

d01_subdata1 <- d01[1:177, 1:4]
d01_subdata2 <- subset(d01, select = c("item", "year", "yield_kg_ha"))
d01_subdata3 <- subset(d01_subdata2, complete.cases(d01_subdata2))
d01_subdata4 <- subset(d01, d01$item == "Apples")
d01_subdata5 <- subset(d01_subdata4, d01_subdata4$country == "Japan")

d01_jp_app_sum <- summary(d01_subdata5)
d01_jp_app_des <- describe(d01_subdata5) #This requires the psych package
d01_jp_app_sum
d01_jp_app_des

hist(d01$yield_kg_ha)
hist(d01_subdata4$yield_kg_ha)
hist(d01_subdata5$yield_kg_ha)

hist_data1 <- hist(d01_subdata4$yield_kg_ha); hist_data2 <- hist(d01_subdata4$yield_kg_ha, breaks = seq(1000, 30000, 1000))
hist_data1$breaks; hist_data2$breaks

n <- length(hist_data1$counts)
counts <- hist_data1$counts; breaks <- hist_data1$breaks
class_names <- NULL
for (j in 1:n) {
  class_names[i] <- paste(breaks[i], "～", breaks[i+1])
}
frequency_table <- data.frame(class = class_names, frequency = counts)

# ---- Visual Time ----
plot(d01_subdata5$yield_kg_ha)
plot(
  x = d01_subdata5$year,
  y = d01_subdata5$yield_kg_ha,
  pch = 19, cex = 0.8, frame = FALSE, col = "red",
  xlab = "year", ylab = "yield_kg_ha"
)
boxplot(yield_kg_ha~item, data = d01)

# ---- Turning in our homework ----
write.csv(d01_subdata5, "d01_jp_app_subdata.csv")
write.csv(d01_jp_app_sum, "d01_jp_app_sum.csv")
write.csv(d01_jp_app_des, "d01_jp_app_des.csv")

