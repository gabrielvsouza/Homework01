library(tidyverse)
#teste

Code
library(tidyverse)
library(caret)
library(dslabs)
data(heights)
View(heights)
str(heights)

heights <- heights %>%
  mutate(group2 = height + 1)
heights

y <- heights$sex
x <- heights$height

set.seed(2007, sample.kind="Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]


y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE)


y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% 
  factor(levels = levels(test_set$sex))
str(y_hat)
mean(y_hat == test_set$sex)

heights %>% group_by(sex) %>% 
  summarize(mean(height), sd(height))

y_hat <- ifelse(x > 62, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
mean(y == y_hat)

-=-=-=-=--=-=-

plot(cutoff, accuracy)
lines(cutoff, accuracy)

#=-=-=-=-=-=

  foo <- function(x){
    rangedValues <- seq(range(x)[1],range(x)[2],by=1)
    sapply(rangedValues,function(i){
      y_hat <- ifelse(x>i,'Male','Female')
      mean(y_hat==train_set$height)
    })
  }
predictions <- apply(train_set[,-1],2,foo)
is.matrix(train_set[,-1])
sapply(predictions,max)	  
=-=-=-=-=-
  
  
  # Now, we can test this cut off on our test
  # set to make sure accuracy is not overly optimistic.
  y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)
# that is a bit lower than the accuracy observed on the training set,
# but it's still better than guessing
