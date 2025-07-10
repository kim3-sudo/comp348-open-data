## Teaching Software Engineering with GenAI LLM Tools ##
###################### Open Data #######################
# S Kim & J Skon
# Kenyon College

# Load data - relative path to the R project
data = read.csv("data/data.csv")

# Likert Plot Generation
## Load necessary packages for plot generation
library(likert)

## Subset data for Likert plot generation
l1 <- data[, substr(names(data), 1, 2) == "L1"]
l2 <- data[, substr(names(data), 1, 2) == "L2"]
l3 <- data[, substr(names(data), 1, 2) == "L3"]
l4 <- data[, substr(names(data), 1, 2) == "L4"]
l5 <- data[, substr(names(data), 1, 2) == "L5"]

## Load the factor names in and provide the order
factors = c('Unable To Answer', 'Strongly Disagree', 'Disagree', 'Neither Agree Nor Disagree', 'Agree', 'Strongly Agree')

## Generate the Likert plot for L1 questions
l1[1:6] <- lapply(l1[1:6], factor, levels = factors)
p1 <- likert(l1)
plot(p1)

## Generate the Likert plot for L2 questions
l2[1:5] <- lapply(l2[1:5], factor, levels = factors)
p2 <- likert(l2)
plot(p2)

## Generate the Likert plot for L3 questions
l3[1:8] <- lapply(l3[1:8], factor, levels = factors)
p3 <- likert(l3)
plot(p3)

## Generate the Likert plot for L4 questions
l4[1:4] <- lapply(l4[1:4], factor, levels = factors)
p4 <- likert(l4)
plot(p4)

## Generate the Likert plot for L5 questions
l5[1:7] <- lapply(l5[1:7], factor, levels = factors)
p5 <- likert(l5)
plot(p5)

## Generate a Likert plot with some cherry-picked variables
cherrypickedvars <- c('L2.1', 'L3.1', 'L3.2', 'L3.7', 'L3.8', 'L4.2')
cherry <- data[cherrypickedvars]
cherry[1:6] <- lapply(cherry[1:6], factor, levels = factors, ordered = FALSE)
pcherry <- likert(cherry)
plot(pcherry)

# Empirical Analysis
## Load necessary packages for empirical analysis
library(mosaic)
library(dplyr)

## Filter data based on skill rankings
## Generate the averages for the self-evaluated skill level
data <- data %>% mutate(compositeskill = rowMeans(x = select(data, matches(match = "S[1-5]"))))
mean(data$compositeskill)

## Subset data for new Likert plot generation
l2s <- data[, substr(names(data), 1, 2) == "L2" | substr(names(data), 1, 6) == 'compos']
l3s <- data[, substr(names(data), 1, 2) == "L3" | substr(names(data), 1, 6) == 'compos']


l2s[1:5] <- lapply(l2s[1:5], factor, levels = factors, ordered = FALSE)
pp2s <- likert(l2s, grouping = l2s[,6])
plot(likert)
