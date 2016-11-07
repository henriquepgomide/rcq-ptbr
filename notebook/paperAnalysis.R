---
title: "Psychometric Properties of the Brazilian Portuguese Version of the Readiness to Change Questionnaire – RCQ-BR"
output: html_notebook
---

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code. 

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Ctrl+Shift+Enter*. 

# Preamble
1. Set seed
2. Load packages
3. Load data

```{r echo = FALSE}
set.seed(12345)

library(psych)       # Exploratory Factor Analysis and Reliability
library(lavaan)      # Confirmatory Factor Analysis
library(car)         # Recode function
library(lattice)     # Trellis graphics
library(tigerstats)  # Crosstables with percentages
rcq  <- read.csv("rcq_df.csv")
```

