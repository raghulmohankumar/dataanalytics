---
title: "Histogram"
output: html_document
---


```{r warning=FALSE,echo=FALSE,message=FALSE}

library(ggplot2)
library(tidyverse)
hst <- read_csv("histo.csv")
hist(hst$Man_hours,main = "Quality Control Histogram",
     xlab = "Activities",
     ylab = "Man Hours",
     las = 1,
     col = c("skyblue", "chocolate2"),
     labels = TRUE,
     ylim = c(0, 6))
```

