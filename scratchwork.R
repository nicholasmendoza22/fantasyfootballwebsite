library(tidyverse)

ggplot(qb, aes_string(x = "averagefantasypointspergame", y = "input$qb_radio"a)) +
  geom_point() +
  xlab(input$qb_radio) +
  ylab("Fantasy Points per Game") +
  ggtitle("Quarterback Stats")