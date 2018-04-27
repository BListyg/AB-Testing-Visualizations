library(dplyr)
library(ggplot2)
library(cowplot)

theme_set(theme_cowplot(font_size=12))

example <- data.frame(

expand.grid(c(100:900), rep(c(1:9) / 10)),

(1000- expand.grid(c(100:900))))

colnames(example) <- c("effect",'beta','no_effect')

example$type_II <- example$beta*example$effect

example$type_I <- 0.05*example$no_effect

example$true_negative <- (1-0.05)*example$no_effect

example$true_positive <- example$effect*(1-example$beta)

example$Power <- 1 - example$beta

example$Power <- as.factor(example$Power)

tp <- ggplot(example, aes(x=(example$effect / 1000), y=(example$true_positive/1000)), group=Power) + geom_line(aes(linetype=Power)) + xlab("Proportion of experiments where an effect was actually present.") + ylab("True Positive Rate")

tn <- ggplot(example, aes(x=(example$effect / 1000), y=(example$true_negative/1000)), group=Power) + geom_line(aes(linetype=Power)) + xlab("Proportion of experiments where an effect was actually present.") + ylab("True Negative Rate")

fpr <- ggplot(example, aes(x=(example$effect / 1000), y=(example$type_I / (example$type_I + example$true_positive)), group=Power)) +
  geom_line(aes(linetype=Power)) + xlab("Proportion of experiments where an effect was actually present.") + ylab("False Positive Rate")

fnr <- ggplot(example, aes(x=(example$effect / 1000), y=(example$type_II / (example$type_II + example$true_negative)), group=Power)) +
  geom_line(aes(linetype=Power)) + xlab("Proportion of experiments where an effect was actually present.") + ylab("False Negative Rate")

plot_grid(tp,
          tn, 
          fpr, 
          fnr,
          labels = c("True Positives",
                     'True Negatives',
                     'False Positives',
                     'False Negatives')
          )
