library(tidyverse)
set.seed(1234)
MakeDf <- function(m, s, lo, hi){
  a <- seq(-3, 3, 0.1)
  b <- dnorm(a)
  ## transform
  a <- a*s
  a <- a + m
  mydf <- tibble(x = a, y = b)
  mydf <- mydf %>% 
    mutate(clr = seq(from = lo, to = hi, length.out = length(x)))
  mydf
}


mydf <- MakeDf(0, 1, -1, 0.1)

## I think that we want to model an interaction
MyInter <- function(x, multiple = 0.4) {
  -1 + x * (multiple + rnorm(1, mean = 0, sd = 0.1))
}
mydf <- mydf %>% 
  mutate(clr2 = MyInter(x))
mydf$clr %>% range
mydf$clr2 %>% range
a <- ggplot(mydf, aes(x = x, y = y, colour = clr2)) +
  geom_line() +
  scale_color_gradient2(midpoint = 0, low = "red", high = "blue") +
  theme_minimal()
a

# mydf1 <- mydf %>% 
#   mutate(x = x,
#          y = y * 0.2,
#          clr2 = MyInter(x))
# mydf2 <- mydf %>% 
#   mutate(x = x + 1,
#          y = y * 0.5,
#          clr2 = MyInter(x))

fivetrials <- pmap(list(
  c(0, 0.2, 0.5, 0.75, 6),
  c(0.2, 0.25, 0.4, 0.3, 0.5),
  c(0.8, 0.8, 0.8, 0.8, 0.3)), function(xs, ys, ss) {
                     mydf %>% 
                       mutate(x = x + xs,
                              y = y * ys,
                              x = x*ss,
                              clr2 = MyInter(x, 0.5))
                   })

names(fivetrials) <- seq_along(fivetrials)
fivetrials <- bind_rows(fivetrials, .id = "dataset") %>% 
  mutate(dataset = as.integer(dataset))
obs <- mydf %>% 
  mutate(x = x + 2,
         y = y * 2,
         clr2 = MyInter(x, multiple = 0.6),
         dataset = max(fivetrials$dataset) + 1)

tot <- bind_rows(fivetrials,
                 obs) 

## converting between scales for plots
there <- function(x) x*10 + 40
back <- function(x) (x-40) / 10
tot <- tot %>% 
  filter(there(x) >= 25)
a <- ggplot(tot, aes(x = x, y = y, colour = clr2, group = dataset)) +
  geom_line(linewidth = 1.5) +
  scale_color_gradient2("",
                        midpoint = 0, low = "blue", high = "orange",
                        # guide = NULL,
                        breaks = c(-1.5, 0, 1.5),   # Define where labels go
                        labels = c("Beneficial", "Null", "Harmful")) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  # facet_wrap(~type, ncol = 1) +
  scale_y_continuous("Participants/patients", labels = function(x) round(10000*x)) +
  scale_x_continuous("Age (years)", labels = there) 
a
png("quickplot.png")
a
dev.off()
