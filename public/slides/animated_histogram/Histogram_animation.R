## Code to generate animated histogram
## Source: https://gist.github.com/thomasp85/88d6e7883883315314f341d2207122a1

library(tweenr) # Available on CRAN
library(ggforce) # Install from thomasp85/ggforce
library(gganimate) # Install from dgrtwo/gganimate

## generate data points
set.seed(2)
x <- sample(9,20, prob=c(1,2,3,4,5,4,3,2,1), replace=T) # x is sample from 1:9 with size 20
df <- data.frame(x = x, y = 15)                         # y starts with height 15 for all

## create lists with incremental countings to include one point at a time
dfs <- list(df)
for(i in seq_len(nrow(df))) {
  dftemp <- tail(dfs, 1)
  dftemp[[1]]$y[i] <- sum(dftemp[[1]]$x[seq_len(i)] == dftemp[[1]]$x[i])
  dfs <- append(dfs, dftemp)
}
dfs <- append(dfs, dfs[rep(length(dfs), 3)])

## create data in frames for animation
dft <- tween_states(dfs, 10, 1, 'cubic-in', 200)
dft$y <- dft$y - 0.5          # adjust the height for every point
dft <- dft[dft$y != 14.5, ]   # remove points that have height=14.5=15-0.5 (original value)
                              # - they shouldn't be plotted yet!
dft$type <- 'Animate'

## conventional histogram
dfh <- data.frame(x=x, type = 'Histogram')

## create ggplot, add points with animation (by frame), add histogram 
p <- ggplot(dft) + 
  geom_circle(aes(x0=x, y0=y, r=0.5, frame = .frame), n=20, fill = 'steelblue') + 
  geom_histogram(aes(x=x), data = dfh, fill = 'forestgreen', color = 'black', binwidth = 1) + 
  coord_fixed(ylim = c(0, 13.5)) + 
  theme_bw() + 
  facet_grid(.~type)

## save animated plot as gif
animation::ani.options(interval = 1/20)
gganimate(p, 'hist_ex.gif', title_frame = FALSE)


