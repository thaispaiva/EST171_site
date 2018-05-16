## Code to generate animated histogram
## Source: https://gist.github.com/thomasp85/88d6e7883883315314f341d2207122a1
## Edited by: Thais Paiva - may/2018

## Modified version for class example - Computational Methods for Actuarial Science
## slides: 16-mais_seguros
## simulation of present value of whole life insurance


library(tweenr)     # Available on CRAN
library(ggforce)    # Available on CRAN
library(gganimate)  # Install from dgrtwo/gganimate

## package and actuarial table for APV evaluations
library(lifecontingencies)
data("soa08Act")


## simulate present values for whole life insurance
set.seed(2018)
r = 100  # number of repetitions
VP.m = VP.var = numeric(r)
for(i in 1:length(VP.m)){
  ## generate sample of lifetime values
  K0 = rLife(10000, soa08Act, 0, type="Kx")
  ## present value of whole life insurance of $1000
  VP = 1000*(1/1.06)^(K0+1)
  ## store mean and variance
  VP.m[i] = mean(VP)
  VP.var[i] = var(VP)
}

## theoretical APV
mu = 1000*Axn(soa08Act, 0)

## categorize VP.m
aux = hist(VP.m)

## create data points
x.ord = rep(aux$mids, aux$counts)     # x has the counts of each break
x = x.ord[order(VP.m)]                # change order
y.top = max(table(x))+5               # maximum height
df <- data.frame(x = x, y = y.top)    # y starts with maximum height for all


## create lists with incremental countings to include one point at a time
dfs <- list(df)
for(i in seq_len(nrow(df))) {
  dftemp <- tail(dfs, 1)
  dftemp[[1]]$y[i] <- sum(dftemp[[1]]$x[seq_len(i)] == dftemp[[1]]$x[i])
  dfs <- append(dfs, dftemp)
}
dfs <- append(dfs, dfs[rep(length(dfs), 3)])

## create data in frames for animation
dft <- tween_states(dfs, tweenlength=10, statelength=2, ease='cubic-in', nframes=200)
dft$y <- dft$y - 0.5                # adjust the height for every point
dft <- dft[dft$y != y.top-0.5, ]    # remove points that have maximum height (max-0.5)
                                    # - they shouldn't be plotted yet!
dft$type <- 'Animate'

## conventional histogram
dfh <- data.frame(x=x, type = 'Histogram')

## create ggplot, add points with animation (by frame), add histogram 
p <- ggplot(dft) + 
  geom_circle(aes(x0=x, y0=y, r=0.5, frame = .frame), n=20, fill = 'steelblue1') + 
  geom_histogram(aes(x=x), data = dfh, fill = 'steelblue4', color = 'black', breaks=aux$breaks) + 
  geom_vline(xintercept=mu, col=2, size=1) +
  coord_fixed(xlim=c(40, 57.5), ylim = c(0, y.top-0.5), ratio=1) + 
  theme_bw() + 
  facet_grid(.~type, scales = "free", space = "free")

## save animated plot as gif
animation::ani.options(interval = 1/20, ani.width=700)
setwd("C:/Users/Thais/Dropbox/Teaching/Disciplinas2018_1/Metodos_Computacionais/EST171_site/static/slides/animated_histogram")
gganimate(p, 'hist_ex.gif', title_frame = FALSE)

# ## Plot just the last frame as png
# ult.f = dft[dft$.frame==310,]
# png(filename="figura.png", width=800, height=400)
# ggplot(ult.f) +
#   geom_circle(aes(x0=x, y0=y, r=0.5, frame = .frame), n=20, fill = 'steelblue1') +
#   geom_histogram(aes(x=x), data = dfh, fill = 'steelblue4', color = 'black', breaks=aux$breaks) +
#   geom_vline(xintercept=mu, col=2, size=1) +
#   coord_fixed(xlim=c(40, 57.5), ylim = c(0, y.top-0.5), ratio = 1) +
#   theme_bw() +
#   facet_grid(.~type,scales = "free", space = "free")
# dev.off()
