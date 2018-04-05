## EST171 - Métodos Computacionais para Análise de Risco
## Código da aula 06 - Mais Gráficos no R

require(ggplot2)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

# Exemplo Starwars
require(dplyr)

ggplot(data = starwars, aes(x = height, y = mass, color = gender)) +
  geom_point()

ggplot(data = starwars, aes(x = height, y = mass)) +
  facet_grid(. ~ gender) +
  geom_point()

ggplot(data = starwars, aes(x = height, y = mass)) +
  geom_smooth()

ggplot(starwars, aes(x = height)) +
  geom_histogram(binwidth = 10)

ggplot(starwars, aes(x = gender)) +
  geom_bar()

