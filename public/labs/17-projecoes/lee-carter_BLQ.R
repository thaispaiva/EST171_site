estimate.leecarter <- function(nmx){
  
  # Usando mortalidade abaixo de 85 anos, em grupos quinquenais, estima o parametros 
# do Lee-Carter.  Entrada é uma matriz de taxas de mortaliudade, linhas são datas
# e as colunas são as idades
  
  log.nmx <- log(nmx)
  ax <- apply(log.nmx, 2, mean)
  swept.mx <- sweep(log.nmx, 2, ax) # calcula a diferen�a por ano e o ax para cada coluna 
  svd.mx <- svd(swept.mx)
  bx <- svd.mx$v[, 1]/sum(svd.mx$v[, 1])
  kt <- svd.mx$d[1] * svd.mx$u[, 1] * sum(svd.mx$v[, 1])
  result <- list(ax = ax, bx = bx, kt = kt)
  return(result)}

# Primeiro passo é ler a base de dados. Vamos usar em grupos quinquenais, mas
# podemos trabalhar com idades simples ou outras idades desejadas. No exemplo
# estamos trabalhando com dados quinquenais, mas a rotina pode ser ajustada
# facilmente para fazer de outras formas. 

sw.fem <- read.table ("SwedenF.txt", header=T)
matrix.mx.swfem <- matrix (0,53,17)
matrix.mx.swfem <- sw.fem
dimnames (matrix.mx.swfem) <- list(seq(1950, 2002), seq(0,80,by=5))

## Passo 2 - rodar o modelo de Lee-Carter original usando a funçao
## que foi lida anteriormente

model.swfem <- estimate.leecarter(matrix.mx.swfem[1:53,])

# preparar os dados de ax e bx
ax.bx.kt.model.swfem<- data.frame(model.swfem$ax, model.swfem$bx)

# preparar os dados de kt
kt <- model.swfem$kt

#Calcular a serie temporal de kt

kt.diff <- diff(kt)
model.kt <- summary(lm(kt.diff ~ 1  ))
kt.drift <- model.kt$coefficients[1,1]
sec <- model.kt$coefficients[1,2]
see <- model.kt$sigma


## Projetar os kts
x <- seq(0,95)
kt.stderr <- ( (x*see^2) + (x*sec)^2 )^.5
kt.forecast <- -9.3756565 + (x * kt.drift)
kt.lo.forecast <- kt.forecast + (1.96*kt.stderr)
kt.hi.forecast <- kt.forecast - (1.96*kt.stderr)

## Alguns gráficos
plot (seq(1950, 2002), kt)
plot(x, kt.forecast)

