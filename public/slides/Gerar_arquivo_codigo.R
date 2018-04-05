# Para gerar um arquivo .R com os chunks de código de um arquivo .Rmd:
  
require(knitr)
purl("01-introducao.Rmd", documentation=0)

# Saída: 'filename.R' com apenas o código extraído do arquivo original

# Opções: documentation=1 inclui o texto no título dos chunks
# Para excluir um chunk do arquivo final, incluir a opção purl=FALSE no cabeçalho do chunk
