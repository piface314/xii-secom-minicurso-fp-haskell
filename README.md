# Minicurso Introdução à Programação Funcional com Haskell

Código do minicurso Introdução à Programação Funcional com Haskell, da XII SECOM - UFV CAF.

## Execução

Para executar, abra o terminal na pasta raiz do projeto, tendo o `ghcup` instalado com o `cabal`,
e execute o comando:

```
$ cabal run minicurso life data/life.txt
```

para jogar o Jogo da Vida de Conway. Você pode passar outros arquivos ao invés do `data/life.txt`.

Ou então, para o Jogo da Velha:

```
$ cabal run minicurso tic-tac-toe
```

Nesse jogo, basta clicar nos quadrados vazios para fazer um movimento. O jogo começa com o O.
Se quiser desfazer algum movimento, aperte Z.
