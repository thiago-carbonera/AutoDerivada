# Projeto AutoDerivada

Este programa em Haskell lê uma expressão matemática em notação prefixa, deriva simbolicamente em relação a x, simplifica o resultado e imprime a expressão final em notação infixa.

---

## Como Compilar e Executar

### Como Compilar
No terminal, dentro da pasta do projeto:
```bash
ghc Derivada.hs -o derivada
```
Isso gera um executável chamado derivada.

### Como Executar
Após compilar, execute:
```bash
./derivada
```

### Como Usar
O programa pedirá uma expressão prefixa.

Exemplo:
```bash
(+ (* 3 (^ x 2)) (* 2 x))
```

Saída:
```bash
Expressão original: ((3 * (x ^ 2)) + (2 * x))
Derivada: ((3 * ((2 * (x ^ 1)) * 1)) + ((2 * 1) + (x * 0)))
Derivada simplificada: ((6 * x) + 2)
```

Digite sair para encerrar o programa.