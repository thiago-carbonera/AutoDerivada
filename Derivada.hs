------------------------ Definição do tipo de dados ------------------------
-- Tipo de Dado Algébrico para representar expressões matemáticas
data Expressao = 
      Const Int
    | Var String
    | Soma Expressao Expressao
    | Produto Expressao Expressao
    | Potencia Expressao Int
    deriving (Show, Eq)


------------------------ Função de pré-processamento ------------------------
-- Adicionando espaços ao redor de parênteses para facilitar a tokenização
adicionaEspacos :: Char -> [Char]
adicionaEspacos c = 
    if c `elem` "()"
        then " " ++ [c] ++ " "
        else [c]


------------------------ Função de tokenização ------------------------
-- Convertendo string em lista de símbolos/tokens
tokenize :: String -> [String]
tokenize s = words (concatMap adicionaEspacos s)


------------------------ Função de parsing ------------------------
-- Construindo a árvore sintática abstrata
parse :: [[Char]] -> (Expressao, [[Char]])

parse ("(":tokens) =
    let (op:tokens1) = tokens
    in case op of
        "+" ->
            let (expr1, tokens2) = parse tokens1
                (expr2, tokens3) = parse tokens2
            in (Soma expr1 expr2, tail tokens3)

        "*" ->
            let (expr1, tokens2) = parse tokens1
                (expr2, tokens3) = parse tokens2
            in (Produto expr1 expr2, tail tokens3)

        "^" ->
            let (expr1, tokens2) = parse tokens1
                (Const n, tokens3) = parse tokens2
            in (Potencia expr1 n, tail tokens3)

        _   -> error "Operador desconhecido"

parse (token:resto) =
    case reads token :: [(Int, String)] of
        [(val, "")] -> (Const val, resto)
        _           -> (Var token, resto)



-------------------------------------------------------------
-- FUNÇÃO 1: DERIVAR
-------------------------------------------------------------
-- Regra geral: derivar expr "x"
-- VOCÊ DEVE IMPLEMENTAR AS REGRAS:
-- d/dx(c) = 0
-- d/dx(x) = 1
-- d/dx(u+v) = u'+v'
-- d/dx(u*v) = u*v' + v*u'
-- d/dx(u^n) = n*u^(n-1)*u'
-------------------------------------------------------------
derivar :: Expressao -> String -> Expressao

derivar (Const _) var = Const 0 -- derivada de constante é zero

derivar (Var x) var 
    | x == var  = Const 1   -- derivada de x em relação a x
    | otherwise = Const 0   -- outra variável vira zero

derivar (Soma u v) var =
    Soma (derivar u var) (derivar v var) -- u' + v'

derivar (Produto u v) var = 
    Soma
        (Produto u (derivar v var))   -- u * v'
        (Produto v (derivar u var))   -- v * u'

derivar (Potencia u n) var = 
    Produto
        (Produto (Const n) (Potencia u (n - 1)))  -- n * u^(n-1)
        (derivar u var)                            -- * u'


-------------------------------------------------------------
-- FUNÇÃO 2: SIMPLIFICAR
-------------------------------------------------------------
-- Regras básicas que você deve implementar:
-- e + 0 = e
-- 0 + e = e
-- c1 + c2 = c3
-- e * 1 = e
-- 1 * e = e
-- e * 0 = 0
-- 0 * e = 0
-- c1 * c2 = c3
-- e^1 = e
-- e^0 = 1
-------------------------------------------------------------
simplificar :: Expressao -> Expressao
simplificar (Soma e1 e2) =
    let s1 = simplificar e1
        s2 = simplificar e2
    in case (s1, s2) of
        (Const 0, e) -> e
        (e, Const 0) -> e

        (Const a, Const b) -> Const (a + b)

        _ -> Soma s1 s2


simplificar (Produto e1 e2) =
    let s1 = simplificar e1
        s2 = simplificar e2
    in case (s1, s2) of
        (Const 0, _) -> Const 0
        (_, Const 0) -> Const 0

        (Const 1, e) -> e
        (e, Const 1) -> e

        -- caso constante * constante
        (Const a, Const b) -> Const (a * b)

        -- caso Const * (Const * e)
        (Const a, Produto (Const b) e) ->
            Produto (Const (a * b)) e

        -- caso (Const * e) * Const
        (Produto (Const a) e, Const b) ->
            Produto (Const (a * b)) e

        -- caso e1 = Const a, e2 = Produto e (Const b)
        (Const a, Produto e (Const b)) ->
            Produto (Const (a * b)) e

        _ -> Produto s1 s2

simplificar (Potencia e n) =
    let s = simplificar e
    in case (s, n) of
        (_, 0) -> Const 1
        (e', 1) -> e'
        _ -> Potencia s n

simplificar e = e   -- caso Const ou Var

-------------------------------------------------------------
-- FUNÇÃO 2.1: SIMPLIFICAR TOTALMENTE
-------------------------------------------------------------
-- Aplica simplificação repetidamente até não haver mais mudanças
-------------------------------------------------------------
simplificarTotal :: Expressao -> Expressao
simplificarTotal expr =
    let s = simplificar expr
    in if s == expr
        then s
        else simplificarTotal s

-------------------------------------------------------------
-- FUNÇÃO 3: IMPRIMIR EM NOTAÇÃO INFIXA
-------------------------------------------------------------
-- Deve produzir strings como:
-- (3 + (2 * x))
-- (x ^ 3)
-- (6 * x)
-------------------------------------------------------------
imprimir :: Expressao -> String
imprimir (Const n) = show n

imprimir (Var x) = x

imprimir (Soma e1 e2) =
    "(" ++ imprimir e1 ++ " + " ++ imprimir e2 ++ ")"

imprimir (Produto e1 e2) =
    "(" ++ imprimir e1 ++ " * " ++ imprimir e2 ++ ")"

imprimir (Potencia e n) =
    "(" ++ imprimir e ++ " ^ " ++ show n ++ ")"

-------------------------------------------------------------
-- FUNÇÃO PRINCIPAL (main)
-------------------------------------------------------------
-- Requisitos:
-- 1. Ler expressão prefixa do usuário
-- 2. Parar quando usuário digitar "sair"
-- 3. Tokenizar e fazer parse
-- 4. Derivar em relação a x
-- 5. Simplificar
-- 6. Mostrar resultados usando imprimir
-------------------------------------------------------------
main :: IO ()
main = loop
  where
    loop = do
        putStrLn "Digite uma expressao prefixa (ou 'sair'):"
        linha <- getLine
        if linha == "sair"
            then putStrLn "Encerrando..."
            else do
                let (expr, _) = parse (tokenize linha)

                putStrLn ("Expressão original: " ++ imprimir expr)

                let d = derivar expr "x"
                putStrLn ("Derivada: " ++ imprimir d)

                let ds = simplificarTotal d
                putStrLn ("Derivada simplificada: " ++ imprimir ds)

                loop  -- continua no loop
