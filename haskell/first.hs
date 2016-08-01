soma :: Int -> Int
soma 1 = 1
soma a = a + soma (a - 1)

fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial(n-1)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

guarda :: Int -> [Char]
guarda x
      | (x == 0) = "Digitou 0"
      | (x == 1) = "Digitou 1"
      | otherwise = "Sé loco"

and' :: Bool -> Bool -> Bool
and' False _ = False
and' _ False = False
and' True True = True

tupla :: (Int, Int) -> (Int, Int) -> (Int, Int)
tupla (a, b) (c, d) = (a + c, b + d)

nomes :: (String, String, String)
nomes = ("Tiago", "André", "Pedro")

selFirst :: (a, b, c) -> a
selFirst (x, _, _) = x

selSecond :: (a, b, c) -> b
selSecond (_, x, _) = x

type Nome = String
type Idade = Int
type Linguagem = String
type Pessoa = (Nome, Idade, Linguagem)

pessoa :: Pessoa
pessoa = ("João", 20, "Haskell")

getNome :: Pessoa -> Nome
getNome (n, i, l) = n
