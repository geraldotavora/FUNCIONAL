--Aula 06/04/2017

-- comprimento da lista
compto [] = 0
compto (a:x) = 1 + compto x

-- validacao
lista[] = True
lista (a:x) = lista x

-- N termos
primeiros 0 _ = []
primeiros _ [] = []
primeiros n (a:x) = a : primeiros(n - 1) x

--verifica
pertence p [] = False
pertence p (a:x)    | p == a = True
                    |otherwise = pertence p x

--adiciona

insere c [] = [c]
insere c (a:x)  | c == a = a:x
                | otherwise = a : insere c x

--maior valor
maior_1 [a] = a
maior_1 (a:x) = if (a > maior_1 x) then a else (maior_1 x)

maior_2 [a] = a
maior_2 (a:b:x) | a > b = maior_2 (a:x)
                |otherwise = maior_2 (b:x)
                
maior_3 [a] = a
maior_3 (a:x)   | a > (maior_3 x) = a
                | otherwise = (maior_3 x)


--somar termos

somar_t x a b = [x + x | x <- [a..b]]

--mult termos
mult x = [x * x | x <- [1..10]]

par :: Int -> Bool
par x = mod x 2 ==0

constroi_lista0 = [x * x | x <- [9.. 39], par x]
constroi_lista1 = [x * x | x <- [9..39], par x, x > 55, x < 75]
constroi_lista2 = [ (x,y) | x <- [1..3], y <- ['a','b','c']]
constroi_lista3 = [ (x,y) | x <- ['a'..'z'], y <- [0..2]]







































