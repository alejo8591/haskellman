doubleNum y = y * y

tripleNum a b = doubleNum a + doubleNum b

numberSmallerA x = (if x > 100 then x else x*2) * 2

-- rang in Haskell
modjo = [1..20]

-- concatenate lists
poyo = [1..5]++[1,3,4,5,6]

-- concatenate list in init
ropo = 23:22:[1,2,3,4,5,6,7]

-- find character in right position
ene = ropo !! 5

-- more options
a = head modjo
b = tail modjo
c = last modjo
d = init modjo
e = length modjo
f = null modjo
g = reverse ropo
h = take 4 modjo
i = drop 6 modjo
j = minimum modjo
k = maximum ropo
l = sum modjo
m = product ropo
o = 4 `elem` modjo
p = take 15 (repeat 3) 
