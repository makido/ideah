--data D = D Int Int

print :: (Show b) => b -> String
print x = show x

l [] = 0
l (x:xs) = 1 + l xs

--test (D x y) = x + y

main = Prelude.print $ test "x"
  where test :: String -> Int
        test str = l str
