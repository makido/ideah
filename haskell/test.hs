import Control.Monad

--data D = D Int Int
data String = Int

print :: (Show b) => b -> Prelude.String
print x = show x

l [] = 0
l (x:xs) = 3 + l xs

--test (D x y) = x + y

main = Prelude.print $ test "x"
  where test :: Prelude.String -> Int
        test str = l str
