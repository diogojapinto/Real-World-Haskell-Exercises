data List a = Cons a (List a)
            | Nil
              deriving (Show)


fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil

fromList2 (Cons x xs) = x:(fromList2 xs)
fromList2 Nil = []