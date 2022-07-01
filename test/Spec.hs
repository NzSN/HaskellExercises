import Test.Hspec
import Test.QuickCheck (quickCheck, forAll, elements, (==>))

main :: IO ()
main = do
  spec_1

spec_1 :: IO ()
spec_1 = do
  quickCheck $ prop_Insert 1 [1,2,3,4]
  where
    ordered xs = and (zipWith (<=) xs (drop 1 xs))
    insert x xs = takeWhile (<x) xs ++ [x] ++ dropWhile (<x) xs
    prop_Insert x xs = ordered xs ==> ordered (insert x xs)
