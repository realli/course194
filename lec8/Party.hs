module Party where

import Employee
import Data.Monoid
import Data.Tree

-- simple cons Employee to GuestList, no complex
-- check or logic
glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp name fun) (GL ls totleFun) = GL (emp : ls) (totleFun + fun)

instance Monoid GuestList where
    mempty = GL [] 0
    (GL l1 fun1) `mappend` (GL l2 fun2) = GL (l1 ++ l2) (fun1 + fun2)


moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- treeFold for Data.Tree
treeFold :: Monoid b => (b -> a -> b) -> b -> Tree a -> b
treeFold f init (Node value forest) =
    mappend init . mconcat $ map (treeFold f mempty) forest


-- nextLevel is function giving boss and all it's subtrees' best guestlist
-- pairs, to compute the best guestlist himself
-- (guestlist1, guestlist2) ----> (bestlist with boss, without boss)
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp ls = (glCons emp withBoss, glCons emp withoutBoss)
    where withBoss =  mconcat $ map snd ls
          withoutBoss = mconcat $ map fst ls

-- maxFun use nextLevel to compute the max fun guest list
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . maxFuns
maxFuns :: Tree Employee -> (GuestList, GuestList)
maxFuns (Node emp ls) = nextLevel emp $ map maxFuns ls

gltToList :: GuestList -> [Name]
gltToList (GL ls _) = map empName ls

gltTotalFun :: GuestList -> Fun
gltTotalFun (GL _ fun) = fun

main :: IO ()
main = do
    str <- readFile "company.txt"
    let tree = read str :: (Tree Employee)
        guestList = maxFun tree
    putStrLn $ "Total fun: " ++ show (gltTotalFun guestList)
    mapM_ putStrLn $ gltToList guestList
    return ()

