List all permutations of a list ([4 kyu][0])

My original solution:

\begin{code}
import Data.List (delete, nub)

permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = nub [ x : ys | x <- xs, ys <- permutations (delete x xs) ]
\end{code}

[0]: https://biturl.io/Permutations
