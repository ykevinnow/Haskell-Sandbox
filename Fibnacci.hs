fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

show_fib :: Int -> IO ()
show_fib n = putStrLn ("Fib of " ++ (show n) ++ " is " ++ (show (fib n)))

-- v1 for collections of all fibs,  not good enough
collect_all_fibs :: Int -> [Int]
collect_all_fibs 0 = [1]
collect_all_fibs 1 = [1,1]
collect_all_fibs n = (collect_all_fibs (n -1)) ++ [fib n]

-- v2 for collections of all fibs, not good enough
add_first_two :: [Int] -> Int
add_first_two [] = 0
add_first_two [x] = x
add_first_two (x:xs) = x + (head xs)

add_last_two :: [Int] -> Int
add_last_two [] = 0
add_last_two xs = add_first_two (reverse xs)

collect_all_fibs_v2 :: Int -> [Int]
collect_all_fibs_v2 0 = [1]
collect_all_fibs_v2 1 = [1,1]
collect_all_fibs_v2 n = (collect_all_fibs_v2 (n-1)) ++ [add_last_two (collect_all_fibs_v2(n-1))]

-- v3 seems good, but will overflow in Ints
collect_all_fibs_v3 :: Int -> [Int]
collect_all_fibs_v3 0 = [1]
collect_all_fibs_v3 1 = [1,1]
collect_all_fibs_v3 n =
    let list = (collect_all_fibs_v3 (n-1))
    in  list ++ [add_last_two list]

-- v4 for integer type, now seems good
add_first_two_v2 :: [Integer] -> Integer
add_first_two_v2 [] = 0
add_first_two_v2 [x] = x
add_first_two_v2 (x:xs) = x + (head xs)

add_last_two_v2 :: [Integer] -> Integer
add_last_two_v2 [] = 0
add_last_two_v2 xs = add_first_two_v2 (reverse xs)

collect_all_fibs_v4 :: Int -> [Integer]
collect_all_fibs_v4 0 = [1]
collect_all_fibs_v4 1 = [1,1]
collect_all_fibs_v4 n =
    let list = (collect_all_fibs_v4 (n-1))
    in  list ++ [add_last_two_v2 list]

-- v5
collect_all_fibs_v5 :: Int -> [Integer]
collect_all_fibs_v5 0 = [1]
collect_all_fibs_v5 1 = [1,1]
collect_all_fibs_v5 n =
    let list = (collect_all_fibs_v5 (n-1))
    in [add_first_two_v2 list] ++ list

collect_all_fibs_v5_1 :: Int -> [Integer]
collect_all_fibs_v5_1 n = reverse (collect_all_fibs_v5 n)
