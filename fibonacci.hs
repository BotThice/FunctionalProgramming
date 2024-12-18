fibo :: (Integral t) => t -> t
fibo n
    | n == 0 = 1
    | n == 1 = 1
    | n > 1 = fibo (n - 1) + fibo (n - 2)
    | otherwise = error "fibonacci on negative number"

    -- type is fibo :: (Integral t) => t -> t
    -- Big-O of fibo(n) is O(2^n)
    -- no, it's too long to compute large number
        -- idea to improve the efficiency is to keep the value of previous fibonacci so it will calculate 1 time and reuse its value.