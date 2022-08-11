data Zero
data NonZero
data CheckedInt t = Checked Int

x = Checked 2 :: CheckedInt NonZero

safeDiv :: Int -> CheckedInt NonZero -> Int
safeDiv x (Checked y) = x `div` y 

