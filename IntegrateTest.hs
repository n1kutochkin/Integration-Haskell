import Simpson.Simpson as Simpson

myInterval :: Simpson.Interval
myInterval = Simpson.Interval (0.0, 1.0) 1001

myFunc :: Double -> Double
myFunc = \x -> x ** 2

main :: IO ()
main = do
  let result = Simpson.integrate myFunc myInterval
  putStrLn $ show result
