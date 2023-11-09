module Main where
import Control.Parallel
import Control.Concurrent


calcIntegralParallel :: (Double -> Double) -> Double -> Double -> Double -> Int -> Double
calcIntegralParallel f leftBound rightBound precision numberOfThreads =
    if numberOfThreads < 1 || rightBound <= leftBound
        then error "interval must be correct and number of threads must be >= 1"
        else calcIntegralParallelRec f leftBound precision intervalSize currentIntervalIndex
        where
            intervalSize = (rightBound - leftBound) / fromIntegral (numberOfThreads)
            currentIntervalIndex = numberOfThreads


-- recursive function that imitates for-loop to launch threads to calculate subintegral for each thread 
-- init of currentIntervalIndex must be equal to number of threads; min ofr currentIntervalIndex is 1
calcIntegralParallelRec :: (Double -> Double) -> Double -> Double -> Double -> Int -> Double
calcIntegralParallelRec f globalLeftBound precision invervalSize currentIntervalIndex =
    if currentIntervalIndex > 1
        then currentSubIntegralValue `par` nextTreadsResults `pseq` currentSubIntegralValue + nextTreadsResults
        else currentSubIntegralValue
        where
            localLeftBound = globalLeftBound + invervalSize * fromIntegral (currentIntervalIndex - 1)
            localRightBound = localLeftBound + invervalSize
            currentSubIntegralValue = calcIntegralWithPrecision f localLeftBound localRightBound precision
            nextTreadsResults = calcIntegralParallelRec f globalLeftBound precision invervalSize (currentIntervalIndex - 1)

-- launch iterative (recursive) procedure to calc integral value on given interval with given precision
calcIntegralWithPrecision :: (Double -> Double) -> Double -> Double -> Double -> Double
calcIntegralWithPrecision f leftBound rightBound precision =
    calcIntegralWithPrecisionRec f leftBound rightBound precision zeroIterationIntegralValue numberOfIntervals
    where
        zeroIterationIntegralValue = calcTrapezoidalRule f leftBound rightBound
        numberOfIntervals = 1


-- It is recursive function that do iterative calculating of integral with given precision (like a while-loop)
-- it calculates number of elementary intervals for each iteration and launch calculations of it 
-- prevIterationIntegral must be calcIntegralWithNumberOfIntervals(f, leftBound, rightBound-leftBound, 1)
calcIntegralWithPrecisionRec :: (Double -> Double) -> Double -> Double -> Double -> Double -> Int -> Double
calcIntegralWithPrecisionRec f leftBound rightBound precision prevIterationIntegral prevIterationNumberOfIntervals =
    if abs (currentIterationIntegral - prevIterationIntegral) < precision
        then currentIterationIntegral
        else calcIntegralWithPrecisionRec f leftBound rightBound precision currentIterationIntegral currentIterationNumberOfIntervals
    where
        currentIterationNumberOfIntervals = prevIterationNumberOfIntervals * 2
        intervalSize = (rightBound - leftBound) / fromIntegral (currentIterationNumberOfIntervals)
        currentIterationIntegral = calcIntegralWithNumberOfIntervalsRec f leftBound intervalSize currentIterationNumberOfIntervals

-- recursive function that imitates for-loop to calculate subintegral on each elementary interval 
-- init of currentInterval must be equal to number of intervals 
calcIntegralWithNumberOfIntervalsRec :: (Double -> Double) -> Double -> Double -> Int -> Double
calcIntegralWithNumberOfIntervalsRec f globalLeftBound intervalSize 0 = 0
calcIntegralWithNumberOfIntervalsRec f globalLeftBound intervalSize currentIntervalIndex =
    calcIntegralWithNumberOfIntervalsRec f globalLeftBound intervalSize (currentIntervalIndex - 1) +
    calcTrapezoidalRule f localLeftBound localRightBound
    where
        localLeftBound = globalLeftBound + intervalSize * fromIntegral (currentIntervalIndex - 1)
        localRightBound = localLeftBound + intervalSize

-- calculates trapezoidal rule for interval
calcTrapezoidalRule :: (Double -> Double) -> Double -> Double -> Double
calcTrapezoidalRule f leftBound rightBound =
    ((f leftBound + f rightBound) * (rightBound - leftBound)) / 2

f :: Double -> Double
f x = sin x

main :: IO()
main = do
    putStrLn "Enter number of threads: "
    numberOfThreadsInput <- getLine

    putStrLn "Enter left bound: "
    leftBoundInput <- getLine

    putStrLn "Enter right bound: "
    rightBoundInput <- getLine


    print(calcIntegralParallel f (read leftBoundInput :: Double) (read rightBoundInput :: Double) 0.0001 (read numberOfThreadsInput :: Int))
