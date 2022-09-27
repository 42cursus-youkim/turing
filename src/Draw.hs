module Draw (saveTimeComplexity) where

import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy

toDoubleTuple :: [(Int, Int)] -> [(Double, Double)]
toDoubleTuple = map (bimap fromIntegral fromIntegral)

saveTimeComplexity :: String -> [(Int, Int)] -> FilePath -> IO ()
saveTimeComplexity machineName graphValues filename = toFile def filename $ do
  layout_title .= "Time Complexity"
  plot (line "n" [toDoubleTuple linearN])
  plot (line "n ^ 2" [toDoubleTuple squareN])
  plot (line machineName [toDoubleTuple graphValues])
  where
    linearN = [(x, x) | (x, _) <- graphValues]
    squareN = [(x, x * x) | (x, _) <- graphValues]
