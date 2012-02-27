{-# LANGUAGE OverloadedStrings #-}
module Example where
import Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Renderer.String (renderHtml)

main :: IO ()
main = do
  let a = renderHtml svgDoc
  putStrLn a

svgDoc :: S.Svg
svgDoc = S.svg $ do
           S.rect ! A.strokeWidth "2"
