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
svgDoc = S.docTypeSvg ! A.version "1.1" ! A.width "150" ! A.height "100" ! A.viewbox "0 0 3 2" $ do
    S.rect ! A.width "1" ! A.height "2" ! A.fill "#008d46"
    S.rect ! A.width "1" ! A.height "2" ! A.fill "#ffffff"
    S.rect ! A.width "1" ! A.height "2" ! A.fill "#d2232c"
