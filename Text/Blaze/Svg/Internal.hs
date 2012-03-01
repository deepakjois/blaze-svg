module Text.Blaze.Svg.Internal where

import Control.Monad.State
import Data.Monoid (mappend, mempty)

import Text.Blaze
import Text.Blaze.Internal

type Svg = HtmlM ()


-- | Construct SVG paths using combinators
--
-- > import Text.Blaze.Svg11 ((!))
-- > import qualified Text.Blaze.Svg11 as S
-- > import qualified Text.Blaze.Svg11.Attributes as A
-- >
-- > svgDoc :: Svg
-- > svgDoc = S.docTypeSvg ! A.version "1.1" ! A.width "150" ! A.height "100" $ do
-- >  path ! d mkPath do
-- >    l 2 3
-- >    m 4 5
mkPath :: State AttributeValue () -> AttributeValue
mkPath path = snd $ runState path mempty

-- Lineto
l :: Show a => a -> a -> State AttributeValue ()
l x y = modify (`mappend` toValue ("L " ++ show x ++ " " ++ show y ++ " "))

-- Lineto (relative)
lr :: Show a => a -> a -> State AttributeValue ()
lr dx dy = modify (`mappend` toValue ("l " ++ show dx ++ " " ++ show dy ++ " "))

-- Moveto
m :: Show a => a -> a -> State AttributeValue ()
m x y = modify (`mappend` toValue ("M " ++ show x ++ " " ++ show y ++ " "))

-- Moveto (relative)
mr :: Show a => a -> a -> State AttributeValue ()
mr dx dy = modify (`mappend` toValue ("m " ++ show dx ++ " " ++ show dy ++ " "))

-- TODO curveTo and arcTo

-- ClosePath
z :: State AttributeValue ()
z = modify (`mappend` toValue "Z")