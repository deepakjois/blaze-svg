{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.Svg where

import Text.Blaze.Internal
import Text.Blaze.Svg.Internal

svg :: Svg
     -> Svg
svg = Parent "svg" "<svg" "</svg>"
