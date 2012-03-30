{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- | A renderer that produces a lazy 'L.Text' value, using the Text Builder.
--
module Text.Blaze.Svg.Renderer.Text
    ( renderSvg
    ) where

import Text.Blaze.Renderer.Text (renderMarkup)

renderSvg = renderMarkup