{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.Svg
    (
      Svg
    , mkPath
    -- | lineTo
    , l, lr
    -- | moveTo
    , m, mr
    ) where

import Text.Blaze.Svg.Internal
