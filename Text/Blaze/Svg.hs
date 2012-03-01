{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.Svg
    (
    -- * Types
      Svg
    -- * Creating paths
    , mkPath
    -- ** lineTo
    , l, lr
    -- ** moveTo
    , m, mr
    ) where

import Text.Blaze.Svg.Internal
