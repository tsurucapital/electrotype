module Graphics.Rendering.FreeTypeGL.Internal.Font
( Font(..), loadFont
) where

import Control.Applicative
import Graphics.Rendering.FreeTypeGL.Internal.TextureFont
import Foreign.ForeignPtr

-- Font:

-- | Represents a loaded font file with a configured face-size.
--
-- For different face sizes, you must load different fonts.
data Font = Font
  { fFont :: TextureFont
  } deriving (Eq, Ord, Show)

-- | Load a 'Font' with a given size.
loadFont
  :: FilePath -- ^ The font filename (e.g: \"foo.ttf\")
  -> TextureAtlas
  -> Float    -- ^ The desired face-size
  -> IO Font  -- ^ The result loaded font
loadFont fileName atlas size = Font <$> newTextureFont atlas fileName size

