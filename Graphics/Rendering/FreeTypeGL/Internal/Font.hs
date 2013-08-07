module Graphics.Rendering.FreeTypeGL.Internal.Font
( Font(..), loadFont
) where

import Control.Applicative
import qualified Graphics.Rendering.FreeTypeGL.Internal.TextureFont as ITF
import Foreign.ForeignPtr

-- Font:

-- | Represents a loaded font file with a configured face-size.
--
-- For different face sizes, you must load different fonts.
data Font = Font
  { fFont :: ForeignPtr ITF.TextureFont
  } deriving (Eq, Ord, Show)

-- | Load a 'Font' with a given size.
loadFont
  :: FilePath -- ^ The font filename (e.g: \"foo.ttf\")
  -> Float    -- ^ The desired face-size
  -> IO Font  -- ^ The result loaded font
loadFont fileName size = Font <$> ITF.new ITF.NotLCD fileName size

