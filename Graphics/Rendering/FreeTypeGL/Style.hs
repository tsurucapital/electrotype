module Graphics.Rendering.FreeTypeGL.Style where

import Graphics.Rendering.FreeTypeGL.Internal.Markup (MarkStyle(..), MarkSpan(..), Marked(..))
import Linear

fg, bg :: V4 Float -> Marked
fg = Marked . return . SpanStyle . MSForegroundColor
bg = Marked . return . SpanStyle . MSBackgroundColor

txt :: String -> Marked
txt = Marked . return . SpanString

underline :: Marked
underline = Marked . return . SpanStyle $ MSUnderline True

plain :: Marked
plain = Marked $ map (\s -> SpanStyle (s False)) [MSUnderline, MSStrikethrough, MSOverline]
