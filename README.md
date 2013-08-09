# electrotype

## Reasonably fast OpenGL text rendering

This package handles loading and rendering fonts in modern OpenGL. It uses the
[FreeType](http://www.freetype.org "FreeType") library to render the glyphs to textures, and then draws the textures
to the screen. The bitmaps for the glyphs are cached in a packed texture atlas.
The C code is based on the [freetype-gl
project](http://code.google.com/p/freetype-gl/ "freetype-gl"), and the Haskell
code is an evolving set of bindings to provide reasonably fast and reasonably
flexible text rendering.

There are 3 primary goals:

1. A mode for fast and simple text rendering for limited character sets and
   without layout capabilities (reusable glyph texture atlantes).

2. A mode for more sophisticated and flexible text rendering, by using pango or
   another layout engine to write full paragraphs (or other blocks of text) to
   texture buffers, and then display the whole texture buffer. For frequently
   changing text, this is slower than using cached glyphs and a texture atlas,
   but more flexible in terms of layout and the number of glyphs.

3. Work in OpenGL 3.2 Core, and don't require scattering across an installation
   various resource files, such as shaders and fonts.

## Installation

You may first need to install a freetype2 dependency package:

```shell
  git clone https://github.com/Peaker/freetype2
  cd freetype2
  cabal install
```

Then, you should be able to install electrotype with a simple cabal install
command. (Not yet tested on Windows after forking from FreeTypeGL.)
