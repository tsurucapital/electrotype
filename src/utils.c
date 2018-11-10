#include "utils.h"

void vertex_buffer_add_text( vertex_buffer_t * buffer, texture_font_t * font,
               wchar_t * text, vec4 * color, vec2 * pen )
{
    size_t i = 0;
    float r = color->red, g = color->green, b = color->blue, a = color->alpha;
    for( i=0; i<wcslen(text); ++i )
    {
        texture_glyph_t *glyph = texture_font_get_glyph( font, text[i] );
        if( glyph != NULL )
        {
            int kerning = 0;
            if( i > 0)
            {
                kerning = texture_glyph_get_kerning( glyph, text[i-1] );
            }
            pen->x += kerning;
            int x0  = (int)( pen->x + glyph->offset_x );
            int y0  = (int)( pen->y + glyph->offset_y );
            int x1  = (int)( x0 + glyph->width );
            int y1  = (int)( y0 - glyph->height );
            float s0 = glyph->s0;
            float t0 = glyph->t0;
            float s1 = glyph->s1;
            float t1 = glyph->t1;
            GLuint indices[6] = {0,1,2, 0,2,3};
            vertex_t vertices[4] = { { x0,y0,0,  s0,t0,  r,g,b,a },
                                     { x0,y1,0,  s0,t1,  r,g,b,a },
                                     { x1,y1,0,  s1,t1,  r,g,b,a },
                                     { x1,y0,0,  s1,t0,  r,g,b,a } };
            vertex_buffer_push_back( buffer, vertices, 4, indices, 6 );
            pen->x += glyph->advance_x;
        }
    }
}

void vertex_buffer_add_char8_len( vertex_buffer_t * buffer, texture_font_t * font,
               char * text, size_t len, vec4 * color, vec2 * pen )
{
    size_t i = 0;
    float r = color->red, g = color->green, b = color->blue, a = color->alpha;
    for( i=0; i<len; ++i )
    {
        texture_glyph_t *glyph = texture_font_get_glyph( font, text[i] );
        if( glyph != NULL )
        {
            int kerning = 0;
            if( i > 0)
            {
                kerning = texture_glyph_get_kerning( glyph, text[i-1] );
            }
            pen->x += kerning;
            int x0  = (int)( pen->x + glyph->offset_x );
            int y0  = (int)( pen->y + glyph->offset_y );
            int x1  = (int)( x0 + glyph->width );
            int y1  = (int)( y0 - glyph->height );
            float s0 = glyph->s0;
            float t0 = glyph->t0;
            float s1 = glyph->s1;
            float t1 = glyph->t1;
            GLuint indices[6] = {0,1,2, 0,2,3};
            vertex_t vertices[4] = { { x0,y0,0,  s0,t0,  r,g,b,a },
                                     { x0,y1,0,  s0,t1,  r,g,b,a },
                                     { x1,y1,0,  s1,t1,  r,g,b,a },
                                     { x1,y0,0,  s1,t0,  r,g,b,a } };
            vertex_buffer_push_back( buffer, vertices, 4, indices, 6 );
            pen->x += glyph->advance_x;
        }
    }
}

float measure_horizontal_text(texture_font_t* font, wchar_t* text)
{
    float width = 0.0f;
    size_t len = wcslen(text);
    size_t i = 0;

    if (len < 1) return width;

    {
        texture_glyph_t* glyph = texture_font_get_glyph(font, text[i]);
        if (glyph != NULL)
        {
            width += glyph->advance_x;
        }
    }

    for (i = 1; i < len; i++)
    {
        texture_glyph_t* glyph = texture_font_get_glyph(font, text[i]);
        if (glyph == NULL) continue;
        width += texture_glyph_get_kerning(glyph, text[i - 1]);
        width += glyph->advance_x;
    }

    return width;
}

float measure_horizontal_char8_len(texture_font_t* font, char* text, size_t len)
{
    float width = 0.0f;
    size_t i = 0;

    if (len < 1) return width;

    {
        texture_glyph_t* glyph = texture_font_get_glyph(font, text[i]);
        if (glyph != NULL)
        {
            width += glyph->advance_x;
        }
    }

    for (i = 1; i < len; i++)
    {
        texture_glyph_t* glyph = texture_font_get_glyph(font, text[i]);
        if (glyph == NULL) continue;
        width += texture_glyph_get_kerning(glyph, text[i - 1]);
        width += glyph->advance_x;
    }

    return width;
}
