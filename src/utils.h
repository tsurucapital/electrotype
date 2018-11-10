#ifndef __UTILS_H__
#define __UTILS_H__

#include <wchar.h>
#include "texture-font.h"
#include "vertex-buffer.h"

void vertex_buffer_add_text( vertex_buffer_t * buffer, texture_font_t * font,
               wchar_t * text, vec4 * color, vec2 * pen );

void vertex_buffer_add_char8_len( vertex_buffer_t * buffer, texture_font_t * font,
               char * text, size_t len, vec4 * color, vec2 * pen );

float measure_horizontal_text( texture_font_t * font, wchar_t * text );
float measure_horizontal_char8_len( texture_font_t * font, char * text, size_t len );

typedef struct {
    float x, y, z;    // position
    float s, t;       // texture
    float r, g, b, a; // color
} vertex_t;

#endif
