#version 130

//uniform vec2 origin;
//uniform vec2 end;
uniform vec2 resolution;
in vec2 position;
in vec4 color;
out vec4 color_from_vshader;

void main() {
    //vec2 adj = (position + 1) * 0.5;
    //vec2 loc = (adj * end + origin) / resolution - 1;
    //vec2 adj = position;
    //vec2 loc = (adj * end + origin) / resolution;
    vec2 loc = (position / resolution) * 2 - 1;
    gl_Position = vec4(loc, 1, 1);
    color_from_vshader = color;
}
