#version 330 core

layout (location = 0) in vec4 vertex; // <vec2 pos, vec2 tex>

out vec2 tex_coord;

void main() {
  vec2 pos = vertex.xy;
  gl_Position = vec4(pos, 0.0, 1.0);
  tex_coord = vertex.zw;
}
