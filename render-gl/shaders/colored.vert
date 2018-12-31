#version 330 core

layout (location = 0) in vec2 pos;
layout (location = 1) in vec4 color;

uniform mat3 model;
uniform mat3 view;

out vec4 color_frag;

void main() {
  vec2 pos = (view * model * vec3(pos, 1.0)).xy;
  gl_Position = vec4(pos, 0.0, 1.0);
  color_frag = color;
}
