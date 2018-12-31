#version 330 core

in vec4 color_frag;

uniform sampler2D tex;

out vec4 color;

void main() {
  color = color_frag;
}
