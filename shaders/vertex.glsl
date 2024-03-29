#version 330 core

layout (location = 0) in vec2 aPos;
layout (location = 1) in vec2 aTexCoord;

out vec2 texCoord;

uniform vec2 pos;

void main()
{
  texCoord = aTexCoord;
  gl_Position = vec4(aPos.x + pos.x, aPos.y + pos.y, 0.0, 1.0);
}
