#version 330 core

in vec2 texCoord;

out vec4 FragColor;

uniform vec4 color;
uniform vec4 borderColor;

void main()
{
  // check pos is in border
  bool isEdge = (texCoord.x < 0.01 || texCoord.x > 0.99 || texCoord.y < 0.01 || texCoord.y > 0.99);
  bool isBorder = (texCoord.x < 0.1 || texCoord.x > 0.9 || texCoord.y < 0.1 || texCoord.y > 0.9);
  if (isBorder && isEdge) FragColor = vec4(0.0, 0.0, 1.0, 1.0);
  else if (isBorder) FragColor = borderColor;
  else FragColor = color;
}

