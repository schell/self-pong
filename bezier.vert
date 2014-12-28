#version 330 core
layout(location = 0) in vec2 position;
layout(location = 1) in vec3 bez;

uniform mat4 projection;
uniform mat4 modelview;

out vec3 fbez;

void main() {
    fbez = bez;
    gl_Position = projection * modelview * vec4(position.xy, 0.0, 1.0);
}
