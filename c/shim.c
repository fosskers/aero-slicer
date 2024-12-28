#include "raylib.h"
#include <stdlib.h>

// --- Vectors --- //

Vector2 *_MakeVector2(float x, float y) {
  Vector2 *v = malloc(sizeof(Vector2));

  v->x = x;
  v->y = y;

  return v;
}

// --- Colours --- //

Color *_MakeColor(unsigned char r, unsigned char g, unsigned char b,
                  unsigned char a) {
  Color *c = malloc(sizeof(Color));

  c->r = r;
  c->g = g;
  c->b = b;
  c->a = a;

  return c;
}

// --- Textures --- //

Texture2D *_LoadTexture(const char *fileName) {
  Texture2D *heap = malloc(sizeof(Texture2D));
  Texture2D stack = LoadTexture(fileName);

  *heap = stack;

  return heap;
}

// --- Window --- //

void _ClearBackground(Color *color) {
  Color stack;
  stack = *color;
  ClearBackground(stack);
}
