#include "raylib.h"
#include <stdlib.h>

// --- Vectors --- //

Vector2 *_MakeVector2(float x, float y) {
  Vector2 *v = malloc(sizeof(Vector2));

  v->x = x;
  v->y = y;

  return v;
}

// --- Rectangles --- //

Rectangle *_MakeRectangle(float x, float y, float width, float height) {
  Rectangle *r = malloc(sizeof(Rectangle));

  r->x = x;
  r->y = y;
  r->width = width;
  r->height = height;

  return r;
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

Color *_ColorAlpha(Color *color, float alpha) {
  Color stack;
  stack = *color;
  Color new = ColorAlpha(stack, alpha);
  Color *heap = malloc(sizeof(Color));
  *heap = new;

  return heap;
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
