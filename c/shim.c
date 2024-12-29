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

// --- Camera --- //

Camera2D *_MakeCamera2D(Vector2 *offset, Vector2 *target, float rotation,
                        float zoom) {
  Camera2D *camera = malloc(sizeof(Camera2D));
  Vector2 o = *offset;
  Vector2 t = *target;

  camera->offset = o;
  camera->target = t;
  camera->rotation = rotation;
  camera->zoom = zoom;

  return camera;
}

// --- Window --- //

void _ClearBackground(Color *color) {
  Color stack;
  stack = *color;
  ClearBackground(stack);
}

void _BeginMode2D(Camera2D *camera) {
  Camera2D stack;
  stack = *camera;
  BeginMode2D(stack);
}

void _DrawText(const char *text, int posX, int posY, int fontSize,
               Color *color) {
  Color stack;
  stack = *color;
  DrawText(text, posX, posY, fontSize, stack);
}

void _DrawCircle(int centerX, int centerY, int radius, Color *color) {
  Color stack;
  stack = *color;
  DrawCircle(centerX, centerY, radius, stack);
}

void _DrawRectangle(int posX, int posY, int width, int height, Color *color) {
  Color stack;
  stack = *color;
  DrawRectangle(posX, posY, width, height, stack);
}
