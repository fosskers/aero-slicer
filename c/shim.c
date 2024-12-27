#include "raylib.h"
#include <stdlib.h>

// --- Vectors --- //

Vector2 *_MakeVector2(float x, float y) {
  Vector2 *v = malloc(sizeof(Vector2));

  v->x = x;
  v->y = y;

  return v;
}

// --- Textures --- //

Texture2D *_LoadTexture(const char *fileName) {
  Texture2D *heap = malloc(sizeof(Texture2D));
  Texture2D stack = LoadTexture(fileName);

  *heap = stack;

  return heap;
}
