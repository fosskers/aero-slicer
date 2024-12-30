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

void _UnloadTexture(Texture2D *texture) {
  Texture2D stack;
  stack = *texture;
  UnloadTexture(stack);
}

bool _IsTextureValid(Texture2D *texture) {
  Texture2D stack;
  stack = *texture;
  return IsTextureValid(stack);
}

void _DrawTexture(Texture2D *texture, int posX, int posY, Color *tint) {
  Texture2D t;
  Color c;
  t = *texture;
  c = *tint;
  DrawTexture(t, posX, posY, c);
}

void _DrawTextureV(Texture2D *texture, Vector2 *position, Color *tint) {
  Texture2D t;
  Vector2 v;
  Color c;
  t = *texture;
  v = *position;
  c = *tint;
  DrawTextureV(t, v, c);
}

void _DrawTextureRec(Texture2D *texture, Rectangle *source, Vector2 *position,
                     Color *tint) {
  Texture2D t;
  Rectangle r;
  Vector2 v;
  Color c;
  t = *texture;
  r = *source;
  v = *position;
  c = *tint;
  DrawTextureRec(t, r, v, c);
}

// --- Sounds and Music --- //

Sound *_LoadSound(const char *fileName) {
  Sound stack = LoadSound(fileName);
  Sound *heap = malloc(sizeof(Sound));
  *heap = stack;
  return heap;
}

void _UnloadSound(Sound *sound) {
  Sound stack;
  stack = *sound;
  UnloadSound(stack);
}

void _PlaySound(Sound *sound) {
  Sound stack;
  stack = *sound;
  PlaySound(stack);
}

Music *_LoadMusicStream(const char *fileName) {
  Music stack = LoadMusicStream(fileName);
  Music *heap = malloc(sizeof(Music));
  *heap = stack;
  return heap;
}

void _UnloadMusicStream(Music *music) {
  Music stack;
  stack = *music;
  UnloadMusicStream(stack);
}

bool _IsMusicStreamPlaying(Music *music) {
  Music stack;
  stack = *music;
  return IsMusicStreamPlaying(stack);
}

void _PlayMusicStream(Music *music) {
  Music stack;
  stack = *music;
  PlayMusicStream(stack);
}

void _UpdateMusicStream(Music *music) {
  Music stack;
  stack = *music;
  UpdateMusicStream(stack);
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

void _DrawCircle(int centerX, int centerY, float radius, Color *color) {
  Color stack;
  stack = *color;
  DrawCircle(centerX, centerY, radius, stack);
}

void _DrawRectangle(int posX, int posY, int width, int height, Color *color) {
  Color stack;
  stack = *color;
  DrawRectangle(posX, posY, width, height, stack);
}

// --- Collision --- //

bool _CheckCollisionRecs(Rectangle *rec1, Rectangle *rec2) {
  Rectangle a;
  Rectangle b;
  a = *rec1;
  b = *rec2;

  return CheckCollisionRecs(a, b);
}
