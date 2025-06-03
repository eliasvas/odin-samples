package main

import "core:fmt"
import "core:os"
import "core:strconv"
import "core:strings"

import rl "vendor:raylib"
import b2 "vendor:box2d"

game_camera :: proc() -> rl.Camera2D {
	w := f32(rl.GetScreenWidth())
	h := f32(rl.GetScreenHeight())

	return {
		zoom = 5.0,
		target = {0.0, 0.0},
		offset = { w/2, h/2 },
	}
}

Entity :: struct {
  hdim : b2.Vec2,

  body_id : b2.BodyId, // this is the position
  polygon : b2.Polygon, // this is the box/circle
  shape_id : b2.ShapeId, // this is both body + polygon?

  color : rl.Color,
}

render_entity :: proc(e : ^Entity) {
  pos := b2.Body_GetWorldPoint(e.body_id, -e.hdim)
  rot := b2.Body_GetRotation(e.body_id)
  rl.DrawRectanglePro({pos.x, pos.y,e.hdim.x*2,e.hdim.y*2}, {0,0}, b2.Rot_GetAngle(rot) * rl.RAD2DEG, e.color);
}

make_sample_box :: proc(world_id : b2.WorldId, pos : b2.Vec2, rot : b2.Rot, hdim : b2.Vec2, body_type : b2.BodyType, density : f32, friction : f32, restitution : f32, color : rl.Color = rl.RAYWHITE) -> Entity {
  body_def : b2.BodyDef = b2.DefaultBodyDef()
  body_def.type = body_type
  body_def.position = pos
  body_def.rotation = rot
  body_id : b2.BodyId = b2.CreateBody(world_id, body_def)
  box : b2.Polygon = b2.MakeBox(hdim.x, hdim.y)
  shape_def : b2.ShapeDef = b2.DefaultShapeDef()
  shape_def.density = density
  shape_def.friction = friction
  shape_def.restitution = restitution
  poly_shape := b2.CreatePolygonShape(body_id, shape_def, box)

  e : Entity
  e.hdim = hdim
  e.body_id = body_id
  e.polygon = box
  e.shape_id = poly_shape
  e.color = color
  return e
}

main :: proc() {
  rl.SetConfigFlags({.WINDOW_RESIZABLE, .VSYNC_HINT})
	rl.InitWindow(800, 600, "rayboxtest");
  rl.SetTargetFPS(144)
	texture := rl.LoadTexture("round_cat.png")

  world_def : b2.WorldDef = b2.DefaultWorldDef()
  world_def.gravity = b2.Vec2{0.0, 100.0}
  world_id : b2.WorldId = b2.CreateWorld(world_def)
  //defer b2.DestroyWorld(world_id)

  ground_entity := make_sample_box(world_id, {0,10}, b2.MakeRot(0), {50,10}, .staticBody, 0,0,0, rl.LIGHTGRAY)
  dyn_entity := make_sample_box(world_id, {0,-20}, b2.MakeRot(1), {4,4}, .dynamicBody, 1.0, 0.3, 1.0, rl.ORANGE)

  for !rl.WindowShouldClose() {
    // Do box2d simulation stuff
    time_step := rl.GetFrameTime()
    sub_step_count : i32 = 4

    b2.World_Step(world_id, time_step, sub_step_count)

    if rl.IsKeyDown(rl.KeyboardKey.RIGHT) do b2.Body_ApplyForceToCenter(dyn_entity.body_id, {10000,0}, true) 
    if rl.IsKeyDown(rl.KeyboardKey.LEFT) do b2.Body_ApplyForceToCenter(dyn_entity.body_id, {-10000,0}, true) 
    if rl.IsKeyPressed(rl.KeyboardKey.UP) do b2.Body_ApplyForceToCenter(dyn_entity.body_id, {0,-500000}, true) 
    if rl.IsKeyPressed(rl.KeyboardKey.DOWN) do b2.Body_ApplyForceToCenter(dyn_entity.body_id, {0,500000}, true) 

    //--------------------------

    rl.BeginDrawing()
    rl.ClearBackground({0, 120, 153, 255})

    // Camera mode (for b2d stuff)
    rl.BeginMode2D(game_camera())

    render_entity(&dyn_entity)
    render_entity(&ground_entity)

    rl.EndMode2D()

    rl.DrawTextureEx(texture, rl.GetMousePosition() - {2*f32(texture.width), 2*f32(texture.height)}, 0, 4, rl.WHITE)

    fps := int(rl.GetFPS());
    buf: [8]byte
    strconv.itoa(buf[:],fps)
    s := strings.clone_to_cstring(strings.concatenate({"fps:", string(buf[:])})) 
    rl.DrawText(s, 0, 0, 20, rl.RAYWHITE)

    rl.EndDrawing()
  }
}
