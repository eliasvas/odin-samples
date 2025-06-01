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

main :: proc() {
  rl.SetConfigFlags({.WINDOW_RESIZABLE, .VSYNC_HINT})
	rl.InitWindow(800, 600, "mini0");
  rl.SetTargetFPS(144)
	texture := rl.LoadTexture("round_cat.png")

  world_def : b2.WorldDef = b2.DefaultWorldDef()
  world_def.gravity = b2.Vec2{0.0, 100.0}
  world_id : b2.WorldId = b2.CreateWorld(world_def)
  //b2.DestroyWorld(world_id)

  // Make a static body
  ground_halfdim := b2.Vec2{50.0,10.0}
  ground_body_def : b2.BodyDef = b2.DefaultBodyDef()
  ground_body_def.position = b2.Vec2{0.0, 10.0}
  ground_id : b2.BodyId = b2.CreateBody(world_id, ground_body_def)
  ground_box : b2.Polygon = b2.MakeBox(ground_halfdim.x, ground_halfdim.y);
  ground_shape_def : b2.ShapeDef = b2.DefaultShapeDef()
  ground_poly_shape:= b2.CreatePolygonShape(ground_id, ground_shape_def, ground_box)

  ///*
  // Make the static body that will follow the cursor 
  other_radius :f32= 64.0
  other_body_def : b2.BodyDef = b2.DefaultBodyDef()
  other_body_def.position = b2.Vec2{0.0, -1000.0}
  other_id : b2.BodyId = b2.CreateBody(world_id, other_body_def)
  other_box : b2.Polygon = b2.MakeRoundedBox(other_radius, other_radius, other_radius);
  other_shape_def : b2.ShapeDef = b2.DefaultShapeDef()
  other_poly_shape:= b2.CreatePolygonShape(other_id, other_shape_def, other_box)
  //*/



  // Make a dynamic body
  halfdim := b2.Vec2{4.0,4.0}
  dyn_body_def : b2.BodyDef = b2.DefaultBodyDef()
  dyn_body_def.type = b2.BodyType.dynamicBody
  dyn_body_def.position = b2.Vec2{0.0, -20.0}
  dyn_body_def.rotation = b2.MakeRot(1)
  dyn_id : b2.BodyId = b2.CreateBody(world_id, dyn_body_def)
  dyn_box : b2.Polygon = b2.MakeBox(halfdim.x, halfdim.y)
  dyn_shape_def : b2.ShapeDef = b2.DefaultShapeDef()
  dyn_shape_def.density = 1.0
  dyn_shape_def.friction = 0.3
  dyn_shape_def.restitution = 1.0

  dyn_poly_shape:= b2.CreatePolygonShape(dyn_id, dyn_shape_def, dyn_box)

  for !rl.WindowShouldClose() {
    // Do box2d simulation stuff
    time_step := rl.GetFrameTime()
    sub_step_count : i32 = 4

    b2.World_Step(world_id, time_step, sub_step_count)
    p := b2.Body_GetWorldPoint(dyn_id, -halfdim)
    rotation := b2.Body_GetRotation(dyn_id)

    sp := b2.Body_GetWorldPoint(ground_id, -ground_halfdim)
    srotation := b2.Body_GetRotation(ground_id)


    if rl.IsKeyDown(rl.KeyboardKey.RIGHT) do b2.Body_ApplyForceToCenter(dyn_id, {10000,0}, true) 
    if rl.IsKeyDown(rl.KeyboardKey.LEFT) do b2.Body_ApplyForceToCenter(dyn_id, {-10000,0}, true) 
    if rl.IsKeyPressed(rl.KeyboardKey.UP) do b2.Body_ApplyForceToCenter(dyn_id, {0,1000000}, true) 

    //--------------------------

    rl.BeginDrawing()
    rl.ClearBackground({0, 120, 153, 255})

    // Camera mode (for b2d stuff)
    rl.BeginMode2D(game_camera())

    rl.DrawRectanglePro({p.x, p.y,halfdim.x*2,halfdim.y*2}, {0,0}, b2.Rot_GetAngle(rotation)* rl.RAD2DEG, rl.LIGHTGRAY);
    rl.DrawRectanglePro({sp.x, sp.y,ground_halfdim.x*2,ground_halfdim.y*2}, {0,0}, b2.Rot_GetAngle(srotation)*rl.RAD2DEG, rl.RED);

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
