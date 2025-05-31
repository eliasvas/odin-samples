package main

import "core:fmt"
import "core:os"

import ease "core:math/ease"

import rl "vendor:raylib"

CAT_SCALE_MIN :: 5
CAT_SCALE_MAX :: 12
Cat :: struct {
  scale : f32,
  state : CatScaleState,
  ease_timer : f32,
  ease_increase : MyEaseFuncs,
  ease_decrease : MyEaseFuncs,
}

CatScaleState :: enum {
  NONE,
  INCREASING,
  DECREASING,
}
MyEaseFuncs :: enum {
  LinearIn,
  LinearOut,
  QuadraticIn,
  QuadraticOut,
  CubicIn,
  CubicOut,
  SineIn,
  SineOut,
  BounceIn,
  BounceOut,
}


do_easing :: proc(func : MyEaseFuncs, num : f32) -> f32 {
  ease_val : f32
  switch func {
  case MyEaseFuncs.LinearIn: ease_val = (num);
  case MyEaseFuncs.LinearOut: ease_val = (num);
  case MyEaseFuncs.QuadraticIn: ease_val = ease.quadratic_in(num);
  case MyEaseFuncs.QuadraticOut: ease_val = ease.quadratic_out(num);
  case MyEaseFuncs.CubicIn: ease_val = ease.cubic_in(num);
  case MyEaseFuncs.CubicOut: ease_val = ease.cubic_out(num);
  case MyEaseFuncs.SineIn: ease_val = ease.sine_in(num);
  case MyEaseFuncs.SineOut: ease_val = ease.sine_out(num);
  case MyEaseFuncs.BounceIn: ease_val = ease.bounce_in(num);
  case MyEaseFuncs.BounceOut: ease_val = ease.bounce_out(num);
  }
  return min(1.0, max(0,ease_val))
}

handleCatState :: proc(c : ^Cat) {
  easing_func_inc :: ease.bounce_out
  easing_func_dec :: ease.bounce_in

  TRANS_MULTIPLIER :: 4.0
  switch state := c.state; state {
  case .NONE:
    c.ease_timer = 0;
    if rl.IsMouseButtonPressed(rl.MouseButton.RIGHT) do c.state = .INCREASING

    c.scale = CAT_SCALE_MIN + (CAT_SCALE_MAX - CAT_SCALE_MIN) * easing_func_inc(c.ease_timer)
  case .INCREASING:
    if rl.IsMouseButtonReleased(rl.MouseButton.RIGHT) do c.state = .DECREASING
    c.ease_timer = min(1.0, c.ease_timer+rl.GetFrameTime()*TRANS_MULTIPLIER);

    c.scale = CAT_SCALE_MIN + (CAT_SCALE_MAX - CAT_SCALE_MIN) * do_easing(c.ease_increase, c.ease_timer)
  case .DECREASING:
    if c.ease_timer <= 0 do c.state = .NONE
    if rl.IsMouseButtonPressed(rl.MouseButton.RIGHT) do c.state = .INCREASING
    c.ease_timer = max(0.0, c.ease_timer-rl.GetFrameTime()*TRANS_MULTIPLIER);

    c.scale = CAT_SCALE_MIN + (CAT_SCALE_MAX - CAT_SCALE_MIN) * do_easing(c.ease_decrease, c.ease_timer)
  }
}

main :: proc() {
  c : Cat = {CAT_SCALE_MIN, .NONE, 0, .BounceIn, .BounceOut}

  rl.SetConfigFlags({.WINDOW_RESIZABLE, .VSYNC_HINT})
	rl.InitWindow(800, 600, "mini0");
	texture := rl.LoadTexture("round_cat.png")
	texturel := rl.LoadTexture("long_cat.png")
  active_texture := texture

    increase_int : i32
    decrease_int : i32
  for !rl.WindowShouldClose() {
    move := rl.GetMouseWheelMove()
    if move != 0.0 do active_texture = texture if active_texture == texturel else texturel

    rl.BeginDrawing()
    rl.ClearBackground({0, 120, 153, 255})

    handleCatState(&c);


    w := rl.MeasureText("RMB to test easing", 20)
    rl.DrawText("RMB to test easing", rl.GetScreenWidth()/2 - w/2.0, 0, 20, rl.LIGHTGRAY);
    w = rl.MeasureText("Scroll to change cat", 20)
    rl.DrawText("Scroll to change cat", rl.GetScreenWidth()/2 - w/2.0, 20, 20, rl.LIGHTGRAY);
    w = rl.MeasureText("Combo boxes control easing", 20)
    rl.DrawText("Combo boxes control easing", rl.GetScreenWidth()/2 - w/2.0, 40, 20, rl.LIGHTGRAY);

    rl.DrawText("press easefunc", 0, 0, 20, rl.LIGHTGRAY);
    rl.GuiComboBox((rl.Rectangle){ 0, 20, 150, 50 }, "LinearIn;LinearOut;QuadraticIn;QuadraticOut;CubicIn;CubicOut;SineIn;SineOut;BounceIn;BounceOut" , auto_cast &increase_int)
    c.ease_increase = auto_cast u32(increase_int);

    w = rl.MeasureText("release easefunc", 20)
    rl.DrawText("release easefunc", rl.GetScreenWidth() - w, 0, 20, rl.LIGHTGRAY);
    rl.GuiComboBox((rl.Rectangle){ f32(rl.GetScreenWidth()) - 125.0, 20, 125, 50 }, "LinearIn;LinearOut;QuadraticIn;QuadraticOut;CubicIn;CubicOut;SineIn;SineOut;BounceIn;BounceOut" , &decrease_int)
    c.ease_decrease = auto_cast u32(decrease_int);

    rl.DrawTextureEx(active_texture, {f32(rl.GetScreenWidth())/2.0, f32(rl.GetScreenHeight())/2.0} - {c.scale*f32(active_texture.width)/2.0,c.scale*f32(active_texture.height)/2.0}, 0, c.scale, rl.WHITE)

    rl.EndDrawing()
  }
}
