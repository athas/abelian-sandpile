let step [n][m] (grid: [n][m]i32) : [n][m]i32 =
  let grains i j = if i >= 0 && i < n && j > 0 && j < m
                   then unsafe grid[i,j]
                   else 0
  let on_cell i j =
    let mine = grains i j
    let w = grains i (j-1)
    let e = grains i (j+1)
    let n = grains (i-1) j
    let s = grains (i+1) j
    in if mine < 4
       then mine +
            i32.bool(w>=4) + i32.bool(e>=4) +
            i32.bool(n>=4) + i32.bool(s>=4)
       else mine - 4
  in tabulate_2d n m on_cell

import "lib/github.com/diku-dk/lys/lys"
import "lib/github.com/athas/matte/colour"

let pixel (grains: i32): argb.colour =
  unsafe ([0xffffff, 0x00bfff, 0xffd700, 0xb03060, 0x000000])[i32.min grains 4]

let screen_point_to_world_point ((centre_x, centre_y): (f32,f32)) (s: f32)
                                ((sw,sh): (i32,i32)) ((ww,wh): (i32,i32))
                                ((x,y): (i32,i32)) =
  let x' = t32 ((centre_x + s * (r32 (x-ww/2) / r32 sw)) * r32 ww)
  let y' = t32 ((centre_y + s * (r32 (y-wh/2) / r32 sh)) * r32 wh)
  in (x', y')

module zoom_wrapper (M: lys) : lys with text_content = M.text_content = {
  type~ state = { inner: M.state
                , centre: (f32, f32)
                , scale: f32
                , width: i32
                , height: i32
                }

  type text_content = M.text_content

  let init seed h w : state = { inner = M.init seed h w
                              , centre = (0.5, 0.5)
                              , scale = 1
                              , width = w
                              , height = h }

  let zoom dy (s : state) =
    s with scale = f32.min 1 (s.scale * (0.99**r32 dy))

  let move (dx, dy) (s: state) =
    s with centre = (s.centre.0 + dx * 0.1 * s.scale,
                     s.centre.1 + dy * 0.1 * s.scale)

  let event (e: event) s =
    match e
    case #keydown {key} ->
      let s' = if      key == SDLK_LEFT then move (-1, 0) s
               else if key == SDLK_RIGHT then move (1, 0) s
               else if key == SDLK_UP then move (0, -1) s
               else if key == SDLK_DOWN then move (0, 1) s
               else s
      in s with inner = M.event e s'.inner
    case #mouse {buttons, x, y} ->
      let (x, y) = screen_point_to_world_point s.centre s.scale
                   (s.width, s.height) (s.width, s.height) (x,y)
      in s with inner = M.event (#mouse {buttons, x, y}) s.inner
    case #wheel {dx=_, dy} ->
      zoom dy s with inner = M.event e s.inner
    case e ->
      s with inner = M.event e s.inner

  let resize h w (s: state) = s with inner = M.resize h w s.inner
                                with width = w
                                with height = h

  let render (s: state) =
    let screen = M.render s.inner
    let pixel y x =
      let (x',y') = screen_point_to_world_point s.centre s.scale
                    (s.width, s.height) (s.width, s.height) (x,y)
      in unsafe screen[y', x']
    in tabulate_2d s.height s.width pixel

  let grab_mouse = M.grab_mouse
  let text_format = M.text_format
  let text_content fps (s: state) = M.text_content fps s.inner
  let text_colour (s: state) = M.text_colour s.inner
}

let map2' [n] [m] 'a 'b 'c (f : a -> b -> c) (as: [n]a) (bs: [m]b) : [n]c =
  map2 f as (bs :> [n]b)

module lys : lys with text_content = () = zoom_wrapper {
  type~ state = {grid:[][]i32, sinks: [][]bool}
  type text_content = ()
  let init _ h w: state = {grid=replicate h (replicate w 0),
                           sinks = replicate h (replicate w false)
                                   with [h/2, w/2] = true}
  let resize h w _ = init 0i32 h w

  let event (e: event) (s: state) =
    match e
    case #step _ ->
      let maybe_drop grains sink = if sink then 4 else grains
      in s with grid = step (map2' (map2' maybe_drop) s.grid s.sinks)
    case #mouse {buttons, x, y} ->
      if buttons != 0
      then s with sinks = (copy s.sinks with [y,x] = true)
      else s
    case _ ->
      s

  let render (s: state) = map (map pixel) s.grid

  let grab_mouse = false
  let text_format () = ""
  let text_content _ _ = ()
  let text_colour _ = argb.black
}
