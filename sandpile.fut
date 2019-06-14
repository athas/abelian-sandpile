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
  unsafe ([0xffffff, 0x00bfff, 0xffd700, 0xb03060])[i32.min grains 3]


module lys : lys with text_content = () = {
  type state = {grid:[][]i32, sinks: [][]bool}
  type text_content = ()
  let init _ h w: state = {grid=replicate h (replicate w 0),
                           sinks = replicate h (replicate w false)
                                   with [h/2, w/2] = true}
  let step [n][m] (_: f32) ({grid: [n][m]i32, sinks: [n][m]bool}) =
    let maybe_drop grains sink = if sink then grains + 1 else grains
    let grid' = map2 (map2 maybe_drop) grid sinks
    in {grid = step grid', sinks}
  let resize h w _ = init 0i32 h w
  let key _ _ s = s
  let mouse (mouse_state: i32) (x: i32) (y: i32) ({grid,sinks}: state) =
    if mouse_state != 0
    then {grid, sinks = copy sinks with [y,x] = true}
    else {grid, sinks}
  let wheel _ _ s = s
  let grab_mouse = false
  let render (s: state) = map (map pixel) s.grid
  let text_format = ""
  let text_content _ _ = ()
  let text_colour _ = argb.black
}
