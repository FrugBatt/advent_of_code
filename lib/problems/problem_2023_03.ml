let year = 2023
let day = 3

type schematic = char array array
type value = {i: int; j: int; v: int}

let get_schematic input : schematic =
  let list_list = List.map (fun s -> s |> String.to_seq |> List.of_seq) input in
  let arr_list = List.map (fun s -> Array.of_list s) list_list in
  let arr = Array.of_list arr_list in
    arr

let is_digit c = c >= '0' && c <= '9'
let number_len i = (int_of_float @@ log10 @@ float_of_int i) + 1
let is_special c = not (is_digit c) && c <> '.'

let get_number schematic i j : int * int =
  let m = Array.length schematic.(0) in
  let rec aux j value =
    if j >= m  || not (is_digit schematic.(i).(j)) then (value, j)
    else aux (j+1) (value * 10 + (Char.code schematic.(i).(j) - 48))
  in aux j 0

let get_values schematic : value list =
  let n = Array.length schematic in
  let m = Array.length schematic.(0) in
  let rec aux i j acc =
    if i >= n then acc
    else if j >= m then aux (i+1) 0 acc
    else
      let v = schematic.(i).(j) in
      if is_digit v then
        let value, j' = get_number schematic i j in
          aux i j' ({i; j; v=value}::acc)
      else aux i (j+1) acc
  in
  aux 0 0 []

module Part_1 = struct

  let is_valid schematic value : bool =
    let n = Array.length schematic in
    let m = Array.length schematic.(0) in
    let len = number_len value.v in
    let b = ref false in
    (* i = -1 *)
    if value.i > 0 then
      for j = value.j - 1 to value.j + len do
        if j < 0 || j >= m then ()
        else if is_special schematic.(value.i - 1).(j) then b := true
      done;
    (* i = 0 *)
    if value.j > 0 then if is_special schematic.(value.i).(value.j - 1) then b := true;
    if value.j + len < m then if is_special schematic.(value.i).(value.j + len) then b := true;
    (* i = 1 *)
    if value.i + 1 < n then
      for j = value.j - 1 to value.j + len do
        if j < 0 || j >= m then ()
        else if is_special schematic.(value.i + 1).(j) then b := true
      done;
    !b

  let run inputs =
    let schematic = get_schematic inputs in
    let values = get_values schematic in
    let valid_values = List.filter (is_valid schematic) values in
    let sum = List.fold_left (fun acc v -> acc + v.v) 0 valid_values in
      Result.Ok (sum |> string_of_int)

end

module Part_2 = struct

  let is_neighbour schematic value i j =
    let n = Array.length schematic in
    let m = Array.length schematic.(0) in
      if i < 0 || i >= n || j < 0 || j >= m then false
      else let len = number_len value.v in
        if i = value.i then
          if j = value.j - 1 || j = value.j + len then true
          else false
        else if i = value.i - 1 || i = value.i + 1 then
          if j >= value.j - 1 && j <= value.j + len then true
          else false
        else false

  let get_gears schematic : (int*int) list =
    let n = Array.length schematic in
    let m = Array.length schematic.(0) in
    let rec aux i j acc =
      if i >= n then acc
      else if j >= m then aux (i+1) 0 acc
      else
        let v = schematic.(i).(j) in
        if v = '*' then aux i (j+1) ((i, j)::acc)
        else aux i (j+1) acc
    in
    aux 0 0 []
  
  let run inputs =
    let schematic = get_schematic inputs in
    let gears = get_gears schematic in
    let values = get_values schematic in
    let gear_ratios = List.filter_map (fun (i, j) ->
      let neighbours = List.filter (fun v -> is_neighbour schematic v i j) values in
      if List.length neighbours = 2 then
        let v1, v2 = List.nth neighbours 0, List.nth neighbours 1 in Some (v1.v * v2.v)
      else None
    ) gears in
    let sum = List.fold_left (fun acc v -> acc + v) 0 gear_ratios in
      Result.Ok (sum |> string_of_int)


end
