let year = 2023
let day = 2

type color =
  | Red
  | Green
  | Blue

type pick = {red: int; green: int; blue: int}
type game = {id: int; picks: pick list}

let get_pick s =
  let color, l =
    if String.ends_with s ~suffix:"red" then Red, 3
    else if String.ends_with s ~suffix:"green" then Green, 5
    else if String.ends_with s ~suffix:"blue" then Blue, 4
    else failwith "Invalid color"
  in
  let n = String.length s in
  let amout = String.sub s 1 (n - l - 2) |> int_of_string in
    (color, amout)

let get_picks s =
  let red = ref 0 and green = ref 0 and blue = ref 0 in
  let picks_s = String.split_on_char ',' s in
  let picks = List.map get_pick picks_s in
  List.iter (fun (color, amount) ->
    match color with
    | Red -> red := amount
    | Green -> green := amount
    | Blue -> blue := amount
  ) picks;
  {red = !red; green = !green; blue = !blue}

let get_game input =
  let n = String.length input in
  let sep = String.index input ':' in
  let id = String.sub input 5 (sep - 5) |> int_of_string in
  let picks_s = String.sub input (sep + 1) (n - sep - 1) |> String.split_on_char ';' in
    {id = id; picks = List.map get_picks picks_s}


module Part_1 = struct

  let run inputs =
    let games = List.map get_game inputs in
    let relevant_ids = List.filter_map (fun g ->
      if List.for_all (fun pick -> pick.red <= 12 && pick.green <= 13 && pick.blue <= 14) g.picks then Some g.id
      else None
    ) games in
    let sum = List.fold_left (+) 0 relevant_ids in
      Result.Ok (string_of_int sum)

end

module Part_2 = struct

  let run inputs =
    let games = List.map get_game inputs in
    let max_pick p1 p2 = {red = max p1.red p2.red; green = max p1.green p2.green; blue = max p1.blue p2.blue} in
    let max_picks = List.map (fun g -> List.fold_left max_pick {red = 0; green = 0; blue = 0} g.picks) games in
    let power_games = List.map (fun p -> p.red * p.green * p.blue) max_picks in
    let sum = List.fold_left (+) 0 power_games in
    Result.Ok (string_of_int sum)

end
