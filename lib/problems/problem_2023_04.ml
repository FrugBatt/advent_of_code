let year = 2023
let day = 4


let get_numbers s = List.filter_map (fun s -> if s = "" then None else Some (int_of_string s)) (String.split_on_char ' ' s)

let contains l x = List.exists (fun y -> y = x) l

let num_id win l =
  let rec aux pt = function
    | [] -> pt
    | x :: xs ->
      if contains win x then aux (pt + 1) xs
      else aux pt xs
  in aux 0 l

module Part_1 = struct

  let run inputs =
    let cropped = List.map (fun s ->
      let l = String.sub s 9 (String.length s - 9) |> String.split_on_char '|' in
        (List.hd l, List.hd @@ List.tl l)
    ) inputs in
    let winning_num = List.map (fun (win, l) -> num_id (get_numbers win) (get_numbers l)) cropped in
    let points = List.map (fun i -> if i = 0 then 0 else 1 lsl (i - 1)) winning_num in
    let sum = List.fold_left (+) 0 points in
      Result.ok @@ string_of_int sum

end

module Part_2 = struct

  let run inputs =
    let cropped = List.map (fun s ->
      let l = String.sub s 9 (String.length s - 9) |> String.split_on_char '|' in
        (List.hd l, List.hd @@ List.tl l)
    ) inputs in
    let winning_num = List.map (fun (win, l) -> num_id (get_numbers win) (get_numbers l)) cropped in
    let n = List.length winning_num in
    let copies = Array.make n 1 in
      List.iteri (fun i x ->
        for k = 1 to x do
          copies.(i + k) <- copies.(i + k) + copies.(i)
        done
      ) winning_num;
    let sum = Array.fold_left (+) 0 copies in
      Result.ok @@ string_of_int sum


end
