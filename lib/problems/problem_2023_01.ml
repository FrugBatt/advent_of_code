let year = 2023
let day = 1

module Part_1 = struct

  let get_first_int s =
    let n = String.length s in
    let rec aux i =
      if s.[i] >= '0' && s.[i] <= '9' then Char.code s.[i] - Char.code '0'
      else if i = n then -1
      else aux (i + 1)
    in aux 0

  let get_last_int s =
    let n = String.length s in
    let rec aux i =
      if s.[i] >= '0' && s.[i] <= '9' then Char.code s.[i] - Char.code '0'
      else if i = -1 then -1
      else aux (i - 1)
    in aux (n - 1)

  let run inputs =
    let first_ints = List.map get_first_int inputs in
    let last_ints = List.map get_last_int inputs in
    let vals = List.map2 (fun a b -> 10 * a + b) first_ints last_ints in
    let sum = List.fold_left (+) 0 vals in
      Result.ok (string_of_int sum)
    
end

module Part_2 = struct

  let is_digit s i =
    if s.[i] >= '0' && s.[i] <= '9' then Some (Char.code s.[i] - Char.code '0')
    else if String.starts_with (String.sub s i ((String.length s) - i)) ~prefix:"one" then Some 1
    else if String.starts_with (String.sub s i ((String.length s) - i)) ~prefix:"two" then Some 2
    else if String.starts_with (String.sub s i ((String.length s) - i)) ~prefix:"three" then Some 3
    else if String.starts_with (String.sub s i ((String.length s) - i)) ~prefix:"four" then Some 4
    else if String.starts_with (String.sub s i ((String.length s) - i)) ~prefix:"five" then Some 5
    else if String.starts_with (String.sub s i ((String.length s) - i)) ~prefix:"six" then Some 6
    else if String.starts_with (String.sub s i ((String.length s) - i)) ~prefix:"seven" then Some 7
    else if String.starts_with (String.sub s i ((String.length s) - i)) ~prefix:"eight" then Some 8
    else if String.starts_with (String.sub s i ((String.length s) - i)) ~prefix:"nine" then Some 9
    else None

  let rec get_first_int s i =
    if i = String.length s then -1
    else match is_digit s i with
      | Some n -> n
      | None -> get_first_int s (i + 1)

  let rec get_last_int s i =
    if i = -1 then -1
    else match is_digit s i with
      | Some n -> n
      | None -> get_last_int s (i - 1)

  let run inputs =
    let first_ints = List.map (fun s -> get_first_int s 0) inputs in
    let last_ints = List.map (fun s -> get_last_int s ((String.length s) - 1)) inputs in
    let vals = List.map2 (fun a b -> 10 * a + b) first_ints last_ints in
    let sum = List.fold_left (+) 0 vals in
      Result.ok (string_of_int sum)

end
