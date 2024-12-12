open Csv

exception PlayerNotFound of string

let find_index header headers =
  let rec aux i = function
    | [] -> failwith ("Header '" ^ header ^ "' not found in the database.")
    | h :: t -> if h = header then i else aux (i + 1) t
  in
  aux 0 headers

let levenshtein_distance s1 s2 =
  let len_s1 = String.length s1 in
  let len_s2 = String.length s2 in
  let matrix = Array.make_matrix (len_s1 + 1) (len_s2 + 1) 0 in
  for i = 0 to len_s1 do
    matrix.(i).(0) <- i
  done;
  for j = 0 to len_s2 do
    matrix.(0).(j) <- j
  done;
  for i = 1 to len_s1 do
    for j = 1 to len_s2 do
      if s1.[i - 1] = s2.[j - 1] then matrix.(i).(j) <- matrix.(i - 1).(j - 1)
      else
        matrix.(i).(j) <-
          1
          + min
              (min matrix.(i - 1).(j) matrix.(i).(j - 1))
              matrix.(i - 1).(j - 1)
    done
  done;
  matrix.(len_s1).(len_s2)

let contains_substring haystack needle =
  let len_haystack = String.length haystack in
  let len_needle = String.length needle in
  let rec search_from i =
    if i > len_haystack - len_needle then false
    else if String.sub haystack i len_needle = needle then true
    else search_from (i + 1)
  in
  if len_needle = 0 then false else search_from 0

let suggest_similar_names csv_file player_name =
  try
    let csv = Csv.load csv_file in
    let headers = List.hd csv in
    let data = List.tl csv in
    let name_index = find_index "Name" headers in

    let lowercase_input_name =
      String.lowercase_ascii (String.trim player_name)
    in
    let exact_match =
      List.exists
        (fun row ->
          let player_name_in_db = String.trim (List.nth row name_index) in
          let lowercase_player_name_in_db =
            String.lowercase_ascii player_name_in_db
          in
          lowercase_input_name = lowercase_player_name_in_db)
        data
    in

    if exact_match then []
    else
      List.fold_left
        (fun acc row ->
          let player_name_in_db = String.trim (List.nth row name_index) in
          let lowercase_player_name_in_db =
            String.lowercase_ascii player_name_in_db
          in
          if lowercase_input_name = lowercase_player_name_in_db then acc
          else
            let distance =
              levenshtein_distance lowercase_input_name
                lowercase_player_name_in_db
            in
            if
              distance <= 2
              || contains_substring lowercase_player_name_in_db
                   lowercase_input_name
            then player_name_in_db :: acc
            else acc)
        [] data
      |> List.rev
      |> List.sort_uniq String.compare
  with
  | Sys_error _ -> failwith ("The file '" ^ csv_file ^ "' does not exist.")
  | e -> raise e

let rec get_player_attributes csv_file player_name =
  try
    let csv = Csv.load csv_file in
    let headers = List.hd csv in
    let data = List.tl csv in
    let name_index = find_index "Name" headers in
    let ovr_index = find_index "OVR" headers in
    let pac_index = find_index "PAC" headers in
    let sho_index = find_index "SHO" headers in
    let pas_index = find_index "PAS" headers in
    let dri_index = find_index "DRI" headers in
    let def_index = find_index "DEF" headers in
    let phy_index = find_index "PHY" headers in
    let rec find_player rows =
      match rows with
      | [] ->
          let error_message =
            "Player '" ^ player_name ^ "' not found in the database."
          in
          raise (PlayerNotFound error_message)
      | row :: rest ->
          if
            String.lowercase_ascii (String.trim (List.nth row name_index))
            = String.lowercase_ascii (String.trim player_name)
          then
            ( List.nth row name_index,
              List.nth row ovr_index,
              List.nth row pac_index,
              List.nth row sho_index,
              List.nth row pas_index,
              List.nth row dri_index,
              List.nth row def_index,
              List.nth row phy_index )
          else find_player rest
    in
    find_player data
  with
  | Sys_error _ -> failwith ("The file '" ^ csv_file ^ "' does not exist.")
  | e -> raise e
