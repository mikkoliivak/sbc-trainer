open Csv

exception PlayerNotFound of string

let find_index header headers =
  let rec aux i = function
    | [] -> failwith ("Header '" ^ header ^ "' not found in the database.")
    | h :: t -> if h = header then i else aux (i + 1) t
  in
  aux 0 headers

let get_player_attributes csv_file player_name =
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
          raise
            (PlayerNotFound
               ("Player '" ^ player_name ^ "' not found in the database."))
      | row :: rest ->
          if
            String.lowercase_ascii (List.nth row name_index)
            = String.lowercase_ascii player_name
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
