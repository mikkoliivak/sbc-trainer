open Csv

exception InsufficientPlayers of string

type player = string list
type filter = headers:string list -> player -> bool

let find_index header headers =
  let rec aux i = function
    | [] -> failwith ("Header '" ^ header ^ "' not found in the database.")
    | h :: t -> if h = header then i else aux (i + 1) t
  in
  aux 0 headers

let load_csv file =
  let csv = Csv.load file in
  let headers = List.hd csv in
  let data = List.tl csv in
  (headers, data)

(** Generic filter application *)
let apply_filters (filters : filter list) headers data =
  List.filter (fun p -> List.for_all (fun f -> f ~headers p) filters) data

(** Specific filters *)
let filter_by_nation nation ~headers p =
  let nation_index = find_index "Nation" headers in
  String.lowercase_ascii (List.nth p nation_index)
  = String.lowercase_ascii nation

let filter_by_min_ovr min_ovr ~headers p =
  let ovr_index = find_index "OVR" headers in
  int_of_string (List.nth p ovr_index) >= min_ovr

(** Position checking *)
let can_play_position player headers position =
  let pos_index = find_index "Position" headers in
  let alt_pos_index = find_index "Alternative positions" headers in
  let player_pos = String.lowercase_ascii (List.nth player pos_index) in
  let alt_positions =
    String.split_on_char ','
      (String.lowercase_ascii (List.nth player alt_pos_index))
    |> List.map String.trim
  in
  let desired_pos = String.lowercase_ascii position in
  if desired_pos = player_pos then player_pos
  else if List.mem desired_pos alt_positions then desired_pos
  else ""

(** Generic player finding function with a comparison strategy *)
let find_player_for_position ~compare data headers position used_players =
  let ovr_index = find_index "OVR" headers in
  let candidates =
    List.filter
      (fun row ->
        let assigned_pos = can_play_position row headers position in
        assigned_pos <> "" && not (List.memq row used_players))
      data
  in
  match candidates with
  | [] -> None
  | _ ->
      let selected_player =
        List.fold_left
          (fun acc row ->
            let ovr = int_of_string (List.nth row ovr_index) in
            let acc_ovr = int_of_string (List.nth acc ovr_index) in
            if compare ovr acc_ovr then row else acc)
          (List.hd candidates) candidates
      in
      Some (selected_player, can_play_position selected_player headers position)

(** Convenience functions for lowest/highest OVR selection *)
let find_lowest_player_for_position = find_player_for_position ~compare:( < )
(* Example for highest: let find_highest_player_for_position =
   find_player_for_position ~compare:(>) *)

(** Original squad building function (throws exception) *)
let build_squad data headers positions =
  let rec allocate_positions positions players acc used_players =
    match positions with
    | [] -> List.rev acc
    | pos :: rest -> (
        match
          find_lowest_player_for_position players headers pos used_players
        with
        | None ->
            raise
              (InsufficientPlayers
                 ("Not enough players for position: " ^ pos ^ "."))
        | Some (player, assigned_position) ->
            let remaining_players =
              List.filter (fun row -> row != player) players
            in
            allocate_positions rest remaining_players
              ((player, assigned_position) :: acc)
              (player :: used_players))
  in
  allocate_positions positions data [] []

(** Result-based build_squad function *)
type build_error =
  | NotEnoughPlayers of string
  | SynergyNotMet of string

let pp_error = function
  | NotEnoughPlayers msg -> "Not enough players: " ^ msg
  | SynergyNotMet msg -> "Synergy not met: " ^ msg

(** Check synergy: e.g., ensure at least min_league_count players share the same
    league *)
let check_synergy squad headers min_league_count =
  let league_index = find_index "League" headers in
  let league_counts = Hashtbl.create 16 in
  List.iter
    (fun (player, _) ->
      let league = List.nth player league_index in
      let count = try Hashtbl.find league_counts league with Not_found -> 0 in
      Hashtbl.replace league_counts league (count + 1))
    squad;
  Hashtbl.fold
    (fun _ count acc -> acc || count >= min_league_count)
    league_counts false

let build_squad_result data headers positions min_league_count =
  (* Try building the squad using the old logic, but catch the exception *)
  try
    let squad = build_squad data headers positions in
    if check_synergy squad headers min_league_count then Ok squad
    else
      Error
        (SynergyNotMet
           ("Requires at least "
           ^ string_of_int min_league_count
           ^ " players from the same league."))
  with InsufficientPlayers msg -> Error (NotEnoughPlayers msg)

let display_squad headers squad =
  let name_index = find_index "Name" headers in
  let ovr_index = find_index "OVR" headers in
  let league_index = find_index "League" headers in
  List.iter
    (fun (player, assigned_position) ->
      Printf.printf "Name: %s, Position: %s, OVR: %s, League: %s\n"
        (List.nth player name_index)
        (String.uppercase_ascii assigned_position)
        (List.nth player ovr_index)
        (List.nth player league_index))
    squad
