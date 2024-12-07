open SquadBuilder

(* Define formations *)
let formations =
  [
    ( "4-3-3 (with CDM)",
      [ "GK"; "RB"; "CB"; "CB"; "LB"; "CDM"; "CM"; "CM"; "RW"; "LW"; "ST" ] );
    ( "4-3-3 (with CAM)",
      [ "GK"; "RB"; "CB"; "CB"; "LB"; "CM"; "CM"; "CAM"; "RW"; "LW"; "ST" ] );
    ( "3-5-2",
      [ "GK"; "CB"; "CB"; "CB"; "CDM"; "CDM"; "CM"; "CAM"; "ST"; "ST"; "RW" ] );
    ( "4-2-3-1",
      [ "GK"; "RB"; "CB"; "CB"; "LB"; "CDM"; "CDM"; "CAM"; "RW"; "LW"; "ST" ] );
    ( "4-4-2",
      [ "GK"; "RB"; "CB"; "CB"; "LB"; "RM"; "CM"; "CM"; "LM"; "ST"; "ST" ] );
    ( "4-3-2-1",
      [ "GK"; "RB"; "CB"; "CB"; "LB"; "CM"; "CM"; "CM"; "RF"; "LF"; "ST" ] );
    ( "4-5-1",
      [ "GK"; "RB"; "CB"; "CB"; "LB"; "CDM"; "CM"; "CM"; "RM"; "LM"; "ST" ] );
    ( "3-4-3",
      [ "GK"; "CB"; "CB"; "CB"; "RM"; "CM"; "CM"; "LM"; "RW"; "LW"; "ST" ] );
    ( "4-1-3-2",
      [ "GK"; "RB"; "CB"; "CB"; "LB"; "CDM"; "RM"; "CM"; "LM"; "ST"; "ST" ] );
  ]

let () =
  let csv_file = "data/all_players.csv" in
  Printf.printf "Enter the nation to filter by: ";
  let nation = read_line () in
  Printf.printf "Enter the minimum OVR to filter by: ";
  let min_ovr = read_int () in
  Printf.printf "Minimum number of players from the same league: ";
  let min_league_count = read_int () in
  Printf.printf "Choose a formation:\n";
  List.iteri
    (fun i (name, _) -> Printf.printf "%d. %s\n" (i + 1) name)
    formations;
  let choice = read_int () in
  let formation =
    if choice >= 1 && choice <= List.length formations then
      snd (List.nth formations (choice - 1))
    else failwith "Invalid formation choice."
  in
  let headers, data = SquadBuilder.load_csv csv_file in

  (* Applying filters using the new pipeline approach *)
  let filters = [ filter_by_nation nation; filter_by_min_ovr min_ovr ] in
  let filtered_data = apply_filters filters headers data in

  match build_squad_result filtered_data headers formation min_league_count with
  | Ok squad ->
      Printf.printf "\nLowest-Rated Squad for Nation: %s, Minimum OVR: %d\n\n"
        nation min_ovr;
      display_squad headers squad
  | Error err -> Printf.printf "Error: %s\n" (pp_error err)
