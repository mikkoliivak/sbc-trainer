open Sbctrainer
open Csv
open PlayerAccess
open SquadBuilder

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

let ask_yes_no question =
  Printf.printf "%s (y/n): " question;
  let rec loop () =
    match String.lowercase_ascii (read_line ()) with
    | "y" -> true
    | "n" -> false
    | _ ->
        Printf.printf "Please enter 'y' or 'n': ";
        loop ()
  in
  loop ()

let rec search_for_player csv_file =
  print_endline "Enter player name:";
  let player_name = read_line () in
  try
    let name, ovr, pac, sho, pas, dri, def, phy =
      get_player_attributes csv_file player_name
    in
    Printf.printf
      "Name: %s, OVR: %s, PAC: %s, SHO: %s, PAS: %s, DRI: %s, DEF: %s, PHY: %s\n"
      name ovr pac sho pas dri def phy
  with
  | PlayerNotFound msg ->
      (* Print error message *)
      Printf.printf "Error: %s\n" msg;

      (* Extract possible suggestions from the message *)
      (* let suggestions = if String.contains msg ':' then let suggestions_part
         = String.split_on_char ':' msg |> List.tl |> String.concat ":" in if
         suggestions_part <> "" then String.split_on_char ',' suggestions_part
         |> List.map String.trim else [] else [] in *)
      let suggestions = suggest_similar_names csv_file player_name in

      if suggestions <> [] then (
        Printf.printf "Did you mean:\n";
        List.iteri
          (fun i suggestion -> Printf.printf "%d. %s\n" (i + 1) suggestion)
          suggestions;

        (* Prompt user to select a player *)
        Printf.printf
          "\n\
           Select a player by entering its corresponding number or press Enter \
           to continue:\n";
        match read_line () with
        | "" -> ()
        | input -> (
            try
              let index = int_of_string input - 1 in
              if index >= 0 && index < List.length suggestions then
                let selected_player = List.nth suggestions index in
                let name, ovr, pac, sho, pas, dri, def, phy =
                  get_player_attributes csv_file selected_player
                in
                Printf.printf
                  "Name: %s, OVR: %s, PAC: %s, SHO: %s, PAS: %s, DRI: %s, DEF: \
                   %s, PHY: %s\n"
                  name ovr pac sho pas dri def phy
            with _ -> ()));

      if ask_yes_no "Do you want to search for another player?" then
        search_for_player csv_file
      else Printf.printf "Returning to main menu.\n"
  | e ->
      Printf.printf "An unexpected error occurred: %s\n" (Printexc.to_string e)

let rec build_squad_interactively headers data =
  let filters = ref [] in

  if ask_yes_no "Do you want to filter by nation?" then (
    Printf.printf "Enter the nation: ";
    let nation = read_line () in
    filters := filter_by_nation nation :: !filters);

  if ask_yes_no "Do you want to filter by OVR range?" then (
    Printf.printf "Enter minimum OVR: ";
    let min_ovr = read_int () in
    Printf.printf "Enter maximum OVR: ";
    let max_ovr = read_int () in
    filters := filter_by_ovr_range min_ovr max_ovr :: !filters);

  if ask_yes_no "Do you want to filter by league?" then (
    Printf.printf "Enter the league: ";
    let league = read_line () in
    filters := filter_by_league league :: !filters);

  if ask_yes_no "Do you want to filter by club?" then (
    Printf.printf "Enter the club: ";
    let club = read_line () in
    filters := filter_by_club club :: !filters);

  let filtered_data = apply_filters !filters headers data in

  Printf.printf "Minimum number of players from the same league for synergy: ";
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

  match build_squad_result filtered_data headers formation min_league_count with
  | Ok squad ->
      Printf.printf "\nLowest-Rated Squad with chosen filters:\n\n";
      display_squad_formation headers squad;
      if ask_yes_no "Do you want to build another squad?" then
        build_squad_interactively headers data
      else Printf.printf "Returning to main menu.\n"
  | Error err ->
      Printf.printf "Error: %s\n" (pp_error err);
      if ask_yes_no "Do you want to try building another squad?" then
        build_squad_interactively headers data
      else Printf.printf "Returning to main menu.\n"

let rec main_menu headers data csv_file =
  Printf.printf "\n========== MAIN MENU ==========\n";
  Printf.printf "1. Build a Squad\n";
  Printf.printf "2. Search for a Player\n";
  Printf.printf "3. Quit\n";
  Printf.printf "Enter your choice: ";
  let choice =
    try int_of_string (read_line ())
    with Failure _ ->
      Printf.printf "Invalid input.\n";
      0
  in
  match choice with
  | 1 ->
      build_squad_interactively headers data;
      main_menu headers data csv_file
  | 2 ->
      search_for_player csv_file;
      main_menu headers data csv_file
  | 3 -> Printf.printf "Goodbye!\n"
  | _ ->
      Printf.printf "Invalid choice.\n";
      main_menu headers data csv_file

let () =
  let csv_file = "data/all_players.csv" in
  let headers, data = SquadBuilder.load_csv csv_file in
  main_menu headers data csv_file
