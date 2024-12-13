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
      name ovr pac sho pas dri def phy;
    if ask_yes_no "Do you want to search for another player?" then
      search_for_player csv_file
  with
  | PlayerNotFound msg ->
      Printf.printf "Error: %s\n" msg;

      let suggestions = suggest_similar_names csv_file player_name in

      if suggestions <> [] then (
        Printf.printf "Did you mean:\n";
        List.iteri
          (fun i suggestion -> Printf.printf "%d. %s\n" (i + 1) suggestion)
          suggestions;

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

  let rec get_min_league_count () =
    Printf.printf "Minimum number of players from the same league for synergy: ";
    match read_line () with
    | "" -> get_min_league_count ()
    | input -> (
        match int_of_string_opt input with
        | None ->
            Printf.printf "Invalid input. Please enter a valid number.\n";
            get_min_league_count ()
        | Some n -> n)
  in
  let min_league_count = get_min_league_count () in

  let rec get_formation_choice () =
    Printf.printf "Choose a formation:\n";
    List.iteri
      (fun i (name, _) -> Printf.printf "%d. %s\n" (i + 1) name)
      formations;
    match read_line () with
    | "" -> get_formation_choice ()
    | input -> (
        match int_of_string_opt input with
        | None ->
            Printf.printf "Invalid input. Please enter a valid number.\n";
            get_formation_choice ()
        | Some n when n >= 1 && n <= List.length formations ->
            snd (List.nth formations (n - 1))
        | _ ->
            Printf.printf "Invalid formation choice.\n";
            get_formation_choice ())
  in
  let formation = get_formation_choice () in

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

let rec search_for_team_sheet headers data =
  Printf.printf
    "Enter the name of the team (club or national team) to search for: ";
  let team_name = read_line () in

  (* Custom player comparator by Name *)
  let compare_players p1 p2 =
    String.compare
      (List.nth p1 (SquadBuilder.find_index "Name" headers))
      (List.nth p2 (SquadBuilder.find_index "Name" headers))
  in

  (* Filter for club players *)
  let players_from_club =
    SquadBuilder.apply_filters
      [ SquadBuilder.filter_by_club team_name ]
      headers data
  in

  (* Filter for nation players *)
  let players_from_nation =
    SquadBuilder.apply_filters
      [ SquadBuilder.filter_by_nation team_name ]
      headers data
  in

  (* Combine and remove duplicates from the player list *)
  let team_players = players_from_club @ players_from_nation in

  (* Sort and remove duplicates from the combined list of players *)
  let unique_players = List.sort_uniq compare_players team_players in

  (* Sort players by OVR descending *)
  let ovr_index = SquadBuilder.find_index "OVR" headers in
  let sorted_players =
    List.sort
      (fun p1 p2 ->
        let ovr1 = int_of_string (List.nth p1 ovr_index) in
        let ovr2 = int_of_string (List.nth p2 ovr_index) in
        if ovr2 > ovr1 then 1 else if ovr2 < ovr1 then -1 else 0)
      unique_players
  in

  (* Take only the top 20 players *)
  let top_20_players =
    List.fold_left
      (fun acc player -> if List.length acc < 20 then player :: acc else acc)
      [] sorted_players
    |> List.rev
  in

  (* If no players are found, show an error and prompt to search again *)
  if List.length top_20_players = 0 then (
    Printf.printf "No players found for the team '%s'.\n" team_name;
    if ask_yes_no "Do you want to search for another team sheet?" then
      search_for_team_sheet headers data
    else Printf.printf "Returning to main menu.\n")
  else (
    Printf.printf "\nTeam Sheet for '%s' (Top 20 Highest OVR):\n\n" team_name;
    SquadBuilder.display_squad_formation headers
      (List.map (fun p -> (p, "")) top_20_players);
    if ask_yes_no "Do you want to search for another team sheet?" then
      search_for_team_sheet headers data
    else Printf.printf "Returning to main menu.\n")

let compare_players_menu () =
  let csv_file = "data/all_players.csv" in
  Printf.printf "\n========== PLAYER COMPARISON ==========\n";
  Printf.printf "Enter the name of the first player: ";
  let player1 = read_line () in
  Printf.printf "Enter the name of the second player: ";
  let player2 = read_line () in
  try
    match compare_two_players csv_file player1 player2 with
    | Some
        ( (name1, ovr1, pac1, sho1, pas1, dri1, def1, phy1),
          (name2, ovr2, pac2, sho2, pas2, dri2, def2, phy2) ) ->
        Printf.printf
          "\n===================== PLAYER COMPARISON =====================\n";
        Printf.printf "| Stat       |  %-15s |  %-15s |\n" name1 name2;
        Printf.printf
          "------------------------------------------------------------\n";
        Printf.printf "| OVR        |  %-15s |  %-15s |\n" ovr1 ovr2;
        Printf.printf "| PAC        |  %-15s |  %-15s |\n" pac1 pac2;
        Printf.printf "| SHO        |  %-15s |  %-15s |\n" sho1 sho2;
        Printf.printf "| PAS        |  %-15s |  %-15s |\n" pas1 pas2;
        Printf.printf "| DRI        |  %-15s |  %-15s |\n" dri1 dri2;
        Printf.printf "| DEF        |  %-15s |  %-15s |\n" def1 def2;
        Printf.printf "| PHY        |  %-15s |  %-15s |\n" phy1 phy2;
        Printf.printf
          "------------------------------------------------------------\n"
    | None -> Printf.printf "Unable to compare players.\n"
  with PlayerNotFound msg -> Printf.printf "Error: %s\n" msg

let rec main_menu headers data csv_file =
  Printf.printf "\n========== MAIN MENU ==========\n";
  Printf.printf "1. Build a Squad\n";
  Printf.printf "2. Search for a Player\n";
  Printf.printf "3. Search for a Team Sheet\n";
  Printf.printf "4. Compare Players\n";
  Printf.printf "5. Quit\n";
  Printf.printf "Enter your choice: ";
  match read_line () with
  | "" -> main_menu headers data csv_file
  | input -> (
      match int_of_string_opt input with
      | None ->
          Printf.printf "Invalid input. Please enter a valid number.\n";
          main_menu headers data csv_file
      | Some 1 ->
          build_squad_interactively headers data;
          main_menu headers data csv_file
      | Some 2 ->
          search_for_player csv_file;
          main_menu headers data csv_file
      | Some 3 ->
          search_for_team_sheet headers data;
          main_menu headers data csv_file
      | Some 4 ->
          compare_players_menu ();
          main_menu headers data csv_file
      | Some 5 -> Printf.printf "Goodbye!\n"
      | Some _ ->
          Printf.printf "Invalid choice.\n";
          main_menu headers data csv_file)

let () =
  let csv_file = "data/all_players.csv" in
  let headers, data = SquadBuilder.load_csv csv_file in
  main_menu headers data csv_file
