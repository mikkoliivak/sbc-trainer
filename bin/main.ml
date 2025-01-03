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

(** [player_career_summary csv_file player_name] prints a summary of the
    player's career, including the teams played for and the positions played. *)
let player_career_summary csv_file player_name =
  let _, _, _, _, _, _, _, _ = get_player_attributes csv_file player_name in
  let headers, data = SquadBuilder.load_csv csv_file in
  let team_index = find_index "Team" headers in
  let position_index = find_index "Position" headers in

  let player_records =
    List.filter
      (fun row ->
        String.lowercase_ascii (List.nth row (find_index "Name" headers))
        = String.lowercase_ascii player_name)
      data
  in

  let career_teams =
    List.fold_left
      (fun acc row ->
        let team = List.nth row team_index in
        if List.mem team acc then acc else team :: acc)
      [] player_records
  in

  let positions_played =
    List.fold_left
      (fun acc row ->
        let pos = List.nth row position_index in
        if List.mem pos acc then acc else pos :: acc)
      [] player_records
  in

  print_endline "";
  Printf.printf "Career Summary for %s:\n" player_name;
  print_endline "";
  Printf.printf "Teams played for: %s\n" (String.concat ", " career_teams);
  Printf.printf "Positions played: %s\n" (String.concat ", " positions_played);
  print_endline ""

(** [ask_yes_no question] prompts the user to answer yes (y) or no (n) to the
    given [question] and returns [true] for yes and [false] for no. *)
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

(** [search_for_player csv_file] allows the user to search for a player by name
    in the [csv_file] and prints their attributes and career summary. If the
    player is not found, it suggests similar names. *)
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
    player_career_summary csv_file name;
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

(** [build_squad_interactively headers data] allows the user to interactively
    create a squad by applying filters and selecting a formation. *)
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

(** [search_for_team_sheet headers data] allows the user to search for a team
    sheet by team name and displays the top 20 highest OVR players for that
    team. *)
let rec search_for_team_sheet headers data =
  Printf.printf
    "Enter the name of the team (club or national team) to search for: ";
  let team_name = read_line () in

  let compare_players p1 p2 =
    String.compare
      (List.nth p1 (SquadBuilder.find_index "Name" headers))
      (List.nth p2 (SquadBuilder.find_index "Name" headers))
  in

  let players_from_club =
    SquadBuilder.apply_filters
      [ SquadBuilder.filter_by_club team_name ]
      headers data
  in

  let players_from_nation =
    SquadBuilder.apply_filters
      [ SquadBuilder.filter_by_nation team_name ]
      headers data
  in

  let team_players = players_from_club @ players_from_nation in

  let unique_players = List.sort_uniq compare_players team_players in

  let ovr_index = SquadBuilder.find_index "OVR" headers in
  let sorted_players =
    List.sort
      (fun p1 p2 ->
        let ovr1 = int_of_string (List.nth p1 ovr_index) in
        let ovr2 = int_of_string (List.nth p2 ovr_index) in
        if ovr2 > ovr1 then 1 else if ovr2 < ovr1 then -1 else 0)
      unique_players
  in

  let top_20_players =
    List.fold_left
      (fun acc player -> if List.length acc < 20 then player :: acc else acc)
      [] sorted_players
    |> List.rev
  in

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

(** [compare_players_menu ()] allows the user to compare two players
    side-by-side by entering their names and viewing their attributes. *)
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

(** [get_filters_from_user ()] prompts the user to apply filters interactively
    and returns a list of filters to be applied to the player data. *)
let rec get_filters_from_user () =
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

  !filters

(** [view_filtered_player_stats_interactively csv_file] allows the user to view
    statistics for filtered players interactively by applying filters and
    displaying the count, min OVR, max OVR, and average OVR. *)
let rec view_filtered_player_stats_interactively csv_file =
  Printf.printf "\n========== FILTERED PLAYER STATISTICS ==========\n";
  Printf.printf
    "You will be asked to apply the same filters as when building a squad.\n\
     After applying filters, the program will show the count, min OVR, max \
     OVR, and avg OVR.\n\n";
  let filters = get_filters_from_user () in
  try
    let stats = get_filtered_players_stats csv_file filters in
    Printf.printf "\nStats for filtered players:\n";
    Printf.printf "Count of players: %d\n" stats.count;
    Printf.printf "Minimum OVR: %d\n" stats.min_ovr;
    Printf.printf "Maximum OVR: %d\n" stats.max_ovr;
    Printf.printf "Average OVR: %.2f\n" stats.avg_ovr
  with e ->
    Printf.printf "An unexpected error occurred: %s\n" (Printexc.to_string e);

    if ask_yes_no "Do you want to view another filtered player statistic?" then
      view_filtered_player_stats_interactively csv_file
    else Printf.printf "Returning to main menu.\n"

(** [main_menu headers data csv_file] displays the main menu for the application
    and allows users to navigate through different options like squad building,
    player search, team search, and player comparison. *)
let rec main_menu headers data csv_file =
  Printf.printf "\n========== MAIN MENU ==========\n";
  Printf.printf "1. Build a Squad\n";
  Printf.printf "2. Search for a Player\n";
  Printf.printf "3. Search for a Team Sheet\n";
  Printf.printf "4. Compare Players\n";
  Printf.printf "5. View Filtered Player Statistics\n";
  Printf.printf "6. Quit\n";
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
      | Some 5 ->
          view_filtered_player_stats_interactively csv_file;
          main_menu headers data csv_file
      | Some 6 -> Printf.printf "Goodbye!\n"
      | Some _ ->
          Printf.printf "Invalid choice.\n";
          main_menu headers data csv_file)

let () =
  let csv_file = "data/all_players.csv" in
  let headers, data = SquadBuilder.load_csv csv_file in
  main_menu headers data csv_file
