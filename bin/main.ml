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

(* A helper function to read 'y'/'n' answers *)
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

let () =
  let csv_file = "data/all_players.csv" in
  let headers, data = SquadBuilder.load_csv csv_file in

  let rec main_loop () =
    (* Start building a list of filters based on user input *)
    let filters = ref [] in

    (* Ask about nation filter *)
    if ask_yes_no "Do you want to filter by nation?" then (
      Printf.printf "Enter the nation: ";
      let nation = read_line () in
      filters := filter_by_nation nation :: !filters);

    (* Ask about OVR range filter *)
    if ask_yes_no "Do you want to filter by OVR range?" then (
      Printf.printf "Enter minimum OVR: ";
      let min_ovr = read_int () in
      Printf.printf "Enter maximum OVR: ";
      let max_ovr = read_int () in
      filters := filter_by_ovr_range min_ovr max_ovr :: !filters);

    (* Ask about position filter *)
    if ask_yes_no "Do you want to filter by position?" then (
      Printf.printf "Enter the position: ";
      let pos = read_line () in
      filters := filter_by_position pos :: !filters);

    (* Ask about league filter *)
    if ask_yes_no "Do you want to filter by league?" then (
      Printf.printf "Enter the league: ";
      let league = read_line () in
      filters := filter_by_league league :: !filters);

    (* Ask about club filter *)
    if ask_yes_no "Do you want to filter by club?" then (
      Printf.printf "Enter the club: ";
      let club = read_line () in
      filters := filter_by_club club :: !filters);

    (* Now that we have all chosen filters, apply them *)
    let filtered_data = apply_filters !filters headers data in

    (* Ask for synergy requirement *)
    Printf.printf "Minimum number of players from the same league for synergy: ";
    let min_league_count = read_int () in

    (* Ask which formation *)
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

    match
      build_squad_result filtered_data headers formation min_league_count
    with
    | Ok squad ->
        Printf.printf "\nLowest-Rated Squad with chosen filters:\n\n";
        display_squad_formation headers squad;
        (* Ask if they want to build another squad *)
        if ask_yes_no "Do you want to build another squad?" then main_loop ()
        else Printf.printf "Goodbye!\n"
    | Error err ->
        Printf.printf "Error: %s\n" (pp_error err);
        (* Ask if they want to build another squad even after error *)
        if ask_yes_no "Do you want to try building another squad?" then
          main_loop ()
        else Printf.printf "Goodbye!\n"
  in

  (* Start the loop *)
  main_loop ()
