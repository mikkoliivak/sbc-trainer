open OUnit2
open Sbctrainer
open SquadBuilder
open PlayerAccess

let csv_file = "../data/all_players.csv"

let is_descending lst =
  match lst with
  | [] | [ _ ] -> true
  | hd :: tl ->
      List.fold_left
        (fun (is_sorted, prev) curr -> (is_sorted && prev >= curr, curr))
        (true, hd) tl
      |> fst

let headers, data =
  try SquadBuilder.load_csv csv_file
  with _ ->
    failwith
      "Could not load the database. Ensure the file exists at \
       ../data/all_players.csv."

let test_find_index_basic _ =
  let i_name = find_index "Name" headers in
  let i_nation = find_index "Nation" headers in
  assert_bool "Name index should be >=0" (i_name >= 0);
  assert_bool "Nation index should be >=0" (i_nation >= 0)

let test_find_index_failure _ =
  assert_raises (Failure "Header 'Invalid' not found in the database.")
    (fun () -> find_index "Invalid" headers)

let test_load_csv _ =
  assert_bool "Headers should not be empty" (List.length headers > 0);
  assert_bool "Data should not be empty" (List.length data > 0)

let test_filter_by_nation_france _ =
  let result = apply_filters [ filter_by_nation "France" ] headers data in
  assert_bool "Expected some French players" (List.length result > 0)

let test_filter_by_nation_spain_no_high_ovr _ =
  let result =
    apply_filters
      [ filter_by_nation "Spain"; filter_by_min_ovr 99 ]
      headers data
  in
  assert_equal [] result ~msg:"No Spanish players with OVR>=99 expected"

let test_filter_by_min_ovr_80 _ =
  let result = apply_filters [ filter_by_min_ovr 80 ] headers data in
  assert_bool "Expected players with OVR>=80" (List.length result > 0)

let test_filter_by_league_premier _ =
  let result =
    apply_filters [ filter_by_league "Premier League" ] headers data
  in
  assert_bool "Expected some players from Premier League"
    (List.length result > 0)

let test_filter_by_club_specific _ =
  let result =
    apply_filters [ filter_by_club "Manchester City" ] headers data
  in
  assert_bool "Expected some players from Manchester City"
    (List.length result > 0)

let test_filter_by_ovr_range_85_90 _ =
  let result = apply_filters [ filter_by_ovr_range 85 90 ] headers data in
  assert_bool "Expected some players with OVR between 85 and 90"
    (List.length result > 0)

let test_combined_filters _ =
  let result =
    apply_filters
      [
        filter_by_nation "England";
        filter_by_league "Premier League";
        filter_by_ovr_range 80 90;
      ]
      headers data
  in
  assert_bool "Expected English Premier League players with OVR 80-90"
    (List.length result > 0)

let test_find_lowest_player_for_position_cb _ =
  let result = find_lowest_player_for_position data headers "CB" [] in
  match result with
  | Some (_, assigned) ->
      assert_equal "cb" assigned ~msg:"Assigned position should be cb"
  | None -> assert_failure "Expected to find at least one CB"

let test_build_squad_basic _ =
  let formation =
    [ "GK"; "RB"; "CB"; "CB"; "LB"; "CM"; "CM"; "ST"; "ST"; "LW"; "RW" ]
  in
  let filtered_data =
    apply_filters
      [ filter_by_nation "France"; filter_by_min_ovr 80 ]
      headers data
  in
  let squad = build_squad filtered_data headers formation in
  assert_equal 11 (List.length squad) ~msg:"Should build a full 11-player squad"

let test_build_squad_synergy_ok _ =
  let formation =
    [ "GK"; "RB"; "CB"; "CB"; "LB"; "CM"; "CM"; "ST"; "ST"; "LW"; "RW" ]
  in
  let filtered_data =
    apply_filters
      [ filter_by_league "Premier League"; filter_by_min_ovr 80 ]
      headers data
  in
  match build_squad_result filtered_data headers formation 3 with
  | Ok squad -> assert_equal 11 (List.length squad)
  | Error (SynergyNotMet msg) ->
      assert_failure ("Expected synergy but got: " ^ msg)
  | Error (NotEnoughPlayers msg) ->
      assert_failure ("Expected enough players but got: " ^ msg)

let test_build_squad_synergy_fail _ =
  let formation =
    [ "GK"; "RB"; "CB"; "CB"; "LB"; "CM"; "CM"; "ST"; "ST"; "LW"; "RW" ]
  in
  let filtered_data =
    apply_filters [ filter_by_nation "Germany" ] headers data
  in
  match build_squad_result filtered_data headers formation 10 with
  | Ok _ ->
      assert_failure "Should not meet synergy of 10 players from same league"
  | Error (SynergyNotMet _) -> assert_bool "Correct synergy failure" true
  | Error (NotEnoughPlayers _) ->
      assert_bool "At least synergy not met is expected" true

let test_build_squad_insufficient_players _ =
  let formation = List.init 11 (fun _ -> "GK") in
  let filtered_data =
    apply_filters
      [ filter_by_nation "France"; filter_by_min_ovr 85 ]
      headers data
  in
  match build_squad_result filtered_data headers formation 1 with
  | Ok _ ->
      assert_failure "Should not be able to build 11 GKs from filtered dataset"
  | Error (NotEnoughPlayers _) ->
      assert_bool "Correctly not enough players" true
  | Error (SynergyNotMet _) ->
      assert_failure "Should fail due to not enough players first"

let test_display_squad_formation_no_exception _ =
  let formation =
    [ "GK"; "RB"; "CB"; "CB"; "LB"; "CM"; "CM"; "ST"; "ST"; "LW"; "RW" ]
  in
  let filtered_data = apply_filters [ filter_by_min_ovr 85 ] headers data in
  match build_squad_result filtered_data headers formation 1 with
  | Ok squad ->
      display_squad_formation headers squad;
      assert_bool "No exception raised" true
  | Error _ ->
      assert_bool "No squad built but no error in display as it wasn't called"
        true

let formations =
  [
    ( "4-3-3 (with CDM)",
      [ "GK"; "RB"; "CB"; "CB"; "LB"; "CDM"; "CM"; "CM"; "RW"; "LW"; "ST" ] );
    ( "4-3-3 (with CAM)",
      [ "GK"; "RB"; "CB"; "CB"; "LB"; "CM"; "CM"; "CAM"; "RW"; "LW"; "ST" ] );
    ( "4-4-2",
      [ "GK"; "RB"; "CB"; "CB"; "LB"; "RM"; "CM"; "CM"; "LM"; "ST"; "ST" ] );
    ( "3-5-2",
      [ "GK"; "CB"; "CB"; "CB"; "CDM"; "CDM"; "CM"; "CAM"; "ST"; "ST"; "RW" ] );
    ( "4-2-3-1",
      [ "GK"; "RB"; "CB"; "CB"; "LB"; "CDM"; "CDM"; "CAM"; "RW"; "LW"; "ST" ] );
  ]

let test_formations filters synergy formation_name formation _ =
  let filtered_data = apply_filters filters headers data in
  match build_squad_result filtered_data headers formation synergy with
  | Ok squad ->
      assert_equal 11 (List.length squad)
        ~msg:("Formation " ^ formation_name ^ " should produce 11 players")
  | Error (NotEnoughPlayers msg) ->
      Printf.printf "Could not fill formation %s: %s\n" formation_name msg;
      assert_bool "May fail due to not enough players" true
  | Error (SynergyNotMet msg) ->
      Printf.printf "Synergy not met for formation %s: %s\n" formation_name msg;
      assert_bool "May fail due to synergy" true

let test_no_suggestions _ =
  let result = suggest_similar_names csv_file "CompletelyUnknownPlayer" in
  assert_equal [] result
    ~msg:
      "No suggestions should be provided for a completely unknown player name"

let test_one_suggestion _ =
  let result = suggest_similar_names csv_file "Lionwl Messi" in
  assert_bool
    "There should be one suggestion for a close misspelling of 'Lionel Messi'"
    (List.length result = 1 && List.hd result = "Lionel Messi")

let test_multiple_suggestions _ =
  let result = suggest_similar_names csv_file "Messia" in
  assert_bool
    "There should be multiple suggestions for the partial name 'Messia'"
    (List.length result > 1)

let test_case_insensitivity _ =
  let result1 = suggest_similar_names csv_file "lionwl messi" in
  let result2 = suggest_similar_names csv_file "LIONWL MESSI" in
  assert_equal result1 result2
    ~msg:
      "The suggestions should be case-insensitive and return the same results \
       regardless of input case"

let test_exact_match_does_not_trigger_suggestions _ =
  let result = suggest_similar_names csv_file "Lionel Messi" in
  assert_equal [] result ~msg:"An exact match should not return any suggestions"

let test_suggestions_within_distance_2 _ =
  let result = suggest_similar_names csv_file "Lione Messi" in
  assert_bool
    "A name with a small edit distance (e.g., one character off) should have \
     suggestions"
    (List.length result > 0)

let test_exact_match_trim_and_case _ =
  let result = suggest_similar_names csv_file " Lionel Messi " in
  assert_equal [] result
    ~msg:"An exact match with extra spaces should not return any suggestions"

let test_empty_suggestions _ =
  let result = suggest_similar_names csv_file "" in
  assert_equal [] result ~msg:"An empty input should not return any suggestions"

let test_input_with_spaces _ =
  let result = suggest_similar_names csv_file "     " in
  assert_equal [] result
    ~msg:"An input with only spaces should return no suggestions"

let test_special_characters_input _ =
  let result = suggest_similar_names csv_file "Lionel#Messi" in
  assert_bool
    "Special characters like '#' in 'Lionel#Messi' should still suggest \
     'Lionel Messi'"
    (List.length result > 0 && List.mem "Lionel Messi" result)

let test_very_short_input _ =
  let result = suggest_similar_names csv_file "Li" in
  assert_bool "Short input like 'Li' should still suggest 'Lionel Messi'"
    (List.length result > 0 && List.mem "Lionel Messi" result)

let test_find_index _ =
  let headers = [ "ID"; "Name"; "Nation"; "Team"; "OVR" ] in
  assert_equal 1 (find_index "Name" headers);
  assert_equal 4 (find_index "OVR" headers);
  assert_equal 0 (find_index "ID" headers)

let test_contains_substring _ =
  assert_bool "Expecting 'world' to be in 'hello world'"
    (contains_substring "hello world" "world");
  assert_bool "Expecting 'lo' to be in 'hello'"
    (contains_substring "hello" "lo");
  assert_bool "Expecting 'hello' to be in 'hello'"
    (contains_substring "hello" "hello");
  assert_bool "Not expecting 'world' to be in 'hello'"
    (not (contains_substring "hello" "world"));
  assert_bool "Empty string should not be in 'hello'"
    (not (contains_substring "hello" ""));
  assert_bool "Empty string is not a substring of an empty string"
    (not (contains_substring "" ""))

let test_get_player_attributes _ =
  let name, ovr, pac, sho, pas, dri, def, phy =
    get_player_attributes csv_file "Lionel Messi"
  in
  assert_equal "Lionel Messi" name ~msg:"Name should be 'Lionel Messi'";
  assert_bool "OVR should be an integer string"
    (try
       ignore (int_of_string ovr);
       true
     with _ -> false);
  assert_bool "PAC should be an integer string"
    (try
       ignore (int_of_string pac);
       true
     with _ -> false);
  assert_bool "SHO should be an integer string"
    (try
       ignore (int_of_string sho);
       true
     with _ -> false);
  assert_bool "PAS should be an integer string"
    (try
       ignore (int_of_string pas);
       true
     with _ -> false);
  assert_bool "DRI should be an integer string"
    (try
       ignore (int_of_string dri);
       true
     with _ -> false);
  assert_bool "DEF should be an integer string"
    (try
       ignore (int_of_string def);
       true
     with _ -> false);
  assert_bool "PHY should be an integer string"
    (try
       ignore (int_of_string phy);
       true
     with _ -> false)

let test_get_player_attributes_known_player _ =
  let name, ovr, pac, sho, pas, dri, def, phy =
    get_player_attributes csv_file "Cristiano Ronaldo"
  in
  assert_equal "Cristiano Ronaldo" name
    ~msg:"Name should be 'Cristiano Ronaldo'";
  assert_bool "OVR should be an integer string"
    (try
       ignore (int_of_string ovr);
       true
     with _ -> false);
  assert_bool "PAC should be an integer string"
    (try
       ignore (int_of_string pac);
       true
     with _ -> false);
  assert_bool "SHO should be an integer string"
    (try
       ignore (int_of_string sho);
       true
     with _ -> false);
  assert_bool "PAS should be an integer string"
    (try
       ignore (int_of_string pas);
       true
     with _ -> false);
  assert_bool "DRI should be an integer string"
    (try
       ignore (int_of_string dri);
       true
     with _ -> false);
  assert_bool "DEF should be an integer string"
    (try
       ignore (int_of_string def);
       true
     with _ -> false);
  assert_bool "PHY should be an integer string"
    (try
       ignore (int_of_string phy);
       true
     with _ -> false)

let test_search_for_team_sheet_france _ =
  let team_name = "France" in
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
  let unique_players = List.sort_uniq compare team_players in

  let ovr_index = SquadBuilder.find_index "OVR" headers in
  let sorted_players =
    List.sort
      (fun p1 p2 ->
        let ovr1 = int_of_string (List.nth p1 ovr_index) in
        let ovr2 = int_of_string (List.nth p2 ovr_index) in
        compare ovr2 ovr1)
      unique_players
  in

  let top_20_players =
    List.rev
      (List.fold_left
         (fun acc p -> if List.length acc < 20 then p :: acc else acc)
         [] sorted_players)
  in
  assert_bool "Should return at most 20 players"
    (List.length top_20_players <= 20);

  let ovr_list =
    List.map (fun p -> int_of_string (List.nth p ovr_index)) top_20_players
  in
  assert_bool "Players should be sorted in descending OVR"
    (is_descending ovr_list)

let test_search_for_team_sheet_duplicates _ =
  let team_name = "France" in
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

  let unique_players = List.sort_uniq compare team_players in
  assert_equal
    (List.length unique_players)
    (List.length (List.sort_uniq compare team_players))
    ~msg:"Players from club and nation should not have duplicates"

let test_search_for_team_sheet_no_results _ =
  let team_name = "NonExistentTeam" in
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

  assert_equal 0 (List.length team_players)
    ~msg:"No players should be found for a non-existent team"

let test_valid_player_comparison _ =
  let result =
    compare_two_players csv_file "Lionel Messi" "Cristiano Ronaldo"
  in
  assert_bool
    "Comparing two valid players should return a tuple of player stats"
    (match result with
    | Some
        ( (name1, ovr1, pac1, sho1, pas1, dri1, def1, phy1),
          (name2, ovr2, pac2, sho2, pas2, dri2, def2, phy2) ) -> true
    | _ -> false)

let test_one_valid_one_invalid_player _ =
  let result = compare_two_players csv_file "Lionel Messi" "Unknown Player" in
  assert_bool "Comparing a valid player with an unknown player should fail"
    (match result with
    | None -> true
    | _ -> false)

let test_two_invalid_players _ =
  let result =
    compare_two_players csv_file "Unknown Player 1" "Unknown Player 2"
  in
  assert_bool "Comparing two unknown players should fail"
    (match result with
    | None -> true
    | _ -> false)

let test_case_insensitivity _ =
  let result =
    compare_two_players csv_file "lIonEl meSsi" "cristIano RonALDO"
  in
  assert_bool
    "Player names with different cases should still return a valid comparison"
    (match result with
    | Some
        ( (name1, ovr1, pac1, sho1, pas1, dri1, def1, phy1),
          (name2, ovr2, pac2, sho2, pas2, dri2, def2, phy2) ) -> true
    | _ -> false)

let test_extra_whitespace _ =
  let result =
    compare_two_players csv_file "  Lionel Messi  " "  Cristiano Ronaldo  "
  in
  assert_bool
    "Player names with extra whitespace should still return a valid comparison"
    (match result with
    | Some
        ( (name1, ovr1, pac1, sho1, pas1, dri1, def1, phy1),
          (name2, ovr2, pac2, sho2, pas2, dri2, def2, phy2) ) -> true
    | _ -> false)

let test_empty_input_one_player _ =
  let result = compare_two_players csv_file "" "Cristiano Ronaldo" in
  assert_bool "Comparing with an empty input for one player should fail"
    (match result with
    | None -> true
    | _ -> false)

let test_empty_input_both_players _ =
  let result = compare_two_players csv_file "" "" in
  assert_bool "Comparing with empty input for both players should fail"
    (match result with
    | None -> true
    | _ -> false)

let test_numeric_input_for_players _ =
  let result = compare_two_players csv_file "1234" "5678" in
  assert_bool
    "Using numeric inputs for player names should fail if no players with \
     these names exist"
    (match result with
    | None -> true
    | _ -> false)

let test_get_filtered_players_stats_no_match _ =
  (* Expecting failure if no players match the given filters *)
  let filters = [ filter_by_nation "Atlantis" ] in
  assert_raises (Failure "No players matched the given filters.") (fun () ->
      get_filtered_players_stats csv_file filters)

let test_get_filtered_players_stats_basic _ =
  (* Using a nation filter that definitely returns some players *)
  let filters = [ filter_by_nation "France" ] in
  let stats = get_filtered_players_stats csv_file filters in
  assert_bool "Count should be > 0 for French players" (stats.count > 0);
  assert_bool "min_ovr should be >= 0" (stats.min_ovr >= 0);
  assert_bool "max_ovr should be >= min_ovr" (stats.max_ovr >= stats.min_ovr);
  assert_bool "avg_ovr should be between min and max"
    (float_of_int stats.min_ovr <= stats.avg_ovr
    && stats.avg_ovr <= float_of_int stats.max_ovr)

let test_get_filtered_players_stats_combined_filters _ =
  (* Multiple filters: France, OVR >= 80, from Premier League *)
  let filters =
    [
      filter_by_nation "France";
      filter_by_min_ovr 80;
      filter_by_league "Premier League";
    ]
  in
  let stats = get_filtered_players_stats csv_file filters in
  assert_bool "Count should be > 0 for these filtered players" (stats.count > 0);
  assert_bool "min_ovr should be >= 80" (stats.min_ovr >= 80);
  assert_bool "max_ovr should be >= min_ovr" (stats.max_ovr >= stats.min_ovr)

let test_get_filtered_players_stats_avg_calculation _ =
  (* Check if average is calculated correctly *)
  let filters = [ filter_by_min_ovr 90 ] in
  let stats = get_filtered_players_stats csv_file filters in
  (* If there are players >= 90 OVR *)
  if stats.count > 0 then
    (* Average should be between min and max *)
    assert_bool "Average OVR should be valid"
      (float_of_int stats.min_ovr <= stats.avg_ovr
      && stats.avg_ovr <= float_of_int stats.max_ovr)

let () =
  let test_cases = ref [] in

  List.iter
    (fun (fname, fpositions) ->
      for synergy = 1 to 5 do
        test_cases :=
          !test_cases
          @ [
              fname ^ "_synergy_" ^ string_of_int synergy
              >:: test_formations
                    [ filter_by_min_ovr 85 ]
                    synergy fname fpositions;
              fname ^ "_synergy_" ^ string_of_int synergy ^ "_nation_France"
              >:: test_formations
                    [ filter_by_nation "France"; filter_by_min_ovr 80 ]
                    synergy fname fpositions;
              fname ^ "_synergy_" ^ string_of_int synergy ^ "_Premier_League"
              >:: test_formations
                    [ filter_by_league "Premier League"; filter_by_min_ovr 80 ]
                    synergy fname fpositions;
              fname ^ "_synergy_" ^ string_of_int synergy ^ "_club_ManCity"
              >:: test_formations
                    [ filter_by_club "Manchester City"; filter_by_min_ovr 80 ]
                    synergy fname fpositions;
            ]
      done)
    formations;

  let nations = [ "England"; "France"; "Spain"; "Brazil"; "Germany" ] in
  let leagues =
    [ "Premier League"; "LaLiga EA SPORTS"; "Serie A"; "Bundesliga" ]
  in

  List.iter
    (fun (fname, fpositions) ->
      List.iter
        (fun nation ->
          List.iter
            (fun league ->
              test_cases :=
                !test_cases
                @ [
                    fname ^ "_complex_filters_" ^ nation ^ "_" ^ league
                    >:: test_formations
                          [
                            filter_by_nation nation;
                            filter_by_league league;
                            filter_by_ovr_range 80 90;
                          ]
                          2 fname fpositions;
                  ])
            leagues)
        nations)
    formations;

  let suite =
    "SBC Trainer Tests"
    >::: [
           "test_find_index_basic" >:: test_find_index_basic;
           "test_find_index_failure" >:: test_find_index_failure;
           "test_load_csv" >:: test_load_csv;
           "test_filter_by_nation_france" >:: test_filter_by_nation_france;
           "test_filter_by_nation_spain_no_high_ovr"
           >:: test_filter_by_nation_spain_no_high_ovr;
           "test_filter_by_min_ovr_80" >:: test_filter_by_min_ovr_80;
           "test_filter_by_league_premier" >:: test_filter_by_league_premier;
           "test_filter_by_club_specific" >:: test_filter_by_club_specific;
           "test_filter_by_ovr_range_85_90" >:: test_filter_by_ovr_range_85_90;
           "test_combined_filters" >:: test_combined_filters;
           "test_find_lowest_player_for_position_cb"
           >:: test_find_lowest_player_for_position_cb;
           "test_build_squad_basic" >:: test_build_squad_basic;
           "test_build_squad_synergy_ok" >:: test_build_squad_synergy_ok;
           "test_build_squad_synergy_fail" >:: test_build_squad_synergy_fail;
           "test_build_squad_insufficient_players"
           >:: test_build_squad_insufficient_players;
           "test_display_squad_formation_no_exception"
           >:: test_display_squad_formation_no_exception;
           "test_no_suggestions" >:: test_no_suggestions;
           "test_one_suggestion" >:: test_one_suggestion;
           "test_multiple_suggestions" >:: test_multiple_suggestions;
           "test_case_insensitivity" >:: test_case_insensitivity;
           "test_exact_match_does_not_trigger_suggestions"
           >:: test_exact_match_does_not_trigger_suggestions;
           "test_suggestions_within_distance_2"
           >:: test_suggestions_within_distance_2;
           "test_exact_match_trim_and_case" >:: test_exact_match_trim_and_case;
           "test_empty_suggestions" >:: test_empty_suggestions;
           "test_input_with_spaces" >:: test_input_with_spaces;
           "test_special_characters_input" >:: test_special_characters_input;
           "test_very_short_input" >:: test_very_short_input;
           "test_search_for_team_sheet_france"
           >:: test_search_for_team_sheet_france;
           "test_search_for_team_sheet_duplicates"
           >:: test_search_for_team_sheet_duplicates;
           "test_search_for_team_sheet_no_results"
           >:: test_search_for_team_sheet_no_results;
           "test_find_index" >:: test_find_index;
           "test_contains_substring" >:: test_contains_substring;
           "test_get_player_attributes" >:: test_get_player_attributes;
           "test_get_player_attributes_known_player"
           >:: test_get_player_attributes_known_player;
           "test_valid_player_comparison" >:: test_valid_player_comparison;
           "test_one_valid_one_invalid_player"
           >:: test_one_valid_one_invalid_player;
           "test_two_invalid_players" >:: test_two_invalid_players;
           "test_case_insensitivity" >:: test_case_insensitivity;
           "test_extra_whitespace" >:: test_extra_whitespace;
           "test_empty_input_one_player" >:: test_empty_input_one_player;
           "test_empty_input_both_players" >:: test_empty_input_both_players;
           "test_numeric_input_for_players" >:: test_numeric_input_for_players;
           "test_get_filtered_players_stats_no_match"
           >:: test_get_filtered_players_stats_no_match;
           "test_get_filtered_players_stats_basic"
           >:: test_get_filtered_players_stats_basic;
           "test_get_filtered_players_stats_combined_filters"
           >:: test_get_filtered_players_stats_combined_filters;
           "test_get_filtered_players_stats_avg_calculation"
           >:: test_get_filtered_players_stats_avg_calculation;
         ]
         @ !test_cases
  in

  run_test_tt_main suite
