open OUnit2
open Sbctrainer
open SquadBuilder
open PlayerAccess

let csv_file = "../data/all_players.csv"

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
         ]
         @ !test_cases
  in

  run_test_tt_main suite
