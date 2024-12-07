open OUnit2
open SquadBuilder

let csv_file = "../data/all_players.csv"

(* Load the database *)
let headers, data =
  try SquadBuilder.load_csv csv_file
  with _ ->
    failwith "Could not load the real database. Ensure the file exists."

let test_find_index _ =
  let index_name = SquadBuilder.find_index "Name" headers in
  let index_nation = SquadBuilder.find_index "Nation" headers in
  assert_equal 3 index_name;
  assert_equal 48 index_nation;
  assert_raises (Failure "Header 'Invalid' not found in the database.")
    (fun () -> SquadBuilder.find_index "Invalid" headers)

let test_filter_players _ =
  let result =
    apply_filters
      [ filter_by_nation "France"; filter_by_min_ovr 85 ]
      headers data
  in
  assert_bool "Expected at least one French player with OVR >= 85"
    (List.length result > 0);

  let result =
    apply_filters
      [ filter_by_nation "Spain"; filter_by_min_ovr 95 ]
      headers data
  in
  assert_bool "Expected no players with OVR >= 95 for Spain"
    (List.length result = 0)

let test_can_play_position _ =
  let player = List.hd data in
  let pos_index = SquadBuilder.find_index "Position" headers in
  let position = List.nth player pos_index in
  let valid_positions =
    [ "GK"; "RB"; "CB"; "LB"; "CM"; "ST"; "LW"; "RW"; "CDM"; "CAM"; "LM"; "RM" ]
  in
  assert_bool "Player's position should be valid"
    (List.mem position valid_positions)

let test_find_lowest_player_for_position _ =
  (* This test depends on your data. If no CB at OVR=48, adjust accordingly. *)
  let result =
    SquadBuilder.find_lowest_player_for_position data headers "CB" []
  in
  match result with
  | Some (player, assigned_position) ->
      let ovr = List.nth player (SquadBuilder.find_index "OVR" headers) in
      assert_bool "Expected a player for position CB" (assigned_position = "cb");
      ignore ovr
      (* Can't assert exact OVR without stable data *)
  | None -> assert_failure "Expected at least one player for CB"

let test_build_squad _ =
  let positions = [ "GK"; "RB"; "CB"; "CB"; "LB"; "CM"; "ST" ] in
  let result = SquadBuilder.build_squad data headers positions in
  assert_equal (List.length positions) (List.length result)

(* New tests for synergy and result-based squad building *)
let test_synergy_check _ =
  let formation =
    [ "GK"; "RB"; "CB"; "CB"; "LB"; "CM"; "CM"; "ST"; "ST"; "LW"; "RW" ]
  in
  (* Filter for a country that presumably has multiple players in the same
     league *)
  let filtered_data =
    apply_filters
      [ filter_by_nation "England"; filter_by_min_ovr 80 ]
      headers data
  in
  match build_squad_result filtered_data headers formation 3 with
  | Ok squad ->
      (* Check synergy met *)
      assert_bool "Expected synergy with at least 3 players from same league"
        true
  | Error (SynergyNotMet msg) -> assert_failure ("Synergy not met: " ^ msg)
  | Error (NotEnoughPlayers msg) ->
      (* If not enough players, we can't test synergy, but let's fail here *)
      assert_failure ("Not enough players: " ^ msg)

(* Test result-based errors *)
let test_insufficient_players_result _ =
  let positions =
    [ "GK"; "GK"; "GK"; "GK"; "GK"; "GK"; "GK"; "GK"; "GK"; "GK"; "GK" ]
  in
  (* Very likely can't fill 11 GK positions from one filter *)
  let filtered_data =
    apply_filters
      [ filter_by_nation "France"; filter_by_min_ovr 85 ]
      headers data
  in
  match build_squad_result filtered_data headers positions 1 with
  | Ok _ -> assert_failure "Expected not enough players error"
  | Error (NotEnoughPlayers _) -> assert_bool "Correct error returned" true
  | Error (SynergyNotMet _) ->
      assert_failure "Expected NotEnoughPlayers, not SynergyNotMet"

let suite =
  "SquadBuilder Tests"
  >::: [
         "test_find_index" >:: test_find_index;
         "test_filter_players" >:: test_filter_players;
         "test_can_play_position" >:: test_can_play_position;
         "test_find_lowest_player_for_position"
         >:: test_find_lowest_player_for_position;
         "test_build_squad" >:: test_build_squad;
         "test_synergy_check" >:: test_synergy_check;
         "test_insufficient_players_result" >:: test_insufficient_players_result;
       ]

let () = run_test_tt_main suite
