type player = string list
(** Represents a single player (a row from the CSV). *)

type filter = headers:string list -> player -> bool
(** A filter function takes headers and a player, and returns true if the player
    passes the filter. *)

exception InsufficientPlayers of string
(** Exception raised when there are not enough players for a required position. *)

(** Error types returned by the result-based squad building function. *)
type build_error =
  | NotEnoughPlayers of string
  | SynergyNotMet of string

val pp_error : build_error -> string
(** [pp_error err] returns a human-readable string for the given [err]. *)

val find_index : string -> string list -> int
(** [find_index header headers] returns the index of [header] in [headers] or
    raises Failure if not found. *)

val load_csv : string -> string list * string list list
(** [load_csv file] loads the CSV file specified by [file] and returns a tuple
    (headers, data_rows). *)

val apply_filters :
  filter list -> string list -> string list list -> string list list
(** [apply_filters filters headers data] applies a list of filters to the player
    data. Only players passing all filters are included in the result. *)

(** Filters **)

val filter_by_nation : string -> headers:string list -> player -> bool
(** [filter_by_nation nation ~headers player] returns true if [player]'s nation
    matches [nation]. *)

val filter_by_min_ovr : int -> headers:string list -> player -> bool
(** [filter_by_min_ovr min_ovr ~headers player] returns true if [player]'s OVR
    >= [min_ovr]. *)

val filter_by_position : string -> headers:string list -> player -> bool
(** [filter_by_position position ~headers player] returns true if [player] can
    play [position]. *)

val filter_by_league : string -> headers:string list -> player -> bool
(** [filter_by_league league ~headers player] returns true if [player]'s league
    matches [league]. *)

val filter_by_club : string -> headers:string list -> player -> bool
(** [filter_by_club club ~headers player] returns true if [player]'s club
    matches [club]. *)

val filter_by_ovr_range : int -> int -> headers:string list -> player -> bool
(** [filter_by_ovr_range min_ovr max_ovr ~headers player] returns true if
    [player]'s OVR is between [min_ovr] and [max_ovr]. *)

(** Squad Building **)

val can_play_position : player -> string list -> string -> string
(** [can_play_position player headers position] returns the position string if
    the player can play [position], otherwise "". *)

val find_player_for_position :
  compare:(int -> int -> bool) ->
  string list list ->
  string list ->
  string ->
  player list ->
  (player * string) option
(** [find_player_for_position ~compare data headers position used_players] finds
    a player who can play [position], chosen by a comparison strategy on OVR.
    Returns Some (player, assigned_pos) or None if none found. *)

val find_lowest_player_for_position :
  string list list ->
  string list ->
  string ->
  player list ->
  (player * string) option
(** [find_lowest_player_for_position data headers position used_players]
    specifically finds the lowest OVR player who can play [position]. *)

val build_squad :
  string list list -> string list -> string list -> (player * string) list
(** [build_squad data headers positions] builds a squad using the lowest OVR
    player that can fill each position in [positions]. Raises
    InsufficientPlayers if it cannot find enough players. *)

val build_squad_result :
  string list list ->
  string list ->
  string list ->
  int ->
  ((player * string) list, build_error) result
(** [build_squad_result data headers positions min_league_count] builds a squad
    and also checks if there's at least [min_league_count] players from the same
    league. Returns Ok squad or Error if not possible. *)

(** Display **)

val display_squad_formation : string list -> (player * string) list -> unit
(** [display_squad_formation headers squad] prints the squad in a team-sheet
    style formation, grouping them by position lines (GK, Defense, Midfield,
    Attack). *)
