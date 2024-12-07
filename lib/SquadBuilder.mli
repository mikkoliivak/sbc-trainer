type player = string list
(** The type representing a single player entry from the CSV, i.e. a row of
    strings. *)

type filter = headers:string list -> player -> bool
(** A type representing a filter function that checks if a player passes certain
    criteria. *)

exception InsufficientPlayers of string
(** Exception raised when there are not enough players to fill a requested
    position. *)

(** Error types returned by result-based squad building functions. *)
type build_error =
  | NotEnoughPlayers of string
  | SynergyNotMet of string

val pp_error : build_error -> string
(** [pp_error err] returns a printable string for the given build error. *)

val find_index : string -> string list -> int
(** [find_index header headers] returns the index of [header] in the [headers]
    list, or raises [Failure] if not found. *)

val load_csv : string -> string list * string list list
(** [load_csv file] loads the CSV file, returning a tuple of (headers,
    data_rows). *)

val apply_filters :
  filter list -> string list -> string list list -> string list list
(** [apply_filters filters headers data] applies a list of filters to the player
    data. Only players passing all filters are returned. *)

val filter_by_nation : string -> headers:string list -> player -> bool
(** [filter_by_nation nation ~headers player] checks if [player] is from the
    given [nation]. *)

val filter_by_min_ovr : int -> headers:string list -> player -> bool
(** [filter_by_min_ovr min_ovr ~headers player] checks if [player]'s OVR is at
    least [min_ovr]. *)

val can_play_position : player -> string list -> string -> string
(** [can_play_position player headers position] returns the position the player
    would occupy if they can play [position], otherwise returns the empty
    string. *)

val find_lowest_player_for_position :
  string list list ->
  string list ->
  string ->
  player list ->
  (player * string) option
(** [find_lowest_player_for_position data headers position used_players] finds
    the lowest OVR player from [data] who can play [position] and is not in
    [used_players]. Returns [Some (player, assigned_position)] or [None] if no
    player is found. *)

val build_squad :
  string list list -> string list -> string list -> (player * string) list
(** [build_squad data headers positions] builds a squad of players by assigning
    each position from [positions] to the lowest OVR player who can play that
    position. Raises [InsufficientPlayers] if it cannot find enough suitable
    players. *)

val build_squad_result :
  string list list ->
  string list ->
  string list ->
  int ->
  ((player * string) list, build_error) result
(** [build_squad_result data headers positions min_league_count] attempts to
    build a squad while also checking synergy requirements (at least
    [min_league_count] players from the same league). Returns [Ok squad] on
    success or [Error err] if insufficient players or synergy requirements are
    not met. *)

val display_squad : string list -> (player * string) list -> unit
(** [display_squad headers squad] prints the resulting squad with player names,
    positions, and OVRs. *)
