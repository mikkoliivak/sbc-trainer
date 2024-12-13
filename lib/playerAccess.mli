exception PlayerNotFound of string
(** Exception raised when a specified player is not found in the database. *)

val find_index : string -> string list -> int
(** [find_index header headers] returns the index of [header] in [headers], or
    raises [Failure] if not found. *)

val levenshtein_distance : string -> string -> int
(** [levenshtein_distance s1 s2] calculates the minimum number of
    single-character edits (insertions, deletions, or substitutions) required to
    change string [s1] into string [s2]. Returns the distance as an integer. *)

val contains_substring : string -> string -> bool
(** [contains_substring haystack needle] checks if [needle] is a substring of
    [haystack]. Returns [true] if the substring is found, otherwise [false]. *)

val suggest_similar_names : string -> string -> string list
(** [suggest_similar_names csv_file player_name] returns a list of player names
    that are similar to [player_name] in the CSV file [csv_file]. *)

val get_player_attributes :
  string ->
  string ->
  string * string * string * string * string * string * string * string
(** [get_player_attributes csv_file player_name] searches for [player_name] in
    the CSV file [csv_file]. If found, returns a tuple containing (Name, OVR,
    PAC, SHO, PAS, DRI, DEF, PHY). If the player is not found, raises
    [PlayerNotFound]. *)

val compare_two_players :
  string ->
  string ->
  string ->
  ((string * string * string * string * string * string * string * string)
  * (string * string * string * string * string * string * string * string))
  option
(** [compare_two_players csv_file player1 player2] compares two players
    [player1] and [player2] in the CSV file [csv_file]. Returns a tuple of their
    attributes for comparison. If either player is not found, raises
    [PlayerNotFound]. *)

type player_stats = {
  count : int;
  min_ovr : int;
  max_ovr : int;
  avg_ovr : float;
}
(** Represents aggregate statistics for a set of filtered players:
    - [count]: number of players included
    - [min_ovr]: minimum OVR among players
    - [max_ovr]: maximum OVR among players
    - [avg_ovr]: average OVR among players *)

val get_filtered_players_stats :
  string -> (headers:string list -> string list -> bool) list -> player_stats
(** [get_filtered_players_stats csv_file filters] applies the given [filters] to
    the player data in [csv_file], and computes statistics on the resulting set
    of players. Returns a [player_stats] record containing count, min OVR, max
    OVR, and avg OVR of the filtered players. Raises [Failure] if no players
    match the filters or if CSV cannot be loaded. *)
