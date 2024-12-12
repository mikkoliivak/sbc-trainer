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
