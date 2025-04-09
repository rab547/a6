type 'a t
(** Represents a set storing values of type 'a *)

val empty : 'a t
(** Represents the empty set*)

val is_empty : 'a t -> bool
(** [is_empty set] returns true if [set] is empty, and false otherwise*)

val mem : 'a -> 'a t -> bool
(** [mem value set] will return true if [value] is in [set] and false otherwise*)

val insert : 'a -> 'a t -> 'a t
(** [mem value prev_set] will return a set which containes all the values of
    [prev_set] in addition to [value] (as long as value is not already in
    [prev_set]. In this case it will just return [prev_set])*)
