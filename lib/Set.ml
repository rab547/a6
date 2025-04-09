type 'a t =
  | Nil
  | Two of 'a t * 'a * 'a t
  | Three of 'a t * 'a * 'a t * 'a * 'a t

(* AF: The abstract type 'a t represents a 2-3 tree where:
Nil represents an empty tree
Two(l, v, r) is a 2-node with left subtree l, value v, and right subtree r
Three(l, v1, m, v2, r) is a 3-node with left subtree l, values v1 and v2,
middle subtree m, and right subtree r*)

(*RI: For any 2-3 tree t:
All values in the left subtree of a node are less than the node's value(s)
All values in the right subtree of a node are greater than the node's value(s)
For a Three node, all values in the middle subtree are between v1 and v2
For a Three node, v1 < v2
All leaves are at the same depth
All siblings have the same height
Every path through the tree has the same length
 *)
let empty = Nil
let is_empty t = t = Nil

let rec mem x tree =
  match tree with
  | Nil -> false
  | Two (left, value, right) ->
      x = value || (x < value && mem x left) || mem x right
  | Three (left, value1, middle, value2, right) ->
      x = value1 || x = value2
      || (x < value1 && mem x left)
      || (x > value1 && x < value2 && mem x middle)
      || mem x right

let rec insert new_val tree =
  let rec insert_helper new_val tree =
    match tree with
    | Nil -> (Two (Nil, new_val, Nil), true)
    | Two (left, value, right) ->
        if new_val < value then
          let new_left, grew = insert_helper new_val left in
          if not grew then (Two (new_left, value, right), false)
          else
            match new_left with
            | Two (left_left, left_val, left_right) ->
                (Three (left_left, left_val, left_right, value, right), false)
            | _ -> (Two (new_left, value, right), true)
        else if new_val > value then
          let new_right, grew = insert_helper new_val right in
          if not grew then (Two (left, value, new_right), false)
          else
            match new_right with
            | Two (right_left, right_val, right_right) ->
                (Three (left, value, right_left, right_val, right_right), false)
            | _ -> (Two (left, value, new_right), true)
        else (Two (left, new_val, right), false)
    | Three (left, value_1, middle, value_2, right) ->
        if new_val < value_1 then
          let new_left, grew = insert_helper new_val left in
          if not grew then
            (Three (new_left, value_1, middle, value_2, right), false)
          else (Two (Two (new_left, value_1, middle), value_2, right), true)
        else if new_val > value_1 && new_val < value_2 then
          let new_middle, grew = insert_helper new_val middle in
          if not grew then
            (Three (left, value_1, new_middle, value_2, right), false)
          else (Two (Two (left, value_1, new_middle), value_2, right), true)
        else if new_val > value_2 then
          let new_right, grew = insert_helper new_val right in
          if not grew then
            (Three (left, value_1, middle, value_2, new_right), false)
          else (Two (left, value_1, Two (middle, value_2, new_right)), true)
        else (Three (left, value_1, middle, value_2, right), false)
  in
  let final_tree, _ = insert_helper new_val tree in
  final_tree
