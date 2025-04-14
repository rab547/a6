type 'a t =
  | Nil
  | Two of 'a t * 'a * 'a t
  | Three of 'a t * 'a * 'a t * 'a * 'a t

(* AF: The abstract type 'a t represents a 2-3 tree where: Nil represents an
   empty tree Two(l, v, r) is a 2-node with left subtree l, value v, and right
   subtree r Three(l, v1, m, v2, r) is a 3-node with left subtree l, values v1
   and v2, middle subtree m, and right subtree r*)

(*RI: For any 2-3 tree t: All values in the left subtree of a node are less than
  the node's value(s) All values in the right subtree of a node are greater than
  the node's value(s) For a Three node, all values in the middle subtree are
  between v1 and v2 For a Three node, v1 < v2 All leaves are at the same depth
  All siblings have the same height Every path through the tree has the same
  length *)
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

type 'a has_grown =
  | Not_Grown of 'a t
  | Grown of 'a t * 'a * 'a t

let rec insert x tree =
  let rec insert_helper tree =
    match tree with
    | Nil -> Grown (Nil, x, Nil)
    | Two (left, value, right) ->
        if x = value then Not_Grown tree
        else if x < value then
          let returnable =
            let inserted = insert_helper left in
            match inserted with
            | Not_Grown t' -> Not_Grown (Two (t', value, right))
            | Grown (left1, value1, left2) ->
                Not_Grown (Three (left1, value1, left2, value, right))
          in
          returnable
        else
          let returnable =
            let inserted = insert_helper right in
            match inserted with
            | Not_Grown t' -> Not_Grown (Two (left, value, t'))
            | Grown (right1, value1, right2) ->
                Not_Grown (Three (left, value, right1, value1, right2))
          in
          returnable
    | Three (left, value1, middle, value2, right) ->
        if x = value1 || x = value2 then Not_Grown tree
        else if x < value1 then
          let returnable =
            let inserted = insert_helper left in
            match inserted with
            | Not_Grown t' ->
                Not_Grown (Three (t', value1, middle, value2, right))
            | Grown (left1, value, left2) ->
                Grown
                  ( Two (left1, value, left2),
                    value1,
                    Two (middle, value2, right) )
          in
          returnable
        else if x > value2 then
          let returnable =
            let inserted = insert_helper right in
            match inserted with
            | Not_Grown t' ->
                Not_Grown (Three (left, value1, middle, value2, t'))
            | Grown (right1, value, right2) ->
                Grown
                  ( Two (left, value1, middle),
                    value2,
                    Two (right1, value, right2) )
          in
          returnable
        else
          let returnable =
            let inserted = insert_helper middle in
            match inserted with
            | Not_Grown t' ->
                Not_Grown (Three (left, value1, t', value2, right))
            | Grown (middle1, value, middle2) ->
                Grown
                  ( Two (left, value1, middle1),
                    value,
                    Two (middle2, value2, right) )
          in
          returnable
  in
  match insert_helper tree with
  | Not_Grown t' -> t'
  | Grown (left, value, right) -> Two (left, value, right)
