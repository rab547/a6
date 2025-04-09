open OUnit2
open A6.Set

(** [test_is_empty_on_empty] tests that empty is detirmined to be empty *)
let test_is_empty_empty _ =
  assert_equal ~msg:"An empty tree should be deemed empty" true (is_empty empty)

(** [test_is_empty_insert] asserts that a non-empty tree is not considered empty
*)
let test_is_empty_insert _ =
  let tree = insert 0 empty in
  assert_equal ~msg:"Non-empty tree should not be empty" false (is_empty tree)

(** [test_mem_empty] asserts that an arbitrary value is not present in the empty
    tree*)
let test_mem_empty _ =
  assert_equal ~msg:"No value should be present in the empty tree" false
    (mem 0 empty)

(** [test_mem_insert_same_value] tests that a value exists after it has been
    inserted *)
let test_mem_insert_same_value _ =
  let tree = insert 10 empty in
  assert_equal ~msg:"Inserted value should be regarded as present" true
    (mem 10 tree)

(** [test_mem_insert_different_values] asserts that the insertion of one value doesnt
    affect the presense of another *)
let test_mem_insert_different_values _ =
  let tree = insert 10 empty in
  assert_equal ~msg:"Insertion shouldn't affect other values" false (mem 5 tree);
  let tree2 = insert 5 tree in
  assert_equal ~msg:"Original value should still exist" true (mem 10 tree2)

(** [test_comprehensive_tree] tests multiple operations working together *)
let test_comprehensive_tree _ =
  let tree1 = empty in
  let tree2 = insert 10 tree1 in
  let tree3 = insert 5 tree2 in
  let tree4 = insert 15 tree3 in
  let tree5 = insert 10 tree4 in
  let tree6 = insert 20 tree5 in
  let tree7 = insert 0 tree6 in
  let tree8 = insert 3 tree7 in
  let tree9 = insert 18 tree8 in
  let tree10 = insert 3 tree9 in
  let tree11 = insert 18 tree10 in
  let tree12 = insert 7 tree11 in
  let tree13 = insert 12 tree12 in
  assert_equal true (is_empty tree1);
  assert_equal false (is_empty tree2);
  assert_equal true (mem 10 tree2);
  assert_equal false (mem 5 tree2);
  assert_equal true (mem 5 tree3);
  assert_equal false (mem 15 tree3);
  assert_equal true (mem 15 tree4);
  assert_equal false (mem 20 tree4);
  assert_equal true (mem 10 tree5);
  assert_equal true (mem 20 tree6);
  assert_equal true (mem 0 tree7);
  assert_equal true (mem 3 tree8);
  assert_equal true (mem 18 tree9);
  assert_equal true (mem 3 tree9);
  assert_equal true (mem 3 tree10);
  assert_equal true (mem 18 tree11);
  assert_equal true (mem 7 tree12);
  assert_equal true (mem 12 tree13)
  


let tests =
  "test suite"
  >::: [
         "test_is_empty_on_empty" >:: test_is_empty_empty;
         "test_is_empty_insert" >:: test_is_empty_insert;
         "test_mem_empty" >:: test_mem_empty;
         "test_mem_insert_same_value" >:: test_mem_insert_same_value;
         "test_mem_insert_different_values" >:: test_mem_insert_different_values;
         "test_comprehensive" >:: test_comprehensive_tree;
       ]

let _ = run_test_tt_main tests
