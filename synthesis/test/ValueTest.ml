open Avenir
open Value

let equals _ =
  Equality.same_values (make (1, 99)) (make (1, 99))

let inequals _ =
  Equality.diff_values (make (88, 99)) (make (1, 99))

let leq _ =
  Equality.leq_values
    (make (88,100))
    (make (99,100))

let add_ok _ =
  Equality.same_values
    (make (99,8))
    (add (make (98,8)) (big_make (Bigint.one, 8)))

let add_wrap _ =
  Equality.same_values
    (make (1,2))
    (add (make (3,2)) (make (1,2)))

let sat_add_top _ =
  Equality.same_values
    (make (3,2))
    (sat_add (make (3,2)) (make (1,2)))

let multiply _ =
  Equality.same_values
    (make (6,3))
    (multiply (make (3,3)) (make (2,3)))

let subtract_ok _ =
  Equality.same_values
    (make (0,3))
    (subtract (make (1,3)) (make (1,3)))

let subtract_wrap_0 _ =
  Equality.same_values
    (make (6,3))
    (subtract (make (0,3)) (make (1,3)))

let subtract_wrap _ =
  Equality.same_values
    (make (6,3))
    (subtract (make (1,3)) (make (2,3)))

let subtract_sat _ =
  Equality.same_values
    (make (0,3))
    (sat_subtract (make (1,3)) (make (2,3)))

let mask_ok _ =
  Equality.same_values
    (make (1,4))
    (mask (make (9,4)) (make (7,4)))

let xor_ok _ =
  Equality.same_values
    (make (3,2))
    (xor (make (2,2)) (make (1,2)))

let or_ok _ =
    Equality.same_values
    (or_ (make (3,2)) (make (2,2)))
    (or_ (make (3,2)) (make (1,2)))

let shl_truncs _ =
  Equality.same_values
    (make (2,2))
    (shl (make (3,2)) (make (1,2)))

let cast_truncs_larger _ =
  Equality.same_values
    (make (3,2))
    (cast 2 (make (15,4)))

let cast_idents_smaller _ =
  Equality.same_values
    (make (2,2))
    (cast 2 (make (2,4)))

let resize_resizes _ =
  Equality.same_values
    (make (2,2))
    (resize (make (2,4)) 2)

let resize_is_dangerous _ =
  Equality.same_values
    (unsafe_make (4,2))
    (resize (make (4,4)) 2)

let slice_ok _ =
  Equality.same_values
    (make (3,2))
    (slice 3 1 (make (6,4)))

let concat_ok _ =
  Equality.same_values
    (make (101,8))
    (concat (make (6,4)) (make (5,4)))

let test : unit Alcotest.test_case list = [
    Alcotest.test_case "Equal values are equal" `Quick equals;
    Alcotest.test_case "Inequal values are not equal" `Quick inequals;
    Alcotest.test_case "Smaller values are less than" `Quick leq;
    Alcotest.test_case "98#8 + 1#8 = 99#8" `Quick add_ok;
    Alcotest.test_case "3#2 + 2#2 = 1#2" `Quick add_wrap;
    Alcotest.test_case "3#2 |+| 1#2 = 3#2" `Quick sat_add_top;
    Alcotest.test_case "3#3 * 2#3 = 6#3" `Quick multiply;
    Alcotest.test_case "1#3 - 1#3 = 0#3" `Quick subtract_ok;
    Alcotest.test_case "0#3 - 1#3 = 6#3" `Quick subtract_wrap_0;
    Alcotest.test_case "1#3 - 2#3 = 6#3" `Quick subtract_wrap;
    Alcotest.test_case "1#3 - 2#3 = 0#3" `Quick subtract_sat;
    Alcotest.test_case "9#4 & 7#4 = 1#4" `Quick mask_ok;
    Alcotest.test_case "2#3 & 1#2 = 3#2" `Quick xor_ok;
    Alcotest.test_case "3#2 << 1#2 = 2#2" `Quick shl_truncs;
    Alcotest.test_case "cast 2 15#4 = 3#2" `Quick cast_truncs_larger;
    Alcotest.test_case "cast 2 2#4 = 2#2" `Quick cast_idents_smaller;
    Alcotest.test_case "resize 2#4 2 = 2#2" `Quick resize_resizes;
    Alcotest.test_case "resize 4#4 2 = 4#2" `Quick resize_is_dangerous;
    Alcotest.test_case "6#4[2:1] = 3#2" `Quick slice_ok;
    Alcotest.test_case "6#4@5#4 = 101#8" `Quick concat_ok;
  ]
