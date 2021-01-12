open Avenir
open VCGen
open Core 

let demo _ = 
let open Avenir.Test in 
  Printf.printf "CALLING FUNCTION %!";
  let res = deparse_equality ["ipv4.src", 8; "ipv4.dst", 8; "eth.src", 8; "eth.dst", 8; "eth.proto", 9]
    ["ipv4.src", 8; "ipv4.dst", 8; "eth.src", 8; "eth.dst", 8; "eth.proto", 9; "ipv6.src", 8; "ipv6.dst", 8]  in
  Printf.printf "%s" (res |> to_string)

let test : unit Alcotest.test_case list = [
  Alcotest.test_case "Printing VC example" `Quick demo;]
