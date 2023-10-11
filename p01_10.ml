(*P1*)
let rec last list =
  match list with [] -> None | t :: [] -> Some t | _ :: q -> last q

let%test _ = last [] = None
let%test _ = last [ 1 ] = Some 1
let%test _ = last [ 1; 2; 3 ] = Some 3

(*P2*)
let rec last_two list =
  match list with
  | [] | [ _ ] -> None
  | [ a; b ] -> Some (a, b)
  | _ :: q -> last_two q

let%test _ = last_two [] = None
let%test _ = last_two [ 1 ] = None
let%test _ = last_two [ 1; 2 ] = Some (1, 2)
let%test _ = last_two [ 1; 2; 3 ] = Some (2, 3)

(*P3*)
let rec at n list =
  match list with
  | [] -> None
  | t :: q -> if n < 1 then None else if n = 1 then Some t else at (n - 1) q

let%test _ = at 2 [] = None
let%test _ = at 1 [ 1 ] = Some 1
let%test _ = at 2 [ 1 ] = None
let%test _ = at 2 [ 1; 2 ] = Some 2

(*P4*)
let length list =
  let rec aux n l = match l with [] -> n | _ :: q -> aux (n + 1) q in
  aux 0 list

let%test _ = length [] = 0
let%test _ = length [ 1 ] = 1
let%test _ = length [ 1; 2 ] = 2

(*P5*)
let rev list =
  let rec aux acc list =
    match list with [] -> acc | t :: q -> aux (t :: acc) q
  in
  aux [] list

let%test _ = rev [] = []
let%test _ = rev [ 1 ] = [ 1 ]
let%test _ = rev [ 1; 2 ] = [ 2; 1 ]
let%test _ = rev [ 1; 2; 3 ] = [ 3; 2; 1 ]
let%test _ = rev [ "a"; "b"; "c" ] = [ "c"; "b"; "a" ]

(*P6*)
let is_palindrome list = rev list = list
let%test _ = is_palindrome [ "a"; "b"; "c"; "b"; "a" ]
let%test _ = is_palindrome []
let%test _ = is_palindrome [ 1 ]
let%test _ = is_palindrome [ 1; 2; 1 ]
let%test _ = is_palindrome [ 1; 2; 2; 1 ]
let%test _ = is_palindrome [ 1; 2; 3; 2; 1 ]
let%test _ = not (is_palindrome [ 1; 2; 3 ])
let%test _ = not (is_palindrome [ "a"; "b"; "c" ])

(*P7*)
type 'a node = One of 'a | Many of 'a node list

let rec flatten l =
  match l with
  | [] -> []
  | t :: q -> (
      match t with
      | One a -> a :: flatten q
      | Many list -> flatten list @ flatten q)

let%test _ = flatten [] = []
let%test _ = flatten [ One 1 ] = [ 1 ]
let%test _ = flatten [ Many [ One 1; One 2 ]; One 3 ] = [ 1; 2; 3 ]

let%test _ =
  flatten [ One 1; Many [ One 2; Many [ One 3; One 4 ]; One 5 ]; One 6 ]
  = [ 1; 2; 3; 4; 5; 6 ]

(*P8*)
let rec compress list =
  match list with
  | [] -> []
  | [ a ] -> [ a ]
  | a :: b :: q -> if a = b then compress (a :: q) else a :: compress (b :: q)

let%test _ = compress [] = []
let%test _ = compress [ 1 ] = [ 1 ]
let%test _ = compress [ 1; 1 ] = [ 1 ]
let%test _ = compress [ 1; 2 ] = [ 1; 2 ]
let%test _ = compress [ 1; 1; 2 ] = [ 1; 2 ]
let%test _ = compress [ 1; 2; 2 ] = [ 1; 2 ]
let%test _ = compress [ 1; 2; 3 ] = [ 1; 2; 3 ]
let%test _ = compress [ 1; 1; 2; 2; 3; 3; 3 ] = [ 1; 2; 3 ]

let%test _ =
  compress
    [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = [ "a"; "b"; "c"; "a"; "d"; "e" ]

(*P9*)
let pack list =
  match list with
  | [] -> []
  | t :: q ->
      let rec aux elts acc list =
        match list with
        | [] -> elts :: acc
        | e :: r -> (
            match elts with
            | [] -> failwith "Should never happen"
            | c :: _ ->
                if e = c then aux (e :: elts) acc r
                else aux [ e ] (elts :: acc) r)
      in
      rev (aux [ t ] [] q)

let%test _ = pack [] = []
let%test _ = pack [ 1 ] = [ [ 1 ] ]
let%test _ = pack [ 1; 1 ] = [ [ 1; 1 ] ]
let%test _ = pack [ 1; 2 ] = [ [ 1 ]; [ 2 ] ]
let%test _ = pack [ 1; 1; 2 ] = [ [ 1; 1 ]; [ 2 ] ]
let%test _ = pack [ 1; 2; 2 ] = [ [ 1 ]; [ 2; 2 ] ]
let%test _ = pack [ 1; 1; 2; 2; 2 ] = [ [ 1; 1 ]; [ 2; 2; 2 ] ]

let%test _ =
  pack [ "a"; "a"; "b"; "c"; "c"; "c" ]
  = [ [ "a"; "a" ]; [ "b" ]; [ "c"; "c"; "c" ] ]

(*P10*)
let encode list =
  List.map (fun l -> (length l, List.hd l)) (pack list)

let%test _ = encode [] = []
let%test _ = encode [1] = [(1, 1)]
let%test _ = encode [1; 1] = [(2, 1)]
let%test _ = encode [1; 2] = [(1, 1); (1, 2)]
let%test _ = encode [1; 1; 2] = [(2, 1); (1, 2)]
let%test _ = encode [1; 2; 2] = [(1, 1); (2, 2)]
let%test _ = encode [1; 1; 2; 2; 2] = [(2, 1); (3, 2)]
let%test _ = encode ["a"; "a"; "b"; "c"; "c"; "c"] = [(2, "a"); (1, "b"); (3, "c")]
let%test _ =
  encode
    [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = [ (4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e") ]
