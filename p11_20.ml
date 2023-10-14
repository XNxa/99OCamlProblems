(* P11 *)
type 'a rle = One of 'a | Many of int * 'a

let encode l =
  let rec aux acc list =
    match list with
    | [] -> acc
    | t :: q -> (
        match acc with
        | [] -> aux [ One t ] q
        | a :: b -> (
            match a with
            | One e when e = t -> aux (Many (2, t) :: b) q
            | Many (n, e) when e = t -> aux (Many (n + 1, t) :: b) q
            | _ -> aux (One t :: acc) q))
  in
  List.rev (aux [] l)

let%test _ = encode [] = []
let%test _ = encode [ "a" ] = [ One "a" ]
let%test _ = encode [ "a"; "b"; "c" ] = [ One "a"; One "b"; One "c" ]

let%test _ =
  encode [ "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"; "c" ]
  = [ Many (2, "a"); Many (3, "b"); Many (4, "c") ]

let%test _ = encode (List.init 1000 (fun _ -> "a")) = [ Many (1000, "a") ]

let%test _ =
  encode
    [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = [
      Many (4, "a");
      One "b";
      Many (2, "c");
      Many (2, "a");
      One "d";
      Many (4, "e");
    ]

(* P12 *)
let rec decode l =
  match l with
  | [] -> []
  | One e :: q -> e :: decode q
  | Many (n, e) :: q -> List.init n (fun _ -> e) @ decode q

let%test _ = decode [ One 1 ] = [ 1 ]

let%test _ =
  decode [ One 1; Many (3, 2); One 3; Many (2, 4) ] = [ 1; 2; 2; 2; 3; 4; 4 ]

let%test _ =
  decode [ Many (2, "a"); Many (3, "b"); Many (1, "c") ]
  = [ "a"; "a"; "b"; "b"; "b"; "c" ]

let%test _ = decode [ One "a"; One "b"; One "c" ] = [ "a"; "b"; "c" ]
let%test _ = decode [ One ""; Many (2, "") ] = [ ""; ""; "" ]
let%test _ = decode [ Many (0, "a"); Many (0, "b") ] = []
let%test _ = decode [ One ""; One "" ] = [ ""; "" ]

(* P13 *)

(* I didn't understand the question  *)

(* P14 *)
let rec duplicate l = match l with [] -> [] | t :: q -> t :: t :: duplicate q

let tailrec_duplicate l =
  let rec aux acc l =
    match l with [] -> acc | t :: q -> aux (t :: t :: acc) q
  in
  List.rev (aux [] l)

let%test _ = duplicate [] = []
let%test _ = duplicate [ 1 ] = [ 1; 1 ]
let%test _ = duplicate [ 1; 2 ] = [ 1; 1; 2; 2 ]

let%test _ =
  duplicate [ true; false; true ] = [ true; true; false; false; true; true ]

let%test _ = tailrec_duplicate [] = []
let%test _ = tailrec_duplicate [ 1 ] = [ 1; 1 ]
let%test _ = tailrec_duplicate [ 1; 2 ] = [ 1; 1; 2; 2 ]

let%test _ =
  tailrec_duplicate [ true; false; true ]
  = [ true; true; false; false; true; true ]

(* P15 *)
let replicate l n =
  let rec ajouter n acc t =
    if n < 1 then acc else ajouter (n - 1) (t :: acc) t
  in
  (* let rec aux acc l =
       match l with [] -> acc | t :: q -> aux (ajouter n acc t) q
     in
     List.rev (aux [] l) *)
  List.fold_left (ajouter n) [] (List.rev l)

let%test _ = replicate [] 3 = []
let%test _ = replicate [ 1; 2; 3 ] 0 = []
let%test _ = replicate [ 1; 2; 3 ] 1 = [ 1; 2; 3 ]
let%test _ = replicate [ 1; 2; 3 ] 2 = [ 1; 1; 2; 2; 3; 3 ]

let%test _ =
  replicate [ "a"; "b"; "c" ] 3
  = [ "a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c" ]

(* P16 *)
let drop l n =
  let rec aux i acc l =
    match l with
    | [] -> acc
    | t :: q when i > 0 -> aux (i - 1) (t :: acc) q
    | _ :: q -> aux (n - 1) acc q
  in
  List.rev (aux (n - 1) [] l)

let%test _ = drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 1 = []

let%test _ =
  drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 10
  = [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i" ]

let%test _ =
  drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 11
  = [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ]

let%test _ =
  drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
  = [ "a"; "b"; "d"; "e"; "g"; "h"; "j" ]

let%test _ = drop [] 3 = []
let%test _ = drop [ "a"; "b"; "c" ] 0 = []

(* P17 *)
let split l n =
  let rec aux i acc l =
    match l with
    | [] -> (List.rev acc, l)
    | t :: q -> if i <= 0 then (List.rev acc, l) else aux (i - 1) (t :: acc) q
  in
  aux n [] l

let%test _ = split [] 0 = ([], [])
let%test _ = split [ 1; 2; 3 ] 0 = ([], [ 1; 2; 3 ])
let%test _ = split [ 1; 2; 3 ] 1 = ([ 1 ], [ 2; 3 ])
let%test _ = split [ 1; 2; 3 ] 2 = ([ 1; 2 ], [ 3 ])
let%test _ = split [ 1; 2; 3 ] 3 = ([ 1; 2; 3 ], [])
let%test _ = split [ 1; 2; 3 ] 4 = ([ 1; 2; 3 ], [])

(* P18 *)
let slice l i k =
  let rec aux j acc l =
    match l with
    | [] -> acc
    | t :: q ->
        if i <= j && j <= k then aux (j + 1) (t :: acc) q else aux (j + 1) acc q
  in
  List.rev (aux 0 [] l)

let%test _ =
  slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 0 0 = [ "a" ]

let%test _ =
  slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 2 6
  = [ "c"; "d"; "e"; "f"; "g" ]

let%test _ =
  slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 0 9
  = [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ]

let%test _ =
  slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 5 5 = [ "f" ]

let%test _ = slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 7 4 = []

(* P19 *)
let rotate l n =
  match l with
  | [] -> []
  | _ ->
      let g, d = split l (n mod List.length l) in
      d @ g

let%test _ = rotate [] 0 = []
let%test _ = rotate [ 1; 2; 3 ] 0 = [ 1; 2; 3 ]
let%test _ = rotate [ 1; 2; 3 ] 1 = [ 2; 3; 1 ]
let%test _ = rotate [ 1; 2; 3 ] 2 = [ 3; 1; 2 ]
let%test _ = rotate [ 1; 2; 3 ] 3 = [ 1; 2; 3 ]
let%test _ = rotate [ 1; 2; 3 ] 4 = [ 2; 3; 1 ]
let%test _ = rotate [ 1; 2; 3 ] 5 = [ 3; 1; 2 ]
let%test _ = rotate [ 1; 2; 3 ] 6 = [ 1; 2; 3 ]
let%test _ = rotate [ 1; 2; 3; 4; 5 ] 2 = [ 3; 4; 5; 1; 2 ]
let%test _ = rotate [ 1; 2; 3; 4; 5 ] 3 = [ 4; 5; 1; 2; 3 ]
let%test _ = rotate [ 1; 2; 3; 4; 5 ] 4 = [ 5; 1; 2; 3; 4 ]
let%test _ = rotate [ 1; 2; 3; 4; 5 ] 5 = [ 1; 2; 3; 4; 5 ]
let%test _ = rotate [ 1; 2; 3; 4; 5 ] 6 = [ 2; 3; 4; 5; 1 ]
let%test _ = rotate [ 1; 2; 3; 4; 5 ] 7 = [ 3; 4; 5; 1; 2 ]

let rec remove_at i l =
  match l with
  | [] -> []
  | t :: q -> if i = 0 then q else t :: remove_at (i - 1) q

let%test _ = remove_at 0 [ 1; 2; 3; 4; 5 ] = [ 2; 3; 4; 5 ]
let%test _ = remove_at 4 [ 1; 2; 3; 4; 5 ] = [ 1; 2; 3; 4 ]
let%test _ = remove_at 2 [ "a"; "b"; "c"; "d" ] = [ "a"; "b"; "d" ]
let%test _ = remove_at 10 [ "a"; "b"; "c"; "d" ] = [ "a"; "b"; "c"; "d" ]
