open Core.Std

let sample l ~size =
  let source = Array.of_list l in
  let len = List.length l in
  List.init size ~f:(fun _ -> source.(Random.int len))
;;

let group_by l ~f =
  List.sort l ~cmp:(fun x1 x2 -> Poly.compare (f x1) (f x2))
  |> List.group ~break:(fun x1 x2 -> (f x1) <> (f x2))
;;

let scan_list l ~f =
  let rec loop f y l =
    match l with
    | [] -> [y]
    | x :: xs -> y :: loop f (f y x) xs
  in
  match l with
  | [] -> []
  | x :: xs -> loop f x xs
;;
