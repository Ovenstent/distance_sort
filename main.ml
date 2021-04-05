type num = {
  hundreds : int;
  tens : int;
  units : int;
}

exception Stop of int * num list

module H = Hashtbl.Make (
    struct
      type t = int * int list
      let equal = (=)
      let hash (i, l) = List.fold_left (+) i l
    end
    )

module S = Set.Make(Int)
module M = Map.Make(Int)

let hundreds i = i / 100
let tens i = (i / 10) mod 10
let units i = i mod 10

let to_num i = {
  hundreds = hundreds i;
  tens = tens i;
  units = units i
}

let pp fmt i = Format.fprintf fmt "%i%i%i" i.hundreds i.tens i.units

let abs i1 i2 =
  min (abs (i1 - i2)) (10 - abs (i1 - i2))

let dist i1 i2 =
  abs i1.hundreds i2.hundreds +
  abs i1.tens i2.tens +
  abs i1.units i2.units

(** Returns (Some i) where i is the distance between i1 and i2.
    If dist i1 i2 > threshold, returns None. *)
let dist_threshold threshold i1 i2 =
  try
    let hund = abs i1.hundreds i2.hundreds in
    if hund > threshold then raise Exit;
    let cent = hund + abs i1.tens i2.tens in
    if cent > threshold then raise Exit;
    let units = cent + abs i1.units i2.units in
    if units > threshold then raise Exit;
    Some units
  with Exit -> None

let find_closests i l =
  assert (l <> []);
  List.fold_left
    (
      fun (d, acc) elt ->
        match dist_threshold d elt i with
        | None -> (d, acc) (* dist elt i > d *)
        | Some i ->
          if i = d
          then (d, elt :: acc)
          else (i, [elt]) (* i < d *)
    )
    (28, []) (* 28 is the max distance between two 3-digits numbers *)
    l
(*
let closests s =
  S.fold
    (fun i acc ->
       match M.find_opt i acc with
       | Some _ -> acc
       | None ->
         let dist, close_to_i = find_closests i (S.remove i s) in
         S.fold
           (fun (close_i : int) (acc : (int * S.t) M.t) ->
              M.add close_i (dist, (S.add i (S.remove close_i close_to_i))) acc)
           close_to_i
           (M.add i (dist, close_to_i) acc)
    )
    s
    M.empty

let closests_of map i l =
  match M.find_opt i map with
  | None -> assert false
  | Some (d, close) ->
    d, S.inter (S.of_list l) close
*)

let remove_first elt l =
  let rec aux acc = function
    | [] -> assert false
    | hd :: tl -> if hd = elt then acc @ tl else aux (hd :: acc) tl
  in
  aux [] l

let print_int_list fmt l =
  Format.pp_print_list
    ~pp_sep:(fun fmt _ -> Format.fprintf fmt "--")
    (fun fmt -> Format.fprintf fmt "%i")
    fmt
    l

let main l =
  let rec search
      ((max_distance, last_candidate) : int * num list)
      acc_list
      current_distance
      last_elt
      l =
    try
      match l with
      | [] ->
        Format.printf "DONE. DISTANCE: %i@." current_distance;
        (current_distance, acc_list)
      (* Reversed, but it has the same overall distance *)
      | _ ->
        let dist, closests = find_closests last_elt l in
        (* Format.printf
          "Closests of %i in %a (distance %i) = %a@."
          last_elt
          print_int_list l
          dist
          print_int_list (S.elements closests);*)
        let new_current_distance = current_distance + dist in
        if max_distance <= new_current_distance
        then
          begin(*
          Format.printf "Curent distance %i >= last distance %i@."
            new_current_distance
            max_distance;
            Format.printf "ABORT@."; *)
            max_distance, last_candidate
          end
        else
          begin (*
          Format.printf "Curent distance %i < last distance %i@."
            new_current_distance
            max_distance; *)
            List.fold_left
              (fun acc closest ->
                 search
                   acc
                   (closest :: acc_list)
                   new_current_distance
                   closest
                   (remove_first closest l)
              )
              (max_distance, last_candidate)
              closests
          end
    with
    | Sys.Break -> raise (Stop (max_distance, last_candidate))
  in
  Format.printf "Start@.";
  List.fold_left
    (fun acc first ->
       Format.printf "Trying with %a as first element@." pp first;
       search
         acc
         [first]
         0
         first
         (remove_first first l)
    )
    (max_int, [])
    l

let l = List.map to_num
  [
    850;
    462;
    119;
    721;
    845;
    481;
    862;
    124;
    403;
    480;
    071;
    294;
    054;
    803;
    817;
    843;
    710;
    201;
    251;
    234;
    440;
    272;
    779;
    901;
    200;
    317;
    883;
    334;
    785;
    115;
    471;
    225;
    801;
    218;
    788;
    840;
    580;
    219;
    043;
    342;
    451;
    796;
    864;
    082;
    324;
    083;
    891;
    629;
    768;
    907;
    827;
    956;
    933;
    667;
    745;
    878;
    337;
    154;
    839;
    739;
    746;
    738;
    437;
    969;
    448;
    365;
    239;
    911;
    633;
    886;
    638;
    433;
    438;
    198;
    209;
    016;
    873;
    605;
    470;
    981;
    495
  ]

exception NotUnique of num

let only_uniques l =
  let rec aux rev_acc = function
    | [] -> ()
    | hd :: tl ->
      if List.mem hd rev_acc || List.mem hd tl then
        raise (NotUnique hd) in
  aux [] l

let () =
  Sys.catch_break true;
  only_uniques l;
  let dist, l = try main l with Stop (d, l) -> d, l in
  Format.printf "Optimized distance = %i@." dist;
  Format.printf "Optimized list = %a@."
    (Format.pp_print_list ~pp_sep:(fun fmt _ -> Format.fprintf fmt "--") (fun fmt -> Format.fprintf fmt "%a" pp))
    l

(*
177 mouvements
850--840--839--739--738--638--438--448--440--451--471--470--480--580--481--462--272--251--342--334--234--225--124--115--16--907--817--827--629--721--710--911--901--801--891--981--71--82--83--883--873--864--862--843--43--54--154--365--495--294--324--433--633--933--845--745--746--956--969--779--788--768--667--878--886--796--785--605--803--403--201--200--209--219--218--317--337--437--239--119--198

181 :
850--840--839--739--738--638--438--448--440--451--471--470--480--580--481--462--272--251--342--334--234--225--124--115--16--907--817--827--845--745--746--956--54--154--43--843--933--633--433--403--495--294--201--200--209--219--218--119--198--788--878--779--768--667--796--886--785--605--803--883--873--864--862--969--71--82--83--981--891--801--901--911--710--721--629--239--337--437--317--324--365

*)

(* 175 mov
Optimized list = 198--119--239--437--337--317--218--219--209--200--201--901--911--721--710--629--827--817--907--16--115--294--83--82--71--981--891--801--803--883--873--862--864--845--745--746--956--969--878--779--667--768--788--796--886--785--605--403--495--365--154--54--43--933--843--633--433--324--124--225--234--334--342--251--272--462--481--580--480--470--471--451--440--448--438--638--738--739--839--840--850

 *)
