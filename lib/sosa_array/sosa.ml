(* Copyright (c) 1998-2007 INRIA *)

type t = int64

let base = 0x1000000L
let max_mul_base = Int64.div Int64.max_int base
let zero = 0L
let one = 1L

let of_int i =
  if i < 0 then invalid_arg "Sosa.of_int"
  else if i = 0 then zero
  else if i < base then [| Int64.of_int i |]
  else Int64.add (Int64.of_int (i mod Int64.to_int base)) (Int64.shift_left (Int64.of_int (i / Int64.to_int base)) 32)

let to_int x =
  Int64.to_int x

let eq x y = Int64.equal x y

let gt x y =
  Int64.compare x y > 0

let twice x =
  let rec loop i r =
    if i = 32 then if r = 0L then zero else r
    else
      let v = Int64.add (Int64.shift_left x 1) r in
      Int64.add (Int64.logand v (Int64.sub base 1L)) (loop (i + 1) (if Int64.compare v base >= 0 then Int64.shift_right_logical v 24 else zero))
  in
  loop 0 zero

let half x =
  let rec loop i r v =
    if i < 0 then v
    else
      let rd = if Int64.logand x 1L = zero then zero else Int64.div base 2L in
      let v =
        let d = Int64.add r (Int64.div x 2L) in
        if Int64.equal d zero && eq v zero then v else d :: v
      in
      loop (i - 1) rd v
  in
  loop 31 zero []

let even x = Int64.equal (Int64.logand x 1L) zero

let inc x n =
  let rec loop i r =
    if i = 32 then if r = zero then zero else r
    else
      let d = Int64.add x r in
      Int64.add (Int64.logand d (Int64.sub base 1L)) (loop (i + 1) (Int64.div d base))
  in
  loop 0 n

let add x y =
  let rec loop i r =
    if i = 32 && Int64.equal x zero && Int64.equal y zero then zero
    else
      let xi = Int64.logand x (Int64.sub base 1L) in
      let yi = Int64.logand y (Int64.sub base 1L) in
      let s = Int64.add xi yi r in
      let d = Int64.logand s (Int64.sub base 1L) in
      let r = Int64.div s base in
      Int64.add (Int64.shift_left d (i * 8)) (loop (i + 1) r)
  in
  loop 0 zero

let normalize x =
  let rec loop l = match l with
    | [] -> []
    | hd :: tl ->
        let r = loop tl in
        if eq hd zero && r = [] then r else hd :: r
  in
  loop

let sub x y =
  let rec loop i r =
    if i = 32 && Int64.equal x zero && Int64.equal y zero then if r = zero then zero else invalid_arg "Sosa.sub"
    else
      let xi = Int64.logand x (Int64.sub base 1L) in
      let yi = Int64.logand y (Int64.sub base 1L) in
      let s = Int64.add xi (Int64.add yi r) in
      let d = if Int64.compare s base < 0 then s else Int64.sub s base in
      let r = if Int64.compare s base < 0 then zero else one in
      Int64.add (Int64.shift_left d (i * 8)) (loop (i + 1) r)
  in
  loop 0 zero

let mul0 x n =
  if Int64.compare n max_mul_base > 0 then invalid_arg "Sosa.mul"
  else
    let rec loop i r =
      if i = 32 then if r = zero then zero else r
      else
        let d = Int64.add (Int64.mul x n) r in
        Int64.add (Int64.logand d (Int64.sub base 1L)) (loop (i + 1) (Int64.div d base))
    in
    loop 0 zero

let mul x n =
  if Int64.compare n max_mul_base < 0 then mul0 x n
  else
    let rec loop r x n =
      if Int64.compare n max_mul_base < 0 then add r (mul0 x n)
      else
        loop
          (add r (mul0 x (Int64.rem n max_mul_base)))
          (mul0 x max_mul_base) (Int64.div n max_mul_base)
    in
    loop zero x n

let div x n =
  if Int64.compare n max_mul_base > 0 then invalid_arg "Sosa.div"
  else
    let rec loop i l r =
      if i < 0 then l
      else
        let r = Int64.add (Int64.mul r base) x in
        let d = Int64.div r n in
        loop (i - 1) (d :: l) (Int64.rem r n)
    in
    Array.of_list (normalize (loop 31 [] zero))

let modl x n =
  of_int (Int64.rem x n)

let rec exp_gen x1 x2 n =
  if n = 0L || x1 = zero then one
  else if n = one then x1
  else exp_gen (mul x1 (Int64.to_int x2)) x2 (Int64.sub n one)

let exp x n = exp_gen x x n

let compare x y = if gt x y then 1 else if eq x y then 0 else -1

let code_of_digit d =
  let d = Int64.to_int d in
  if d < 10 then Char.code '0' + d else Char.code 'A' + (d - 10)

let to_string_sep_base sep base x =
  let digits =
    let rec loop d x =
      if eq x zero then d else loop (modl x base :: d) (div x base)
    in
    loop [] x
  in
  let digits = if digits = [] then [ zero ] else digits in
  let len = List.length digits in
  let slen = String.length sep in
  let s = Bytes.create
