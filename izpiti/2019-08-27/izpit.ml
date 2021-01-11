let odstej_trojici (x1, x2, x3) (y1, y2, y3) =
  (x1 - y2, x2 - y2, x3 - y3)

let max_rezultat_do_n f n = 
  let rec pomozna acc m f n =
    if m > n then acc
    else if (f m) > acc then pomozna (f m) (m + 1) f n
    else pomozna acc (m + 1) f n 
  in pomozna (f 0) 0 f n 

let pocisti_seznam sez =
  let rec pomozna acc = function
    | [] -> acc
    | x :: xs -> (
      if x = None then pomozna acc xs
      else if x = Some a then pomozna (Some a :: acc) xs
    )
  in pomozna [] sez

let preveri_urejenost sez =
  let rec aux min_sodo max_liho = function
    | [] -> true
    | x :: xs -> (
      if x mod 2 = 0 then x > min_sodo && aux x max_liho xs
      else x < max_sodo && aux min_sodo x xs
    )
  in aux -99999 99999 sez

(* Druga naloga *)

type 'a gnezdenje =
  | Element of 'a
  | Podseznam of 'a gnezdenje list

let gnezdenje_primer = [Element 1; Element 2; Podseznam[Element 3; Podseznam[Element 4]; Podseznam[]]; Podseznam[Element 5]]

let rec najvecja_globina sez = match sez with
  | [] -> 1
  | Element x :: xs -> najvecja_globina xs
  | Podseznam ys :: xs -> max (najvecja_globina podsez + 1) (najvecja_globina xs)

let rec preslikaj f = function
  | [] -> []
  | Element x :: xs -> Element (f x) :: preslikaj f xs
  | Podseznam ys :: xs -> Podseznam (preslikaj f ys) :: preslikaj f xs

let rec splocsi sez = match sez with
  | [] -> []
  | Element x :: xs -> Element x :: splosci xs
  | Podseznam ys :: xs -> splosci ys @ splosci xs

let rec alternirajoci_konstruktorji sez = match sez with
  | [] -> true
  | [x] -> true
  | Element _ :: Podseznam p :: xs -> alternirajoci_konstruktorji (Podseznam p :: xs)
  | Podseznam _ :: Element x :: xs -> alternirajoci_konstruktorji (Element x :: xs)
  | _ -> false

let rec zlozi_gnezdenje f acc g = match g with
  | Element x ->  f acc x
  | Podseznam l -> List.fold (zlozi_gnezdenje f) acc l

let zlozi_preko_gnezdenja f acc sez =
  zlozi_gnezdenje f acc (Podseznam g_list)
