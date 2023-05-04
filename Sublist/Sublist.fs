module Sublist

type SublistType = Equal | Sublist | Superlist | Unequal

let sublist xs ys =
    let rec sublist (a: List<'a>) (b: List<'a>) =
        match a, b with
        | _, [] -> Unequal
        | c, d when c.Length <= d.Length && c = List.take c.Length d -> Sublist
        | _, [e] -> Unequal
        | c, hd::tl -> sublist c tl
    let rec superlist (a: List<'a>) (b: List<'a>) =
        match a, b with
        | [], _ -> Unequal
        | c, d when d.Length <= c.Length && d = List.take d.Length c -> Superlist
        | [e], _ -> Unequal
        | hd::tl, d -> superlist tl d
    match xs, ys with
    | a, b when a = b -> Equal
    | [], _ -> Sublist
    | _, [] -> Superlist
    | a, b ->
        if xs.Length < ys.Length then
            sublist xs ys
        else
            superlist xs ys

