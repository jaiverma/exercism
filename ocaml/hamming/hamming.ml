open Base

type nucleotide = A | C | G | T

let is_equal (a: nucleotide) (b: nucleotide) =
    match (a, b) with
    | (A, A) | (C, C) | (G, G) | (T, T) -> true
    | (_, _) -> false

let hamming_distance (strand_a: nucleotide list) (strand_b: nucleotide list) =
    match (strand_a, strand_b) with
    | [], [] -> Ok 0
    | [], _  -> Error "left strand must not be empty"
    | _, []  -> Error "right strand must not be empty"
    | a, b   ->
        if List.length a <> List.length b then
            Error "left and right strands must be of equal length"
        else
            List.zip_exn a b
                |> List.map ~f:(fun (x, y) ->
                    if is_equal x y then 0 else 1)
                |> List.fold_left
                    ~f:(+)
                    ~init:0
                |> fun x -> Ok x
