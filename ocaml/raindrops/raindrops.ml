let prime_factors (num: int) =
    let rec pf_impl (num: int) (divisor: int) =
        if num = 1 then []
        else
            let rem = num mod divisor in
            if rem = 0 then divisor :: pf_impl (num / divisor) divisor
            else pf_impl num (divisor + 1)
    in

    pf_impl num 2

let unique_list (l: 'a list) =
    let tbl = Hashtbl.create 100 in

    let rec uniq_impl = function
        | [] -> []
        | x::xs ->
            match (Hashtbl.find_opt tbl x) with
            | Some _ -> uniq_impl xs
            | None ->
                Hashtbl.add tbl x true;
                x :: uniq_impl xs
    in

    uniq_impl l

let raindrop (num: int) =
    let pf = prime_factors num |> unique_list in

    let find_fn n = fun x -> x = n in
    let is_three = pf |> List.find_opt (find_fn 3) in
    let is_five = pf |> List.find_opt (find_fn 5) in
    let is_seven = pf |> List.find_opt (find_fn 7) in

    match (is_three, is_five, is_seven) with
        | (None, None, None) ->
            string_of_int num
        | (_, _, _) ->
            pf
                |> List.map (fun n ->
                    match n with
                    | 3 -> "Pling"
                    | 5 -> "Plang"
                    | 7 -> "Plong"
                    | _ -> "")
                |> List.fold_left (^) ""
