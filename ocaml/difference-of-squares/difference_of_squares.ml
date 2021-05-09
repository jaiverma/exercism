let square_of_sum (n: int) =
    let n_f = n |> float_of_int in
    (n_f *. (n_f +. 1.) /. 2.) ** 2. |> int_of_float

let sum_of_squares (n: int) =
    let n_f = n |> float_of_int in
    (n_f *. (n_f +. 1.) *. (2. *. n_f +. 1.)) /. 6. |> int_of_float

let difference_of_squares (n: int) =
    (n |> square_of_sum) - (n |> sum_of_squares)
