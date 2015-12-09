open Core.Std

open Optimize

let passes = [ Optimize.constfold ]

let optimized_of_sast sexpr =
  List.fold_left passes ~init:sexpr ~f:(fun sexpr pass -> pass sexpr)
