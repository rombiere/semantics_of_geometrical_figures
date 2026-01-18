open Geo

module M = Necromonads.ID

module IMap = Map.Make( Stdlib.Int )


module Types = struct
  type point = float * float
  type segment = point * point
  type polygon = Geo_semantics.Figure_utils.FigureUtils.polygon
  type ident = int
  type 'a env = 'a IMap.t
end


module Spec = struct
  include Unspec (M) (Types)
  include Geo_semantics.Figure_utils

  let in_env (env, id) = if IMap.mem id env then TT else FF
  let get_env (env, id) = IMap.find id env
  let set_env (env, id, v) = IMap.add id v env
  let empty_venv = IMap.empty

  let eq_point (p1, p2) = if p1 = p2 then M.ret TT else M.ret FF

  let eq_segment (seg1, seg2) = if FigureUtils.eq_segment seg1 seg2 then M.ret TT else M.ret FF

  let eq_polygon (poly1, poly2) = if FigureUtils.eq_polygon poly1 poly2 then M.ret TT else M.ret FF

  let poly_includes_pt (poly, pt) = if FigureUtils.poly_contains_pt pt poly then M.ret TT else M.ret FF

  let seg_includes_pt (seg, pt) = if FigureUtils.seg_contains_pt seg pt then M.ret TT else M.ret FF

  let poly_includes_seg (poly, seg) = if FigureUtils.poly_contains_seg poly seg then M.ret TT else M.ret FF

  let seg_seg_inter (seg1, seg2) =
    if FigureUtils.eq_segment seg1 seg2 then M.ret (Segment seg1) else 
    match FigureUtils.seg_seg_inter seg1 seg2 with
    | Some inter -> M.ret (Point inter)
    | None -> M.ret Empty

  let seg_poly_inter (seg, poly) =
    let inter = FigureUtils.seg_poly_inter seg poly in
    let inter_geo = List.fold_right (fun s acc -> Cons (Segment s, acc)) inter Nil in
    M.ret inter_geo
  
  let poly_poly_inter (poly1, poly2) =
    let polys = FigureUtils.clip poly1 poly2 in
    let figs = List.map (fun p -> Polygon p) polys in
    let inter = List.fold_right (fun f acc -> Cons (f, acc)) figs Nil in
    M.ret inter

  let seg_union_seg (seg1, seg2) =
    let segs = FigureUtils.union_segments seg1 seg2 in
    let figs = List.map (fun s -> Segment s) segs in
    let union = List.fold_right (fun f acc -> Cons (f, acc)) figs Nil in
    M.ret union

  let poly_union_poly (poly1, poly2) =
    let polys = FigureUtils.union poly1 poly2 in
    let figs = List.map (fun p -> Polygon p) polys in
    let union = List.fold_right (fun f acc -> Cons (f, acc)) figs Nil in
    M.ret union

  let polygon_difference (poly1, poly2) =
    let polys = FigureUtils.subtract poly1 poly2 in
    let figs = List.map (fun p -> Polygon p) polys in
    let diff = List.fold_right (fun f acc -> Cons (f, acc)) figs Nil in
    M.ret diff

  let seg_difference (seg1, seg2) =
    let segs = FigureUtils.subtract_segments seg1 seg2 in
    let figs = List.map (fun s -> Segment s) segs in
    let diff = List.fold_right (fun f acc -> Cons (f, acc)) figs Nil in
    M.ret diff

end

open MakeInterpreter(Spec)

(* Fonction de conversion figure/geometry -> string *)
let string_of_figure = function
  | Empty -> "∅"
  | Point (x, y) -> Printf.sprintf "Point(%.1f,%.1f)" x y
  | Segment ((x1, y1), (x2, y2)) -> Printf.sprintf "Seg[(%.1f,%.1f)-(%.1f,%.1f)]" x1 y1 x2 y2
  | Polygon poly ->
      let ring_str ring = String.concat "," (List.map (fun (x, y) -> Printf.sprintf "(%.1f,%.1f)" x y) ring) in
      let outer_str = ring_str poly.outer in
      let holes_str = if poly.holes = [] then ""
        else " holes:[" ^ String.concat ";" (List.map ring_str poly.holes) ^ "]"
      in
      Printf.sprintf "Poly[%s%s]" outer_str holes_str

let rec string_of_geometry = function
  | Nil -> "[]"
  | Cons (fig, rest) ->
      let rest_str = string_of_geometry rest in
      let fig_str = string_of_figure fig in
      if rest_str = "[]" then fig_str else fig_str ^ "; " ^ rest_str

let string_of_value = function
  | VGeometry g -> string_of_geometry g
  | VBool b -> match b with TT -> "True" | FF -> "False"

(* Exemples d'utilisation de la sémantique opérationnelle *)

let test_intersection_union_difference () =
  Printf.printf "\n=== Ex1: A ∩ B, A ∪ B, A Δ B, A \\ B ===\n";
  let a = Geo_semantics.Figure_utils.FigureUtils.{ outer = [(0., 0.); (4., 0.); (4., 4.); (0., 4.)]; holes = [] } in
  let b = Geo_semantics.Figure_utils.FigureUtils.{ outer = [(2., 2.); (6., 2.); (6., 6.); (2., 6.)]; holes = [] } in
  Printf.printf "A = %s\nB = %s\n"
    (string_of_geometry (Cons (Polygon a, Nil)))
    (string_of_geometry (Cons (Polygon b, Nil)));
  let a_geo, b_geo = Cons (Polygon a, Nil), Cons (Polygon b, Nil) in
  let inter_val =
    run_eval(
      LetIn (0, Geometry a_geo,
      LetIn (1, Geometry b_geo,
      Intersection (Var 0, Var 1)))
    )
  in
  let union_val = run_eval(
    LetIn (0, Geometry a_geo,
    LetIn (1, Geometry b_geo,
    Union (Var 0, Var 1)))
  ) in
  let sym_diff_val =run_eval(
    LetIn (0, Geometry a_geo,
    LetIn (1, Geometry b_geo,
    SymmetricDifference (Var 0, Var 1)))
  ) in
  let diff_val = run_eval(
      LetIn (0, Geometry a_geo,
      LetIn (1, Geometry b_geo,
      Difference (Var 0, Var 1)))
  ) in
  Printf.printf "A ∩ B = %s \nA ∪ B = %s \nA Δ B = %s\nA \\ B = %s\n"
    (string_of_value inter_val) (string_of_value union_val) (string_of_value sym_diff_val) (string_of_value diff_val)

let test_associativity_union () =
  let a = Geo_semantics.Figure_utils.FigureUtils.{ outer = [(0., 0.); (2., 0.); (2., 2.); (0., 2.)]; holes = [] } in
  let b = Geo_semantics.Figure_utils.FigureUtils.{ outer = [(1., 1.); (3., 1.); (3., 3.); (1., 3.)]; holes = [] } in
  let c = Geo_semantics.Figure_utils.FigureUtils.{ outer = [(2., 2.); (4., 2.); (4., 4.); (2., 4.)]; holes = [] } in
  let a_geo = Cons (Polygon a, Nil) in
  let b_geo = Cons (Polygon b, Nil) in
  let c_geo = Cons (Polygon c, Nil) in
  Printf.printf "\n=== Ex2: Associativité : A ∪ (B ∪ C) = (A ∪ B) ∪ C ===\nA = %s\nB = %s\nC = %s\nRésultat du test d'égalité: %s\nRésultat attendu: True\n"
    (string_of_geometry a_geo)
    (string_of_geometry b_geo)
    (string_of_geometry c_geo)
    (string_of_value (run_eval (
      LetIn (0, Geometry a_geo,
      LetIn (1, Geometry b_geo,
      LetIn (2, Geometry c_geo,
      LetIn (3, Union (Var 1, Var 2),
      LetIn (4, Union (Var 0, Var 3),
      LetIn (5, Union (Var 0, Var 1),
      LetIn (6, Union (Var 5, Var 2),
      Eq (Var 4, Var 6)))))))))))
    
let test_union_contains_intersection () =
  let a = Geo_semantics.Figure_utils.FigureUtils.{ outer = [(0., 0.); (2., 0.); (2., 2.); (0., 2.)]; holes = [] } in
  let b = Geo_semantics.Figure_utils.FigureUtils.{ outer = [(1., 1.); (3., 1.); (3., 3.); (1., 3.)]; holes = [] } in
  let a_geo = Cons (Polygon a, Nil) in
  let b_geo = Cons (Polygon b, Nil) in
  Printf.printf "\n=== Ex3: (A ∪ B) ⊇ (A ∩ B) ===\nA = %s\nB = %s\nRésultat du test d'inclusion: %s\nRésultat attendu: True\n"
    (string_of_geometry a_geo)
    (string_of_geometry b_geo)
    (string_of_value (run_eval (
      LetIn (0, Geometry a_geo,
      LetIn (1, Geometry b_geo,
      LetIn (2, Intersection (Var 0, Var 1),
      LetIn (3, Union (Var 0, Var 1),
      Includes (Var 3, Var 2))))))))

let test_distributivity () =
  let a = Geo_semantics.Figure_utils.FigureUtils.{ outer = [(0., 0.); (2., 0.); (2., 2.); (0., 2.)]; holes = [] } in
  let b = Geo_semantics.Figure_utils.FigureUtils.{ outer = [(1., 1.); (3., 1.); (3., 3.); (1., 3.)]; holes = [] } in
  let c = Geo_semantics.Figure_utils.FigureUtils.{ outer = [(2., 2.); (4., 2.); (4., 4.); (2., 4.)]; holes = [] } in
  let a_geo = Cons (Polygon a, Nil) in
  let b_geo = Cons (Polygon b, Nil) in
  let c_geo = Cons (Polygon c, Nil) in
  Printf.printf "\n=== Ex4: Distributivité : A ∩ (B ∪ C) = (A ∩ B) ∪ (A ∩ C) ===\nA = %s\nB = %s\nC = %s\nRésultat du test d'égalité: %s\nRésultat attendu: True\n"
    (string_of_geometry a_geo)
    (string_of_geometry b_geo)
    (string_of_geometry c_geo)
    (string_of_value (run_eval (
      LetIn (0, Geometry a_geo,
      LetIn (1, Geometry b_geo,
      LetIn (2, Geometry c_geo,
      LetIn (3, Union (Var 1, Var 2),
      LetIn (4, Intersection (Var 0, Var 3),
      LetIn (5, Intersection (Var 0, Var 1),
      LetIn (6, Intersection (Var 0, Var 2),
      LetIn (7, Union (Var 5, Var 6),
      Eq (Var 4, Var 7))))))))))))

let test_symmetric_difference_not_union () =
    let a = Geo_semantics.Figure_utils.FigureUtils.{ outer = [(0., 0.); (2., 0.); (2., 2.); (0., 2.)]; holes = [] } in
    let b = Geo_semantics.Figure_utils.FigureUtils.{ outer = [(1., 1.); (3., 1.); (3., 3.); (1., 3.)]; holes = [] } in
    let a_geo = Cons (Polygon a, Nil) in
    let b_geo = Cons (Polygon b, Nil) in
    Printf.printf "\n=== Ex5: A ∪ B = A Δ B ===\nA = %s\nB = %s\nRésultat du test d'égalité: %s\nRésultat attendu: False\n"
      (string_of_geometry a_geo)
      (string_of_geometry b_geo)
      (string_of_value (run_eval (
        LetIn (0, Geometry a_geo,
        LetIn (1, Geometry b_geo,
        LetIn (3, SymmetricDifference (Var 0, Var 1),
        LetIn (4, Union (Var 0, Var 1),
        Eq (Var 3, Var 4))))))))

let test_distributivity_of_difference_on_union () =
  let a_geo = Cons (Segment ((0., 0.), (3., 0.)), Nil) in
  let b_geo = Cons (Segment ((0., 0.), (1., 0.)), Nil) in
  let c_geo = Cons (Segment ((2., 0.), (3., 0.)), Nil) in
  Printf.printf "\n=== Ex6: Distributivité de la différence sur l'union : A \\ (B ∪ C) = (A \\ B) ∪ (A \\ C) ===\nA = %s\nB = %s\nC = %s\nRésultat du test d'égalité: %s\nRésultat attendu: False\n"
    (string_of_geometry a_geo)
    (string_of_geometry b_geo)
    (string_of_geometry c_geo)
    (string_of_value (run_eval (
      LetIn (0, Geometry a_geo,
      LetIn (1, Geometry b_geo,
      LetIn (2, Geometry c_geo,
      LetIn (3, Union (Var 1, Var 2),
      LetIn (4, Difference (Var 0, Var 3),
      LetIn (5, Difference (Var 0, Var 1),
      LetIn (6, Difference (Var 0, Var 2),
      LetIn (7, Union (Var 5, Var 6),
      Eq (Var 4, Var 7))))))))))))


(* Exécution de tous les exemples *)
let () =
  Printf.printf "\n=== EXEMPLES ===\n";
  test_intersection_union_difference ();
  test_associativity_union ();
  test_union_contains_intersection ();
  test_distributivity ();
  test_symmetric_difference_not_union ();
  test_distributivity_of_difference_on_union ();
  Printf.printf "\n=== FIN ===\n"
