open Geo_semantics.Figure_utils.FigureUtils

(* ========== Helper Functions ========== *)

(* Crée un polygone simple (sans trous) à partir d'une liste de points *)
let make_polygon (pts : (float * float) list) : polygon =
  { outer = pts; holes = [] }

(* Crée un polygone avec trous *)
let make_polygon_with_holes (outer : (float * float) list) (holes : (float * float) list list) : polygon =
  { outer = outer; holes = holes }

(* Normalise un ring : le point le plus en bas à gauche en premier *)
let normalize_ring (r : ring) : ring =
  if r = [] then [] else
  let min_point = List.fold_left (fun (min_x, min_y) (x, y) ->
    if y < min_y || (y = min_y && x < min_x) then (x, y) else (min_x, min_y)
  ) (List.hd r) r in
  let rec rotate lst =
    match lst with
    | [] -> []
    | hd :: tl -> if eq_point hd min_point then lst else rotate (tl @ [hd])
  in
  rotate r

let normalize_polygon (poly : polygon) : polygon =
  { outer = normalize_ring poly.outer;
    holes = List.map normalize_ring poly.holes }

let eq_polygon_test (poly1 : polygon) (poly2 : polygon) : bool =
  let norm1 = normalize_polygon poly1 in
  let norm2 = normalize_polygon poly2 in
  eq_ring norm1.outer norm2.outer &&
  List.length norm1.holes = List.length norm2.holes &&
  let sorted_holes1 = List.sort compare norm1.holes in
  let sorted_holes2 = List.sort compare norm2.holes in
  List.for_all2 eq_ring sorted_holes1 sorted_holes2

(* Compare deux points pour le tri (par x, puis par y) *)
let compare_points (x1, y1) (x2, y2) =
  if abs_float (x1 -. x2) < eps then
    if abs_float (y1 -. y2) < eps then 0
    else if y1 < y2 then -1 else 1
  else if x1 < x2 then -1 else 1

(* Compare deux rings normalisés point par point *)
let rec compare_rings (r1 : ring) (r2 : ring) : int =
  match r1, r2 with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | p1 :: rest1, p2 :: rest2 ->
    let cmp = compare_points p1 p2 in
    if cmp <> 0 then cmp else compare_rings rest1 rest2

(* Compare deux polygones normalisés *)
let compare_polygons (poly1 : polygon) (poly2 : polygon) : int =
  let cmp_outer = compare_rings poly1.outer poly2.outer in
  if cmp_outer <> 0 then cmp_outer
  else
    let cmp_nholes = compare (List.length poly1.holes) (List.length poly2.holes) in
    if cmp_nholes <> 0 then cmp_nholes
    else
      let sorted_holes1 = List.sort compare_rings poly1.holes in
      let sorted_holes2 = List.sort compare_rings poly2.holes in
      let rec compare_hole_lists h1 h2 =
        match h1, h2 with
        | [], [] -> 0
        | [], _ -> -1
        | _, [] -> 1
        | hole1::rest1, hole2::rest2 ->
            let cmp = compare_rings hole1 hole2 in
            if cmp <> 0 then cmp else compare_hole_lists rest1 rest2
      in
      compare_hole_lists sorted_holes1 sorted_holes2

(* Compare deux géométries (listes de polygones) après normalisation et tri *)
let eq_geometry (geom1 : polygon list) (geom2 : polygon list) : bool =
  let normalize_geometry geom =
    geom
    |> List.map normalize_polygon
    |> List.sort compare_polygons
  in
  let norm1 = normalize_geometry geom1 in
  let norm2 = normalize_geometry geom2 in
  List.length norm1 = List.length norm2 &&
  List.for_all2 eq_polygon_test norm1 norm2

let point_testable : (float * float) Alcotest.testable =
  let pp fmt (x, y) = Format.fprintf fmt "(%.2f, %.2f)" x y in
  let equal (x1, y1) (x2, y2) = 
    abs_float (x1 -. x2) < eps && abs_float (y1 -. y2) < eps in
  Alcotest.testable pp equal

let option_point_testable : (float * float) option Alcotest.testable =
  Alcotest.option point_testable

let candidate_testable : (float * (float * float)) Alcotest.testable =
  let pp fmt (t, (x, y)) = Format.fprintf fmt "(%.2f, (%.2f, %.2f))" t x y in
  let equal (t1, p1) (t2, p2) = 
    abs_float (t1 -. t2) < eps && eq_point p1 p2 in
  Alcotest.testable pp equal

let candidate_list_testable : (float * (float * float)) list Alcotest.testable =
  Alcotest.list candidate_testable

let geometry_testable : polygon list Alcotest.testable =
  let pp fmt geom =
    let pp_ring r =
      "[" ^ String.concat "; " (List.map (fun (x, y) -> Printf.sprintf "(%.2f, %.2f)" x y) r) ^ "]"
    in
    let pp_poly poly =
      let outer_str = "outer: " ^ pp_ring poly.outer in
      let holes_str = if poly.holes = [] then ""
        else "; holes: [" ^ String.concat "; " (List.map pp_ring poly.holes) ^ "]"
      in
      "{" ^ outer_str ^ holes_str ^ "}"
    in
    Format.fprintf fmt "[%s]"
      (String.concat "; " (List.map pp_poly geom))
  in
  Alcotest.testable pp eq_geometry

(* ========== Tests pour eq_point ========== *)

let test_approx_point_equal () =
  let p1 = (1.0, 2.0) in
  let p2 = (1.0, 2.0) in
  let result = eq_point p1 p2 in
  Alcotest.(check bool) "points égaux" true result

let test_approx_point_close () =
  let p1 = (1.0, 2.0) in
  let p2 = (1.0 +. eps /. 2., 2.0) in
  let result = eq_point p1 p2 in
  Alcotest.(check bool) "points très proches" true result

let test_approx_point_far () =
  let p1 = (1.0, 2.0) in
  let p2 = (3.0, 4.0) in
  let result = eq_point p1 p2 in
  Alcotest.(check bool) "points éloignés" false result

(* ========== Tests pour seg_seg_inter ========== *)

let test_seg_seg_inter_intersecting () =
  let seg1 = ((0., 0.), (2., 2.)) in
  let seg2 = ((0., 2.), (2., 0.)) in
  let expected_result = (1., 1.) in
  let result = seg_seg_inter seg1 seg2 in
  match result with
  | Some p -> Alcotest.(check point_testable) "intersection au centre" expected_result p
  | None -> Alcotest.fail "Devrait trouver une intersection"

let test_seg_seg_inter_parallel () =
  let seg1 = ((0., 0.), (2., 0.)) in
  let seg2 = ((0., 1.), (2., 1.)) in
  let result = seg_seg_inter seg1 seg2 in
  Alcotest.(check option_point_testable) "segments parallèles" None result

  let test_seg_seg_inter_non_overlapping () =
  let seg1 = ((0., 0.), (1., 0.)) in
  let seg2 = ((2., 0.), (3., 0.)) in
  let result = seg_seg_inter seg1 seg2 in
  Alcotest.(check option_point_testable) "segments non-chevauchants" None result

let test_seg_seg_inter_endpoint () =
  let seg1 = ((0., 0.), (1., 1.)) in
  let seg2 = ((1., 1.), (2., 0.)) in
  let result = seg_seg_inter seg1 seg2 in
  let expected_result = Some (1., 1.) in
  Alcotest.(check (option point_testable)) "intersection à une extrémité" expected_result result

(* ========== Tests pour poly_contains_pt ========== *)

let test_poly_contains_pt_inside () =
  let square = make_polygon [(0., 0.); (2., 0.); (2., 2.); (0., 2.)] in
  let pt = (1., 1.) in
  let result = poly_contains_pt pt square in
  Alcotest.(check bool) "point à l'intérieur d'un carré" true result

let test_poly_contains_pt_outside () =
  let square = make_polygon [(0., 0.); (2., 0.); (2., 2.); (0., 2.)] in
  let pt = (3., 3.) in
  let result = poly_contains_pt pt square in
  Alcotest.(check bool) "point à l'extérieur d'un carré" false result

let test_poly_contains_pt_on_edge () =
  let square = make_polygon [(0., 0.); (2., 0.); (2., 2.); (0., 2.)] in
  let pt = (1., 0.) in
  let result = poly_contains_pt pt square in
  Alcotest.(check bool) "point sur un bord" true result

let test_poly_contains_pt_on_vertex () =
  let square = make_polygon [(0., 0.); (2., 0.); (2., 2.); (0., 2.)] in
  let pt = (0., 0.) in
  let result = poly_contains_pt pt square in
  Alcotest.(check bool) "point sur un sommet" true result

(* ========== Tests pour param_of_point_on_segment ========== *)

let test_param_start_point () =
  let a = (0., 0.) in
  let b = (4., 0.) in
  let p = (0., 0.) in
  let result = param_of_point_on_segment a b p in
  Alcotest.(check (float 0.01)) "paramètre au point de départ" 0.0 result

let test_param_end_point () =
  let a = (0., 0.) in
  let b = (4., 0.) in
  let p = (4., 0.) in
  let result = param_of_point_on_segment a b p in
  Alcotest.(check (float 0.01)) "paramètre au point d'arrivée" 1.0 result

let test_param_midpoint () =
  let a = (0., 0.) in
  let b = (4., 0.) in
  let p = (2., 0.) in
  let result = param_of_point_on_segment a b p in
  Alcotest.(check (float 0.01)) "paramètre au milieu" 0.5 result

(* ========== Tests pour candidate_points_on_segment ========== *)

let test_candidate_points_crossing_square () =
  let square = make_polygon [(0., 0.); (2., 0.); (2., 2.); (0., 2.)] in
  let seg = ((-1., 1.), (3., 1.)) in
  let candidates = candidate_points_on_segment seg square in
  let expected_candidates = [
    (0.25, (0., 1.));
    (0.75, (2., 1.))
  ] in
  Alcotest.(check candidate_list_testable) "points candidats pour un segment traversant un carré" expected_candidates candidates

let test_candidate_points_inside_square () =
  let square = make_polygon [(0., 0.); (2., 0.); (2., 2.); (0., 2.)] in
  let seg = ((0.5, 0.5), (1.5, 1.5)) in
  let candidates = candidate_points_on_segment seg square in
  let expected_candidates = [
    (1.0, (1.5, 1.5));
    (0.0, (0.5, 0.5));
  ] in
  Alcotest.(check candidate_list_testable) "segment entièrement dans le carré" expected_candidates candidates

let test_candidate_points_outside_square () =
  let square = make_polygon [(0., 0.); (2., 0.); (2., 2.); (0., 2.)] in
  let seg = ((3., 3.), (4., 4.)) in
  let candidates = candidate_points_on_segment seg square in
  let expected_candidates = [] in
  Alcotest.(check candidate_list_testable) "segment à l'extérieur du carré" expected_candidates candidates

(* ========== Tests pour build_inside_segments ========== *)

let test_build_single_segment () =
  let seg = ((-1., 1.), (3., 1.)) in
  let poly = make_polygon [(0., 0.); (2., 0.); (2., 2.); (0., 2.)] in
  let candidates = [
    (0.25, (0., 1.));
    (0.75, (2., 1.))
  ] in
  let result = build_inside_segments seg poly candidates in
  let expected_result = [((0., 1.), (2., 1.))] in
  Alcotest.(check (Alcotest.list (Alcotest.pair point_testable point_testable)))
    "un segment intérieur" expected_result result

(* ========== Tests pour l'algorithme de clipping ========== *)

let test_clip_overlapping_squares () =
  let square1 = make_polygon [(0., 0.); (2., 0.); (2., 2.); (0., 2.)] in
  let square2 = make_polygon [(1., 1.); (3., 1.); (3., 3.); (1., 3.)] in
  let result = clip square1 square2 in
  let expected_result = [make_polygon [(1., 1.); (2., 1.); (2., 2.); (1., 2.)]] in
  Alcotest.(check geometry_testable) "deux carrés qui se chevauchent" expected_result result

let test_clip_disjoint_squares () =
  let square1 = make_polygon [(0., 0.); (1., 0.); (1., 1.); (0., 1.)] in
  let square2 = make_polygon [(2., 2.); (3., 2.); (3., 3.); (2., 3.)] in
  let result = clip square1 square2 in
  Alcotest.(check geometry_testable) "deux carrés disjoints" [] result

let test_clip_contained_square () =
  let square1 = make_polygon [(0., 0.); (4., 0.); (4., 4.); (0., 4.)] in
  let square2 = make_polygon [(1., 1.); (3., 1.); (3., 3.); (1., 3.)] in
  let result = clip square1 square2 in
  Alcotest.(check geometry_testable) "un carré contenu dans l'autre" [square2] result

let test_clip_vertex_on_edge () =
  let triangle1 = make_polygon [(1., 1.); (3., 1.); (2., 3.)] in
  let triangle2 = make_polygon [(1., 2.); (3., 2.); (2., 0.)] in
  let result = clip triangle1 triangle2 in
  let expected_result = [make_polygon [(1.5, 1.); (2., 2.); (2.5, 1.); (2., 0.)]] in
  Alcotest.(check geometry_testable) "triangles : sommet sur arête (cas dégénéré)" expected_result result

let test_clip_cross_and_square () =
  let cross = make_polygon
    [(1., 0.); (2., 0.); (2., 1.); (3., 1.); (3., 2.);
     (2., 2.); (2., 3.); (1., 3.); (1., 2.); (0., 2.);
     (0., 1.); (1., 1.)]
  in
  let square = make_polygon [(1., 1.); (3., 1.); (3., 3.); (1., 3.)] in
  let result = clip cross square in
  let expected_result = [make_polygon [(1., 1.); (3., 1.); (3., 2.); (2., 2.); (2., 3.); (1., 3.)]] in
  Alcotest.(check geometry_testable) "croix et carré avec arêtes partagées (cas dégénéré)" expected_result result

let test_clip_H_and_square () =
  let h_shape = make_polygon
    [(0., 0.); (1., 0.); (1., 1.); (2., 1.); (2., 0.); (3., 0.);
     (3., 3.); (2., 3.); (2., 2.); (1., 2.); (1., 3.); (0., 3.)]
  in
  let square = make_polygon [(0.5, 0.5); (2.5, 0.5); (2.5, 2.5); (0.5, 2.5)] in
  let result = clip h_shape square in
  let expected_result = [
    make_polygon [(0.5, 0.5); (1., 0.5); (1., 1.); (2., 1.); (2., 0.5);(2.5, 0.5); 
    (2.5, 2.5); (2., 2.5); (2., 2.); (1., 2.); (1., 2.5); (0.5, 2.5)]
  ] in
  Alcotest.(check geometry_testable) "formes complexes : H et carré" expected_result result

let test_clip_bridge_and_bar () =
  let bridge = make_polygon
    [(0., 2.); (1., 2.); (1., 3.); (2., 3.); (2., 2.); (3., 2.);
     (3., 3.); (2., 3.); (2., 4.); (1., 4.); (1., 3.); (0., 3.)]
  in
  let bar = make_polygon [(0., 2.); (3., 2.); (3., 2.5); (0., 2.5)] in
  let result = clip bridge bar in
  let expected_result = [
    make_polygon [(0., 2.); (1., 2.); (1., 2.5); (0., 2.5)];
    make_polygon [(2., 2.); (3., 2.); (3., 2.5); (2., 2.5)]
  ] in
  Alcotest.(check geometry_testable) "pont et barre : intersection avec résultats disjoints" expected_result result

(* ========== Tests pour union ========== *)

let test_union_disjoint_squares () =
  let square1 = make_polygon [(0., 0.); (1., 0.); (1., 1.); (0., 1.)] in
  let square2 = make_polygon [(2., 2.); (3., 2.); (3., 3.); (2., 3.)] in
  let result = union square1 square2 in
  Alcotest.(check geometry_testable) "union de deux carrés disjoints" [square1; square2] result

let test_union_overlapping_squares () =
  let square1 = make_polygon [(0., 0.); (2., 0.); (2., 2.); (0., 2.)] in
  let square2 = make_polygon [(1., 1.); (3., 1.); (3., 3.); (1., 3.)] in
  let result = union square1 square2 in
  let expected_result = [make_polygon [(0., 0.); (2., 0.); (2., 1.); (3., 1.); (3., 3.); (1., 3.); (1., 2.); (0., 2.)]] in
  Alcotest.(check geometry_testable) "union de deux carrés qui se chevauchent" expected_result result

let test_union_contained_square () =
  let large_square = make_polygon [(0., 0.); (4., 0.); (4., 4.); (0., 4.)] in
  let small_square = make_polygon [(1., 1.); (3., 1.); (3., 3.); (1., 3.)] in
  let result = union large_square small_square in
  Alcotest.(check geometry_testable) "union avec un carré contenu" [large_square] result

let test_union_identical_squares () =
  let square = make_polygon [(0., 0.); (2., 0.); (2., 2.); (0., 2.)] in
  let result = union square square in
  Alcotest.(check geometry_testable) "union de deux carrés identiques" [square] result

let test_union_touching_squares () =
  let square1 = make_polygon [(0., 0.); (1., 0.); (1., 1.); (0., 1.)] in
  let square2 = make_polygon [(1., 0.); (2., 0.); (2., 1.); (1., 1.)] in
  let result = union square1 square2 in
  let expected_result = [make_polygon [(0., 0.); (2., 0.); (2., 1.); (0., 1.)]] in
  Alcotest.(check geometry_testable) "union de deux carrés qui partagent une arête (cas dégénéré)" expected_result result

let test_union_triangles () =
  let triangle1 = make_polygon [(0., 0.); (2., 0.); (1., 2.)] in
  let triangle2 = make_polygon [(1., 1.); (3., 1.); (2., 3.)] in
  let result = union triangle1 triangle2 in
  Alcotest.(check bool) "triangles qui se chevauchent partiellement" true (List.length result = 1 && List.length (List.hd result).outer = 7)

(* ========== Tests pour subtract ========== *)

let test_subtract_disjoint_squares () =
  let square1 = make_polygon [(0., 0.); (2., 0.); (2., 2.); (0., 2.)] in
  let square2 = make_polygon [(3., 3.); (5., 3.); (5., 5.); (3., 5.)] in
  let result = subtract square1 square2 in
  Alcotest.(check geometry_testable) "soustraction de carrés disjoints" [square1] result

let test_subtract_contained_square () =
  let large_square = make_polygon [(0., 0.); (4., 0.); (4., 4.); (0., 4.)] in
  let small_square = make_polygon [(1., 1.); (3., 1.); (3., 3.); (1., 3.)] in
  let result = subtract large_square small_square in
  (* Le résultat devrait être un polygone avec un trou *)
  let expected_result = [make_polygon_with_holes
    [(0., 0.); (4., 0.); (4., 4.); (0., 4.)]
    [[(1., 1.); (3., 1.); (3., 3.); (1., 3.)]]
  ] in
  Alcotest.(check geometry_testable) "soustraction d'un carré contenu crée un trou" expected_result result

let test_subtract_overlapping_squares () =
  let square1 = make_polygon [(0., 0.); (2., 0.); (2., 2.); (0., 2.)] in
  let square2 = make_polygon [(1., 1.); (3., 1.); (3., 3.); (1., 3.)] in
  let result = subtract square1 square2 in
  (* Le résultat devrait être une forme en L *)
  let expected_result = [make_polygon [(0., 0.); (2., 0.); (2., 1.); (1., 1.); (1., 2.); (0., 2.)]] in
  Alcotest.(check geometry_testable) "soustraction de carrés qui se chevauchent" expected_result result

let test_subtract_identical_squares () =
  let square = make_polygon [(0., 0.); (2., 0.); (2., 2.); (0., 2.)] in
  let result = subtract square square in
  Alcotest.(check geometry_testable) "soustraction de carrés identiques" [] result

let test_subtract_creates_multiple_polygons () =
  (* Un grand carré moins un petit carré au milieu qui touche les bords *)
  let large_square = make_polygon [(0., 0.); (4., 0.); (4., 4.); (0., 4.)] in
  let bar = make_polygon [(1.5, 0.); (2.5, 0.); (2.5, 4.); (1.5, 4.)] in
  let result = subtract large_square bar in
  (* Le résultat devrait être deux rectangles *)
  let expected_result = [
    make_polygon [(0., 0.); (1.5, 0.); (1.5, 4.); (0., 4.)];
    make_polygon [(2.5, 0.); (4., 0.); (4., 4.); (2.5, 4.)]
  ] in
  Alcotest.(check geometry_testable) "soustraction créant deux polygones" expected_result result

let test_subtract_polygon_with_hole () =
  (* Tester la soustraction impliquant un polygone qui a déjà un trou *)
  let poly_with_hole = make_polygon_with_holes 
    [(0., 0.); (6., 0.); (6., 6.); (0., 6.)]
    [[(2., 2.); (4., 2.); (4., 4.); (2., 4.)]]
  in
  let small_square = make_polygon [(1., 1.); (3., 1.); (3., 3.); (1., 3.)] in
  let result = subtract poly_with_hole small_square in
  (* Le résultat devrait avoir au moins un polygone avec possiblement deux trous *)
  let has_valid_structure = 
    List.length result >= 1 && 
    List.for_all (fun p -> List.length p.outer >= 3) result
  in
  Alcotest.(check bool) "soustraction d'un polygone avec trou" true has_valid_structure

let test_poly_contains_pt_with_hole () =
  (* Test que poly_contains_pt détecte correctement les trous *)
  let poly_with_hole = make_polygon_with_holes 
    [(0., 0.); (4., 0.); (4., 4.); (0., 4.)]
    [[(1., 1.); (3., 1.); (3., 3.); (1., 3.)]]
  in
  let pt_outside = (5., 5.) in
  let pt_in_outer = (0.5, 0.5) in
  let pt_in_hole = (2., 2.) in
  Alcotest.(check bool) "point extérieur au polygone avec trou" false (poly_contains_pt pt_outside poly_with_hole);
  Alcotest.(check bool) "point dans la partie pleine" true (poly_contains_pt pt_in_outer poly_with_hole);
  Alcotest.(check bool) "point dans le trou" false (poly_contains_pt pt_in_hole poly_with_hole)

(* ========== Suites de tests ========== *)

let approx_point_suite = [
  "points égaux", `Quick, test_approx_point_equal;
  "points très proches", `Quick, test_approx_point_close;
  "points éloignés", `Quick, test_approx_point_far;
]

let seg_seg_inter_suite = [
  "segments qui se croisent", `Quick, test_seg_seg_inter_intersecting;
  "segments parallèles", `Quick, test_seg_seg_inter_parallel;
  "segments non-chevauchants", `Quick, test_seg_seg_inter_non_overlapping;
  "intersection à une extrémité", `Quick, test_seg_seg_inter_endpoint;
]

let poly_contains_pt_suite = [
  "point à l'intérieur d'un carré", `Quick, test_poly_contains_pt_inside;
  "point à l'extérieur d'un carré", `Quick, test_poly_contains_pt_outside;
  "point sur un bord", `Quick, test_poly_contains_pt_on_edge;
  "point sur un sommet", `Quick, test_poly_contains_pt_on_vertex;
]

let param_of_point_suite = [
  "paramètre au point de départ", `Quick, test_param_start_point;
  "paramètre au point d'arrivée", `Quick, test_param_end_point;
  "paramètre au milieu", `Quick, test_param_midpoint;
]

let candidate_points_suite = [
  "points candidats pour un segment traversant un carré", `Quick, test_candidate_points_crossing_square;
  "segment entièrement dans le carré", `Quick, test_candidate_points_inside_square;
  "segment à l'extérieur du carré", `Quick, test_candidate_points_outside_square;
]

let build_inside_segments_suite = [
  "un segment intérieur", `Quick, test_build_single_segment;
]

let clip_suite = [
  "deux carrés qui se chevauchent", `Quick, test_clip_overlapping_squares;
  "deux carrés disjoints", `Quick, test_clip_disjoint_squares;
  "un carré contenu dans l'autre", `Quick, test_clip_contained_square;
  "triangles : sommet sur arête (cas dégénéré)", `Quick, test_clip_vertex_on_edge;
  "croix et carré avec arêtes partagées (cas dégénéré)", `Quick, test_clip_cross_and_square;
  "formes complexes : H et carré", `Quick, test_clip_H_and_square;
  "pont et barre : intersection avec résultats disjoints", `Quick, test_clip_bridge_and_bar;
]

let union_suite = [
  "union de deux carrés disjoints", `Quick, test_union_disjoint_squares;
  "union de deux carrés qui se chevauchent", `Quick, test_union_overlapping_squares;
  "union avec un carré contenu", `Quick, test_union_contained_square;
  "union de deux carrés identiques", `Quick, test_union_identical_squares;
  "union de deux carrés qui partagent une arête (cas dégénéré)", `Quick, test_union_touching_squares;
  "union de deux triangles qui se chevauchent", `Quick, test_union_triangles;
]

let subtract_suite = [
  "soustraction de carrés disjoints", `Quick, test_subtract_disjoint_squares;
  "soustraction d'un carré contenu crée un trou", `Quick, test_subtract_contained_square;
  "soustraction de carrés qui se chevauchent", `Quick, test_subtract_overlapping_squares;
  "soustraction de carrés identiques", `Quick, test_subtract_identical_squares;
  "soustraction créant deux polygones", `Quick, test_subtract_creates_multiple_polygons;
  "soustraction d'un polygone avec trou", `Quick, test_subtract_polygon_with_hole;
  "point dans polygone avec trou", `Quick, test_poly_contains_pt_with_hole;
]

(* ========== Fonction principale ========== *)

let () =
  Alcotest.run "Tests de géométrie" [
    "eq_point", approx_point_suite;
    "seg_seg_inter", seg_seg_inter_suite;
    "poly_contains_pt", poly_contains_pt_suite;
    "param_of_point_on_segment", param_of_point_suite;
    "candidate_points_on_segment", candidate_points_suite;
    "build_inside_segments", build_inside_segments_suite;
    "clip", clip_suite;
    "union", union_suite;
    "subtract", subtract_suite;
  ];
  ()
