module FigureUtils = struct 
  let eps = 1e-9

  type point = float * float
  type segment = point * point
  type ring = point list
  type polygon = { outer: ring; holes: ring list }

  let eq_point (pt1: point) (pt2: point) : bool =
    let (x1,y1) = pt1 in
    let (x2,y2) = pt2 in
    abs_float (x1 -. x2) < eps && abs_float (y1 -. y2) < eps

  let eq_segment (seg1: segment) (seg2: segment) : bool =
    let (a1,b1) = seg1 in
    let (a2,b2) = seg2 in
    (eq_point a1 a2 && eq_point b1 b2) || (eq_point a1 b2 && eq_point b1 a2)

  let ring_to_gpc_contour (r: ring) : int * Clip.gpc_vertex array =
    let n = List.length r in
    let vertices = Array.make n { Clip.x = 0.0; Clip.y = 0.0 } in
    List.iteri (fun i (x, y) ->
      vertices.(i) <- { Clip.x; Clip.y }
    ) r;
    (n, vertices)

  let normalize_ccw_early (pts: ring) : ring =
    let n = List.length pts in
    if n < 3 then pts else
    let rec sum acc i =
      if i = n then acc
      else
        let (x1, y1) = List.nth pts i in
        let (x2, y2) = List.nth pts ((i + 1) mod n) in
        sum (acc +. (x1 *. y2 -. x2 *. y1)) (i + 1)
    in
    let area = sum 0. 0 /. 2. in
    if area < 0. then List.rev pts else pts

  let polygon_to_gpc (poly: polygon) : Clip.gpc_polygon =
    let outer_ccw = normalize_ccw_early poly.outer in
    let normalize_cw (pts: ring) : ring =
      let n = List.length pts in
      if n < 3 then pts else
      let rec sum acc i =
        if i = n then acc
        else
          let (x1, y1) = List.nth pts i in
          let (x2, y2) = List.nth pts ((i + 1) mod n) in
          sum (acc +. (x1 *. y2 -. x2 *. y1)) (i + 1)
      in
      let area = sum 0. 0 /. 2. in
      if area > 0. then List.rev pts else pts
    in
    let holes_cw = List.map normalize_cw poly.holes in
    
    let outer_contour = ring_to_gpc_contour outer_ccw in
    let hole_contours = List.map ring_to_gpc_contour holes_cw in
    let all_contours = outer_contour :: hole_contours in
    let ncontours = List.length all_contours in
    let hole_flags = Array.make ncontours 0 in
    for i = 1 to ncontours - 1 do
      hole_flags.(i) <- 1
    done;
    (ncontours, hole_flags, Array.of_list all_contours)

  let gpc_to_polygons (gpc_poly: Clip.gpc_polygon) : polygon list =
    let (ncontours, hole_flags, contours) = gpc_poly in
    
    let outer_rings = ref [] in
    let holes = ref [] in
    
    for i = 0 to ncontours - 1 do
      let (nverts, vertices) = contours.(i) in
      let pts = ref [] in
      for j = 0 to nverts - 1 do
        let v = vertices.(j) in
        pts := (v.Clip.x, v.Clip.y) :: !pts
      done;
      let ring = List.rev !pts in
      if hole_flags.(i) = 0 then
        outer_rings := ring :: !outer_rings
      else
        holes := ring :: !holes
    done;
    
    let outer_list = List.rev !outer_rings in
    let holes_list = List.rev !holes in
    
    let hole_in_outer hole outer =
      if List.length hole = 0 then false
      else
        let test_pt = List.hd hole in
        let n = List.length outer in
        if n < 3 then false else
        let rec loop i j inside =
          if i = n then inside
          else
            let xi, yi = List.nth outer i in
            let xj, yj = List.nth outer j in
            let x, y = test_pt in
            let intersect =
              ((yi > y) <> (yj > y)) &&
              (x <= (xj -. xi) *. (y -. yi) /. (yj -. yi) +. xi)
            in
            loop (i + 1) i (if intersect then not inside else inside)
        in
        loop 0 (n - 1) false
    in
    
    List.map (fun outer ->
      let my_holes = List.filter (fun hole -> hole_in_outer hole outer) holes_list in
      { outer; holes = my_holes }
    ) outer_list

  let min_point_index (pts: ring) =
    List.mapi (fun i pt -> (i,pt)) pts
    |> List.fold_left (fun (idx_min, pt_min) (i,pt) ->
        let (x,y) = pt and (xm,ym) = pt_min in
        if x < xm || (x = xm && y < ym) then (i,pt) else (idx_min, pt_min)
       ) (0, List.hd pts)
    |> fst

  let rotate_to_min (pts: ring) : ring =
    let n = List.length pts in
    let idx = min_point_index pts in
    List.init n (fun i -> List.nth pts ((idx + i) mod n))

  let signed_area (pts: ring) : float =
    let n = List.length pts in
    let rec sum acc i =
      if i = n then acc
      else
        let (x1, y1) = List.nth pts i in
        let (x2, y2) = List.nth pts ((i + 1) mod n) in
        sum (acc +. (x1 *. y2 -. x2 *. y1)) (i + 1)
    in
    sum 0. 0 /. 2.

  let normalize_ccw (pts: ring) : ring =
    if signed_area pts < 0. then List.rev pts else pts

  let rotate_to_min_ccw (pts: ring) : ring =
    let rotated = rotate_to_min pts in
    normalize_ccw rotated

  let eq_ring (r1: ring) (r2: ring) : bool =
    let pts1 = rotate_to_min r1 in
    let pts2 = rotate_to_min r2 in
    let rev_pts2 = List.rev pts2 in
    let rec aux l1 l2 = match l1, l2 with
      | [], [] -> true
      | p1::rest1, p2::rest2 -> eq_point p1 p2 && aux rest1 rest2
      | _, _ -> false
    in
    (aux pts1 pts2 || aux pts1 rev_pts2) ||
    let rev_pts2_rotated = rotate_to_min (List.rev pts2) in
    aux pts1 rev_pts2_rotated

  let eq_polygon (poly1: polygon) (poly2: polygon) : bool =
    eq_ring poly1.outer poly2.outer &&
    List.length poly1.holes = List.length poly2.holes &&
    let sorted_holes1 = List.sort compare poly1.holes in
    let sorted_holes2 = List.sort compare poly2.holes in
    List.for_all2 eq_ring sorted_holes1 sorted_holes2
 
  (* Point ∈ Polygon → bool *)
  let ring_contains_pt (pt: point) (r: ring) : bool =
    let x, y = pt in
    let n = List.length r in
    if n < 3 then false
    else
      let on_segment p a b =
        let cross =
          (fst p -. fst a) *. (snd b -. snd a)
          -. (snd p -. snd a) *. (fst b -. fst a)
        in
        abs_float cross < eps &&
        fst p >= min (fst a) (fst b) -. eps &&
        fst p <= max (fst a) (fst b) +. eps &&
        snd p >= min (snd a) (snd b) -. eps &&
        snd p <= max (snd a) (snd b) +. eps
      in

      let rec check_border i =
        if i = n then false
        else
          let a = List.nth r i in
          let b = List.nth r ((i + 1) mod n) in
          if on_segment pt a b then true
          else check_border (i + 1)
      in

      if check_border 0 then true
      else
        let rec loop i j inside =
          if i = n then inside
          else
            let xi, yi = List.nth r i in
            let xj, yj = List.nth r j in
            let intersect =
              ((yi > y) <> (yj > y)) &&
              (x <= (xj -. xi) *. (y -. yi) /. (yj -. yi) +. xi)
            in
            loop (i + 1) i (if intersect then not inside else inside)
        in
        loop 0 (n - 1) false

  let poly_contains_pt (pt: point) (poly: polygon) : bool =
    if not (ring_contains_pt pt poly.outer) then false
    else
      not (List.exists (fun hole -> ring_contains_pt pt hole) poly.holes)
  
  (* Segment ∩ Segment → point *)
  let seg_inter_seg (seg1: segment) (seg2: segment) =
    let (x1, y1), (x2, y2) = seg1 in
    let (x3, y3), (x4, y4) = seg2 in
    let denom =
      (x1 -. x2) *. (y3 -. y4) -. (y1 -. y2) *. (x3 -. x4)
    in
    if abs_float denom < eps then
      None
    else
      let t =
        ((x1 -. x3) *. (y3 -. y4) -. (y1 -. y3) *. (x3 -. x4)) /. denom
      in
      let u =
        -.((x1 -. x2) *. (y1 -. y3) -. (y1 -. y2) *. (x1 -. x3)) /. denom
      in

      if t >= 0. && t <= 1. && u >= 0. && u <= 1. then
        Some (
          x1 +. t *. (x2 -. x1),
          y1 +. t *. (y2 -. y1)
        )
      else
        None  
  
  (* Segment ∩ Polygon → segment list *)
  let param_of_point_on_segment (a : point) (b : point) (p : point) : float =
    let ax, ay = a in
    let bx, by = b in
    let px, py = p in
    if abs_float (bx -. ax) >= abs_float (by -. ay) then
      (px -. ax) /. (bx -. ax)
    else
      (py -. ay) /. (by -. ay)

  let candidate_points_on_segment_ring (seg : segment) (r : ring) : (float * point) list =
    let a, b = seg in
    let n = List.length r in
    let acc = ref [] in

    for i = 0 to n - 1 do
      let p1 = List.nth r i in
      let p2 = List.nth r ((i+1) mod n) in
      match seg_inter_seg (a,b) (p1,p2) with
      | None -> ()
      | Some ip ->
          let t = param_of_point_on_segment a b ip in
          if t >= 0. && t <= 1. then
            acc := (t, ip) :: !acc
    done;
    !acc

  let candidate_points_on_segment (seg : segment) (poly : polygon) : (float * point) list =
    let a, b = seg in
    let acc = ref [] in
    
    acc := candidate_points_on_segment_ring seg poly.outer @ !acc;
    
    List.iter (fun hole ->
      acc := candidate_points_on_segment_ring seg hole @ !acc
    ) poly.holes;

    if poly_contains_pt a poly then acc := (0., a) :: !acc;
    if poly_contains_pt b poly then acc := (1., b) :: !acc;

    !acc

  let build_inside_segments (seg : segment) (poly : polygon) (points : (float * point) list) : segment list =
    let a, b = seg in
    let sorted_points =
      points
      |> List.sort (fun (t1,_) (t2,_) -> compare t1 t2)
      |> List.fold_left (fun acc (t,p) ->
          match acc with
          | (_,p2)::_ when eq_point p p2 -> acc
          | _ -> (t,p)::acc
        ) []
      |> List.rev
    in

    let rec aux acc = function
      | (t1,p1)::(t2,p2)::rest ->
          let mid_t = (t1 +. t2) /. 2. in
          let mid_point =
            (fst a +. mid_t *. (fst b -. fst a),
            snd a +. mid_t *. (snd b -. snd a))
          in
          if poly_contains_pt mid_point poly then
            aux ((p1,p2)::acc) ((t2,p2)::rest)
          else
            aux acc ((t2,p2)::rest)
      | _ -> List.rev acc
    in
    aux [] sorted_points

  let poly_inter_seg (poly : polygon) (seg : segment) : segment list =
    let candidates = candidate_points_on_segment seg poly in
    build_inside_segments seg poly candidates

  (* Point ∈ Segment → bool *)
  let seg_contains_pt (seg : segment) (pt : point) : bool =
    let (x1, y1), (x2, y2) = seg in
    let (x, y) = pt in
    let cross = (y -. y1) *. (x2 -. x1) -. (x -. x1) *. (y2 -. y1) in
    if abs_float cross > eps then false
    else
      x >= min x1 x2 -. eps && x <= max x1 x2 +. eps &&
      y >= min y1 y2 -. eps && y <= max y1 y2 +. eps

  (* Polygon ∩ Polygon *)
  let clip (poly1: polygon) (poly2: polygon) : polygon list =
    let gpc1 = polygon_to_gpc poly1 in
    let gpc2 = polygon_to_gpc poly2 in
    let result = Clip.gpcml_clippolygon Clip.Intersection gpc1 gpc2 in
    let results = gpc_to_polygons result in
    List.map (fun p -> { outer = rotate_to_min_ccw p.outer; holes = List.map rotate_to_min_ccw p.holes }) results

  (* Polygon ∪ Polygon *)
  let union (poly1: polygon) (poly2: polygon) : polygon list =
    let gpc1 = polygon_to_gpc poly1 in
    let gpc2 = polygon_to_gpc poly2 in
    let result = Clip.gpcml_clippolygon Clip.Union gpc1 gpc2 in
    let results = gpc_to_polygons result in
    List.map (fun p -> { outer = rotate_to_min_ccw p.outer; holes = List.map rotate_to_min_ccw p.holes }) results

  (* Polygon \ Polygon → Polygons *)
  let subtract (poly1: polygon) (poly2: polygon) : polygon list =
    let gpc1 = polygon_to_gpc poly1 in
    let gpc2 = polygon_to_gpc poly2 in
    let result = Clip.gpcml_clippolygon Clip.Difference gpc1 gpc2 in
    let results = gpc_to_polygons result in
    List.map (fun p -> { outer = rotate_to_min_ccw p.outer; holes = List.map rotate_to_min_ccw p.holes }) results

  (* Segment \ Segment → Segments *)
  let subtract_segments (seg1: segment) (seg2: segment) : segment list =
    let (x1, y1), (x2, y2) = seg1 in
    let (sx1, sy1), (sx2, sy2) = seg2 in
    
    let cross1 = (sx1 -. x1) *. (y2 -. y1) -. (sy1 -. y1) *. (x2 -. x1) in
    let cross2 = (sx2 -. x1) *. (y2 -. y1) -. (sy2 -. y1) *. (x2 -. x1) in
    
    if abs_float cross1 > eps || abs_float cross2 > eps then
      [seg1]
    else
      let param_t pt =
        let (px, py) = pt in
        if abs_float (x2 -. x1) >= abs_float (y2 -. y1) then
          (px -. x1) /. (x2 -. x1)
        else
          (py -. y1) /. (y2 -. y1)
      in
      
      let t1_start = 0. in
      let t1_end = 1. in
      let t2_start = param_t (sx1, sy1) in
      let t2_end = param_t (sx2, sy2) in
      
      let t2_min = min t2_start t2_end in
      let t2_max = max t2_start t2_end in
      
      let result = ref [] in
      
      (if t1_start < t2_min -. eps then
        let pt_start = (x1 +. t1_start *. (x2 -. x1), y1 +. t1_start *. (y2 -. y1)) in
        let pt_end = (x1 +. min (t2_min -. eps) t1_end *. (x2 -. x1), y1 +. min (t2_min -. eps) t1_end *. (y2 -. y1)) in
        if eq_point pt_start pt_end = false then
          result := (pt_start, pt_end) :: !result
      );
      
      (if t1_end > t2_max +. eps then
        let pt_start = (x1 +. max (t2_max +. eps) t1_start *. (x2 -. x1), y1 +. max (t2_max +. eps) t1_start *. (y2 -. y1)) in
        let pt_end = (x1 +. t1_end *. (x2 -. x1), y1 +. t1_end *. (y2 -. y1)) in
        if eq_point pt_start pt_end = false then
          result := (pt_start, pt_end) :: !result
      );
      
      !result

  (* Segment ∪ Segment → Segments *)
  let union_segments (seg1: segment) (seg2: segment) : segment list =
    let (x1, y1), (x2, y2) = seg1 in
    let (sx1, sy1), (sx2, sy2) = seg2 in
    
    let cross1 = (sx1 -. x1) *. (y2 -. y1) -. (sy1 -. y1) *. (x2 -. x1) in
    let cross2 = (sx2 -. x1) *. (y2 -. y1) -. (sy2 -. y1) *. (x2 -. x1) in
    
    if abs_float cross1 > eps || abs_float cross2 > eps then
      [seg1; seg2]
    else
      let param_t pt =
        let (px, py) = pt in
        if abs_float (x2 -. x1) >= abs_float (y2 -. y1) then
          (px -. x1) /. (x2 -. x1)
        else
          (py -. y1) /. (y2 -. y1)
      in
      
      let t1_start = 0. in
      let t1_end = 1. in
      let t2_start = param_t (sx1, sy1) in
      let t2_end = param_t (sx2, sy2) in
      
      let t_min = min (min t1_start t1_end) (min t2_start t2_end) in
      let t_max = max (max t1_start t1_end) (max t2_start t2_end) in
      
      if t_max -. t_min < eps then
        [seg1]
      else
        let pt_start = (x1 +. t_min *. (x2 -. x1), y1 +. t_min *. (y2 -. y1)) in
        let pt_end = (x1 +. t_max *. (x2 -. x1), y1 +. t_max *. (y2 -. y1)) in
        [(pt_start, pt_end)]
end
