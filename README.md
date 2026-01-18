# Projet : Sémantique des figures géométriques

Projet de sémantiques avancées - M2 SIF

Un interpréteur de langage dédié à la géométrie computationnelle, écrit en Skel. Le projet implémente des opérations géométriques sur des points, segments et polygones.

Une bibliothèque d'opérations géométriques élémentaires a d'abord dû être écrite en OCaml. Ces opérations incluent :
- Égalité de points, segments, polygones
- Test d'inclusion de points dans segments et polygones
- Intersection segment-segment
- Intersection segment-polygone
- Opérations sur polygones (intersection, union, différence)

## 🚀 Lancement rapide

```bash
git clone https://github.com/rombiere/geo_semantics.git 
cd geo_semantics
opam install camlgpc
dune build
dune exe geo_semantics
```

## ✨ Fonctionnalités

### Types géométriques supportés
- **Point** : coordonnées (x, y)
- **Segment** : paire de points
- **Polygon** : polygone avec ring extérieur et trous optionnels
- **Geometry** : collection de figures géométriques

### Opérations implémentées

#### Opérations booléennes
- **Égalité** (`Eq`) : teste l'égalité entre deux géométries
- **Intersection** (`Intersects`) : teste si deux géométries s'intersectent
- **Inclusion** (`Includes`) : teste si une géométrie contient une autre
- **IsEmpty** : teste si une géométrie est vide

#### Opérations géométriques
- **Intersection** (`Intersection`) : calcule l'intersection de deux géométries
- **Union** (`Union`) : calcule l'union de deux géométries
- **Différence** (`Difference`) : calcule la différence A \ B
- **Différence symétrique** (`SymmetricDifference`) : calcule (A \ B) ∪ (B \ A)

### 📋 Prérequis
 
### Logiciels requis
- **OCaml** >= 5.1.1 (testé avec 5.4.0 installé localement)
- **Opam** >= 2.1 (gestionnaire de paquets OCaml, testé avec 2.4.1)
- **Dune** >= 3.20 (système de build, testé avec 3.20.2)

### Dépendances
- **Necroml** : Compilateur pour le langage dédié
- **Alcotest** : Framework de tests unitaires

### Installation des dépendances

```bash
# Initialiser Opam (si première utilisation)
opam init

# Créer un switch OCaml
opam switch create necro

# Installer les dépendances
opam install dune alcotest camlgpc

# Installer Necroml
eval $(opam env)
opam repository add necro https://gitlab.inria.fr/skeletons/opam-repository.git#necro
opam install necrolib

```

## 🚀 Installation et Utilisation

### Cloner le dépôt

```bash
git clone https://github.com/rombiere/geo_semantics.git
```

### Compiler le projet

```bash
cd geo_semantics
dune build
```

### Exécuter le programme principal

Le programme principal exécute plusieurs exemples d'opérations géométriques :

```bash
dune exec geo_semantics
```

**Sortie attendue :**
```
=== EXEMPLES ===

=== Ex1: A ∩ B, A ∪ B, A Δ B, A \ B ===
A = Poly[(0.0,0.0),(4.0,0.0),(4.0,4.0),(0.0,4.0)]
B = Poly[(2.0,2.0),(6.0,2.0),(6.0,6.0),(2.0,6.0)]
A ∩ B = Poly[(4.0,2.0),(4.0,4.0),(2.0,4.0),(2.0,2.0)] 
A ∪ B = Poly[(4.0,0.0),(4.0,2.0),(6.0,2.0),(6.0,6.0),(2.0,6.0),(2.0,4.0),(0.0,4.0),(0.0,0.0)] 
A Δ B = Poly[(4.0,4.0),(4.0,2.0),(6.0,2.0),(6.0,6.0),(2.0,6.0),(2.0,4.0)]; Poly[(4.0,0.0),(4.0,2.0),(2.0,2.0),(2.0,4.0),(0.0,4.0),(0.0,0.0)]
A \ B = Poly[(4.0,0.0),(4.0,2.0),(2.0,2.0),(2.0,4.0),(0.0,4.0),(0.0,0.0)]

=== Ex2: Associativité : A ∪ (B ∪ C) = (A ∪ B) ∪ C ===
A = Poly[(0.0,0.0),(2.0,0.0),(2.0,2.0),(0.0,2.0)]
B = Poly[(1.0,1.0),(3.0,1.0),(3.0,3.0),(1.0,3.0)]
C = Poly[(2.0,2.0),(4.0,2.0),(4.0,4.0),(2.0,4.0)]
Résultat du test d'égalité: True
Résultat attendu: True

=== Ex3: (A ∪ B) ⊇ (A ∩ B) ===
A = Poly[(0.0,0.0),(2.0,0.0),(2.0,2.0),(0.0,2.0)]
B = Poly[(1.0,1.0),(3.0,1.0),(3.0,3.0),(1.0,3.0)]
Résultat du test d'inclusion: True
Résultat attendu: True

=== Ex4: Distributivité : A ∩ (B ∪ C) = (A ∩ B) ∪ (A ∩ C) ===
A = Poly[(0.0,0.0),(2.0,0.0),(2.0,2.0),(0.0,2.0)]
B = Poly[(1.0,1.0),(3.0,1.0),(3.0,3.0),(1.0,3.0)]
C = Poly[(2.0,2.0),(4.0,2.0),(4.0,4.0),(2.0,4.0)]
Résultat du test d'égalité: True
Résultat attendu: True

=== Ex5: A ∪ B = A Δ B ===
A = Poly[(0.0,0.0),(2.0,0.0),(2.0,2.0),(0.0,2.0)]
B = Poly[(1.0,1.0),(3.0,1.0),(3.0,3.0),(1.0,3.0)]
Résultat du test d'égalité: False
Résultat attendu: False

=== Ex6: Distributivité de la différence sur l'union : A \ (B ∪ C) = (A \ B) ∪ (A \ C) ===
A = Seg[(0.0,0.0)-(3.0,0.0)]
B = Seg[(0.0,0.0)-(1.0,0.0)]
C = Seg[(2.0,0.0)-(3.0,0.0)]
Résultat du test d'égalité: False
Résultat attendu: False

=== FIN ===
```

**Note :** Les A, B et C dans les exemples ne sont pas toujours les mêmes

## 📁 Architecture du projet

```
geo_semantics/
├── README.md                 # Ce fichier
├── dune-project              # Configuration Dune
├── geo_semantics.opam           # Fichier de dépendances Opam
│
├── bin/                      
│   ├── dune                  # Configuration de build
│   ├── geo.sk                # Spécification Skel du langage
│   └── main.ml               # Programme principal avec exemples
│
├── lib/                   
│   ├── dune
│   └── figure_utils.ml       # Utilitaires géométriques (GPC)
│
└── test/                    
    ├── dune
    └── geometry_tests.ml     # Suite de tests des primitives géométriques
```


## 📖 Manuel d'utilisation

### Créer des géométries

#### Points
Un point est défini par ses coordonnées (x, y) :

```ocaml
let p1 = (1.0, 2.0)
let p2 = (3.5, 4.5)
```

#### Segments
Un segment est une paire de points :

```ocaml
let seg = ((0.0, 0.0), (3.0, 0.0))
let seg_geo = Cons (Segment seg, Nil)
```

#### Polygones simples
Un polygone simple (sans trous) est défini par une liste de points formant son contour extérieur :

```ocaml
let poly = { 
  outer = [(0.0, 0.0); (4.0, 0.0); (4.0, 4.0); (0.0, 4.0)]; 
  holes = [] 
}
let poly_geo = Cons (Polygon poly, Nil)
```

#### Polygones avec trous
Un polygone avec trous (holes) inclut des anneaux intérieurs :

```ocaml
let poly_with_holes = { 
  outer = [(0.0, 0.0); (10.0, 0.0); (10.0, 10.0); (0.0, 10.0)]; (* contour extérieur *)
  holes = [
    [(2.0, 2.0); (4.0, 2.0); (4.0, 4.0); (2.0, 4.0)];  (* trou 1 *)
    [(6.0, 6.0); (8.0, 6.0); (8.0, 8.0); (6.0, 8.0)]   (* trou 2 *)
  ]
}
```

### Effectuer des opérations

#### Opérations booléennes (Intersection, Union, Différence)

```ocaml
(* Intersection de deux géométries *)
let a_geo = Cons (Polygon poly_a, Nil)
let b_geo = Cons (Polygon poly_b, Nil)
let result = run_eval(
  LetIn (0, Geometry a_geo,
  LetIn (1, Geometry b_geo,
  Intersection (Var 0, Var 1)))
)

(* Union *)
let result_union = run_eval(
  LetIn (0, Geometry a_geo,
  LetIn (1, Geometry b_geo,
  Union (Var 0, Var 1)))
)

(* Différence (A \ B) *)
let result_diff = run_eval(
  LetIn (0, Geometry a_geo,
  LetIn (1, Geometry b_geo,
  Difference (Var 0, Var 1)))
)

(* Différence symétrique (A Δ B) *)
let result_sym_diff = run_eval(
  LetIn (0, Geometry a_geo,
  LetIn (1, Geometry b_geo,
  SymmetricDifference (Var 0, Var 1)))
)
```

#### Tests d'inclusion et d'égalité

```ocaml
(* Tester si A contient B *)
let contains = run_eval(
  LetIn (0, Geometry a_geo,
  LetIn (1, Geometry b_geo,
  Includes (Var 0, Var 1)))
)

(* Tester l'égalité entre deux géométries *)
let equal = run_eval(
  LetIn (0, Geometry a_geo,
  LetIn (1, Geometry b_geo,
  Eq (Var 0, Var 1)))
)
```

#### Chaîner les opérations

```ocaml
(* Exemple: (A ∩ B) ∪ C *)
let result = run_eval(
  LetIn (0, Geometry a_geo,
  LetIn (1, Geometry b_geo,
  LetIn (2, Geometry c_geo,
  LetIn (3, Intersection (Var 0, Var 1),
  Union (Var 3, Var 2)))))
)
```

### Afficher les résultats

```ocaml
Printf.printf "Résultat: %s\n" (string_of_value result)
```


## 🧪 Tests

Le projet utilise **Alcotest** pour les tests unitaires. Les tests couvrent les implémentations primitives géométriques en OCaml.
### Lancer les tests
```bash
dune test
```

Deux tests échouent en raison des limitations suivantes :

Les polygones partageant plusieurs arêtes présentent des instabilités dans la bibliothèque GPC lors du calcul des intersections ou unions, provoquant des résultats imprévisibles et des échecs d'assertions dans les tests. Ces instabilités numériques sont des limitations inhérentes de la dépendance de la bibliothèque géométrique utilisée.

## 📚 Références

- [Necroml](https://skeletons.inria.fr/) - Skeletal semantics
- [camlgcp](https://github.com/johnwhitington/camlgpc) - OCaml interface to Alan Murta's General Polygon Clipper.
- [Dune](https://dune.readthedocs.io/) - Documentation Dune
- [Alcotest](https://github.com/mirage/alcotest) - Framework de tests

## 👥 Auteur

Projet réalisé par Paul Laurent dans le cadre du cours ASM (Advanced Semantics) du M2 SIF

