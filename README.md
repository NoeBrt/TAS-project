```
    __                    __        __            ______                __
   / /   ____ _____ ___  / /_  ____/ /___ _      / ____/___ _____ ___  / /
  / /   / __ `/ __ `__ \/ __ \/ __  / __ `/_____/ /   / __ `/ __ `__ \/ /
 / /___/ /_/ / / / / / / /_/ / /_/ / /_/ /_____/ /___/ /_/ / / / / / / /
/_____/\__,_/_/ /_/ /_/_.___/\__,_/\__,_/      \____/\__,_/_/ /_/ /_/_/

```

**EN COURS**

**Ce qui a été fait pour l'instant:**
Tout jusqu'a la partie 3.5 Unification



Project Ocaml Dune pour simuler un evaluateur-typeur de Lambda calcul.

## Prérequis

- OCaml (recommandé: 4.12+)
- Dune (>= 2.0)
- opam (optionnel, pour gérer les dépendances)

```bash
# Créer un switch local (optionnel)
opam switch create . --empty

# Installer les dépendances du projet
opam install . --deps-only
```

## Setup

```bash
# Compiler le projet
dune build

# Exécuter les tests
dune runtest

# Run main
dune exec main
```

## Structure

```bash
eval-typeur
├── bin
│   └── main.ml
├── lib
│   ├── ast.ml # definition des types
│   ├── parser.ml # lexer - parser
│   └── printer.ml # pretty printer
└── test
    └── test_lambda.ml
```
