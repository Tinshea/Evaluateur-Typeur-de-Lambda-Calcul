# Évaluateur et Typeur de λ-calcul

## Description

Un évaluateur et typeur pour le λ-calcul étendu avec des références, des listes et le polymorphisme. Le projet implémente un système de types à la Hindley-Milner avec polymorphisme faible pour les références.

## Prérequis

- OCaml (>= 4.13.0)
- Dune (>= 2.0)
- Menhir
- OUnit2 (pour les tests)

## Structure du Projet

Le projet est organisé en plusieurs fichiers :

- **`lambda.ml`** : Définit la représentation des termes du λ-calcul (AST) ainsi que les fonctions d'impression pour afficher les termes de manière lisible.
  
- **`types.ml`** : Définit la représentation des types simples et contient des fonctions pour générer des équations de typage à partir des termes.
  
- **`eval.ml`** : Contient les fonctions pour évaluer les termes du λ-calcul, y compris la substitution et la réduction selon la stratégie Call-by-Value.


## Installation et Compilation

1. Installez les dépendances nécessaires via OPAM :
   ```bash
   opam install dune menhir ounit2
   ```

2. Clonez le dépôt :
   ```bash
   git clone <URL_DU_DEPOT>
   cd Evaluateur-Typeur-de-Lambda-Calcul
   ```

3. Compilez le projet avec dune :
   ```bash
   dune build
   ```

## Utilisation

Pour exécuter le programme et tester l'évaluateur et le typeur, utilisez la commande suivante :
```bash
dune exec src/test_name.exe
```

## Fonctionnalités

### Types supportés
- Types simples (→)
- Types polymorphes (∀)
- Types de base (Int, Bool)
- Types références (Ref)
- Types listes
- Type Unit

### Constructions supportées
- Abstraction (λx.t)
- Application
- Let-binding
- Références (création, déréférencement, assignation)
- Listes (création, head, tail)
- Opérations arithmétiques
- Conditionnelles
- Point fixe (pour la récursion)

### Système de types
- Inférence de types à la Hindley-Milner
- Polymorphisme paramétrique
- Polymorphisme faible pour les références
- Occur check pour éviter les types récursifs invalides
- Gestion des variables de types fraîches

## Ajout Bonus : Parser et Gestion des Exceptions

### Tests du Parser (`test_parser.ml`)
- Vérifie l'analyse syntaxique des termes
- Tests des constructions de base (lambda, application, let)
- Tests des références et listes
- Tests des opérateurs arithmétiques

### Gestion des Exceptions
Une gestion partielle des exceptions a également été mise en place, avec des tests associés dans eval_test.ml.

Pour exécuter :


### Remerciements

- **Yanis Tabellout** : Pour son aide précieuse dans la gestion des adresses de la partie 5.1.
- **Tests assistés par IA** : Une partie des tests a été générée et validée avec l'aide de ChatGPT et Claude 3.5.
- **Débogage assisté** : Le débogage a bénéficié d'une assistance IA pour simplifier et optimiser le code.
- **Ressource académique** : L'évaluateur de calcul lambda en OCaml, tel que présenté par Christophe Deleuze, a servi de référence technique (disponible [ici](https://raw.githubusercontent.com/wiki/cdeleuze/lambda.ml/lambda.pdf)).



