# Évaluateur et Typeur de λ-calcul

Un rapport de projet est situé à la racine du répertoire du projet. Il contient une description détaillée de l'implémentation de l'évaluateur et du typeur pour le λ-calcul pur, ainsi que des explications sur les différentes parties du projet et leur évolution au cours du développement.

## Description

Un évaluateur et typeur pour le λ-calcul pur.

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

- **`syntax.ml`** : Contient la définition des types simples et les fonctions associées pour générer les équations de typage à partir des termes.

- **`test_types.ml`** : Contient une suite de tests pour vérifier le bon fonctionnement du système de types, y compris les tests pour la généralisation, l'unification et l'inférence de types.

- **`test_eval.ml`** : Contient les tests pour vérifier l'évaluation des termes, incluant les tests pour les fonctionnalités impératives et la gestion des exceptions.


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
# Exécute les tests du typeur
dune exec src/test_types.exe

# Exécute les tests de l'évaluateur
dune exec src/test_eval.exe

# Exécute les tests du parseur/lexeur
dune exec src/test_parser.exe
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
- Inférence de types
- Polymorphisme paramétrique
- Polymorphisme faible pour les références
- Occur check pour éviter les types récursifs invalides
- Gestion des variables de types fraîches

## Ajout Bonus : Parser/Lexer et Gestion des Exceptions

### Parser/Lexer
- Vérifie l'analyse syntaxique des termes
- Tests des constructions de base (lambda, application, let)
- Tests des références et listes
- Tests des opérateurs arithmétiques

### Gestion des Exceptions
Une gestion partielle des exceptions a également été mise en place, avec des tests associés dans eval_test.ml.

Pour exécuter :


### Aides et Remerciements

- **Yanis Tabellout** : Pour son aide précieuse dans la gestion des adresses de la partie 5.1.
- **Tests assistés par IA** : Une partie des tests a été générée et validée avec l'aide de ChatGPT et Claude 3.5.
- **Débogage assisté** : Le débogage a bénéficié d'une assistance IA pour simplifier et optimiser le code.
- **Ressource** : L'évaluateur de calcul lambda en OCaml, tel que présenté par Christophe Deleuze, a servi de référence technique (disponible [ici](https://raw.githubusercontent.com/wiki/cdeleuze/lambda.ml/lambda.pdf)).



