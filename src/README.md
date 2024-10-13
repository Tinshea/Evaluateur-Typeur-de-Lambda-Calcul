
# Évaluateur et Typeur de λ-calcul

## Description

Ce projet a pour objectif de construire un évaluateur et un typeur pour le λ-calcul, un modèle formel de calcul qui est à la base des langages de programmation fonctionnels. Le projet permet de simuler l'exécution de programmes écrits en λ-calcul et de vérifier la validité des types de ces programmes.

## Structure du Projet

Le projet est organisé en plusieurs fichiers :

- **`lambda.ml`** : Définit la représentation des termes du λ-calcul (AST) ainsi que les fonctions d'impression pour afficher les termes de manière lisible.
  
- **`types.ml`** : Définit la représentation des types simples et contient des fonctions pour générer des équations de typage à partir des termes.
  
- **`eval.ml`** : Contient les fonctions pour évaluer les termes du λ-calcul, y compris la substitution et la réduction selon la stratégie Call-by-Value.

- **`unify.ml`** : Implémente l'algorithme d'unification pour résoudre les équations de typage générées par le typeur.

- **`main.ml`** : Le point d'entrée du programme qui teste l'évaluateur et le typeur sur des exemples de termes.

## Installation

1. Assurez-vous d'avoir OCaml installé sur votre machine. Vous pouvez installer OCaml via OPAM, le gestionnaire de paquets OCaml.
   
2. Clonez ce dépôt sur votre machine :
   ```bash
   git clone <URL_DU_DEPOT>
   ```

3. Naviguez vers le dossier du projet :
   ```bash
   cd nom_du_projet
   ```

4. Compilez les fichiers OCaml :
   ```bash
   ocamlc -o evaluateur lambda.ml types.ml eval.ml unify.ml main.ml
   ```

## Utilisation

Pour exécuter le programme et tester l'évaluateur et le typeur, utilisez la commande suivante :
```bash
./evaluateur
```

## Exemples

Le programme inclut des exemples de termes dans le fichier `main.ml`, où vous pouvez voir comment les termes sont évalués et typés.

## Contributions

Les contributions sont les bienvenues ! Si vous souhaitez ajouter des fonctionnalités ou corriger des bogues, n'hésitez pas à soumettre une pull request.

## Licence

Ce projet est sous licence MIT. Consultez le fichier LICENSE pour plus de détails.
