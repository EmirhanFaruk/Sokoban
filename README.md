# Sokoban

Sokoban est un jeu de puzzle classique développé en OCaml, où le joueur doit déplacer des caisses sur des cibles spécifiques dans un environnement à labyrinthe. Ce projet inclut un système de menus et une gestion de modes de terminal (canoniques et non-canoniques) pour une expérience utilisateur optimisée.


## Table des matières

- [Aperçu du jeu](#aperçu-du-jeu)
- [Installation](#installation)
  - [Installation d'OCaml](#installation-docaml)
  - [Clonage de Projet et Installation des Dépendances](#clonage-de-projet-et-installation-des-dépendances)
- [Utilisation](#utilisation)
- [OS](#os)
- [Contrôles principaux pendant la partie](#contrôles-principaux-pendant-la-partie)
- [Remerciements](#remerciements)


## Aperçu du jeu

Dans Sokoban, vous incarnez un personnage qui doit déplacer des caisses vers des emplacements marqués (les cibles) en poussant chaque caisse une par une. Les mouvements doivent être stratégiques, car une fois une caisse coincée dans un coin, elle ne peut plus être déplacée. L'objectif est de placer toutes les caisses sur les cibles du niveau et de le faire avec le moins de mouvement possibles.

## Installation

### Installation d'OCamL

1. Pour commencer il faut installer opam:
    ```bash
    sudo apt install opam
    ```
  
2. Puis on active opam:
    ```bash
    opam init
    ```

3. Après avoir activé opam, il faut installer OCamL et changer l'envrionnement de celui ci:
    ```bash
    opam switch create 4.14.1
    eval $(opam env)
    ```

### Clonage de Projet et Installation des Dépendances


1. Clonez le dépôt :

    Sous format SSH :
    ```bash
    git clone git@moule.informatique.univ-paris-diderot.fr:pan/ocaml-project.git
    ```
    ou sousformat HTTPS :
     ```bash
    git clone https://moule.informatique.univ-paris-diderot.fr/pan/ocaml-project.git
     ```

    Puis, rendez-vous dans le répertoire du projet :
    ```bash
    cd sokoban
    ```
   
2. Installer les dépendances:

    Pour installer les dépendances de jeu:
    ```bash
    opam install . --deps-only
    ```

    - Si ça ne marche pas, vous pouvez les installer manuellement:
    ```bash
    opam install dune unix
    ```
    

3. Compilez le projet :
    ```bash
    dune build
    ```

## Utilisation

Lancez le jeu avec la commande suivante :

```bash
dune excec sokoban
```

Dans le jeu, vous pouvez naviguer dans le menus pour ***lancer une nouvelle partie***, ***lire les règles***, regarder le ***scoreboard*** et ***quitter le jeu***.


## OS 

Lors de l'exécution du programme, si vous êtes sous un systeme **Windows**, vous n'aurez pas accès au mode non canonique du terminale, c'est a dire que chaque entrée que vous donnez au terminale, vous devrez appyer sur ***Entrée*** pour qu'elle soit prise en compte. Sinon le jeu est sous format non canonique et chaque entrée que vous faites au terminal sera prise en compte immédiatement, sauf quand nous vous demanderons de donner un pseudo.


## Contrôles principaux pendant la partie

- **Déplacer le personnage** : 

Sous Unix/Linux nous utilisons les fleches directionnelles:
- `↑` : haut
- `↓` : bas
- `←` : gauche
- `→` : droite

Sous Windows, nous utilisons les touches suivante:
- `W` ou `Z` : haut
- `S` : bas
- `A` ou `Q` : gauche
- `D` : droite


- **Recommencer le niveau** : En utilisant la touche `R`.
- **Annuler un déplacement** : En utilisant la touche `U`.
- **Rétablir un déplacement** : En utilisant la touche `I`.
- **Retourner au menu** : En utilisant la touche `X`.

## Remerciements

Merci aux créateurs de **Sokoban** original pour ce jeu de puzzle intemporel.

### Bon jeu a vous ! 



