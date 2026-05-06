Typage (dans le fichiier typing)
    La fonction tp_exprr permet de déterminer le type d’une expression:

        -Constante : renvoie le type directement

        -Variable : on vérifie que la variable est déclarée dans l’environnement, puis on récupère son type

        -Opération binaire : on vérifie que les types des deux opérandes sont compatibles

        -Appel de fonction CallE : on vérifie que la fonction existe, que le nombre d’arguments est correct, et que les types sont compatibles avec les paramètres

    Vérification des instructions (fonction tp_stmt):

        -Assignation : on calcule le type de l’expression dans Tp_expr et on met à jour l’environnement avec ce type

        -Block : les instructions sont traitées a la suite en propageant les types dans l’environnement

        -Return : on calcule le type de l’expression retournée et on l’accumule avec les autres types de retour possibles

        -Condition : on vérifie que la condition est de type booléen, puis on analyse les deux branches et on fusionne leurs environnements et types de retour

        -Boucle While : on applique un calcul de point fixe pour déterminer un environnement stable après la boucle

        -Appel de fonction (`CallS`) : guère les instructions.



Etape 3:
    Parser : ajout de fonctions avec les types : return type, union types, appels dans les expressions, != et variables locales

    Pprinter : génération de code HTML pour les schémas input/print, sinon JS standard

    Typing : correction des vérifications de types, blocs, returns, whiles, conds...

    Interf : suppression des affichages supplémentaires afin de pouvoir enregistrer directement le résultat au format .html


Note d'Alexis, concernant la partie typing :

    Difficultés rencontrées
        -Compréhension de l'arborescence de classe et de son usage au debut du projet.
        -La fonction tp_fundefn n’a pas été implémentée faute de temps.
        -Implémentation de toute les parties de tp_stmt, en particulier (aide du groupe angel/chris ):
            -le calcul de point fixe pour les boucles while
            -la gestion des blocs

    Usage de copilote 
        -aide dans la fonction callE pour créer une mutuellement récursives avec `and`
        -decouverte de raccourci pour raccourcir mon code comme `List.assoc` et `try with`.
        
Note de Florian, concernant le partie parser : 
    J'ai perdu du temps au début parce que j'avais tenté de créer des statements non demandés (comme par exemple le statement elif), et j'ai compris plus tard que ceux-ci n'étaient pas demandées, je les ai donc supprimés
    J'ai du m'aider de ChatGPT pour réussir à correctement traduire la grammaire Python en Ocaml pour les statements function_body et function_def
    J'ai également recu l'aide de Chris, du groupe Chris/Angel, qui m'a aidé à comprendre comment intégrer les expressions
