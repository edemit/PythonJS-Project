
pour le typage (Typing) notre code se structure de cette manière 
tp_stmt vérifie : 
    - les assignations qui est effectué de la manière suivante, dans tp_expr il y a 4 possibilitées:

        - l'expression est une constante, on renvoie donc juste son type 
        - l'expression est une variable, on vérifie donc qu'elle est deja déclaré dans l'environnement 
        - l'expression est une opération, on vérifie que l'opération est entre deux types qui font sens,
            par exemple une opération aritmétique ne se fait pas entre des booléens ou une comparaison entre un string et un int
        - pour finir si l'expression est une fonction, on vérifie que ses paramètres de la fonctions sont bon en quantité et typages ou si la fonction existe dans l'environnement

    - les Blocks qui lis les statements a la suite en mettant a jour l'environnement
    - les Returns qui renvoie le type du retour 
    - les Whiles qui verifie le type des variables dans la boucle
    - les Conds on verif que on a une condition booléenne puis on fusionne les types de retour


et l'environnement est mit a jour à chaque assignation, 


    difficulté de cette partie 
        j'ai eu des problèmes sur comment réutiliser tp_expr dans tp_callE, 
        j'ai pu m'aider de copilot pour apprendre l'existance de "List.assoc","and" et aussi le "try" que j'utilise pour callE a la fin de tp_expr
        j'ai reçu aussi de l'aide pour la fonction tp_stmt du groupe chris/angel qui ont mieux compris le sujet que moi notamment sur le fixpoint du While que je n'arrivais pas realiser et sur le Block 

        je n'ai pas eu le temps de faire tp_fundefn 

Etape 3:
    Parser : ajout de fonctions avec les types : return type, union types, appels dans les expressions, != et variables locales

    Pprinter : génération de code HTML pour les schémas input/print, sinon JS standard

    Typing : correction des vérifications de types, blocs, returns, whiles, conds...

    Interf : suppression des affichages supplémentaires afin de pouvoir enregistrer directement le résultat au format .html