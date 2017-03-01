(* 
   Module CSV

   Lecture de fichiers CSV.

**)


exception Fichier_mal_forme

(** [lire_csv f] lit le fichier f au format CSV et retourne le contenu
    sous forme d'une liste de tableaux.

    Leve l'exception Fichier_mal_forme si le nombre de champs par
    ligne n'est pas coherent.
*)
val lire_csv : string -> string list list


