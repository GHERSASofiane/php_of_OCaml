type user = { 
  id: int;
  mutable nom: string; 
  mutable date_naissance: string;
  mutable mail: string;
  mutable telephone: string;
}

val write : user -> unit
val search : int -> user
val delete : int -> unit
val list : unit -> user list

