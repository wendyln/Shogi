(** A module to encapsulate the function to call a Game state with the potential
    for random actions to occur. *)

val crazy_step : State.game -> string -> State.game
(** [crazy_step game command] is a State.game type that takes in a command and a
    previous game state and has the potential for "crazy" actions within the
    game. *)