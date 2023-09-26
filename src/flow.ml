module type S = sig
  type t
  type error

  val pp_error : error Fmt.t
  val read : t -> ([ `Data of Cstruct.t | `End_of_input ], error) result
  val writev : t -> Cstruct.t list -> (unit, error) result
  val close : t -> unit
end
