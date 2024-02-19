module Float : sig
  include module type of Batteries.Float

  val pp : Format.formatter -> t -> unit
end

module Int8 : Num_interfaces.NumLike
module UInt8 : Num_interfaces.NumLike
module UInt32 : Num_interfaces.NumLike
module UInt64 : Num_interfaces.NumLike
