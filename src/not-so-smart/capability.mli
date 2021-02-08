(** Capabilities module.

    When the client talks with the server, it needs to inform capabilities (what
    it can handle). This is the exhaustive list of capabilities on the current
    Smart protocol. Then, the server responds too with capabilities.

    The common part between the client and the server of capabilities should
    diverge how we handle the Smart protocol. For example, if the client does
    not allow [`Shallow] objects, we permit to define shallow objects on the API
    of the fetch command but we don't use them to notice to the server. *)

type t = [ Capability_v1.t | Capability_v2.t ]

val to_string : t -> string
(** [to_string c] returns a string representaiton of the capability [c]. *)

exception Capability_expect_value of string
(** Exception to inform than the capability expects a value. *)

val of_string : ?protocol_v:[> `V1 | `V2 ] -> ?value:string -> string -> t
(** [of_capability s] tries to decode [s] to a capability. If the capability
    excepts a value, we raise [Capability_expect_value].

    [protocol_v] has default value [`V1].

    @raise Capability_expect_value if capability (for protocol v1) expects a value
            but value argument isn't given. *)

val pp : t Fmt.t
(** Pretty-printer of {!t}. *)

val filter_by : protocol_v:[> `V1 | `V2 ] -> t list -> t list
(** filters a capability list by protocol version *)

val compare : t -> t -> int
(** Comparison function of {!t}. *)

val equal : t -> t -> bool
(** Equal function of {!t}. *)
