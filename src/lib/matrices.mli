(** Provide a thin matrix interfaces for [float array array]'s. *)

type t = Vectors.t array

(** [row m i] returns the [i]th row of matrix [m].*)
val row : t -> int -> Vectors.t

(** [column m i] returns the [i]th colum of matrix [m].*)
val column : t -> int -> Vectors.t

(** [dim m] the dimensions (row, columns) of the matrix [m]. *)
val dim : t -> int * int

(** [transpose m] returns the transpose of [m]. *)
val transpose : t -> t

(** [diagonal v] create a diagonal matrix from vector [v]. *)
val diagonal : Vectors.t -> t

(** [equal d x y] two matrices are equal if they have the same dimensions
  and all pairwise elements are not [Util.significantly_different_from ?d]
  from each other. *)
val equal : ?d:float -> t -> t -> bool

(** [add x y] add two matrices. *)
val add : t -> t -> t

(** [sub x y] subtraction. *)
val sub : t -> t -> t

(** [mult s v] scalar multiplication. *)
val mult : float -> t -> t

(** [identity n] create the identity matrix of rank [n]. *)
val identity : int -> t

(** [prod m n] matrix product [m * n]

    @raise Invalid_argument if matrix sizes are not compatible. *)
val prod : t -> t -> t

val prod_row_vector : Vectors.t -> t -> Vectors.t

val prod_column_vector : t -> Vectors.t -> Vectors.t
