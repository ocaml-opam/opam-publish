(**************************************************************************)
(*                                                                        *)
(*    Copyright 2026 Kate Deplaix                                         *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module String : sig
  val ends_with : suffix:string -> string -> bool
end
