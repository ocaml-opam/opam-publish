(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(* This module contains functions that was in `OpamStd` but where moved
   to `OpamCompat`. We keep an compatibility module to be able to not depend
   only on the last unreleased version of `OpamStd`. *)
module String = struct
  (** NOTE: OCaml >= 4.13 *)
  let ends_with ~suffix s =
    let x = String.length suffix in
    let n = String.length s in
    n >= x &&
    let rec chk i = i >= x || suffix.[i] = s.[i+n-x] && chk (i+1) in
    chk 0
end
