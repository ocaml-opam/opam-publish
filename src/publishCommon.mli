(**************************************************************************)
(*                                                                        *)
(*    Copyright 2026 Kate Deplaix                                         *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

val git_query : ?dir:OpamFilename.Dir.t -> string list -> string option

val git_command :
  ?dir:OpamFilename.Dir.t -> ?verbose:bool -> string list -> unit

val repo_of_dir :
  ?remote:string -> OpamFilename.Dir.t -> (string * string) option

val tag_of_dir : OpamFilename.Dir.t -> string option
