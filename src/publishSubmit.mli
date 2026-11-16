(**************************************************************************)
(*                                                                        *)
(*    Copyright 2026 Kate Deplaix                                         *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

type github_repo = string * string (* owner, name *)

val submit :
  OpamFilename.Dir.t ->
  token:string option ->
  dry_run:bool ->
  output_patch:OpamFilename.t option ->
  no_browser:bool ->
  github_repo ->
  string option ->
  string ->
  string ->
  OpamPackage.t list ->
  (string * (string * int) option) list ->
  unit
