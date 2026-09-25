(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open Containers
open Js_of_ocaml_compiler

let tailcall = Tailcall.f
let deadcode program = fst (Deadcode.f (Pure_fun.f program) program)

let inline program =
  let program, variables_uses = Deadcode.f (Pure_fun.f program) program in
  Inline.f ~profile:Profile.O3 program variables_uses

let flow_simple program = Flow.f program
let flow = Flow.f
let phi = Phisimpl.f
let eval (p, info) = Eval.f info p

let specialize' (program, flow_info) =
  let return_values = Code.return_values program in

  let shape, set_shape =
    Flow.the_shape_of ~return_values ~pure:Pure_fun.empty ~blocks:false
      flow_info
  in

  let program =
    Specialize.f ~shape ~set_shape
      ~update_def:(fun x expr -> Flow.Info.update_def flow_info x expr)
      program
    |> Specialize_js.f flow_info
  in

  (program, flow_info)

let specialize p = fst (specialize' p)
(* let ( +> ) f g x = g (f x) *)

let round1 =
  Fun.(tailcall %> inline %> deadcode %> flow_simple %> specialize' %> eval)

let rec loop max round i p =
  let p' = round p in
  if i >= max || Code.equal p' p then p' else loop max round (i + 1) p'

let exact_calls ~deadcode_sentinel program =
  let _, info = Global_flow.f ~fast:false program in

  let program =
    Global_deadcode.f ~deadcode_sentinel (Pure_fun.f program) program info
  in

  Specialize.f
    ~shape:(fun f ->
      match Global_flow.function_arity info f with
      | None -> Shape.top
      | Some arity -> Shape.funct ~arity ~pure:false ~res:Shape.top)
    ~set_shape:(fun _ _ -> ())
    ~update_def:(fun x expr -> Global_flow.update_def info x expr)
    program

let o1 =
  Fun.(
    tailcall %> flow_simple %> specialize' %> eval %> inline %> deadcode
    %> tailcall %> phi %> flow %> specialize' %> eval %> inline %> deadcode
    %> flow %> specialize' %> eval %> inline %> deadcode %> phi %> flow
    %> specialize)

let round2 = Fun.(flow %> specialize' %> eval %> deadcode %> o1)
let o3 = Fun.(loop 10 round1 1 %> loop 10 round2 1)

let optimizes_program =
  let deadcode_sentinel = Code.Var.fresh_n "undef" in
  Fun.(o3 %> deadcode %> exact_calls ~deadcode_sentinel %> deadcode)
